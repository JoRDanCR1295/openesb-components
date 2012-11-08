/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)MarshalHandler.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.swift.runtime.provider;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaParticle;
import org.apache.xmlbeans.SchemaProperty;
import org.apache.xmlbeans.SchemaType;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;

import com.sun.encoder.swift.runtime.provider.SwiftCharEscapeCoder.Result;

/**
 * This class implements SAX <code>ContentHandler</code> interface.
 * It translates SAX events into Swift standard encoding rule encoded data.
 * 
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
public final class MarshalHandler implements ContentHandler {

    private final String mCharset;
    private final OutputStream mOutputStream;
    private final SchemaGlobalElement mRootElement;
    private Writer mWriter;
    private DelimDataWriter mDelimDataWriter;
    private MarshalerFactory mMarshalFactory = new MarshalerFactory();
    private boolean mReceivedFirst = false;
    private Marshaler mMarshaler;
    private String mGroupNamePrefix;
    
    public MarshalHandler(SchemaGlobalElement element, OutputStream output) {
    	this(element, output, "ASCII");
    }
    
    public MarshalHandler(SchemaGlobalElement element, OutputStream output,
            String charset) {
        if (element == null) {
            throw new NullPointerException("no element");
        }
        mRootElement = element;
        if (output == null) {
            throw new NullPointerException("no output");
        }
        mOutputStream = output;
        mWriter = null;
        mCharset = charset;
        mDelimDataWriter = null;
        mGroupNamePrefix = element.getName().getLocalPart() + ".";
    }
    
    public MarshalHandler(SchemaGlobalElement element, Writer output) {
        if (element == null) {
            throw new NullPointerException("no element");
        }
        mRootElement = element;
        if (output == null) {
            throw new NullPointerException("no output");
        }
        mWriter = output;
        mOutputStream = null;
        mCharset = null;
        mDelimDataWriter = null;
        mGroupNamePrefix = element.getName().getLocalPart() + ".";
    }
    
    public void setDocumentLocator(Locator locator) {
        //no-op
    }

    public void startDocument() throws SAXException {
        if (mWriter == null) {
            if (mCharset != null) {
                try {
                    mWriter = new OutputStreamWriter(mOutputStream, mCharset);
                } catch (UnsupportedEncodingException e) {
                    throw new SAXException(e);
                }
            } else {
                try {
                    mWriter = new OutputStreamWriter(mOutputStream, "ASCII");
                } catch (UnsupportedEncodingException e) {
                    throw new SAXException(e);
                }
            }
        }
        mDelimDataWriter = new DelimDataWriter(mWriter);
    }

    public void endDocument() throws SAXException {
    	try {
			mWriter.flush();
		} catch (IOException e) {
			throw new SAXException(e);
		}
    }

    public void startPrefixMapping(String prefix, String uri)
            throws SAXException {
        //no-op
    }

    public void endPrefixMapping(String prefix) throws SAXException {
        //no-op
    }

    public void startElement(String uri, String localName, String qName,
            Attributes atts) throws SAXException {
        if (!mReceivedFirst) {
            if (!mRootElement.getName().getLocalPart().equals(localName)) {
                throw new SAXException(
                        "Expecting '" + mRootElement.getName()
                        + "', found '{"
                        + uri + "}" + localName + "'");
            }
            mMarshaler = mMarshalFactory.getDocumentMarshaler(mRootElement,
                    mDelimDataWriter, mGroupNamePrefix);
            mReceivedFirst = true;
        } else {
            mMarshaler = mMarshaler.startElement(uri, localName, qName, atts);
        }
    }

    public void endElement(String uri, String localName, String qName)
            throws SAXException {
        mMarshaler = mMarshaler.endElement(uri, localName, qName);
    }

    public void characters(char[] ch, int start, int length)
            throws SAXException {
        mMarshaler.characters(ch, start, length);
    }

    public void ignorableWhitespace(char[] ch, int start, int length)
            throws SAXException {
        // TODO Auto-generated method stub

    }

    public void processingInstruction(String target, String data)
            throws SAXException {
        //no-op
    }

    public void skippedEntity(String name) throws SAXException {
        //no-op
    }
    
    public String getGroupNamePrefix() {
        return mGroupNamePrefix;
    }

    public void setGroupNamePrefix(String groupNamePrefix) {
        mGroupNamePrefix = groupNamePrefix;
    }

    private static boolean isSimpleContent(SchemaType XMLType) {
        if (XMLType.isSimpleType()
                || XMLType.getContentType() == SchemaType.SIMPLE_CONTENT
                || XMLType.getContentType() == SchemaType.MIXED_CONTENT
                || XMLType.isURType()) {
            return true;
        }
        return false;
    }
    
    private interface Marshaler {
        
        public Marshaler startElement(String uri, String localName,
                String qName, Attributes atts) throws SAXException;
        
        public Marshaler endElement(String uri, String localName,
                String qName) throws SAXException;
        
        public void characters(char[] ch, int start, int length)
                throws SAXException;
    }
    
    private static final class DelimDataWriter {

        public SwiftCharEscapeCoder mEscapeCoder;
        
        private final Writer mWriter;
        
        private char mSegSep = '\r';
        private char mFieldSep = '|';
        private char mCompoSep = '^';
        private char mRepetSep = '~';
        private char mEscape = '\\';
        private char mSubCompoSep = '&';
        
        public DelimDataWriter(Writer writer) {
            mWriter = writer;
        }
        
        void writeSegName(String segName) throws SAXException {
            try {
                mWriter.write(segName);
            } catch (IOException e) {
                throw new SAXException(e);
            }
        }
        
        void writeSegSep() throws SAXException {
            try {
                mWriter.write(mSegSep);
            } catch (IOException e) {
                throw new SAXException(e);
            }
        }
        
        void writeFieldSep() throws SAXException {
            try {
                mWriter.write(mFieldSep);
            } catch (IOException e) {
                throw new SAXException(e);
            }
        }
        
        void writeCompoSep() throws SAXException {
            try {
                mWriter.write(mCompoSep);
            } catch (IOException e) {
                throw new SAXException(e);
            }
        }
        
        void writeSubCompoSep() throws SAXException {
            try {
                mWriter.write(mSubCompoSep);
            } catch (IOException e) {
                throw new SAXException(e);
            }
        }
        
        void writeRepetSep() throws SAXException {
            try {
                mWriter.write(mRepetSep);
            } catch (IOException e) {
                throw new SAXException(e);
            }
        }
        
        void writeData(String data) throws SAXException {
            try {
                mWriter.write(data);
            } catch (IOException e) {
                throw new SAXException(e);
            }
        }
        
        /**
         * This method is used to write out character data when characters
         * event is fired.
         * 
         * @param data character array
         * @param start start position of the data
         * @param length length of the data
         * @throws SAXException exception happening during writing
         */
        void writeData(char[] data, int start, int length) throws SAXException {
            try {
                if (mEscapeCoder != null) {
                    mEscapeCoder.escape(data, start, length);
                    if (mEscapeCoder.hasResult()) {
                        Result res = mEscapeCoder.getResult();
                        mWriter.write(res.mData, res.mStart, res.mCount);
                        return;
                    }
                }
                mWriter.write(data, start, length);
            } catch (IOException e) {
                throw new SAXException(e);
            }
        }
        
        void endData() throws SAXException {
            
        }
        
        char getSegSep() {
            return mSegSep;
        }
        
        void setSegSep(char sep) {
            mSegSep = sep;
        }
        
        char getFieldSep() {
            return mFieldSep;
        }
        
        void setFieldSep(char sep) {
            mFieldSep = sep;
        }
        
        char getComponentSep() {
            return mCompoSep;
        }
        
        void setComponentSep(char sep) {
            mCompoSep = sep;
        }
        
        char getSubCompoSep() {
            return mSubCompoSep;
        }
        
        void setSubCompoSep(char sep) {
            mSubCompoSep = sep;
        }
        
        char getRepetSep() {
            return mRepetSep;
        }
        
        void setRepetSep(char sep) {
            mRepetSep = sep;
        }

        public char getEscapeChar() {
            return mEscape;
        }

        public void setEscapeChar(char escape) {
            mEscape = escape;
        }
    }
    
    private static final class MarshalerFactory {
        
        private DocumentMarshaler mDocumentMarshaler = null;
        private SegmentMarshaler mSegmentMarshaler = null;
        private FieldMarshaler mFieldMarshaler = null;
        private ComponentMarshaler mComponentMarshaler = null;
        private WildcardMarshaler mWildcardMarshaler = null;
        private PseudoGrpMarshaler mPseudoGrpMarshaler = null;
        private MSHSegmentMarshaler mMSHSegmentMarshaler = null;
        
        Marshaler getDocumentMarshaler(SchemaGlobalElement rootElement,
                DelimDataWriter writer, String pseudoGroupPrefix) {
            if (mDocumentMarshaler == null) {
                mDocumentMarshaler =
                    new DocumentMarshaler(rootElement, writer,
                            pseudoGroupPrefix, 1);
                mDocumentMarshaler.setFactory(this);
            } else {
                mDocumentMarshaler.reset(rootElement.getType());
            }
            return mDocumentMarshaler;
        }
        
        Marshaler getSegmentMarshaler(Marshaler parent, QName name,
                SchemaType type, DelimDataWriter writer) {
            if (mSegmentMarshaler == null) {
                mSegmentMarshaler =
                    new SegmentMarshaler(parent, name, type, writer);
                mSegmentMarshaler.setFactory(this);
            } else {
                mSegmentMarshaler.reset(parent, name, type);
            }
            return mSegmentMarshaler;
        }
        
        Marshaler getFieldMarshaler(Marshaler parent, QName name,
                SchemaType type, DelimDataWriter writer) {
            if (mFieldMarshaler == null) {
                mFieldMarshaler =
                    new FieldMarshaler(parent, name, type, writer);
                mFieldMarshaler.setFactory(this);
            } else {
                mFieldMarshaler.reset(parent, name, type);
            }
            return mFieldMarshaler;
        }
        
        Marshaler getComponentMarshaler(Marshaler parent, QName name,
                SchemaType type, DelimDataWriter writer) {
            if (mComponentMarshaler == null) {
                mComponentMarshaler =
                    new ComponentMarshaler(parent, name, type, writer);
            } else {
                mComponentMarshaler.reset(parent, name, type);
            }
            return mComponentMarshaler;
        }
        
        Marshaler getWildcardMarshaler(Marshaler parent, QName name,
                DelimDataWriter writer, int level) {
            if (mWildcardMarshaler == null) {
                mWildcardMarshaler =
                    new WildcardMarshaler(parent, name, writer, level);
            } else {
                mWildcardMarshaler.reset(parent, name, level);
            }
            return mWildcardMarshaler;
        }
        
        Marshaler getPseudoGrpMarshaler(Marshaler parent, QName name,
                SchemaType type, DelimDataWriter writer,
                String pseudoGroupPrefix) {
            PseudoGrpMarshaler marshaler =
                    new PseudoGrpMarshaler(parent, name, type, writer,
                            pseudoGroupPrefix);
            marshaler.setFactory(this);
            return marshaler;
        }
        
        Marshaler getMSHSegmentMarshaler(Marshaler parent, QName name,
                SchemaType type, DelimDataWriter writer) {
            if (mMSHSegmentMarshaler == null) {
                mMSHSegmentMarshaler =
                    new MSHSegmentMarshaler(parent, name, type, writer);
                mMSHSegmentMarshaler.setFactory(this);
            } else {
                mMSHSegmentMarshaler.reset(parent, name, type);
            }
            return mMSHSegmentMarshaler;
        }
    }
    
    private static class MatchResult {
        public static final int MATCH_ELEMENT = 1;
        public static final int MATCH_WILDCARD = 2;
        
        public int mResult;
        public QName mElementName;
        public SchemaType mElementType;
    }
    
    private static final class DocumentMarshaler extends BaseMarshaler {
        
        private final SchemaGlobalElement mElement;
        private final MatchResult mMatchResult = new MatchResult();
        private final DelimDataWriter mWriter;
        private final String mPseudoGroupPrefix;
        private MarshalerFactory mMarshalFactory = null;
        
        public DocumentMarshaler(SchemaGlobalElement element,
                DelimDataWriter writer, String pseudoGroupPrefix,
                int occursFactor) {
            super(element.getType());
            mElement = element;
            mWriter = writer;
            mPseudoGroupPrefix = pseudoGroupPrefix;
        }

        public void setFactory(MarshalerFactory factory) {
            mMarshalFactory = factory;
        }
        
        @Override
        public void endOfRepetition() {
            //no-op
        }

        public Marshaler startElement(String uri, String localName,
                String qName, Attributes atts) throws SAXException {
            
            match(uri, localName, mMatchResult);
            if (mMatchResult.mResult == MatchResult.MATCH_ELEMENT) {
                if (localName.startsWith(mPseudoGroupPrefix)) {
                    return mMarshalFactory.getPseudoGrpMarshaler(
                            this,
                            mMatchResult.mElementName,
                            mMatchResult.mElementType,
                            mWriter, mPseudoGroupPrefix);
                } else {
                    if ("MSH".equals(localName)) {
                        return mMarshalFactory.getMSHSegmentMarshaler(
                                this,
                                mMatchResult.mElementName,
                                mMatchResult.mElementType,
                                mWriter);
                    }
                    mWriter.writeSegName(localName);
                    mWriter.writeFieldSep();
                    return mMarshalFactory.getSegmentMarshaler(
                            this,
                            mMatchResult.mElementName,
                            mMatchResult.mElementType,
                            mWriter);
                }
            } else {
                mWriter.writeSegName(localName);
                mWriter.writeFieldSep();
                SchemaGlobalElement knownElement =
                    mElement.getTypeSystem().findElement(
                            new QName(uri, localName));
                if (knownElement != null) {
                    return mMarshalFactory.getSegmentMarshaler(
                            this,
                            knownElement.getName(),
                            knownElement.getType(),
                            mWriter);
                } else {
                    return mMarshalFactory.getWildcardMarshaler(
                            this,
                            new QName(uri, localName),
                            mWriter, WildcardMarshaler.SEGMENT_LEVEL);
                }
            }
        }

        public Marshaler endElement(String uri, String localName,
                String qName) throws SAXException {
            if (!mElement.getName().getLocalPart().equals(localName)) {
                throw new SAXException(
                        "Expecting '/" + mElement.getName()
                        + "', found '/{"
                        + uri + "}" + localName + "'");
            }
            return null; 
        }

        public void characters(char[] ch, int start, int length)
                throws SAXException {
            //no-op
        }
    }

    private static final class PseudoGrpMarshaler extends BaseMarshaler {
        
        private final MatchResult mMatchResult = new MatchResult();
        private final DelimDataWriter mWriter;
        private Marshaler mParent;
        private QName mName;
        private String mPseudoGroupPrefix;
        private MarshalerFactory mMarshalFactory = null;
        
        public PseudoGrpMarshaler(Marshaler parent, QName name,
                SchemaType type, DelimDataWriter writer,
                String pseudoGroupPrefix) {
            super(type);
            mParent = parent;
            mName = name;
            mWriter = writer;
            mPseudoGroupPrefix = pseudoGroupPrefix;
        }

        public void setFactory(MarshalerFactory factory) {
            mMarshalFactory = factory;
        }
        
        public void reset(Marshaler parent, QName name, SchemaType type,
                String pseudoGroupPrefix) {
            super.reset(type);
            mParent = parent;
            mName = name;
            mPseudoGroupPrefix = pseudoGroupPrefix;
        }

        @Override
        public void endOfRepetition() {
            //no-op
        }

        public Marshaler startElement(String uri, String localName,
                String qName, Attributes atts) throws SAXException {
            
            match(uri, localName, mMatchResult);
            if (mMatchResult.mResult == MatchResult.MATCH_ELEMENT) {
                if (localName.startsWith(mPseudoGroupPrefix)) {
                    return mMarshalFactory.getPseudoGrpMarshaler(
                            this,
                            mMatchResult.mElementName,
                            mMatchResult.mElementType,
                            mWriter, mPseudoGroupPrefix);
                } else {
                    mWriter.writeSegName(localName);
                    mWriter.writeFieldSep();
                    return mMarshalFactory.getSegmentMarshaler(
                            this,
                            mMatchResult.mElementName,
                            mMatchResult.mElementType,
                            mWriter);
                }
            } else {
                SchemaGlobalElement knownElement =
                    mXMLType.getTypeSystem().findElement(
                            new QName(uri, localName));
                if (knownElement != null) {
                    return mMarshalFactory.getSegmentMarshaler(
                            this,
                            knownElement.getName(),
                            knownElement.getType(),
                            mWriter);
                } else {
                    return mMarshalFactory.getWildcardMarshaler(
                            this, new QName(uri, localName),
                            mWriter, WildcardMarshaler.SEGMENT_LEVEL);
                }
            }
        }

        public Marshaler endElement(String uri, String localName,
                String qName) throws SAXException {
            if (!mName.getLocalPart().equals(localName)) {
                throw new SAXException(
                        "Expecting '/" + mName
                        + "', found '/{"
                        + uri + "}" + localName + "'");
            }
            return mParent; 
        }

        public void characters(char[] ch, int start, int length)
                throws SAXException {
            //no-op
        }
    }

    private static class SegmentMarshaler extends BaseMarshaler {
        
        protected final DelimDataWriter mWriter;
        private final MatchResult mMatchResult = new MatchResult();
        private Marshaler mParent;
        private QName mName;
        private MarshalerFactory mMarshalFactory = null;
        private QName mMatchedSimpleElem = null;
        private boolean mReceivedEscape = false;
        protected boolean mIsFirst = true;
        private int mIndexBeforeMatch;
        
        public SegmentMarshaler(Marshaler parent, QName name,
                SchemaType type, DelimDataWriter writer) {
            super(type);
            mParent = parent;
            mName = name;
            mWriter = writer;
        }

        public void setFactory(MarshalerFactory factory) {
            mMarshalFactory = factory;
        }
        
        public void reset(Marshaler parent, QName name, SchemaType type) {
            super.reset(type);
            mParent = parent;
            mName = name;
            mIsFirst = true;
        }

        @Override
        protected void endOfRepetition() {
        }

        public Marshaler startElement(String uri, String localName,
                String qName, Attributes atts) throws SAXException {
            
            if (mMatchedSimpleElem != null) {
                if ("escape".equals(localName)) {
                    mWriter.writeData("\\");
                    mWriter.writeData(atts.getValue("V"));
                    mWriter.writeData("\\");
                    mReceivedEscape = true;
                    return this;
                }
                throw new SAXException(
                        "Missing end tag for element '"
                        + mMatchedSimpleElem);
            }
            mIndexBeforeMatch = mChildIndex;
            match(uri, localName, mMatchResult);
            if (!mIsFirst) {
                switch (mModel.getParticleType()) {
                case SchemaParticle.SEQUENCE:
                    if (mRepetIndex > 0) {
                        mWriter.writeRepetSep();
                    } else {
                        for (; mIndexBeforeMatch < mChildIndex;
                                mIndexBeforeMatch++) {
                            mWriter.writeFieldSep();
                        }
                    }
                    break;
                case SchemaParticle.CHOICE:
                    //must be repetitive
                    mWriter.writeRepetSep();
                    break;
                case SchemaParticle.ALL:
                    //children of 'all' model group are always non-repeating
                    mWriter.writeFieldSep();
                    break;
                default:
                    //Programming error
                    throw new SAXException("Illegal particle type: "
                            + mModel.getParticleType());
                }
            } else {
                switch (mModel.getParticleType()) {
                case SchemaParticle.SEQUENCE:
                case SchemaParticle.CHOICE:
                    for (; mIndexBeforeMatch < mChildIndex - 1;
                            mIndexBeforeMatch++) {
                        //sub-component is always non-repeating
                        mWriter.writeFieldSep();
                    }
                    break;
                case SchemaParticle.ALL:
                    break;
                default:
                    //Programming error
                    throw new SAXException("Illegal particle type: "
                            + mModel.getParticleType());
                }
            }
            mIsFirst = false;
            if (mMatchResult.mResult == MatchResult.MATCH_ELEMENT) {
                if (MarshalHandler.isSimpleContent(
                        mMatchResult.mElementType)) {
                    mMatchedSimpleElem = mMatchResult.mElementName;
                    return this;
                } else {
                    return mMarshalFactory.getFieldMarshaler(
                            this, mMatchResult.mElementName,
                            mMatchResult.mElementType, mWriter);
                }
            } else {
                return mMarshalFactory.getWildcardMarshaler(
                        this, new QName(uri, localName),
                        mWriter, WildcardMarshaler.FIELD_LEVEL);
            }
        }

        public Marshaler endElement(String uri, String localName,
                String qName) throws SAXException {
            if (mMatchedSimpleElem != null) {
                if (mReceivedEscape) {
                    if ("escape".equals(localName)) {
                        mReceivedEscape = false;
                        return this;
                    }
                    throw new SAXException(
                            "Expecting '/escape', found '/{"
                            + uri + "}" + localName + "'");
                }
                if (!mMatchedSimpleElem.getLocalPart().equals(localName)) {
                    throw new SAXException(
                            "Expecting '/" + mMatchedSimpleElem
                            + "', found '/{"
                            + uri + "}" + localName + "'");
                }
                mWriter.endData();
                mMatchedSimpleElem = null;
                return this;
            } else {
                if (!mName.getLocalPart().equals(localName)) {
                    throw new SAXException(
                            "Expecting '/" + mName
                            + "', found '/{"
                            + uri + "}" + localName + "'");
                }
                mWriter.writeSegSep();
                return mParent;
            }
        }

        public void characters(char[] ch, int start, int length)
                throws SAXException {
            if (length <= 0 || mMatchedSimpleElem == null) {
                return;
            }
            
            mWriter.writeData(ch, start, length);
        }
    }
    
    private static final class MSHSegmentMarshaler extends SegmentMarshaler {
        
        int mCharIndex = -1;
        int mLength = 0;
        
        public MSHSegmentMarshaler(Marshaler parent, QName name,
                SchemaType type, DelimDataWriter writer) {
            super(parent, name, type, writer);
        }

        @Override
        public void characters(char[] ch, int start, int length)
                throws SAXException {
            if (mChildIndex == 0) {
                if (length > 0) {
                    mWriter.writeSegName("MSH");
                    mWriter.setFieldSep(ch[start]);
                    return;
                }
            } else if (mChildIndex == 1) {
                if (mCharIndex == -1) {
                    mCharIndex = 0;
                }
                int oldLength = mLength;
                mLength += length;
                for (; mCharIndex < mLength; mCharIndex++) {
                    switch (mCharIndex) {
                    case 0:
                        mWriter.setComponentSep(
                                ch[start + mCharIndex - oldLength]);
                        break;
                    case 1:
                        mWriter.setRepetSep(
                                ch[start + mCharIndex - oldLength]);
                        break;
                    case 2:
                        mWriter.setEscapeChar(
                                ch[start + mCharIndex - oldLength]);
                        break;
                    case 3:
                        mWriter.setSubCompoSep(
                                ch[start + mCharIndex - oldLength]);
                    default:
                        //not possible, but let it go
                    }
                }
                mWriter.mEscapeCoder =
                    new SwiftCharEscapeCoder(mWriter.getFieldSep(),
                            mWriter.getComponentSep(), mWriter.getSubCompoSep(),
                            mWriter.getRepetSep(), mWriter.getEscapeChar());
                //no escaping for this one
                mWriter.writeData(new String(ch, start, length));
                return;
            }
            super.characters(ch, start, length);
        }
    }
    
    private static final class FieldMarshaler extends BaseMarshaler {
        
        private final MatchResult mMatchResult = new MatchResult();
        private final DelimDataWriter mWriter;
        private Marshaler mParent;
        private QName mName;
        private MarshalerFactory mMarshalFactory = null;
        private QName mMatchedSimpleElem = null;
        private boolean mReceivedEscape = false;
        private boolean mIsFirst = true;
        private int mIndexBeforeMatch;
        
        public FieldMarshaler(Marshaler parent, QName name,
                SchemaType type, DelimDataWriter writer) {
            super(type);
            mParent = parent;
            mName = name;
            mWriter = writer;
        }

        public void setFactory(MarshalerFactory factory) {
            mMarshalFactory = factory;
        }
        
        public void reset(Marshaler parent, QName name, SchemaType type) {
            super.reset(type);
            mParent = parent;
            mName = name;
            mIsFirst = true;
        }

        @Override
        protected void endOfRepetition() {
        }

        public Marshaler startElement(String uri, String localName,
                String qName, Attributes atts) throws SAXException {
            
            if (mMatchedSimpleElem != null) {
                if ("escape".equals(localName)) {
                    mWriter.writeData("\\");
                    mWriter.writeData(atts.getValue("V"));
                    mWriter.writeData("\\");
                    mReceivedEscape = true;
                    return this;
                }
                throw new SAXException(
                        "Missing end tag for element '"
                        + mMatchedSimpleElem);
            }
            mIndexBeforeMatch = mChildIndex;
            match(uri, localName, mMatchResult);
            if (!mIsFirst) {
                switch (mModel.getParticleType()) {
                case SchemaParticle.SEQUENCE:
                    for (; mIndexBeforeMatch < mChildIndex;
                            mIndexBeforeMatch++) {
                        //component is always non-repeating
                        mWriter.writeCompoSep();
                    }
                    break;
                case SchemaParticle.CHOICE:
                    //component is always non-repeating
                    throw new SAXException("Don't know how to handle"
                            + " repetitive component with name '{"
                            + uri + "}" + localName + "'");
                case SchemaParticle.ALL:
                    //component is always non-repeating
                    mWriter.writeCompoSep();
                    break;
                default:
                    //Programming error
                    throw new SAXException("Illegal particle type: "
                            + mModel.getParticleType());
                }
            } else {
                switch (mModel.getParticleType()) {
                case SchemaParticle.SEQUENCE:
                case SchemaParticle.CHOICE:
                    for (; mIndexBeforeMatch < mChildIndex - 1;
                            mIndexBeforeMatch++) {
                        //sub-component is always non-repeating
                        mWriter.writeCompoSep();
                    }
                    break;
                case SchemaParticle.ALL:
                    break;
                default:
                    //Programming error
                    throw new SAXException("Illegal particle type: "
                            + mModel.getParticleType());
                }
            }
            mIsFirst = false;
            if (mMatchResult.mResult == MatchResult.MATCH_ELEMENT) {
                if (MarshalHandler.isSimpleContent(
                        mMatchResult.mElementType)) {
                    mMatchedSimpleElem = mMatchResult.mElementName;
                    return this;
                } else {
                    return mMarshalFactory.getComponentMarshaler(
                            this, mMatchResult.mElementName,
                            mMatchResult.mElementType, mWriter);
                }
            } else {
                return mMarshalFactory.getWildcardMarshaler(
                        this, new QName(uri, localName),
                        mWriter, WildcardMarshaler.COMPO_LEVEL);
            }
        }

        public Marshaler endElement(String uri, String localName,
                String qName) throws SAXException {
            if (mMatchedSimpleElem != null) {
                if (mReceivedEscape) {
                    if ("escape".equals(localName)) {
                        mReceivedEscape = false;
                        return this;
                    }
                    throw new SAXException(
                            "Expecting '/escape', found '/{"
                            + uri + "}" + localName + "'");
                }
                if (!mMatchedSimpleElem.getLocalPart().equals(localName)) {
                    throw new SAXException(
                            "Expecting '/" + mMatchedSimpleElem
                            + "', found '/{"
                            + uri + "}" + localName + "'");
                }
                mWriter.endData();
                mMatchedSimpleElem = null;
                return this;
            } else {
                if (!mName.getLocalPart().equals(localName)) {
                    throw new SAXException(
                            "Expecting '/" + mName
                            + "', found '/{"
                            + uri + "}" + localName + "'");
                }
                return mParent;
            }
        }

        public void characters(char[] ch, int start, int length)
                throws SAXException {
            if (length <= 0 || mMatchedSimpleElem == null) {
                return;
            }
            
            mWriter.writeData(ch, start, length);
        }
    }
    
    private static final class ComponentMarshaler extends BaseMarshaler {
        
        private final MatchResult mMatchResult = new MatchResult();
        private final DelimDataWriter mWriter;
        private Marshaler mParent;
        private QName mName;
        private QName mMatchedSimpleElem = null;
        private boolean mReceivedEscape = false;
        private boolean mIsFirst = true;
        private int mIndexBeforeMatch;
        
        public ComponentMarshaler(Marshaler parent, QName name,
                SchemaType type, DelimDataWriter writer) {
            super(type);
            mParent = parent;
            mName = name;
            mWriter = writer;
        }

        public void reset(Marshaler parent, QName name, SchemaType type) {
            super.reset(type);
            mParent = parent;
            mName = name;
            mIsFirst = true;
        }

        @Override
        protected void endOfRepetition() {
        }

        public Marshaler startElement(String uri, String localName,
                String qName, Attributes atts) throws SAXException {
            
            if (mMatchedSimpleElem != null) {
                if ("escape".equals(localName)) {
                    mWriter.writeData("\\");
                    mWriter.writeData(atts.getValue("V"));
                    mWriter.writeData("\\");
                    mReceivedEscape = true;
                    return this;
                }
                throw new SAXException(
                        "Missing end tag for element '"
                        + mMatchedSimpleElem);
            }
            mIndexBeforeMatch = mChildIndex;
            match(uri, localName, mMatchResult);
            if (!mIsFirst) {
                switch (mModel.getParticleType()) {
                case SchemaParticle.SEQUENCE:
                    for (; mIndexBeforeMatch < mChildIndex;
                            mIndexBeforeMatch++) {
                        //sub-component is always non-repeating
                        mWriter.writeSubCompoSep();
                    }
                    break;
                case SchemaParticle.CHOICE:
                    //sub-component is always non-repeating
                    throw new SAXException("Don't know how to handle"
                            + " repetitive subcomponent with name '{"
                            + uri + "}" + localName + "'");
                case SchemaParticle.ALL:
                    //sub-component is always non-repeating
                    mWriter.writeSubCompoSep();
                    break;
                default:
                    //Programming error
                    throw new SAXException("Illegal particle type: "
                            + mModel.getParticleType());
                }
            } else {
                switch (mModel.getParticleType()) {
                case SchemaParticle.SEQUENCE:
                case SchemaParticle.CHOICE:
                    for (; mIndexBeforeMatch < mChildIndex - 1;
                            mIndexBeforeMatch++) {
                        //sub-component is always non-repeating
                        mWriter.writeSubCompoSep();
                    }
                    break;
                case SchemaParticle.ALL:
                    break;
                default:
                    //Programming error
                    throw new SAXException("Illegal particle type: "
                            + mModel.getParticleType());
                }
            }
            mIsFirst = false;
            if (mMatchResult.mResult == MatchResult.MATCH_ELEMENT) {
                if (!MarshalHandler.isSimpleContent(
                        mMatchResult.mElementType)) {
                    throw new SAXException(
                            "Element '{"
                            + uri + "}" + localName
                            + "' must match simple content type, "
                            + "but found complex content type.");
                }
            }
            mMatchedSimpleElem = mMatchResult.mElementName;
            return this;
        }

        public Marshaler endElement(String uri, String localName,
                String qName) throws SAXException {
            if (mMatchedSimpleElem != null) {
                if (mReceivedEscape) {
                    if ("escape".equals(localName)) {
                        mReceivedEscape = false;
                        return this;
                    }
                    throw new SAXException(
                            "Expecting '/escape', found '/{"
                            + uri + "}" + localName + "'");
                }
                if (!mMatchedSimpleElem.getLocalPart().equals(localName)) {
                    throw new SAXException(
                            "Expecting '/" + mMatchedSimpleElem
                            + "', found '/{"
                            + uri + "}" + localName + "'");
                }
                mWriter.endData();
                mMatchedSimpleElem = null;
                return this;
            } else {
                if (!mName.getLocalPart().equals(localName)) {
                    throw new SAXException(
                            "Expecting '/" + mName
                            + "', found '/{"
                            + uri + "}" + localName + "'");
                }
                return mParent;
            }
        }

        public void characters(char[] ch, int start, int length)
                throws SAXException {
            if (length <= 0 || mMatchedSimpleElem == null) {
                return;
            }
            
            mWriter.writeData(ch, start, length);
        }
    }
    
    private static final class WildcardMarshaler implements Marshaler {
        
        public static final int SEGMENT_LEVEL = 1;
        public static final int FIELD_LEVEL = 2;
        public static final int COMPO_LEVEL = 3;
        public static final int SUBCOMPO_LEVEL = 4;
        public static final int NUM_OF_LEVELS = SUBCOMPO_LEVEL + 1; 

        private final DelimDataWriter mWriter;
        
        private Marshaler mParent;
        private int mStartingAt;
        private int mCurrentLevel;
        private QName[] mLastMatched = new QName[NUM_OF_LEVELS];
        private int[] mChildIndex = new int[NUM_OF_LEVELS];
        private int[] mRepetIndex = new int[NUM_OF_LEVELS];
        private boolean[] mStartTagOpen = new boolean[NUM_OF_LEVELS];
        
        public WildcardMarshaler(Marshaler parent, QName name,
                DelimDataWriter writer, int level) {
            mWriter = writer;
            reset(parent, name, level);
        }
        
        public void reset(Marshaler parent, QName name, int level) {
            mParent = parent;
            mStartingAt = level;
            mCurrentLevel = mStartingAt + 1;
            Arrays.fill(mLastMatched, null);
            Arrays.fill(mChildIndex, -1);
            Arrays.fill(mRepetIndex, -1);
            Arrays.fill(mStartTagOpen, false);
            mLastMatched[mStartingAt] = name;
        }
        
        public Marshaler startElement(String uri, String localName,
                String qName, Attributes atts) throws SAXException {
            if (mStartTagOpen[mCurrentLevel]) {
                mCurrentLevel++;
                if (mCurrentLevel >= NUM_OF_LEVELS) {
                    throw new SAXException("Wildcard goes too deep."
                            + " Cannot be deeper than sub-component level."
                            + " Level: " + mCurrentLevel
                            + " Name: '{" + uri + "}" + localName + "'");
                }
                mLastMatched[mCurrentLevel] = new QName(uri, localName);
                mStartTagOpen[mCurrentLevel] = true;
                mChildIndex[mCurrentLevel] = 0;
                mRepetIndex[mCurrentLevel] = 0;
                return this;
            }
            if (mChildIndex[mCurrentLevel] >= 0) {
                if (mLastMatched[mCurrentLevel].getLocalPart().equals(
                        localName)) {
                    mRepetIndex[mCurrentLevel]++;
                } else {
                    mLastMatched[mCurrentLevel] = new QName(uri, localName);
                    mChildIndex[mCurrentLevel]++;
                    mRepetIndex[mCurrentLevel] = 0;
                }
                switch (mCurrentLevel) {
                case FIELD_LEVEL:
                    if (mRepetIndex[mCurrentLevel] > 0) {
                        mWriter.writeRepetSep();
                    } else {
                        mWriter.writeFieldSep();
                    }
                    break;
                case COMPO_LEVEL:
                    mWriter.writeCompoSep();
                    break;
                case SUBCOMPO_LEVEL:
                    mWriter.writeSubCompoSep();
                    break;
                default:
                    //Not possible unless there is programming error
                    throw new SAXException("Illegal level: "
                            + mCurrentLevel + ", name: '{"
                            + uri + "}" + localName + "'");
                }
            } else {
                mLastMatched[mCurrentLevel] = new QName(uri, localName);
                mChildIndex[mCurrentLevel] = 0;
                mRepetIndex[mCurrentLevel] = 0;
            }
            mStartTagOpen[mCurrentLevel] = true;
            return this;
        }

        public Marshaler endElement(String uri, String localName,
                String qName) throws SAXException {
            if (!mStartTagOpen[mCurrentLevel]) {
                mCurrentLevel--;
            } else {
                mWriter.endData();
            }
            if (mLastMatched[mCurrentLevel] == null) {
                //This is not possible unless programming error
                throw new SAXException("No matching start tag for '{"
                        + uri + "}" + localName + "'");
            }
            if (!mLastMatched[mCurrentLevel].getLocalPart().equals(
                    localName)) {
                throw new SAXException("Expecting: '"
                        + mLastMatched[mCurrentLevel]
                        + ", found: '{"
                        + uri + "}" + localName + "'");
            }
            mStartTagOpen[mCurrentLevel] = false;
            if (mCurrentLevel <= mStartingAt) {
                if (mStartingAt == SEGMENT_LEVEL) {
                    mWriter.writeSegSep();
                }
                return mParent;
            } else {
                return this;
            }
        }

        public void characters(char[] ch, int start, int length)
                throws SAXException {
            mWriter.writeData(ch, start, length);
        }
    }
    
    private static abstract class BaseMarshaler implements Marshaler {
        
        protected SchemaParticle mModel;
        protected int mChildrenCount;
        protected SchemaType mXMLType;
        protected int mChildIndex;
        protected int mRepetIndex;
        private Set<QName> mMatchedParts;
        
        public BaseMarshaler(SchemaType xmlType) {
            reset(xmlType);
        }

        public void reset(SchemaType xmlType) {
            mXMLType = xmlType;
            mModel = xmlType.getContentModel();
            mChildrenCount = mModel.countOfParticleChild();
            mChildIndex = -1;
            mRepetIndex = -1;
            mMatchedParts = null;
        }
        
        protected abstract void endOfRepetition();
        
        protected final void match(String uri, String localName,
                MatchResult result) throws SAXException {
            switch (mModel.getParticleType()) {
            case SchemaParticle.SEQUENCE:
                sequenceMatch(uri, localName, result);
                break;
            case SchemaParticle.CHOICE:
                choiceMatch(uri, localName, result);
                break;
            case SchemaParticle.ALL:
                allMatch(uri, localName, result);
                break;
            default:
                //Even though particle can also be wildcard or element, but
                //according to the logic, only model group particles are
                //accepted here
                throw new SAXException(
                        "Illegal particle type: " + mModel.getParticleType()); 
            }
        }
        
        protected final void endMatch(String uri, String localName)
                throws SAXException {
            switch (mModel.getParticleType()) {
            case SchemaParticle.SEQUENCE:
                if (mChildrenCount == 0 || mChildIndex >= mChildrenCount) {
                    return;
                }
                int repet = mRepetIndex;
                for (int i = mChildIndex; i < mChildrenCount; i++) {
                    if (repet <
                            mModel.getParticleChild(i).getIntMinOccurs() - 1) {
                        throw new SAXException("Expecting '"
                                + mModel.getParticleChild(i).getName()
                                + ", found '/{" + uri + "}"
                                + localName + "'");
                    }
                    repet = -1;
                }
                break;
            case SchemaParticle.CHOICE:
                for (int i = 0; i < mChildrenCount; i++) {
                    if (mModel.getParticleChild(i).getIntMinOccurs() == 0) {
                        return;
                    }
                }
                if (mChildIndex == -1) {
                    throw new SAXException("Expecting at least one child"
                            + " in the choice group"
                            + ", found '/{" + uri + "}"
                            + localName + "'");
                }
                break;
            case SchemaParticle.ALL:
                for (int i = 0; i < mChildrenCount; i++) {
                    if (mModel.getParticleChild(i).getIntMinOccurs() > 0
                            && !mMatchedParts.contains(
                                    mModel.getParticleChild(i).getName())) {
                        throw new SAXException("Expecting '"
                                + mModel.getParticleChild(i).getName()
                                + ", found '/{" + uri + "}"
                                + localName + "'");
                    }
                }
                break;
            default:
                //Even though particle can also be wildcard or element, but
                //according to the logic, only model group particles are
                //accepted here
                throw new SAXException(
                        "Illegal particle type: " + mModel.getParticleType()); 
            }
        }
        
        private final void sequenceMatch(String uri, String localName,
                MatchResult result) throws SAXException {
            
            if (mChildIndex == -1) {
                mChildIndex = 0;
                mRepetIndex = -1;
            }
            if (mChildIndex >= mChildrenCount) {
                throw new SAXException(
                        "Exceeding end of a sequence group. "
                        + " No matching particle found for {"
                        + uri + "}" + localName);
            }
            SchemaParticle childPart =
                mModel.getParticleChild(mChildIndex);
            boolean isWildcard =
                    (childPart.getParticleType() == SchemaParticle.WILDCARD);
            if (isWildcard ? !matchWildcard(uri, localName, childPart)
                    : !matchExactly(uri, localName, childPart)) { 
                if (mRepetIndex > 0) {
                    endOfRepetition();
                }
                mRepetIndex = -1;
                mChildIndex++;
                boolean match = false;
                while (mChildIndex < mChildrenCount) {
                    childPart = mModel.getParticleChild(mChildIndex);
                    isWildcard =
                        (childPart.getParticleType()
                                == SchemaParticle.WILDCARD);
                    if (isWildcard ? matchWildcard(uri, localName, childPart)
                            : matchExactly(uri, localName, childPart)) {
                        mRepetIndex = 0;
                        match = true;
                        break;
                    }
                    if (childPart.getIntMinOccurs() > 0) {
                        if (!isWildcard) {
                            throw new SAXException("Expecting element '"
                                    + childPart.getName()
                                    + "', found element '{"
                                    + uri + "}" + localName + "'");
                        } else {
                            throw new SAXException("Expecting element within "
                                    + childPart.getWildcardSet()
                                    + ", found element '{"
                                    + uri + "}" + localName + "'");
                        }
                    }
                    mChildIndex++;
                    mRepetIndex = -1;
                }
                if (!match) {
                    throw new SAXException(
                            "No matching particle found for {"
                            + uri + "}" + localName);
                }
            } else {
                ++mRepetIndex;
                if (childPart.getMaxOccurs() != null
                        && mRepetIndex >= childPart.getIntMaxOccurs()) {
                    throw new SAXException("Too many element '{" + uri + "}"
                            + localName + "'");
                }
            }
            if (childPart.getParticleType() == SchemaParticle.WILDCARD) {
                result.mResult = MatchResult.MATCH_WILDCARD;
                result.mElementName = new QName(uri, localName);
                result.mElementType = null;
            } else {
                //In Swift, the direct child particle of a model group is
                //always either an element or a wildcard
                result.mResult = MatchResult.MATCH_ELEMENT;
                result.mElementName = childPart.getName();
                result.mElementType = childPart.getType();
            }
        }
        
        private final void choiceMatch(String uri, String localName,
                MatchResult result) throws SAXException {

            boolean matchedWildcard = false;
            boolean matchedElement = false;
            SchemaParticle childPart = null;
            if (mChildIndex >= 0) {
                childPart = mModel.getParticleChild(mChildIndex);
                if (childPart.getParticleType()
                        == SchemaParticle.WILDCARD) {
                    if (!matchWildcard(uri, localName, childPart)) {
                        //previously matched a wildcard already, now an
                        //element that does not match the wildcardis coming.
                        //This is not allowed.
                        throw new SAXException(
                                "A wildcard has been matched for the"
                                + " choice group. Putting an element '{"
                                + uri + "}" + localName
                                + "' that does not match the wildcard"
                                + " is not allowed");
                    } else {
                        matchedWildcard = true;
                    }
                } else {
                    if (!matchExactly(uri, localName, childPart)) {
                        //previously matched an element already, now a
                        //different element is coming. This is not allowed.
                        throw new SAXException(
                                "An element with name '"
                                + childPart.getName()
                                + "' already occurred in the choice group."
                                + " Putting an element with a different name '{"
                                + uri + "}" + localName + "' is not allowed");
                    } else {
                        matchedElement = true;
                    }
                }
            }
            if (matchedWildcard) {
                mRepetIndex++;
                if (childPart.getMaxOccurs() != null
                        && mRepetIndex >= childPart.getIntMaxOccurs()) {
                    throw new SAXException("Too many element '{" + uri + "}"
                            + localName + "'");
                }
                result.mResult = MatchResult.MATCH_WILDCARD;
                result.mElementName = new QName(uri, localName);
                result.mElementType = null;
                return;
            }
            if (matchedElement) {
                mRepetIndex++;
                if (childPart.getMaxOccurs() != null
                        && mRepetIndex >= childPart.getIntMaxOccurs()) {
                    throw new SAXException("Too many element '{" + uri + "}"
                            + localName + "'");
                }
                result.mResult = MatchResult.MATCH_ELEMENT;
                result.mElementName = childPart.getName();
                result.mElementType = childPart.getType();
                return;
            }
            //Has never been matched before, match it.
            for (int i = 0; i < mChildrenCount; i++) {
                childPart = mModel.getParticleChild(i);
                if (childPart.getParticleType() == SchemaParticle.WILDCARD) {
                    if (matchWildcard(uri, localName, childPart)) {
                        matchedWildcard = true;
                        mChildIndex = i;
                        break;
                    }
                } else if (matchExactly(uri, localName, childPart)) {
                    mChildIndex = i;
                    matchedElement = true;
                    break;
                }
            }
            if (matchedWildcard) {
                mRepetIndex++;
                result.mResult = MatchResult.MATCH_WILDCARD;
                result.mElementName = new QName(uri, localName);
                result.mElementType = null;
                return;
            }
            if (matchedElement) {
                mRepetIndex++;
                result.mResult = MatchResult.MATCH_ELEMENT;
                result.mElementName = childPart.getName();
                result.mElementType = childPart.getType();
                return;
            }
            throw new SAXException(
                    "No matching particle found for '{"
                    + uri + "}" + localName + "'");
        }
        
        private final void allMatch(String uri, String localName,
                MatchResult result) throws SAXException {
            if (mMatchedParts == null) {
                mMatchedParts = new HashSet<QName>();
            }
            QName name = new QName(uri, localName);
            //"all" model group can only have element declarations
            SchemaProperty elemProp = mXMLType.getElementProperty(name);
            if (elemProp == null) {
                throw new SAXException(
                        "No matching particle found for '{"
                        + uri + "}" + localName + "'");
            }
            if (mMatchedParts.contains(name)) {
                //"all" model group can only have child particles with
                //maxOccurs equal to one
                throw new SAXException("Too many element '{" + uri + "}"
                        + localName + "'");
            }
            mMatchedParts.add(name);
            result.mResult = MatchResult.MATCH_ELEMENT;
            result.mElementName = elemProp.getName();
            result.mElementType = elemProp.getType();
            
            //we don't know what the child index is, but at least set
            //the repetition index
            mRepetIndex = 0;
        }
        
        private boolean matchWildcard(String uri, String localPart,
                SchemaParticle wildcard) {
            return wildcard.getWildcardSet().contains(
                    new QName(uri, localPart));
        }
        
        private boolean matchExactly(String uri, String localPart,
                SchemaParticle particle) {
            //Only checking the local name for the sake of performance
            return localPart.equals(particle.getName().getLocalPart());
        }
    }
}
