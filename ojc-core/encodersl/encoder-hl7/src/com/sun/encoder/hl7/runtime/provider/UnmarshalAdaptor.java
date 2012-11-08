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
 * @(#)UnmarshalAdaptor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.hl7.runtime.provider;

import com.sun.encoder.hl7.HL7Encoder;
import com.sun.encoder.hl7.util.Util;
import com.sun.encoder.tools.xml.SchemaLocationAttributes;
import java.io.IOException;
import java.util.Arrays;
import java.util.Iterator;

import javax.xml.namespace.QName;

import org.apache.xmlbeans.QNameSet;
import org.apache.xmlbeans.QNameSetBuilder;
import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaParticle;
import org.apache.xmlbeans.SchemaType;
import org.apache.xmlbeans.SchemaTypeSystem;
import org.xml.sax.ContentHandler;
import org.xml.sax.DTDHandler;
import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;

import com.sun.encoder.hl7.i18n.Messages;
import com.sun.encoder.hl7.runtime.LexicalException;
import com.sun.encoder.hl7.runtime.provider.HL7CharEscapeCoder.Result;
import com.sun.encoder.hl7.runtime.provider.Lexer.SegType;
import java.net.URL;
import java.util.Stack;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * This class implements an unmarshal adaptor for decoding HL7 standard encoding
 * rule encoded data. It implements SAX XMLReader interface, but the input
 * source is not in XML format but in HL7 standard encoding rule encoded.
 * The adaptor translates HL7 standard encoding rule encoded data into SAX
 * events.
 * 
 * @author Jun Xu
 * @since 6.0
 * @version
 */
public final class UnmarshalAdaptor implements XMLReader {

    /** Logger object.*/
    private Logger mLog = Logger.getLogger(getClass().getName());

    private Stack<String> mPath = new Stack<String>();
    
    static final String LN = System.getProperty("line.separator");

    private static Messages mMessages = Messages.getMessages(UnmarshalAdaptor.class);

    // Namespace URI for extension components (Z segments or extra fields,
    // components, subcomponents at the end
    public static final String NS_URI_FOR_EXTENSIONS = "urn:com.sun.encoder.hl7.ns.extentions";

    private static final EmptyAttributes mEmptyAttributes = new EmptyAttributes();
    private static QNameSet mQNameSetForExtensions;

    static {
        QNameSetBuilder qnSetBuilder = new QNameSetBuilder();
        qnSetBuilder.addNamespace(NS_URI_FOR_EXTENSIONS);
        mQNameSetForExtensions = qnSetBuilder.toQNameSet();
    }

    // private static int mCallCount = 0;

    private final URL mSchemaLocation;

    // global (root) level schema element.
    private final SchemaGlobalElement mElement;

    private final int mBufferCapacity;

    // group name prefix, e.g. "ADT_A01."
    private String mGroupNamePrefix;
    private EntityResolver mEntityResolver;
    private DTDHandler mDTDHandler;
    private ContentHandler mContentHandler;
    private ErrorHandler mErrorHandler;
    private RawMessageModeEnum mRawMessageMode;

    /**
     * Creates a new instance of UnmarshalAdaptor.
     * @param schemaLocation URL representing schema location.
     * @param element SchemaGlobalElement.
     */
    public UnmarshalAdaptor(URL schemaLocation, SchemaGlobalElement element, RawMessageModeEnum rawDataMode) {
        this(schemaLocation, element, Lexer.DEFAULT_BUF_CAPACITY, rawDataMode);
    }

    /**
     * Creates a new instance of UnmarshalAdaptor.
     * @param schemaLocation URL representing schema location.
     * @param element SchemaGlobalElement.
     * @param bufferCapacity buffer capacity.
     */
    public UnmarshalAdaptor(URL schemaLocation, SchemaGlobalElement element,
            int bufferCapacity, RawMessageModeEnum rawDataMode) {
        if (element == null) {
            throw new NullPointerException(
                mMessages.getString("HL7ENC-E0001.Global_Element_is_Null"));
        }
        mSchemaLocation = schemaLocation;
        mElement = element;
        mBufferCapacity = bufferCapacity;
        mGroupNamePrefix = Util.getGroupNamePrefix(element);
        mRawMessageMode = rawDataMode;
    }

    public boolean getFeature(String name)
            throws SAXNotRecognizedException, SAXNotSupportedException {
        if ("http://xml.org/sax/features/namespaces".equals(name)) {
            return true;
        }
        if ("http://xml.org/sax/features/namespace-prefixes".equals(name)) {
            return false;
        }
        throw new SAXNotRecognizedException();
    }

    public void setFeature(String name, boolean value)
            throws SAXNotRecognizedException, SAXNotSupportedException {
        if ("http://xml.org/sax/features/namespaces".equals(name)) {
            if (!value) {
                throw new SAXNotSupportedException(
                    mMessages.getString("HL7ENC-E0024.Feature_is_not_supported",
                    new Object[]{name, "" + value}));
            }
            return;
        }
        if ("http://xml.org/sax/features/namespace-prefixes".equals(name)) {
            if (value) {
                throw new SAXNotSupportedException(
                    mMessages.getString("HL7ENC-E0024.Feature_is_not_supported",
                    new Object[]{name, "" + value}));
            }
            return;
        }
        throw new SAXNotRecognizedException();
    }

    public Object getProperty(String name)
            throws SAXNotRecognizedException, SAXNotSupportedException {
        return null;
    }

    public void setProperty(String name, Object value)
        throws SAXNotRecognizedException, SAXNotSupportedException {
    }

    public void setEntityResolver(EntityResolver resolver) {
        mEntityResolver = resolver;
    }

    public EntityResolver getEntityResolver() {
        return mEntityResolver;
    }

    public void setDTDHandler(DTDHandler handler) {
        mDTDHandler = handler;
    }

    public DTDHandler getDTDHandler() {
        return mDTDHandler;
    }

    public void setContentHandler(ContentHandler handler) {
        mContentHandler = handler;
    }

    public ContentHandler getContentHandler() {
        return mContentHandler;
    }

    public void setErrorHandler(ErrorHandler handler) {
        mErrorHandler = handler;
    }

    public ErrorHandler getErrorHandler() {
        return mErrorHandler;
    }

    public String getGroupNamePrefix() {
        return mGroupNamePrefix;
    }

    public void setGroupNamePrefix(String groupNamePrefix) {
        mGroupNamePrefix = groupNamePrefix;
    }

    public void parse(InputSource input)
            throws IOException, SAXException {
        if (mLog.isLoggable(Level.FINEST)) {
            mLog.finest("Parse from input source=" + input.getClass().getName());
        }
        if (mContentHandler == null) {
            throw new NullPointerException(
                mMessages.getString("HL7ENC-E0025.ContentHandler_is_missing"));
        }
        
        boolean preLoadAll = mRawMessageMode.ordinal() > RawMessageModeEnum.NONE.ordinal();
        Lexer lexer = new Lexer(input, mBufferCapacity, preLoadAll);

        // start document and start root element
        mContentHandler.startDocument();
        mContentHandler.startPrefixMapping("xsi",
            "http://www.w3.org/2001/XMLSchema-instance");
        mContentHandler.startElement(mElement.getName().getNamespaceURI(),
            mElement.getName().getLocalPart(),
            mElement.getName().getLocalPart(),
            new SchemaLocationAttributes(mElement.getName().getNamespaceURI(),
            mSchemaLocation));
        mPath.push(mElement.getName().getLocalPart());

        // create and populate the state object
        ParsingState state = new ParsingState(mElement, mContentHandler, lexer,
            mGroupNamePrefix);
        state.mPublicId = input.getPublicId();
        state.mSystemId = input.getSystemId();
        state.mSemanticLevel = SemLevel.DOCUMENT;
        state.mModelLevel = 0;
        state.mQNamePath[0] = mElement.getName(); // root level QName
        state.mModelLevelPendingStartTagAt = -1;
        state.mMinFactor[state.mModelLevel] = 1;
        lexer.fillNextToken(state.mToken);
        if (state.mToken.mTokenType == Token.Type.SEG_NAME) {
            state.mSegName = new String(state.mToken.mChars,
                state.mToken.mOffset, state.mToken.mCount);
        }

        int sequence = 1;
        handleParticle(mElement.getType().getContentModel(), state, sequence);

        // end root element and finish document
        mContentHandler.endElement(mElement.getName().getNamespaceURI(),
            mElement.getName().getLocalPart(),
            mElement.getName().getLocalPart());
        mContentHandler.endPrefixMapping("xsi");
        mContentHandler.endDocument();
        mPath.pop();
    }

    public void parse(String systemId)
            throws IOException, SAXException {
        parse(new InputSource(systemId));
    }

    private String getDebugInfo(SchemaParticle particle) {
        StringBuilder sb = new StringBuilder();
        sb.append("Parse ").append(mPath.toString().replace(", ", "/"));
        if (particle.countOfParticleChild() > 0) {
            sb.append(" with ").append(particle.countOfParticleChild()).append(" children");
        }
        return sb.toString();
    }

    /**
     * Delegate to various parse methods to handle the schema particle based
     * on the particle type.
     * @param particle schema parti.
     * @param state parsing state.
     * @param sequence sequence number withint its parent.
     * @return true if successfully parse out data, or false otherwise.
     * @throws java.io.IOException
     * @throws org.xml.sax.SAXException
     */
    private boolean handleParticle(SchemaParticle particle,
            ParsingState state, int sequence)
            throws IOException, SAXException {
        if (mLog.isLoggable(Level.FINER)) {
            String msg = getDebugInfo(particle);
            if (mLog.isLoggable(Level.FINEST)) {
                mLog.finest(msg + ", seqNum=[" + sequence + "], state=[" + state + "].");
            } else {
                mLog.finer(msg + ".");
            }
        }

        if (state.mToken.mTokenType == Token.Type.EOF) {
            return false;
        }
        String errMsg = null;

        switch (particle.getParticleType()) {

            case SchemaParticle.ELEMENT:

                if (particle.getName().getLocalPart().equals("RawMessage")) {
                    mContentHandler.startElement(
                        HL7Encoder.HL7_NS, "RawMessage",
                        "RawMessage", mEmptyAttributes);
                    String fullMessage = state.mLexer.getFullMessageText();
                    mContentHandler.characters(fullMessage.toCharArray(), 0, fullMessage.length());
                    mContentHandler.endElement(
                        HL7Encoder.HL7_NS, "RawMessage",
                        "RawMessage");
                } else if (state.mSemanticLevel > SemLevel.SEGMENT) {
                    return parseFieldCompSubcomp(particle, state, sequence);
                } else if (state.mSemanticLevel == SemLevel.DOCUMENT) {
                    return parseSegmentOrPseudo(particle, state, sequence);
                } else {
                    errMsg = mMessages.getString("HL7ENC-E0031.Illegal_semantic_level",
                        new Object[]{"" + state.mSemanticLevel});
                    // should not be possible unless programming error
                    throw new IllegalStateException(errMsg);
                }

            case SchemaParticle.SEQUENCE:
                return parseSequence(particle, state);

            case SchemaParticle.CHOICE:
                return parseChoice(particle, state);

            case SchemaParticle.ALL:
                return parseAll(particle, state);

            case SchemaParticle.WILDCARD:
                return parseWildcard(particle, state, sequence);

            default:
                errMsg = mMessages.getString("HL7ENC-E0008.Illegal_Particle_Type",
                    new Object[]{"" + particle.getParticleType()});
                // should not be possible unless programming error
                throw new IllegalStateException(errMsg);
        }
    }

    /**
     * Parse out fields, components or subcomponents from a segment.
     * @param particle representing a segment.
     * @param state parsing state.
     * @param sequence sequence number within segment's parent.
     * @return false if nothing gets successfully parsed, or true otherwise.
     * @throws java.io.IOException
     * @throws org.xml.sax.SAXException
     */
    private boolean parseFieldCompSubcomp(SchemaParticle particle,
            ParsingState state, int sequence)
            throws IOException, SAXException {
        mPath.push(particle.getName().getLocalPart());
        if (mLog.isLoggable(Level.FINE)) {
            String msg = getDebugInfo(particle);
            if (mLog.isLoggable(Level.FINER)) {
                mLog.finer(msg + ", seqNum=[" + sequence + "], state=[" + state + "].");
            } else {
                mLog.fine(msg + ".");
            }
        }
        int occu = 0;
        QName qName;
        if (isComplexType(particle.getType())) {
            if (state.mSemanticLevel == SemLevel.SUBCOMPONENT) {
                throwException(mMessages.getString("HL7ENC-E0028.Sub_comp_must_have_simple_content"),
                    null, state);
            }
            boolean p = parsePseudoGroup(particle, state, sequence);
            mPath.pop();
            return p;
        }
        // now particle must be of non-complex type.
        if (!state.mToken.mIsConsumed) {
            final int maxOccu = (particle.getMaxOccurs() != null)
                ? particle.getIntMaxOccurs() : Integer.MAX_VALUE;
            final int semLevel = state.mSemanticLevel;
            int nonQualiConsumed = 0;
            while ((occu < maxOccu) && state.mToken.mTokenType == Token.Type.VALUE) {
                if (nonQualiConsumed > 0) {
                    // the previous delimiter is not an expected one and
                    // the token value must have been consumed
                    if (state.mToken.mTokenType == Token.Type.VALUE
                        && state.mToken.mDelimType == Delimiter.Type.DELIM_NOT_READ) {
                        state.mLexer.fillNextToken(state.mToken);
                        continue;
                    } else if ((state.mToken.mDelimType & Delimiter.mDelimQualiMask[semLevel]) != 0) {
                        nonQualiConsumed = 0;
                        state.mToken.mIsConsumed = true;
                    }
                    if ((state.mToken.mDelimType & Delimiter.mDelimEndMask[semLevel]) != 0) {
                        break;
                    }
                    forwardToken(state, semLevel);
                    continue;
                } else {
                    if ((state.mToken.mDelimType & Delimiter.mDelimQualiMask[semLevel]) == 0) {
                        // The delimiter type is not expected at
                        // this location. But always pick up the
                        // first one even though the delimiter is
                        // not as expected. This is to allow old
                        // version processor to process newer
                        // version messages.
                        // //For elementary content, need to see a
                        // //qualified delimiter for the specific
                        // level
                        // throwException("The delimiter is not"
                        // + " expected at the location.", null,
                        // state);
                        nonQualiConsumed++;
                    }
                }
                if (state.mToken.mDelimType == Delimiter.Type.DELIM_NOT_READ
                    && state.mToken.mCount == 0) {
                    state.mLexer.fillNextToken(state.mToken);
                    if (state.mToken.mTokenType != Token.Type.VALUE
                        || (state.mToken.mDelimType & Delimiter.mDelimQualiMask[semLevel]) == 0) {
                        break;
                    }
                    if (state.mToken.mDelimType == Delimiter.Type.DELIM_NOT_READ
                        && state.mToken.mCount == 0) {
                        // This should not be possible
                        throwException(mMessages.getString("HL7ENC-E0026.No_consecutive_data_read"),
                            null, state);
                    }
                } // end-- if (state.mToken.mDelimType == Token....
                if (state.mToken.mCount <= 0) {
                    // i.e. this token has no data
                    state.mToken.mIsConsumed = true;
                } else {
                    if (state.mModelLevelPendingStartTagAt >= 0) {
                        // fire the start element events that
                        // are pending
                        for (int j = state.mModelLevelPendingStartTagAt; j < state.mModelLevel; j++) {
                            qName = state.mQNamePath[j];
                            state.mContentHandler.startElement(
                                qName.getNamespaceURI(), qName.getLocalPart(),
                                qName.getLocalPart(), mEmptyAttributes);
                        }
                        state.mModelLevelPendingStartTagAt = -1;
                    }
                    qName = particle.getName();
                    state.mContentHandler.startElement(
                        qName.getNamespaceURI(), qName.getLocalPart(),
                        qName.getLocalPart(), mEmptyAttributes);
                    fireCharacters(state, false);
                    // set to already consumed
                    state.mToken.mIsConsumed = true;
                    while (state.mToken.mTokenType == Token.Type.VALUE
                        && state.mToken.mDelimType == Delimiter.Type.DELIM_NOT_READ) {
                        state.mLexer.fillNextToken(state.mToken);
                        if (state.mToken.mTokenType == Token.Type.VALUE) {
                            fireCharacters(state, false);
                        }
                        state.mToken.mIsConsumed = true;
                    }
                    state.mContentHandler.endElement(
                        qName.getNamespaceURI(), qName.getLocalPart(),
                        qName.getLocalPart());
                    occu++;
                }

                if ((state.mToken.mDelimType & Delimiter.mDelimEndMask[semLevel]) != 0) {
                    break; // break while ((occu < maxOccu) && state.mToken.mTokenType == Token.VALUE)
                }
                forwardToken(state, semLevel);
            } // end-- while ((occu < maxOccu) && state.mToken.mTokenType == Token.VALUE)
        } // end-- if (!state.mToken.mIsConsumed)
        if (occu < particle.getIntMinOccurs() && state.mMinFactor[state.mModelLevel] > 0) {
            String errMsg = mMessages.getString("HL7ENC-E0027.Element_must_occur",
                new Object[]{particle.getName().getLocalPart(),
                    "" + particle.getIntMinOccurs()});
            throwException(errMsg, null, state);
        }
        while ((state.mToken.mDelimType & Delimiter.mDelimEndMask[state.mSemanticLevel]) == 0) {
            // Haven't seen the end terminator for this level, so keep moving forward on data.
            forwardToken(state, state.mSemanticLevel);
        }
        forwardToken(state, state.mSemanticLevel);
        mPath.pop();
        return (occu > 0);
    }

    /**
     * Parse a segment or a pseudo (group of elements).
     * @param particle schema particle that parsing is based on.
     * @param state parsing state.
     * @param sequence sequence number within its parent.
     * @return false if nothing gets successfully parsed, or true otherwise.
     * @throws java.io.IOException
     * @throws org.xml.sax.SAXException
     */
    private boolean parseSegmentOrPseudo(SchemaParticle particle,
            ParsingState state, int sequence)
            throws IOException, SAXException {
        mPath.push(particle.getName().getLocalPart());
        if (mLog.isLoggable(Level.FINE)) {
            String msg = getDebugInfo(particle);
            if (mLog.isLoggable(Level.FINER)) {
                mLog.finer(msg + ", seqNum=[" + sequence + "], state=[" + state + "].");
            } else {
                mLog.fine(msg + ".");
            }
        }
        int occu = 0;
        QName qName;
        String localPart = particle.getName().getLocalPart();
        if (localPart.startsWith(state.mGroupPrefix)
            || Util.MESSAGEBATCH.equals(localPart)) {
            // a grouping element. Strip off the element and continue
            // on the content model
            boolean b = parsePseudoGroup(particle, state, sequence);
            mPath.pop();
            return b;
        }
        // now updates semantic level up one level down (i.e. SEGMENT)
        state.mSemanticLevel++;
        final int maxOccu = (particle.getMaxOccurs() != null)
            ? particle.getIntMaxOccurs() : Integer.MAX_VALUE;
        while ((occu < maxOccu) && state.mToken.mTokenType == Token.Type.SEG_NAME
                && localPart.equals(state.mSegName)) {
            state.mMinFactor[state.mModelLevel] = 1;
            if (state.mModelLevelPendingStartTagAt >= 0) {
                // fire the start element events that are pending
                for (int j = state.mModelLevelPendingStartTagAt; j < state.mModelLevel; j++) {
                    qName = state.mQNamePath[j];
                    state.mContentHandler.startElement(
                        qName.getNamespaceURI(), qName.getLocalPart(),
                        qName.getLocalPart(), mEmptyAttributes);
                }
                // reset to negative
                state.mModelLevelPendingStartTagAt = -1;
            }
            qName = particle.getName();
            state.mContentHandler.startElement(
                qName.getNamespaceURI(), qName.getLocalPart(),
                qName.getLocalPart(), mEmptyAttributes);
            // Fill up token first.
            state.mLexer.fillNextToken(state.mToken);
            // now parse the segment. it must be of complex type
            handleParticle(particle.getType().getContentModel(), state, sequence);
            state.mContentHandler.endElement(
                qName.getNamespaceURI(), qName.getLocalPart(),
                qName.getLocalPart());
            // skip unrecognized fields, and fill next token
            skipToNextSegment(state);
            occu++;
        }
        if (occu < particle.getIntMinOccurs()
            && state.mMinFactor[state.mModelLevel] > 0) {
            String errMsg = mMessages.getString("HL7ENC-E0030.Segment_must_occur",
                new Object[]{localPart, "" + particle.getIntMinOccurs()});
            throwException(errMsg, null, state);
        }
        state.mSemanticLevel--;
        mPath.pop();
        return (occu > 0);
    }

    /**
     * Parse a SEQUENCE particle representing a xs:sequence group.
     * @param particle schema particle that parsing is based on.
     * @param state parsing state.
     * @return false if nothing gets successfully parsed, or true otherwise.
     * @throws java.io.IOException
     * @throws org.xml.sax.SAXException
     */
    private boolean parseSequence(SchemaParticle particle,
            ParsingState state)
            throws IOException, SAXException {
        if (mLog.isLoggable(Level.FINE)) {
            String msg = getDebugInfo(particle);
            if (mLog.isLoggable(Level.FINER)) {
                mLog.finer(msg + ", state=[" + state + "].");
            } else {
                mLog.fine(msg + ".");
            }
        }
        final int seqChildCount = particle.countOfParticleChild();
        boolean seqResult = false;
        if (seqChildCount <= 0) {
            // i.e. no child in the sequence. nothing to parse.
            return seqResult;
        }

        // this variable is to keep track if state.mSemanticLevel is increased
        boolean isSemanticLevelIncreased = false;
        if (state.mSemanticLevel != SemLevel.DOCUMENT) {
            // note: if Semantic Level is DOCUMENT, then it stays as DOCUMENT
            state.mSemanticLevel++;
            isSemanticLevelIncreased = true;
        }
        // increase model level in state (sequence complex type)
        state.mModelLevel++;

        for (int j = 0; j < seqChildCount; j++) {
            // update on effective minimum occurrence factor
            if (!seqResult && state.mMinFactor[state.mModelLevel - 1] == 0) {
                state.mMinFactor[state.mModelLevel] = 0;
            } else {
                state.mMinFactor[state.mModelLevel] = 1;
            }
            SchemaParticle childParticle = particle.getParticleChild(j);
            if (state.mToken.mIsConsumed) {
                if (childParticle.getIntMinOccurs() >= 1) {
                    // see open-esb issue 2119
                    String errMsg = mMessages.getString("HL7ENC-E0027.Element_must_occur",
                        new Object[]{childParticle.getName().getLocalPart(),
                            "" + childParticle.getIntMinOccurs()});
                    throwException(errMsg, null, state);
                }
                // i.e no more data left for this sequence type level
                break; // break-- for (int j = 0; j < seqChildCount; j++)
            }
            // now parse this child
            boolean result = handleParticle(childParticle, state, j + 1);
            seqResult = (result || seqResult);
        }

        // revert back the model level in state.
        state.mModelLevel--;
        // revert back the semantic level in state if increased earlier.
        if (isSemanticLevelIncreased) {
            state.mSemanticLevel--;
        }
        return seqResult;
    }

    /**
     * Parse a CHOICE particle representing a xs:choice group.
     * @param particle choice particle that parsing is based on.
     * @param state parsing state.
     * @return false if nothing gets successfully parsed, or true otherwise.
     * @throws java.io.IOException
     * @throws org.xml.sax.SAXException
     */
    private boolean parseChoice(SchemaParticle particle,
            ParsingState state)
            throws IOException, SAXException {
        if (mLog.isLoggable(Level.FINE)) {
            String msg = getDebugInfo(particle);
            if (mLog.isLoggable(Level.FINER)) {
                mLog.finer(msg + ", state=[" + state + "].");
            } else {
                mLog.fine(msg + ".");
            }
        }
        final int choiceChildCount = particle.countOfParticleChild();
        boolean choiceResult = false;
        if (choiceChildCount > 0) {
            boolean isSmntcLvlIncrsd = false;
            if (state.mSemanticLevel != SemLevel.DOCUMENT) {
                state.mSemanticLevel++;
                isSmntcLvlIncrsd = true;
            }
            // increase model level (all complex type)
            state.mModelLevel++;
            state.mMinFactor[state.mModelLevel] = 0;
            for (int j = 0; j < choiceChildCount; j++) {
                boolean result = handleParticle(particle.getParticleChild(j),
                    state, j + 1);
                if (result) {
                    choiceResult = true;
                    break;
                }
                if (state.mToken.mIsConsumed) {
                    break;
                }
            }
            state.mModelLevel--;
            if (isSmntcLvlIncrsd) {
                state.mSemanticLevel--;
            }
        }
        return choiceResult;
    }

    /**
     * Parse an ALL particle representing an xs:all group.
     * @param particle all particle that parsing is based on.
     * @param state parsing state.
     * @return false if nothing gets successfully parsed, or true otherwise.
     * @throws java.io.IOException
     * @throws org.xml.sax.SAXException
     */
    private boolean parseAll(SchemaParticle particle,
            ParsingState state)
            throws IOException, SAXException {
        if (mLog.isLoggable(Level.FINE)) {
            String msg = getDebugInfo(particle);
            if (mLog.isLoggable(Level.FINER)) {
                mLog.finer(msg + ", state=[" + state + "].");
            } else {
                mLog.fine(msg + ".");
            }
        }
        final int allChildCount = particle.countOfParticleChild();
        boolean allResult = false;
        if (allChildCount > 0) {
            boolean semanticLevelIncreased = false;
            if (state.mSemanticLevel != SemLevel.DOCUMENT) {
                state.mSemanticLevel++;
                semanticLevelIncreased = true;
            }
            // increase model level (any complex type)
            state.mModelLevel++;
            state.mMinFactor[state.mModelLevel] = 0;
            boolean continueLoop = true;
            boolean[] found = new boolean[allChildCount];
            Arrays.fill(found, false);
            for (int j = 0; continueLoop && j < allChildCount; j++) {
                continueLoop = false;
                for (int k = 0; k < allChildCount; k++) {
                    boolean result = handleParticle(
                        particle.getParticleChild(k), state, k + 1);
                    if (result) {
                        if (found[k]) {
                            throwException(mMessages.getString("HL7ENC-E0032.Unexpected_Token"), null, state);
                        } else {
                            found[k] = true;
                        }
                        allResult = true;
                        continueLoop = true;
                        break;
                    }
                    if (state.mToken.mIsConsumed) {
                        break;
                    }
                }
            }
            for (int j = 0; j < allChildCount; j++) {
                if (allResult && particle.getParticleChild(j).getIntMinOccurs() > 0
                    && !found[j]) {
                    throwException(mMessages.getString("HL7ENC-E0033.Particle_is_missing"), null, state);
                }
            }
            state.mModelLevel--;
            if (semanticLevelIncreased) {
                state.mSemanticLevel--;
            }
        }
        return allResult;
    }

    /**
     * Parse a pseudo group (e.g. FHS.7 of complex content extending TS)
     * @param element SchemaParticle that parsing is based on.
     * @param state parsing state.
     * @param sequence sequence number within its parent.
     * @return false if nothing gets successfully parsed, or true otherwise.
     * @throws java.io.IOException
     * @throws org.xml.sax.SAXException
     */
    private boolean parsePseudoGroup(SchemaParticle element,
            ParsingState state, int sequence)
            throws IOException, SAXException {
        if (mLog.isLoggable(Level.FINER)) {
            String msg = getDebugInfo(element);
            if (mLog.isLoggable(Level.FINEST)) {
                mLog.finest(msg + ", seqNum=[" + sequence + "], state=[" + state + "].");
            } else {
                mLog.finer(msg + ".");
            }
        }
        int occu = 0;
        int minOccu = element.getIntMinOccurs();
        if (element.getMaxOccurs() != null) {
            int maxOccu = element.getIntMaxOccurs();
            while (occu < maxOccu) {
                if (state.mModelLevelPendingStartTagAt == -1
                    || state.mModelLevelPendingStartTagAt > state.mModelLevel) {
                    state.mModelLevelPendingStartTagAt = state.mModelLevel;
                }
                state.mQNamePath[state.mModelLevel] = element.getName();
                if (occu >= minOccu) {
                    state.mMinFactor[state.mModelLevel] = 0;
                }
                SchemaParticle subModel = element.getType().getContentModel();
                // keep track if model level is increased
                boolean isMdlLvlIncrsd = false;
                if (subModel.getParticleType() == SchemaParticle.ELEMENT
                        || subModel.getParticleType() == SchemaParticle.WILDCARD) {
                    // increase model level (pseudo or any complex type)
                    state.mModelLevel++;
                    isMdlLvlIncrsd = true;
                }
                boolean result = handleParticle(subModel, state, sequence);
                if (isMdlLvlIncrsd) {
                    state.mModelLevel--;
                }
                if (result) {
                    state.mContentHandler.endElement(
                        element.getName().getNamespaceURI(),
                        element.getName().getLocalPart(),
                        element.getName().getLocalPart());
                    occu++;
                } else {
                    /*Check for null repetition */
                    if (state.mToken.mIsConsumed) {
                        if ((state.mToken.mDelimType & Delimiter.mDelimEndMask[state.mSemanticLevel]) != 0) {
                            break; // break-- while (true)
                        }
                    } else {
                        break; // break-- while (occu < maxOccu)  
                    }
                }
                if (state.mSemanticLevel > SemLevel.SEGMENT) {
                    if ((state.mToken.mDelimType & Delimiter.mDelimEndMask[state.mSemanticLevel]) != 0) {
                        break; // break-- while (occu < maxOccu)
                    }
                    forwardToken(state, state.mSemanticLevel);
                }
            } // end-- while (occu < maxOccu)
        } else {
            //i.e. maxOccurs unbounded. into infinite loop until got break
            while (true) {
                if (state.mModelLevelPendingStartTagAt == -1
                    || state.mModelLevelPendingStartTagAt > state.mModelLevel) {
                    state.mModelLevelPendingStartTagAt = state.mModelLevel;
                }
                state.mQNamePath[state.mModelLevel] = element.getName();
                if (occu >= minOccu) {
                    state.mMinFactor[state.mModelLevel] = 0;
                }
                SchemaParticle subModel = element.getType().getContentModel();
                boolean isMdlLvlIncrsd = false;
                if (subModel.getParticleType() == SchemaParticle.ELEMENT
                    || subModel.getParticleType() == SchemaParticle.WILDCARD) {
                    // increase model level (pseudo or any complex type)
                    state.mModelLevel++;
                    isMdlLvlIncrsd = true;
                }
                boolean result = handleParticle(subModel, state, sequence);
                if (isMdlLvlIncrsd) {
                    state.mModelLevel--;
                }
                if (!result) {
                    // nothing in data has matched
                    if (state.mToken.mIsConsumed) {
                        if ((state.mToken.mDelimType & Delimiter.mDelimEndMask[state.mSemanticLevel]) != 0) {
                            break; // break-- while (true)
                        }
                    } else {
                        break;
                    }
                } else {
                    state.mContentHandler.endElement(
                        element.getName().getNamespaceURI(),
                        element.getName().getLocalPart(),
                        element.getName().getLocalPart());
                    occu++;
                }
                if (state.mSemanticLevel > SemLevel.SEGMENT) {
                    if ((state.mToken.mDelimType & Delimiter.mDelimEndMask[state.mSemanticLevel]) != 0) {
                        break; // break-- while (true)
                    }
                    forwardToken(state, state.mSemanticLevel);
                }
            } // end-- while (true)
        }
        if (occu < element.getIntMinOccurs()
            && state.mMinFactor[state.mModelLevel] > 0) {
            String errMsg = mMessages.getString("HL7ENC-E0027.Element_must_occur",
                new Object[] {element.getName().getLocalPart(),
                    "" + element.getIntMinOccurs() });
            throwException(errMsg, null, state);
        }
        forwardToken(state, state.mSemanticLevel);
        return (occu > 0);
    }

    /**
     * move forward to fill next token if needed(?)
     * @param state parsing state.
     * @param semanticLevel semantic level, such as SemLevel.SEGMENT.
     * @throws java.io.IOException
     * @throws com.sun.encoder.hl7.runtime.LexicalException
     */
    private void forwardToken(ParsingState state, int semanticLevel)
            throws IOException, LexicalException {
        if (mLog.isLoggable(Level.FINEST)) {
            mLog.finest("forwardToken, semanticLevel=[" + semanticLevel
                    + "], state=[" + state + "].");
        }
        if (state.mToken.mTokenType == Token.Type.VALUE
            && (state.mToken.mDelimType == Delimiter.Type.DELIM_NOT_READ
            || Delimiter.mDelimType2Level[state.mToken.mDelimType] >= semanticLevel)) {
            state.mLexer.fillNextToken(state.mToken);
        }
    }

    /**
     * Skip to next segment.
     * @param state
     * @throws java.io.IOException
     * @throws com.sun.encoder.hl7.runtime.LexicalException
     * @throws org.xml.sax.SAXException
     */
    private static void skipToNextSegment(ParsingState state)
            throws IOException, LexicalException, SAXException {
        while (state.mLexer.fillNextToken(state.mToken)) {
            if (state.mToken.mTokenType == Token.Type.EOF
                || state.mToken.mTokenType == Token.Type.SEG_NAME) {
                // i.e. just fill up a segment tag name, or EOF reached
                break;
            }
        }
        if (state.mToken.mTokenType == Token.Type.SEG_NAME) {
            // i.e. just fill up a segment tag name
            if (state.mToken.mDelimType != Delimiter.Type.DELIM_NOT_READ) {
                // i.e. delimiter has already been read
                state.mSegName = new String(state.mToken.mChars, state.mToken.mOffset, state.mToken.mCount);
            } else {
                // DELIM_NOT_READ, i.e. segment name got cut by buffer boundary
                state.mSB.setLength(0);
                // store former part of segment name
                state.mSB.append(state.mToken.mChars, state.mToken.mOffset, state.mToken.mCount);
                state.mLexer.fillNextToken(state.mToken);
                if (state.mToken.mTokenType != Token.Type.SEG_NAME
                        || state.mToken.mDelimType != Delimiter.Type.FIELD_SEP) {
                    throwException(mMessages.getString("HL7ENC-E0034.Failed_reading_Segment_name"), null, state);
                }
                // put latter part of segment name
                state.mSB.append(state.mToken.mChars, state.mToken.mOffset, state.mToken.mCount);
                state.mSegName = state.mSB.toString();
                // check if segment name is a special header segment
                if (Util.isHeaderSegmentName(state.mSegName)) {
                    state.mLexer.mSegType = SegType.HDR_SEG;
                    // reverse the "+ 1" operations for rawPos and col which
                    // have been done during above fillNextToken() step where
                    // it was treated as a normal segment instead of a special
                    // header segment there
                    state.mLexer.mRawPos--;
                    state.mLexer.mCol--;
                }
            }
        }
    }

    /**
     * Parse a WILDCARD particle representing an xs:any group.
     * @param particle all particle to be parsed.
     * @param state parsing state.
     * @param sequence sequence number within its parent.
     * @return false if nothing gets successfully parsed, or true otherwise.
     * @throws java.io.IOException
     * @throws org.xml.sax.SAXException
     */
    private boolean parseWildcard(SchemaParticle wildcard,
            ParsingState state, int sequence)
            throws IOException, SAXException {
        if (mLog.isLoggable(Level.FINE)) {
            String msg = getDebugInfo(wildcard);
            if (mLog.isLoggable(Level.FINER)) {
                mLog.finer(msg + ", seqNum=[" + sequence + "], state=[" + state + "].");
            } else {
                mLog.fine(msg + ".");
            }
        }
        int occu;
        String errMsg = null;

        switch (state.mSemanticLevel) {
            case SemLevel.DOCUMENT:
                occu = 0;
                while (state.mToken.mTokenType == Token.Type.SEG_NAME) {
                    if (state.mModelLevelPendingStartTagAt >= 0) {
                        // fire the start element events that are pending
                        for (int j = state.mModelLevelPendingStartTagAt; j < state.mModelLevel; j++) {
                            state.mContentHandler.startElement(
                                state.mQNamePath[j].getNamespaceURI(),
                                state.mQNamePath[j].getLocalPart(),
                                state.mQNamePath[j].getLocalPart(),
                                mEmptyAttributes);
                        }
                        state.mModelLevelPendingStartTagAt = -1;
                    }
                    SchemaGlobalElement[] element = new SchemaGlobalElement[1];
                    QName qName = crackOutWildcardElement(wildcard, state.mSegName,
                        state.mRootElement.getTypeSystem(), element);
                    if (qName == null) {
                        errMsg = mMessages.getString("HL7ENC-E0035.Unable_to_construct_QName",
                            new Object[]{state.mSegName});
                        throwException(errMsg, null, state);
                    }
                    state.mLexer.fillNextToken(state.mToken);
                    state.mContentHandler.startElement(qName.getNamespaceURI(),
                        qName.getLocalPart(), qName.getLocalPart(), mEmptyAttributes);
                    state.mSemanticLevel++;
                    if (element[0] != null) {
                        // found the element definition
                        handleParticle(element[0].getType().getContentModel(),
                            state, sequence);
                    } else {
                        // Process all fields
                        processWildcardFields(state, sequence);
                    }
                    state.mSemanticLevel--;
                    state.mContentHandler.endElement(qName.getNamespaceURI(),
                        qName.getLocalPart(), qName.getLocalPart());
                    skipToNextSegment(state);
                    occu++;
                }
                if (occu < wildcard.getIntMinOccurs() && state.mMinFactor[state.mModelLevel] > 0) {
                    errMsg = mMessages.getString("HL7ENC-E0036.Wildcard_segment_must_occur",
                        new Object[]{"" + wildcard.getIntMinOccurs()});
                    throwException(errMsg, null, state);
                }
                return occu > 0;

            case SemLevel.FIELD:
            case SemLevel.COMPONENT:
            case SemLevel.SUBCOMPONENT:
                return processWildcardFields(state, sequence);
            default:
                errMsg = mMessages.getString("HL7ENC-E0031.Illegal_semantic_level",
                    new Object[]{"" + state.mSemanticLevel});
                throw new IllegalStateException(errMsg);
        }
    }

    /**
     * Process wildcard fields.
     * @param state parsing state.
     * @param sequence sequence number within its parent.
     * @return false if nothing gets successfully parsed, or true otherwise.
     * @throws java.io.IOException
     * @throws org.xml.sax.SAXException
     */
    private boolean processWildcardFields(ParsingState state,
            int sequence)
            throws IOException, SAXException {
        String errMsg = null;
        if (state.mToken.mTokenType != Token.Type.VALUE || state.mToken.mIsConsumed) {
            return false;
        }

        QName[] pendingEndTag = new QName[SemLevel.MAX_SEMANTIC_LEVELS];
        Arrays.fill(pendingEndTag, null);
        int curLevel = state.mSemanticLevel;
        if (curLevel < SemLevel.FIELD) {
            curLevel = SemLevel.FIELD;
        }
        int seenLevel = -1;
        int fieldIndex = 0;
        int fieldRepetIndex = -1;
        int compoIndex = 0;
        int subCompoIndex = 0;

        switch (state.mSemanticLevel) {
            case SemLevel.SEGMENT:
                fieldIndex = 0;
                compoIndex = 0;
                subCompoIndex = 0;
                break;
            case SemLevel.FIELD:
                fieldIndex = sequence - 1;
                compoIndex = 0;
                subCompoIndex = 0;
                break;
            case SemLevel.COMPONENT:
                compoIndex = sequence - 1;
                subCompoIndex = 0;
                break;
            case SemLevel.SUBCOMPONENT:
                subCompoIndex = sequence - 1;
                break;
        }
        while (state.mToken.mTokenType == Token.Type.VALUE) {
            boolean useBuffer = false;
            state.mSB.setLength(0);
            // For wildcard processing, we have to see the delimiter
            // since we don't know the structure. It's not like
            // for known elements, we can always fire characters(...)
            // even without seeing the delimiter.
            while (state.mToken.mDelimType == Delimiter.Type.DELIM_NOT_READ
                && state.mToken.mTokenType == Token.Type.VALUE) {
                state.mSB.append(state.mToken.mChars,
                    state.mToken.mOffset, state.mToken.mCount);
                state.mLexer.fillNextToken(state.mToken);
                useBuffer = true;
            }
            if (state.mToken.mTokenType != Token.Type.VALUE) {
                break;
            }
            if (state.mToken.mDelimType == Delimiter.Type.FIELD_SEP && fieldRepetIndex == -1) {
                fieldRepetIndex = 0;
            }
            seenLevel = Delimiter.mDelimType2Level[state.mToken.mDelimType];
            if (curLevel <= seenLevel) {
                for (int j = curLevel; j <= seenLevel; j++) {
                    switch (j) {
                        case SemLevel.FIELD:
                            if (state.mToken.mDelimType == Delimiter.Type.REPET_SEP) {
                                if (fieldRepetIndex == -1) {
                                    fieldRepetIndex = 1;
                                } else {
                                    fieldRepetIndex++;
                                }
                            }
                            state.mSB2.setLength(0);
                            state.mSB2.append("fld");
                            state.mSB2.append(++fieldIndex);
                            if (fieldRepetIndex != -1) {
                                state.mSB2.append('_');
                                state.mSB2.append(++fieldRepetIndex);
                            }
                            break;
                        case SemLevel.COMPONENT:
                            state.mSB2.setLength(0);
                            state.mSB2.append("com");
                            state.mSB2.append(++compoIndex);
                            break;
                        case SemLevel.SUBCOMPONENT:
                            state.mSB2.setLength(0);
                            state.mSB2.append("sub");
                            state.mSB2.append(++subCompoIndex);
                            break;
                        case SemLevel.SEGMENT:
                            // not possible
                        default:
                            errMsg = mMessages.getString("HL7ENC-E0031.Illegal_semantic_level",
                                new Object[]{"" + j});
                            throw new IllegalStateException(errMsg);
                    }
                    pendingEndTag[j] = new QName(NS_URI_FOR_EXTENSIONS, state.mSB2.toString());
                    state.mContentHandler.startElement(pendingEndTag[j].getNamespaceURI(),
                        pendingEndTag[j].getLocalPart(), pendingEndTag[j].getLocalPart(), mEmptyAttributes);
                }
                fireCharacters(state, useBuffer);
                state.mToken.mIsConsumed = true;
                state.mContentHandler.endElement(pendingEndTag[seenLevel].getNamespaceURI(),
                    pendingEndTag[seenLevel].getLocalPart(), pendingEndTag[seenLevel].getLocalPart());
                pendingEndTag[seenLevel] = null;
            } else {
                switch (curLevel) {
                    case SemLevel.FIELD:
                        if (state.mToken.mDelimType == Delimiter.Type.REPET_SEP) {
                            if (fieldRepetIndex == -1) {
                                fieldRepetIndex = 1;
                            } else {
                                fieldRepetIndex++;
                            }
                        }
                        state.mSB2.setLength(0);
                        state.mSB2.append("fld");
                        state.mSB2.append(++fieldIndex);
                        if (fieldRepetIndex != -1) {
                            state.mSB2.append('_');
                            state.mSB2.append(++fieldRepetIndex);
                        }
                        break;
                    case SemLevel.COMPONENT:
                        state.mSB2.setLength(0);
                        state.mSB2.append("com");
                        state.mSB2.append(++compoIndex);
                        break;
                    case SemLevel.SUBCOMPONENT:
                        state.mSB2.setLength(0);
                        state.mSB2.append("sub");
                        state.mSB2.append(++subCompoIndex);
                        break;
                    case SemLevel.SEGMENT:
                        // not possible
                    default:
                        errMsg = mMessages.getString("HL7ENC-E0031.Illegal_semantic_level",
                            new Object[] { "" + curLevel });
                        throw new IllegalStateException(errMsg);
                }
                if (pendingEndTag[curLevel] == null) {
                    pendingEndTag[curLevel] = new QName(NS_URI_FOR_EXTENSIONS,
                        state.mSB2.toString());
                    state.mContentHandler.startElement(pendingEndTag[curLevel].getNamespaceURI(),
                        pendingEndTag[curLevel].getLocalPart(), pendingEndTag[curLevel].getLocalPart(),
                        mEmptyAttributes);
                }
                fireCharacters(state, useBuffer);
                state.mToken.mIsConsumed = true;
                state.mContentHandler.endElement(pendingEndTag[curLevel].getNamespaceURI(),
                    pendingEndTag[curLevel].getLocalPart(), pendingEndTag[curLevel].getLocalPart());
                pendingEndTag[curLevel] = null;
                for (int j = curLevel - 1; j >= seenLevel; j--) {
                    if (pendingEndTag[j] != null) {
                        state.mContentHandler.endElement(pendingEndTag[j].getNamespaceURI(),
                            pendingEndTag[j].getLocalPart(), pendingEndTag[j].getLocalPart());
                        pendingEndTag[j] = null;
                    }
                }
            }
            curLevel = seenLevel;
            if ((state.mToken.mDelimType & Delimiter.mDelimEndMask[state.mSemanticLevel]) != 0) {
                break;
            }
            state.mLexer.fillNextToken(state.mToken);
        }
        if (curLevel < SemLevel.FIELD) {
            curLevel = SemLevel.FIELD;
        }
        forwardToken(state, curLevel);
        return true;
    }

    private static QName crackOutWildcardElement(SchemaParticle wildcard,
            String localName,
            SchemaTypeSystem ts,
            SchemaGlobalElement[] matchedElement) {
        QNameSet qnSet = wildcard.getWildcardSet();
        if (qnSet.containsAll(mQNameSetForExtensions)) {
            matchedElement[0] = null;
            return new QName(NS_URI_FOR_EXTENSIONS, localName);
        }
        if (qnSet.includedURIs() == null) {
            // don't know how to crack out
            return null;
        }
        QName qName;
        SchemaGlobalElement element = null;
        for (Iterator iter = qnSet.includedURIs().iterator(); iter.hasNext();) {
            qName = new QName((String) iter.next(), localName);
            if ((element = ts.findElement(qName)) != null) {
                break;
            }
        }
        if (element != null) {
            matchedElement[0] = element;
            return element.getName();
        }
        // don't know how to crack out
        return null;
    }

    private static boolean isComplexType(SchemaType XMLType) {
        if (XMLType.isSimpleType()
                || XMLType.getContentType() == SchemaType.SIMPLE_CONTENT) {
            return false; // simple type
        }
        if (XMLType.getContentType() == SchemaType.MIXED_CONTENT) {
            return false; // escape type
        }
        if (XMLType.isURType()) {
            return false; // varies type
        }
        return true;
    }

    /**
     * Generate xml characters.
     * @param state parsing state.
     * @param useBuffer true if data is in state.mSB instead of state.mToken.mChars.
     * @throws org.xml.sax.SAXException
     */
    private static void fireCharacters(ParsingState state, boolean useBuffer)
            throws SAXException {
        HL7CharEscapeCoder coder = state.mLexer.mEscapeCoder;
        if (useBuffer) {
            if (state.mChars == null || state.mChars.length < state.mSB.length()) {
                state.mChars = new char[state.mSB.length()];
            }
            state.mSB.getChars(0, state.mSB.length(), state.mChars, 0);
            if (coder != null && state.mToken.mHasEscape) {
                coder.unescape(state.mChars, 0, state.mSB.length(), true);
                fireCoderResult(coder, state);
            } else {
                state.mContentHandler.characters(state.mChars, 0, state.mSB.length());
            }
        } else {
            if (coder != null && state.mToken.mHasEscape) {
                coder.unescape(state.mToken.mChars, state.mToken.mOffset, state.mToken.mCount,
                        state.mToken.mDelimType != Delimiter.Type.DELIM_NOT_READ);
                fireCoderResult(coder, state);
            } else {
                state.mContentHandler.characters(state.mToken.mChars, state.mToken.mOffset, state.mToken.mCount);
            }
        }
    }

    private static void fireCoderResult(HL7CharEscapeCoder coder,
            ParsingState state) throws SAXException {
        Result res;
        while (coder.hasResult()) {
            res = coder.getResult();
            switch (res.mType) {
            case Result.CHARDATA:
                state.mContentHandler.characters(res.mData, res.mStart, res.mCount);
                break;
            case Result.ESCAPE:
                if (state.mEspAttr == null) {
                    state.mEspAttr = new EscapeAttributes();
                }
                state.mEspAttr.setValue(0, new String(res.mData, res.mStart, res.mCount));
                state.mContentHandler.startPrefixMapping("", "");
                state.mContentHandler.startElement("", "escape", "escape", state.mEspAttr);
                state.mContentHandler.endElement("", "escape", "escape");
                state.mContentHandler.endPrefixMapping("");
                break;
            }
        }
    }

    private static void throwException(String msg, Exception cause,
            ParsingState state)
            throws SAXException {
        Locator loc = new LocatorImpl(state);
        StringBuilder sb = new StringBuilder(msg);
        sb.append(". At location: line=").append(loc.getLineNumber());
        sb.append(", column=").append(loc.getColumnNumber());
        throw new SAXParseException(sb.toString(), loc, cause);
    }

    private static class LocatorImpl implements Locator {

        private final ParsingState mState;

        public LocatorImpl(ParsingState state) {
            mState = state;
        }

        public String getPublicId() {
            return mState.mPublicId;
        }

        public String getSystemId() {
            return mState.mSystemId;
        }

        public int getLineNumber() {
            return mState.mToken != null ? mState.mToken.mLine : -1;
        }

        public int getColumnNumber() {
            return mState.mToken != null ? mState.mToken.mCol : -1;
        }
    }
}
