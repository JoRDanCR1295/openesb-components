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
package com.sun.encoder.hl7.runtime.provider;

import com.sun.encoder.hl7.util.Util;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaType;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import com.sun.encoder.hl7.i18n.Messages;

/**
 * This class implements SAX <code>ContentHandler</code> interface. It
 * translates SAX events into HL7 standard encoding rule encoded data.
 * 
 * @author Jun Xu
 * @since 6.0
 * @version
 */
public final class MarshalHandler implements ContentHandler {

    private static Messages mMessages =
        Messages.getMessages(MarshalHandler.class);
    private final String mCharset;
    private final OutputStream mOutputStream;
    private final SchemaGlobalElement mRootElement;
    private Writer mWriter;
    private DelimDataWriter mDelimDataWriter;
    private MarshalerFactory mMarshalFactory = new MarshalerFactory();
    // flag indicating if handling first/root element
    private boolean mIsRootElement = true;
    private Marshaler mMarshaler;
    private String mGroupNamePrefix;

    public MarshalHandler(SchemaGlobalElement element, OutputStream output) {
        this(element, output, "ASCII");
    }

    public MarshalHandler(SchemaGlobalElement element, OutputStream output,
        String charset) {
        if (element == null) {
            throw new NullPointerException(
                mMessages.getString("HL7ENC-E0001.Global_Element_is_Null"));
        }
        mRootElement = element;
        if (output == null) {
            throw new NullPointerException(
                mMessages.getString("HL7ENC-E0002.OutputStream_is_Null"));
        }
        mOutputStream = output;
        mWriter = null;
        mCharset = charset;
        mDelimDataWriter = null;
        mGroupNamePrefix = Util.getGroupNamePrefix(element);
    }

    public MarshalHandler(SchemaGlobalElement element, Writer output) {
        if (element == null) {
            throw new NullPointerException(
                mMessages.getString("HL7ENC-E0001.Global_Element_is_Null"));
        }
        mRootElement = element;
        if (output == null) {
            throw new NullPointerException(
                mMessages.getString("HL7ENC-E0003.Writer_is_Null"));
        }
        mWriter = output;
        mOutputStream = null;
        mCharset = null;
        mDelimDataWriter = null;
        mGroupNamePrefix = Util.getGroupNamePrefix(element);
    }

    public void setDocumentLocator(Locator locator) {
        // no-op
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
        // no-op
    }

    public void endPrefixMapping(String prefix) throws SAXException {
        // no-op
    }

    public void startElement(String uri, String localName, String qName,
        Attributes atts) throws SAXException {
        if (mIsRootElement) {
            if (!mRootElement.getName().getLocalPart().equals(localName)) {
                String errMsg = mMessages.getString("HL7ENC-E0004.Unexpected_Element",
                    new Object[]{mRootElement.getName(), uri, localName});
                throw new SAXException(errMsg);
            }
            mMarshaler = mMarshalFactory.getDocumentMarshaler(mRootElement,
                mDelimDataWriter, mGroupNamePrefix);
            mIsRootElement = false;
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
        // no-op
    }

    public void processingInstruction(String target, String data)
        throws SAXException {
        // no-op
    }

    public void skippedEntity(String name) throws SAXException {
        // no-op
    }

    public String getGroupNamePrefix() {
        return mGroupNamePrefix;
    }

    public void setGroupNamePrefix(String groupNamePrefix) {
        mGroupNamePrefix = groupNamePrefix;
    }

    static boolean isSimpleContent(SchemaType XMLType) {
        if (XMLType.isSimpleType()
            || XMLType.getContentType() == SchemaType.SIMPLE_CONTENT
            || XMLType.getContentType() == SchemaType.MIXED_CONTENT
            || XMLType.isURType()) {
            return true;
        }
        return false;
    }
}
