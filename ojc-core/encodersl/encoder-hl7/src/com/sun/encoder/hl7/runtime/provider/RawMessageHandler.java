/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.encoder.hl7.runtime.provider;

import com.sun.encoder.hl7.i18n.Messages;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.xmlbeans.SchemaGlobalElement;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;

/**
 *
 * @author James
 */
public class RawMessageHandler implements ContentHandler {

    // TODO: thise should probably have its own?
    private static Messages mMessages =
        Messages.getMessages(MarshalHandler.class);
    private final OutputStream mOutputStream;
    private Writer mWriter;
    private final String mCharset;
    private boolean mInRawMessage;

 public RawMessageHandler(SchemaGlobalElement element, OutputStream output) {
        this(element, output, "ASCII");
    }

    public RawMessageHandler(SchemaGlobalElement element, OutputStream output,
        String charset) {
        if (element == null) {
            throw new NullPointerException(
                mMessages.getString("HL7ENC-E0001.Global_Element_is_Null"));
        }
        if (output == null) {
            throw new NullPointerException(
                mMessages.getString("HL7ENC-E0002.OutputStream_is_Null"));
        }
        mOutputStream = output;
        mWriter = null;
        mCharset = charset;
    }

    public RawMessageHandler(SchemaGlobalElement element, Writer out) {
        mWriter = out;
        mCharset = "ASCII";
        mOutputStream = null;
    }

    public RawMessageHandler(SchemaGlobalElement element, Writer out, String charset) {
        mWriter = out;
        mCharset = charset;
        mOutputStream = null;
    }

    public void setDocumentLocator(Locator locator) {
        // noop
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
    }

    public void endDocument() throws SAXException {
         try {
            mWriter.flush();
        } catch (IOException e) {
            throw new SAXException(e);
        }
    }

    public void startPrefixMapping(String prefix, String uri) throws SAXException {
        // no-op
    }

    public void endPrefixMapping(String prefix) throws SAXException {
        // no-op
    }

    public void startElement(String uri, String localName, String qName, Attributes atts) throws SAXException {
        if ("RawMessage".equals(localName)) {
            mInRawMessage = true;
        }
    }

    public void endElement(String uri, String localName, String qName) throws SAXException {
        if ("RawMessage".equals(localName)) {
            mInRawMessage = false;
        }
    }

    public void characters(char[] ch, int start, int length) throws SAXException {
        if (mInRawMessage) {
            try {
                mWriter.write(ch, start, length);
            } catch (IOException ex) {
                throw new SAXException(ex);
            }
        }
    }

    public void ignorableWhitespace(char[] ch, int start, int length) throws SAXException {
        // noop
    }

    public void processingInstruction(String target, String data) throws SAXException {
        // noop
    }

    public void skippedEntity(String name) throws SAXException {
        // noop
    }

}
