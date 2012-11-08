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

package com.sun.encoder.coco.runtime;

import com.sun.encoder.coco.runtime.messages.ErrorManager;
import com.sun.encoder.coco.runtime.messages.Message;
import com.sun.encoder.coco.runtime.messages.MessageCatalog;
import com.sun.encoder.runtime.provider.Misc;
import com.sun.encoder.tools.xml.EmptyAttributes;
import com.sun.encoder.tools.xml.SchemaLocationAttributes;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

import javax.xml.XMLConstants;
import org.xml.sax.Attributes;
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

import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * This class implements an unmarshal adaptor for decoding COBOL Copybook
 * encoded data.  It implements SAX XMLReader interface, but
 * the input source is not in XML format but COBOL Copybook
 * encoded.  The adaptor translates COBOL Copybook encoded data
 * into a set of SAX events.
 * 
 * @author Jun Xu
 * @since 6.0
 */
public final class UnmarshalAdaptor implements XMLReader {
    
    private static final ErrorManager cErrorMgr =
        ErrorManager.getManager("OpenESB.encoder.COBOLCopybook."
                                + UnmarshalAdaptor.class.getName());
    
    private static final Attributes mEmptyAttributes =
        new EmptyAttributes();
    
    private final URL mSchemaLocation;
    private final RuleNode mTopRule;
    private final byte[][] mRedefined;
    private final Integer[] mOccursDependOn;
    
    private EntityResolver mEntityResolver;
    private DTDHandler mDTDHandler;
    private ContentHandler mContentHandler;
    private ErrorHandler mErrorHandler;
    
    private InputStream mInStream;
    private String mPublicId;
    private String mSystemId;
    private long mBytesRead;
    
    /** Logger object.*/
    private Logger mLog = Logger.getLogger(getClass().getName());

    /** Creates a new instance of UnmarshalAdaptor */
    public UnmarshalAdaptor(URL schemaLocation, RuleNode topRule) {
        if (topRule == null) {
            throw new NullPointerException("no top rule.");
        }
        mSchemaLocation = schemaLocation;
        mTopRule = topRule;
        mRedefined =
            new byte[topRule.getContext().getRedefinedNodes().length][];
        mOccursDependOn =
            new Integer[topRule.getContext().getOccursDependOnNodes().length];
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
                        "Feature '" + name + "' with value " + value
                        + " is not supported.");
            }
            return;
        }
        if ("http://xml.org/sax/features/namespace-prefixes".equals(name)) {
            if (value) {
                throw new SAXNotSupportedException(
                        "Feature '" + name + "' with value " + value
                        + " is not supported.");
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

    public void parse(InputSource input) throws IOException, SAXException {
        if (mContentHandler == null) {
            throw new NullPointerException("Missing content handler.");
        }
        mInStream = input.getByteStream();
        if (mInStream == null) {
            throw new NullPointerException("No input.");
        }
        if (!mInStream.markSupported()) {
            mInStream = new BufferedInputStream(mInStream);
        }
        // start XML document
        mContentHandler.startDocument();
        mContentHandler.startPrefixMapping("xsi",
                XMLConstants.W3C_XML_SCHEMA_INSTANCE_NS_URI);
        // parse
        parse(mTopRule, mInStream);
        // end XML document
        mContentHandler.endPrefixMapping("xsi");
        mContentHandler.endDocument();
    }

    public void parse(String systemId) throws IOException, SAXException {
        parse(new InputSource(systemId));
    }
    
    private void parse(RuleNode rule, InputStream inStream)
            throws IOException, SAXException {
        if (mLog.isLoggable(Level.FINEST)) {
            mLog.finest("Parse node '" + rule.toString() + "'");
        } else if (mLog.isLoggable(Level.FINE)) {
            mLog.fine("Parse node '" + rule.getQName().getLocalPart() + "'");
        }
        // check on occursDependOn
        RuleNode occursDependOn = rule.getOccursDependOn();
        int occurs = rule.getMaxOccurs();
        if (occursDependOn != null) {
            if (mOccursDependOn[occursDependOn.getOccursDependOnIndex()] == null) {
                throwException("Occurs Depending On field has not been evaluated: "
                        + occursDependOn.getQName(), null);
            }
            occurs = mOccursDependOn[occursDependOn.getOccursDependOnIndex()];
            if (mLog.isLoggable(Level.FINEST)) {
                mLog.finest("Based on dependency, find maxOccurs=" + occurs);
            }
        }
        // check on redefine index
        int redefineIndex = rule.getRedefineIndex();
        if (redefineIndex >= 0) {
            int size = rule.getCharacteristics().getSize();
            mInStream.mark(size + 1);
            byte[] bytes = new byte[size];
            mInStream.read(bytes);
            mInStream.reset();
            mRedefined[redefineIndex] = bytes;
        }
        // check on redefineNode
        RuleNode redefineNode = rule.getRedefinedNode();
        InputStream currentStream = inStream;
        if (redefineNode != null) {
            if (mRedefined[redefineNode.getRedefineIndex()] == null) {
                throwException("Redefined field has not been evaluated: "
                        + redefineNode.getQName(), null);
            }
            currentStream =
                    new BufferedInputStream(
                        new ByteArrayInputStream(
                            mRedefined[redefineNode.getRedefineIndex()]));
        }
        while (occurs > 0) {
            Attributes attrs = mEmptyAttributes;
            if (rule.isTop()) {
                attrs = new SchemaLocationAttributes(
                            rule.getQName().getNamespaceURI(),
                            mSchemaLocation);
            }
            mContentHandler.startElement(
                    rule.getQName().getNamespaceURI(),
                    rule.getQName().getLocalPart(),
                    rule.getQName().getLocalPart(),
                    attrs);
            
            RuleNode[] children = rule.getChildren();
            if (children != null && children.length > 0) {
                for (int i = 0; i < children.length; i++) {
                    if (mLog.isLoggable(Level.FINEST)) {
                        mLog.finest("Process child node[" + i + "] of " + rule);
                    } else if (mLog.isLoggable(Level.FINE)) {
                        mLog.fine("Process child node[" + i + "] of "
                                + rule.getQName().getLocalPart());
                    }
                    parse(children[i], currentStream);
                }
            } else {
                String data;
                int usage = rule.getCharacteristics().getUsage();
                if (mLog.isLoggable(Level.FINEST)) {
                    mLog.finest("It's a leaf node '" + rule.getQName().getLocalPart()
                            + "' with usage-category=" + rule.getCharacteristics().descUsageCategory());
                }
                switch (usage) {
                    case CobolCharacteristics.USAGE_COMP1:
                        data = String.valueOf(
                                CobolDataConverter.decodeToFloat(currentStream));
                        break;
                    case CobolCharacteristics.USAGE_COMP2:
                        data = String.valueOf(
                                CobolDataConverter.decodeToDouble(currentStream));
                        break;
                    case CobolCharacteristics.USAGE_BINARY:
                    case CobolCharacteristics.USAGE_COMP:
                    case CobolCharacteristics.USAGE_COMP3:
                    case CobolCharacteristics.USAGE_COMP4:
                    case CobolCharacteristics.USAGE_COMP5:
                    case CobolCharacteristics.USAGE_PACKED:
                    case CobolCharacteristics.USAGE_INDEX:
                        data = CobolDataConverter.decodeToBigDecimal(
                                currentStream,
                                rule.getPicture(),
                                rule.getCharacteristics(),
                                rule.getCharEncoding()).toString();
                        break;
                    case CobolCharacteristics.USAGE_DISPLAY:
                    case CobolCharacteristics.USAGE_DISPLAY1:
                        switch (rule.getCharacteristics().getPicCategory()) {
                            case CobolCharacteristics.PIC_EXFLOAT:
                            case CobolCharacteristics.PIC_NUM:
                                data = CobolDataConverter.decodeToBigDecimal(
                                        currentStream,
                                        rule.getPicture(),
                                        rule.getCharacteristics(),
                                        rule.getCharEncoding()).toString();
                                break;
                            case CobolCharacteristics.PIC_ALPHA:
                            case CobolCharacteristics.PIC_ALPHANUM:
                            case CobolCharacteristics.PIC_ALPHANUME:
                            case CobolCharacteristics.PIC_NUME:
                            case CobolCharacteristics.PIC_DBCS:
                                data = CobolDataConverter.decodeToString(
                                        currentStream,
                                        rule.getPicture(),
                                        rule.getCharacteristics(),
                                        rule.getCharEncoding());
                                break;
                            default:
                                Message msg = MessageCatalog.getMessage("CCCB4113");
                                String err =
                                        msg.formatText(
                                        new Object[]{rule.getQName(),
                                            rule.getCharacteristics().getUsage()});
                                cErrorMgr.log(ErrorManager.Severity.ERROR, null, err);
                                throw new SAXException(err);
                        }
                        break;
                    default:
                        Message msg = MessageCatalog.getMessage("CCCB4112");
                        String err = msg.formatText(new Object[]{rule.getQName(),
                                    rule.getCharacteristics().getUsage()});
                        cErrorMgr.log(ErrorManager.Severity.ERROR, null, err);
                        throw new SAXException(err);
                }
                // XML characters
                if (mLog.isLoggable(Level.FINEST)) {
                    mLog.finest("Data parsed for node '" + rule.getQName().getLocalPart()
                            + "'=" + Misc.printable(data));
                }
                char[] chars = new char[data.length()];
                data.getChars(0, data.length(), chars, 0);
                mContentHandler.characters(chars, 0, chars.length);
                final int dependOnIndex = rule.getOccursDependOnIndex();
                if (dependOnIndex >= 0) {
                    mOccursDependOn[dependOnIndex] = new Integer(data);
                }
            }

            // end element
            mContentHandler.endElement(
                    rule.getQName().getNamespaceURI(),
                    rule.getQName().getLocalPart(),
                    rule.getQName().getLocalPart());
            occurs--;
        }
    }
                                
    private void throwException(String msg, Exception cause)
            throws SAXException {
        throw new SAXParseException(msg, new LocatorImpl(), cause);
    }
            
    private class LocatorImpl implements Locator {

        public String getPublicId() {
            return mPublicId;
        }

        public String getSystemId() {
            return mSystemId;
        }

        public int getLineNumber() {
            //COBOL Copybook data is a stream of bytes.  Assume the line number
            //is always one.
            return 1;
        }

        public int getColumnNumber() {
            return (int) mBytesRead;
        }
    }
}
