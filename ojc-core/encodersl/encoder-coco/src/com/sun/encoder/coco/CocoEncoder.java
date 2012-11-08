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
 * @(#)CocoEncoder.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.coco;

import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;

import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.sax.SAXResult;
import javax.xml.transform.sax.SAXSource;

import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaTypeSystem;
import org.apache.xmlbeans.XmlBeans;
import org.apache.xmlbeans.XmlException;
import org.apache.xmlbeans.XmlObject;
import org.apache.xmlbeans.XmlOptions;
import org.apache.xmlbeans.impl.xb.xsdschema.SchemaDocument;
import org.xml.sax.InputSource;

import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderConfigurationException;
import com.sun.encoder.EncoderException;
import com.sun.encoder.EncoderProperties;
import com.sun.encoder.EncoderType;
import com.sun.encoder.MetaRef;
import com.sun.encoder.coco.runtime.MarshalHandler;
import com.sun.encoder.coco.runtime.RuleNode;
import com.sun.encoder.coco.runtime.UnmarshalAdaptor;
import com.sun.encoder.runtime.ReaderInputStream;
import com.sun.encoder.runtime.WriterOutputStream;

import com.sun.encoder.runtime.provider.Misc;
import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Encoder implementation for COBOL Copybook.
 * 
 * @author Jun Xu
 * @since 6.0
 */
class CocoEncoder implements Encoder {

    private final EncoderType mType;  //Should never be null
    private final EncoderProperties mProperties; //Should never be null
                                                 //but might be empty
    private Transformer mTransformer;
    private RuleNode mTopRule;
    private URL mSchemaLocation;
    
    /** Logger object.*/
    private Logger mLog = Logger.getLogger(getClass().getName());

    CocoEncoder(EncoderType type, EncoderProperties properties) {
        mType = type;
        if (properties.immutable()) {
            mProperties = properties;
        } else {
            mProperties = properties.cloneImmutable();
        }
    }
    
    public Source decodeFromString(String input) throws EncoderException {
        return decodeFromString(input, null);
    }

    public String encodeToString(Source input) throws EncoderException {
        return encodeToString(input, null);
    }

    public Source decodeFromBytes(byte[] input) throws EncoderException {
        return decodeFromBytes(input, null);
    }

    public byte[] encodeToBytes(Source xmlSource) throws EncoderException {
        return encodeToBytes(xmlSource, null);
    }

    public Source decodeFromBytes(byte[] input, EncoderProperties properties)
        throws EncoderException {
        if (mLog.isLoggable(Level.FINE)) {
            StringBuffer buf = new StringBuffer("Decode from input byte array");
            buf.append(", total bytes=").append(input.length);
            if (mLog.isLoggable(Level.FINER)) {
                buf.append(", encoderProperties=[");
                buf.append(properties == null ? "null" : properties);
                buf.append("]");
            }
            if (mLog.isLoggable(Level.FINEST)) {
                buf.append(", input bytes=[").append(Misc.printable(input));
                buf.append("]");
            }
            buf.append(".");
            if (mLog.isLoggable(Level.FINEST)) {
                mLog.finest(buf.toString());
            } else if (mLog.isLoggable(Level.FINER)) {
                mLog.finer(buf.toString());
            } else if (mLog.isLoggable(Level.FINE)) {
                mLog.fine(buf.toString());
            }
        }

        return decodeFromStream(new ByteArrayInputStream(input), properties);
    }

    public Source decodeFromReader(Reader input) throws EncoderException {
        return decodeFromReader(input, null);
    }

    public Source decodeFromReader(Reader reader, EncoderProperties properties)
            throws EncoderException {
        if (mLog.isLoggable(Level.FINE)) {
            StringBuffer buf = new StringBuffer("Decode from reader=");
            buf.append(reader.getClass().getName());
            if (mLog.isLoggable(Level.FINER)) {
                buf.append(", encoderProperties=[");
                buf.append(properties == null ? "null" : properties);
                buf.append("]");
            }
            buf.append(".");
            if (mLog.isLoggable(Level.FINER)) {
                mLog.finer(buf.toString());
            } else if (mLog.isLoggable(Level.FINE)) {
                mLog.fine(buf.toString());
            }
        }
        String charCoding = null;
        if (properties != null) {
            charCoding = properties.getPreDecodeCharCoding();
        }
        if (charCoding == null) {
            charCoding = mProperties.getPreDecodeCharCoding();
        }
        if (charCoding == null) {
            charCoding = mTopRule.getContext().getPreDecodeCharCoding();
        }
        if (charCoding == null) {
            throw new EncoderException(
                    "The data nature of the COBOL Copybook encoder type is byte "
                    + "based but the pre-decoding character coding is not set "
                    + "for this encoder. Don't know how to convert character "
                    + "based data into byte based data.");
        } else {
            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine("Use charCoding=" + charCoding);
            }
        }
        ReaderInputStream wrapper;
        try {
            wrapper = new ReaderInputStream(reader, charCoding);
            return decodeFromStream(wrapper, properties);
        } catch (UnsupportedEncodingException e) {
            throw new EncoderException(e);
        }
    }

    public Source decodeFromStream(InputStream input) throws EncoderException {
        return decodeFromStream(input, null);
    }

    public Source decodeFromStream(InputStream input,
            EncoderProperties properties) throws EncoderException {
        InputSource inputSource = new InputSource(input);
        inputSource.setPublicId("byte sequence data");
        UnmarshalAdaptor adaptor =
            new UnmarshalAdaptor(mSchemaLocation, mTopRule);
        return new SAXSource(adaptor, inputSource);
    }

    public Source decodeFromString(String input, EncoderProperties properties)
            throws EncoderException {
        return decodeFromReader(new StringReader(input), properties);
    }
    
    public byte[] encodeToBytes(Source input, EncoderProperties properties)
            throws EncoderException {
        if (mLog.isLoggable(Level.FINE)) {
            StringBuffer buf = new StringBuffer("Encode to bytes from source=");
            buf.append(input.getClass().getName());
            if (mLog.isLoggable(Level.FINER)) {
                buf.append(", encoderProperties=[");
                buf.append(properties == null ? "null" : properties);
                buf.append("]");
            }
            buf.append(".");
            if (mLog.isLoggable(Level.FINER)) {
                mLog.finer(buf.toString());
            } else if (mLog.isLoggable(Level.FINE)) {
                mLog.fine(buf.toString());
            }
        }
        ByteArrayOutputStream outStream = new ByteArrayOutputStream();
        encodeToStream(input, 
                new BufferedOutputStream(outStream), properties);
        byte[] output = outStream.toByteArray();

        if (mLog.isLoggable(Level.FINE)) {
            StringBuffer buf = new StringBuffer();
            buf.append("Before postcoding, output byte size=");
            buf.append(output.length);
            if (mLog.isLoggable(Level.FINEST)) {
                buf.append(", output bytes=[");
                buf.append(Misc.printable(output)).append("]");
            }
            buf.append(".");
            if (mLog.isLoggable(Level.FINEST)) {
                mLog.finest(buf.toString());
            } else if (mLog.isLoggable(Level.FINE)) {
                mLog.fine(buf.toString());
            }
        }
        return output;
    }
    
    public void encodeToStream(Source input, OutputStream output)
            throws EncoderException {
        encodeToStream(input, output, null);
    }
    
    public void encodeToStream(Source input, OutputStream output,
            EncoderProperties properties) throws EncoderException {
        SAXResult result = new SAXResult(
                new MarshalHandler(mTopRule, output));
        try {
            mTransformer.transform(input, result);
            output.flush();
        } catch (TransformerException e) {
            throw new EncoderException(e);
        } catch (IOException e) {
            throw new EncoderException(e);
        }
    }
    
    public String encodeToString(Source input, EncoderProperties properties)
            throws EncoderException {
        Writer writer = new StringWriter();
        encodeToWriter(input, writer, properties);
        return writer.toString();
    }
    
    public void encodeToWriter(Source input, Writer output)
            throws EncoderException {
        encodeToWriter(input, output, null);
    }
    
    public void encodeToWriter(Source input, Writer output,
            EncoderProperties properties) throws EncoderException {
        if (mLog.isLoggable(Level.FINE)) {
            StringBuffer buf = new StringBuffer("Encode to writer from source=");
            buf.append(input.getClass().getName());
            if (mLog.isLoggable(Level.FINER)) {
                buf.append(", encoderProperties=[");
                buf.append(properties == null ? "null" : properties);
                buf.append("]");
            }
            buf.append(".");
            if (mLog.isLoggable(Level.FINER)) {
                mLog.finer(buf.toString());
            } else if (mLog.isLoggable(Level.FINE)) {
                mLog.fine(buf.toString());
            }
        }
        String charCoding = null;
        if (properties != null) {
            charCoding = properties.getPostEncodeCharCoding();
        }
        if (charCoding == null) {
            charCoding = mProperties.getPostEncodeCharCoding();
        }
        if (charCoding == null) {
            charCoding = mTopRule.getContext().getPostEncodeCharCoding();
        }
        if (charCoding == null) {
            throw new EncoderException(
                    "The data nature of the COBOL Copybook encoder type is byte "
                    + "based but the post-encoding character coding is not set "
                    + "for this encoder. Don't know how to convert byte based "
                    + "data into character based data.");
        } else {
            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine("Use charCoding=" + charCoding);
            }
        }

        WriterOutputStream wrapper;
        try {
            wrapper = new WriterOutputStream(output, charCoding);
            encodeToStream(input, wrapper, properties);
            wrapper.flushToWriter(true);
        } catch (IOException e) {
            throw new EncoderException(e);
        }
    }

    public boolean dispose() {
        mTopRule = null;
        return true;
    }

    public EncoderProperties getProperties() {
        return mProperties;
    }

    public EncoderType getType() {
        return mType;
    }

    void setMeta(MetaRef xsd) throws EncoderConfigurationException {
        try {
            XmlObject schemaXmlObj;
            QName qName;
            XmlOptions options;
            SchemaTypeSystem schemaTS;
            if (xsd.getURL() != null) {
                options = new XmlOptions();
                options.put(XmlOptions.COMPILE_DOWNLOAD_URLS, Boolean.TRUE);
                schemaXmlObj = SchemaDocument.Factory.parse(xsd.getURL());
                schemaTS =
                    XmlBeans.compileXsd(new XmlObject[]{schemaXmlObj},
                        SchemaDocument.type.getTypeSystem(), options);
            } else {
                schemaXmlObj =
                    SchemaDocument.Factory.parse(
                            new File(xsd.getPath()));
                schemaXmlObj.validate();
                schemaTS =
                    XmlBeans.compileXsd(new XmlObject[]{schemaXmlObj},
                        SchemaDocument.type.getTypeSystem(), null);
            }
            qName = xsd.getRootElemName();
            if (qName == null) {
                throw new EncoderConfigurationException(
                        "Root element name cannot be null. path='"
                		+ xsd.getPath() + "'");
            }
            mTopRule = RuleNode.readRules(schemaTS, qName);
            if (mTopRule == null) {
                throw new EncoderConfigurationException(
                        "Unable to read metadata for '" + qName + "'");
            }
            mTransformer = TransformerFactory.newInstance().newTransformer();
            if (xsd.getURL() != null) {
                mSchemaLocation = xsd.getURL();
            } else {
                mSchemaLocation = new File(xsd.getPath()).toURL();
            }
        } catch (XmlException e) {
            throw new EncoderConfigurationException(e);
        } catch (IOException e) {
            throw new EncoderConfigurationException(e);
        } catch (TransformerConfigurationException e) {
            throw new EncoderConfigurationException(e);
        } catch (TransformerFactoryConfigurationError e) {
            throw new EncoderConfigurationException(e);
        }
    }

    void setMeta(URL schemaLocation, SchemaGlobalElement rootElemName)
            throws EncoderConfigurationException {
        try {
            mTopRule = RuleNode.readRules(rootElemName.getTypeSystem(),
                    rootElemName.getName());
            if (mTopRule == null) {
                throw new EncoderConfigurationException(
                        "Unable to read metadata for '"
                        + rootElemName.getName() + "'");
            }
            mTransformer = TransformerFactory.newInstance().newTransformer();
            mSchemaLocation = schemaLocation;
        } catch (TransformerConfigurationException e) {
            throw new EncoderConfigurationException(e);
        } catch (TransformerFactoryConfigurationError e) {
            throw new EncoderConfigurationException(e);
        }
    }
}
