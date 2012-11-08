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
 * @(#)HL7Encoder.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.hl7;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.sax.SAXResult;
import javax.xml.transform.sax.SAXSource;

import org.apache.xmlbeans.SchemaAnnotation;
import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaTypeLoader;
import org.apache.xmlbeans.SchemaTypeSystem;
import org.apache.xmlbeans.XmlBeans;
import org.apache.xmlbeans.XmlCursor;
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
import com.sun.encoder.hl7.appinfo.Hl7Encoding;
import com.sun.encoder.hl7.i18n.Messages;
import com.sun.encoder.hl7.runtime.provider.MarshalHandler;
import com.sun.encoder.hl7.runtime.provider.RawMessageHandler;
import com.sun.encoder.hl7.runtime.provider.RawMessageModeEnum;
import com.sun.encoder.hl7.runtime.provider.UnmarshalAdaptor;
import com.sun.encoder.runtime.provider.Misc;
import java.net.URL;

/**
 * Encoder implementation for HL7.
 * 
 * @author Jun Xu
 * @since 6.0
 */
public class HL7Encoder implements Encoder {

    private static Messages mMessages = Messages.getMessages(HL7Encoder.class);
    public static final String DEFAULT_ENCODING = "ASCII";

    /**
     * The qualified name of the "source" attribute in the "appinfo" element
     */
    public static final QName APPINFO_SOURCE_ATTR = new QName("source");

    /**
     * The namespace URN that represents the Sun encoder extension to XSD
     */
    public static final String ENCODER_NAMESPACE = "urn:com.sun:encoder";

    public static final String HL7_ENCODER_NAMESPACE = "urn:com.sun:encoder-hl7-1.0";

    public static final String HL7_NS = "urn:hl7-org:v2xml";

    private final EncoderType mType; // Should never be null

    private final EncoderProperties mProperties; // Should never be null

    // but might be empty
    private Transformer mTransformer;

    private SchemaGlobalElement mRootElement;

    // The pre-decoding charatcer coding read from the metadata (the XSD)
    private String mPreDecodeCharCoding;

    // The post-encoding charatcer coding read from the metadata (the XSD)
    private String mPostEncodeCharCoding;

    private URL mSchemaLocation;

    /** Logger object.*/
    private Logger mLog = Logger.getLogger(getClass().getName());
    private RawMessageModeEnum mRawMessageMode = RawMessageModeEnum.NONE;

    HL7Encoder(EncoderType type, EncoderProperties properties) {
        mType = type;
        if (properties.immutable()) {
            mProperties = properties;
        } else {
            mProperties = properties.cloneImmutable();
        }
    }

    public Source decodeFromString(String input)
        throws EncoderException {
        EncoderProperties encoderProps = null;
        return decodeFromString(input, encoderProps);
    }

    public String encodeToString(Source xmlSource)
        throws EncoderException {
        EncoderProperties encoderProps = null;
        return encodeToString(xmlSource, encoderProps);
    }

    public Source decodeFromBytes(byte[] input)
        throws EncoderException {
        EncoderProperties encoderProps = null;
        return decodeFromBytes(input, encoderProps);
    }

    public byte[] encodeToBytes(Source xmlSource)
        throws EncoderException {
        EncoderProperties encoderProps = null;
        return encodeToBytes(xmlSource, encoderProps);
    }

    public boolean dispose() {
        mRootElement = null;
        mTransformer = null;
        mSchemaLocation = null;
        System.gc();
        if (mLog.isLoggable(Level.FINE)) {
            mLog.fine("Invoked HL7Encoder.dispose() and System.gc().");
        }
        return true;
    }

    /**
     * Sets the xsd schema reference.
     * @param xsd
     * @throws EncoderConfigurationException
     */
    public void setMeta(MetaRef xsd)
            throws EncoderConfigurationException {
        try {
            SchemaTypeSystem schemaTS;
            SchemaTypeLoader typeLoader = XmlBeans.typeLoaderForClassLoader(
                    HL7Encoder.class.getClassLoader());
            String errMsg = null;
            URL xsdurl = xsd.getURL();
            if (xsdurl != null) {
                XmlOptions options = new XmlOptions();
                options.put(XmlOptions.COMPILE_DOWNLOAD_URLS, Boolean.TRUE);
                XmlObject schemaXmlObj = SchemaDocument.Factory.parse(xsdurl);
                schemaTS = XmlBeans.compileXsd(
                        new XmlObject[]{schemaXmlObj},
                        SchemaDocument.type.getTypeSystem(),
                        options);
            } else {
                XmlObject schemaXmlObj = SchemaDocument.Factory.parse(
                        new File(xsd.getPath()));
                schemaXmlObj.validate();
                schemaTS = XmlBeans.compileXsd(
                        new XmlObject[]{schemaXmlObj},
                        SchemaDocument.type.getTypeSystem(),
                        null);
            }
            typeLoader = XmlBeans.typeLoaderUnion(
                    new SchemaTypeLoader[]{typeLoader, schemaTS});
            QName qName = xsd.getRootElemName();
            if (qName == null) {
                errMsg = mMessages.getString("HL7ENC-E2001.Root_Element_is_Null",
                    new Object[] { xsd.getPath() });
                throw new EncoderConfigurationException(errMsg);
            }
            mRootElement = typeLoader.findElement(qName);
            if (mRootElement == null) {
                errMsg = mMessages.getString("HL7ENC-E2002.Global_Element_is_not_found",
                    new Object[] { qName });
                throw new EncoderConfigurationException(errMsg);
            }
            mTransformer = TransformerFactory.newInstance().newTransformer();
            if (xsdurl != null) {
                mSchemaLocation = xsdurl;
            } else {
                mSchemaLocation = new File(xsd.getPath()).toURL();
            }
            initCodings();
        } catch (XmlException e) {
            throw new EncoderConfigurationException(e.getMessage(), e);
        } catch (IOException e) {
            throw new EncoderConfigurationException(e.getMessage(), e);
        } catch (TransformerConfigurationException e) {
            throw new EncoderConfigurationException(e.getMessage(), e);
        } catch (TransformerFactoryConfigurationError e) {
            throw new EncoderConfigurationException(e.getMessage(), e);
        }
    }

    public void setMeta(URL schemaLocation, SchemaGlobalElement rootElemName)
        throws EncoderConfigurationException {
        try {
            mRootElement = rootElemName;
            mTransformer = TransformerFactory.newInstance().newTransformer();
            mSchemaLocation = schemaLocation;
            initCodings();
        } catch (TransformerConfigurationException e) {
            throw new EncoderConfigurationException(e.getMessage(), e);
        } catch (TransformerFactoryConfigurationError e) {
            throw new EncoderConfigurationException(e.getMessage(), e);
        } catch (XmlException e) {
            throw new EncoderConfigurationException(e.getMessage(), e);
        }
    }

    public Source decodeFromBytes(byte[] input, EncoderProperties properties)
        throws EncoderException {
        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder buf = new StringBuilder("Decode from input byte array");
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

    public Source decodeFromReader(Reader input)
        throws EncoderException {
        EncoderProperties encoderProps = null;
        return decodeFromReader(input, encoderProps);
    }

    public Source decodeFromReader(Reader input, EncoderProperties properties)
        throws EncoderException {
        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder buf = new StringBuilder("Decode from input reader");
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
        InputSource inputSource = new InputSource(input);
        inputSource.setPublicId("character sequence data");
        UnmarshalAdaptor adaptor = new UnmarshalAdaptor(mSchemaLocation, mRootElement, mRawMessageMode);
		return new SAXSource(adaptor, inputSource);
    }

    public Source decodeFromStream(InputStream input)
        throws EncoderException {
        return decodeFromStream(input, null);
    }

    public Source decodeFromStream(InputStream input,
            EncoderProperties properties)
            throws EncoderException {
        InputSource inputSource = new InputSource(input);
        inputSource.setPublicId("byte sequence data");

        String enc = null;
        if (properties != null) {
            enc = properties.getPreDecodeCharCoding();
        }
        if (enc == null) {
            enc = mProperties.getPreDecodeCharCoding();
        }
        if (enc == null) {
            enc = mPreDecodeCharCoding;
        }
        if (enc == null) {
            enc = DEFAULT_ENCODING;
            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine("No preDecodeCharCoding is set, so use default encoding="
                        + enc + " as input encoding.");
            }
        } else {
            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine("use preDecodeCharCoding=" + enc + " as input encoding.");
            }
        }
        inputSource.setEncoding(enc);

        UnmarshalAdaptor adaptor =
                new UnmarshalAdaptor(mSchemaLocation, mRootElement, mRawMessageMode);
        return new SAXSource(adaptor, inputSource);
    }

    public Source decodeFromString(String input, EncoderProperties properties)
            throws EncoderException {
        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder buf = new StringBuilder("Decode from string.");
            if (mLog.isLoggable(Level.FINER)) {
                mLog.finer(buf.toString());
            } else if (mLog.isLoggable(Level.FINE)) {
                mLog.fine(buf.toString());
            }
        }
        return decodeFromReader(new StringReader(input), properties);
    }

    public byte[] encodeToBytes(Source src, EncoderProperties properties)
        throws EncoderException {
        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder buf = new StringBuilder("Encode to bytes.");
            if (mLog.isLoggable(Level.FINER)) {
                mLog.finer(buf.toString());
            } else if (mLog.isLoggable(Level.FINE)) {
                mLog.fine(buf.toString());
            }
        }
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        encodeToStream(src, out, properties);
        return out.toByteArray();
    }

    public void encodeToStream(Source in, OutputStream out)
        throws EncoderException {
        encodeToStream(in, out, null);
    }

    public void encodeToStream(Source input, OutputStream out,
        EncoderProperties properties) throws EncoderException {
        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder buf = new StringBuilder("Encode to stream from source=");
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
        String enc = null;
        if (properties != null) {
            enc = properties.getPostEncodeCharCoding();
        }
        if (enc == null) {
            enc = mProperties.getPostEncodeCharCoding();
        }
        if (enc == null) {
            enc = mPostEncodeCharCoding;
        }
        if (enc == null) {
            enc = DEFAULT_ENCODING;
            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine("No postEncodeCharCoding is set, so use default encoding="
                        + enc + " as output encoding.");
            }
        } else {
            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine("use postEncodeCharCoding=" + enc + " as output encoding.");
            }
        }
        
        SAXResult result;
        switch (mRawMessageMode) {
            case WHOLE_MESSAGE:
                result = new SAXResult(new RawMessageHandler(mRootElement, out, enc));
                break;
            default:
                result = new SAXResult(new MarshalHandler(mRootElement, out, enc));
                break;
        }

        try {
            mTransformer.transform(input, result);
            out.flush();
        } catch (TransformerException e) {
            throw new EncoderException(e.getMessage(), e);
        } catch (IOException e) {
            throw new EncoderException(e.getMessage(), e);
        }
    }

    public String encodeToString(Source input, EncoderProperties properties)
        throws EncoderException {
        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder buf = new StringBuilder("Encode to string from source=");
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
		StringWriter writer = new StringWriter();
        encodeToWriter(input, writer, properties);
        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder sb = new StringBuilder(
                    "Encoded string in raw HL7 format=").append(writer.toString());
            mLog.fine(sb.toString());
        }
        return writer.toString();
    }

    public void encodeToWriter(Source in, Writer out)
            throws EncoderException {
        encodeToWriter(in, out, null);
    }

    public void encodeToWriter(Source in, Writer out,
            EncoderProperties properties) throws EncoderException {

        SAXResult result;
        switch (mRawMessageMode) {
            case WHOLE_MESSAGE:
                result = new SAXResult(new RawMessageHandler(mRootElement, out));
                break;
            default:
                result = new SAXResult(new MarshalHandler(mRootElement, out));
                break;
        }

        try {
            mTransformer.transform(in, result);
            out.flush();
        } catch (TransformerException e) {
            throw new EncoderException(e.getMessage(), e);
        } catch (IOException e) {
            throw new EncoderException(e.getMessage(), e);
        }
    }

    /**
     * Gets the immutable properties associated with the encoder
     */
    public EncoderProperties getProperties() {
        return mProperties;
    }

    /**
     * Gets the type of the encoder.
     */
    public EncoderType getType() {
        return mType;
    }

    /**
     * Reads coding information from mRootElement and store them in
     * mPreDecodeCharCoding and mPostEncodeCharCoding.
     * 
     * @throws XmlException
     */
    private void initCodings() throws XmlException {
        SchemaAnnotation anno = mRootElement.getAnnotation();
        if (anno == null) {
            return;
        }
        XmlObject[] xmlObjs = anno.getApplicationInformation();
        if (xmlObjs == null || xmlObjs.length == 0) {
            return;
        }
        Hl7Encoding hl7Encoding = null;
        for (int i = 0; i < xmlObjs.length; i++) {
            XmlCursor cursor = xmlObjs[i].newCursor();
            String source = cursor.getAttributeText(APPINFO_SOURCE_ATTR);
            cursor.dispose();
            if (!ENCODER_NAMESPACE.equals(source)) {
                continue;
            }
            if (xmlObjs[i] instanceof Hl7Encoding) {
                hl7Encoding = (Hl7Encoding) xmlObjs[i];
            } else {
                hl7Encoding = Hl7Encoding.Factory.parse(xmlObjs[i].xmlText());
            }
            if (hl7Encoding != null && hl7Encoding.isSetPreDecodeCharCoding()) {
                mPreDecodeCharCoding = hl7Encoding.getPreDecodeCharCoding();
            }
            if (hl7Encoding != null && hl7Encoding.isSetPostEncodeCharCoding()) {
                mPostEncodeCharCoding = hl7Encoding.getPostEncodeCharCoding();
            }
        }
    }
}
