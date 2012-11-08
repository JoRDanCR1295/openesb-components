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
 * @(#)SwiftEncoder.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.swift;

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
import org.apache.xmlbeans.SchemaTypeLoader;
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
import com.sun.encoder.swift.runtime.provider.MarshalHandler;
import com.sun.encoder.swift.runtime.provider.UnmarshalAdaptor;
import java.net.URL;

/**
 * Encoder implementation for Swift.
 * 
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
class SwiftEncoder implements Encoder {
    
    private final EncoderType mType;  //Should never be null
    private final EncoderProperties mProperties; //Should never be null
                                                 //but might be empty
    private Transformer mTransformer;
    private SchemaGlobalElement mRootElement;
    private URL mSchemaLocation;
    
    SwiftEncoder(EncoderType type, EncoderProperties properties) {
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

    public String encodeToString(Source xmlSource) throws EncoderException {
        return encodeToString(xmlSource, null);
    }

    public Source decodeFromBytes(byte[] input) throws EncoderException {
        return decodeFromBytes(input, null);
    }

    public byte[] encodeToBytes(Source xmlSource) throws EncoderException {
        return encodeToBytes(xmlSource, null);
    }

    public boolean dispose() {
        mRootElement = null;
        return true;
    }

    public void setMeta(MetaRef xsd) throws EncoderConfigurationException {
        try {
            XmlObject schemaXmlObj;
            QName qName;
            XmlOptions options;
            SchemaTypeSystem schemaTS;
            SchemaTypeLoader typeLoader =
                XmlBeans.typeLoaderForClassLoader(
                        SwiftEncoder.class.getClassLoader());
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
            typeLoader =
                XmlBeans.typeLoaderUnion(
                    new SchemaTypeLoader[]{typeLoader,
                            schemaTS});
            qName = xsd.getRootElemName();
            if (qName == null) {
                throw new EncoderConfigurationException(
                        "Root element name cannot be null. path='"
                		+ xsd.getPath() + "'");
            }
            mRootElement = typeLoader.findElement(qName);
            if (mRootElement == null) {
                throw new EncoderConfigurationException(
                        "Unable to find global element '" + qName + "'");
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

    public void setMeta(URL schemaLocation, SchemaGlobalElement rootElemName)
            throws EncoderConfigurationException {
        try {
            mRootElement = rootElemName;
            mTransformer = TransformerFactory.newInstance().newTransformer();
            mSchemaLocation = schemaLocation;
        } catch (TransformerConfigurationException e) {
            throw new EncoderConfigurationException(e);
        } catch (TransformerFactoryConfigurationError e) {
            throw new EncoderConfigurationException(e);
        }
    }

    public Source decodeFromBytes(byte[] input, EncoderProperties properties)
            throws EncoderException {
        return decodeFromStream(new ByteArrayInputStream(input), properties);
    }

    public Source decodeFromReader(Reader input) throws EncoderException {
        return decodeFromReader(input, null);
    }

    public Source decodeFromReader(Reader input, EncoderProperties properties)
            throws EncoderException {
        InputSource inputSource = new InputSource(input);
        inputSource.setPublicId("character sequence data");
        UnmarshalAdaptor adaptor =
            new UnmarshalAdaptor(mSchemaLocation, mRootElement);
        return new SAXSource(adaptor, inputSource);
    }

    public Source decodeFromStream(InputStream input) throws EncoderException {
        return decodeFromStream(input, null);
    }

    public Source decodeFromStream(InputStream input,
            EncoderProperties properties) throws EncoderException {
        InputSource inputSource = new InputSource(input);
        inputSource.setPublicId("byte sequence data");
        UnmarshalAdaptor adaptor =
            new UnmarshalAdaptor(mSchemaLocation, mRootElement);
        return new SAXSource(adaptor, inputSource);
    }

    public Source decodeFromString(String input, EncoderProperties properties)
            throws EncoderException {
        return decodeFromReader(new StringReader(input), properties);
    }

    public byte[] encodeToBytes(Source src, EncoderProperties properties)
            throws EncoderException {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        encodeToStream(src, new BufferedOutputStream(out), properties);
        return out.toByteArray();
    }

    public void encodeToStream(Source in, OutputStream out)
            throws EncoderException {
        encodeToStream(in, out, null);
    }

    public void encodeToStream(Source in, OutputStream out,
            EncoderProperties properties) throws EncoderException {
        SAXResult result = new SAXResult(
                new MarshalHandler(mRootElement, out));
        try {
            mTransformer.transform(in, result);
            out.flush();
        } catch (TransformerException e) {
            throw new EncoderException(e);
        } catch (IOException e) {
            throw new EncoderException(e);
        }
    }

    public String encodeToString(Source src, EncoderProperties properties)
            throws EncoderException {
        StringWriter writer = new StringWriter();
        encodeToWriter(src, writer, properties);
        return writer.toString();
    }

    public void encodeToWriter(Source in, Writer out) throws EncoderException {
        encodeToWriter(in, out, null);
    }

    public void encodeToWriter(Source in, Writer out,
            EncoderProperties properties) throws EncoderException {
        SAXResult result = new SAXResult(
                new MarshalHandler(mRootElement, out));
        try {
            mTransformer.transform(in, result);
            out.flush();
        } catch (TransformerException e) {
            throw new EncoderException(e);
        } catch (IOException e) {
            throw new EncoderException(e);
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
}
