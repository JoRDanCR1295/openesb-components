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
 * @(#)CustomEncoder.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.custom;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.io.Writer;

import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.sax.SAXResult;
import javax.xml.transform.sax.SAXSource;

import org.apache.xmlbeans.SchemaGlobalElement;
import org.xml.sax.ContentHandler;
import org.xml.sax.DTDHandler;
import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;
import org.xml.sax.XMLReader;

import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderConfigurationException;
import com.sun.encoder.EncoderException;
import com.sun.encoder.EncoderProperties;
import com.sun.encoder.EncoderType;
import com.sun.encoder.MetaRef;
import com.sun.encoder.custom.runtime.provider.BudOutput;
import com.sun.encoder.custom.runtime.provider.Nodes;
import com.sun.encoder.custom.runtime.provider.NodesFactory;
import com.sun.encoder.custom.runtime.provider.OtdDelim;
import com.sun.encoder.custom.runtime.provider.Parse;
import com.sun.encoder.runtime.CoderFactory;
import com.sun.encoder.runtime.StringCoder;
import com.sun.encoder.runtime.TransCoder;
import com.sun.encoder.runtime.provider.Misc;
import com.sun.encoder.runtime.provider.StringOtdInputStreamImpl;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Encoder implementation for custom encoder.
 *
 * @author Jun Xu
 * @since 6.0
 */
class CustomEncoder implements Encoder {

    /** encoder type. Should never be null. */
    private final EncoderType mEncoderType;

    /** encoder properties, should never be null but might be empty. */
    private final EncoderProperties mEncoderProps;

    /** transformer object. */
    private Transformer mTransformer;

    /** Nodes object holding the complete set of node descriptors. */
    private Nodes mNodes;

    /** Logger object.*/
    private Logger mLog = Logger.getLogger(getClass().getName());

    /**
     * Constructs a new CustomEncoder instance.
     * @param encoderType encoder type.
     * @param encoderProps encoder properties.
     */
    CustomEncoder(EncoderType encoderType, EncoderProperties encoderProps) {
        mEncoderType = encoderType;
        if (encoderProps.immutable()) {
            mEncoderProps = encoderProps;
        } else {
            mEncoderProps = encoderProps.cloneImmutable();
        }
    }

    public Source decodeFromString(String input)
        throws EncoderException {
        EncoderProperties encoderProps = null;
        return decodeFromString(input, encoderProps);
    }

    public String encodeToString(Source input)
        throws EncoderException {
        EncoderProperties encoderProps = null;
        return encodeToString(input, encoderProps);
    }

    public Source decodeFromBytes(byte[] input)
        throws EncoderException {
        EncoderProperties encoderProps = null;
        return decodeFromBytes(input, encoderProps);
    }

    public byte[] encodeToBytes(Source input)
        throws EncoderException {
        EncoderProperties encoderProps = null;
        return encodeToBytes(input, encoderProps);
    }

    public boolean dispose() {
        //nothing needs to be cleaned up
        return true;
    }

    /**
     * Sets the xsd meta.
     * @param xsd xsd meta reference.
     * @throws com.sun.encoder.EncoderConfigurationException
     */
    public void setMeta(MetaRef xsd)
        throws EncoderConfigurationException {
        if (xsd.getURL() != null) {
            mNodes = NodesFactory.loadFromXSD(
                    xsd.getURL(), xsd.getRootElemName());
        } else {
            mNodes = NodesFactory.loadFromXSD(
                    new File(xsd.getPath()), xsd.getRootElemName());
        }
        try {
            mTransformer = TransformerFactory.newInstance().newTransformer();
        } catch (TransformerConfigurationException e) {
            throw new EncoderConfigurationException(e);
        } catch (TransformerFactoryConfigurationError e) {
            throw new EncoderConfigurationException(e);
        }
    }

    /**
     * Sets the meta based on root element.
     * @param rootElement the root element.
     * @throws com.sun.encoder.EncoderConfigurationException
     */
    public void setMeta(SchemaGlobalElement rootElement)
            throws EncoderConfigurationException {
        mNodes = NodesFactory.loadFromXSD(rootElement);
        try {
            mTransformer = TransformerFactory.newInstance().newTransformer();
        } catch (TransformerConfigurationException e) {
            throw new EncoderConfigurationException(e);
        } catch (TransformerFactoryConfigurationError e) {
            throw new EncoderConfigurationException(e);
        }
    }

    public Source decodeFromBytes(byte[] input, EncoderProperties encoderProps)
        throws EncoderException {
        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder buf = new StringBuilder("Decode from input byte array");
            buf.append(", total bytes=").append(input.length);
            if (mLog.isLoggable(Level.FINER)) {
                buf.append(", encoderProperties=[");
                buf.append(encoderProps == null ? "null" : encoderProps);
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

        String preDecodeCharCoding = null;
        if (encoderProps != null) {
            preDecodeCharCoding = encoderProps.getPreDecodeCharCoding();
        }
        if (preDecodeCharCoding == null) {
            preDecodeCharCoding = mEncoderProps.getPreDecodeCharCoding();
        }
        byte[] bytes;
        TransCoder transCoder = null;
        if (preDecodeCharCoding != null) {
            if (!CoderFactory.hasCoder(preDecodeCharCoding)) {
                throw new EncoderException("Unsupported encoding: \'"
                    + preDecodeCharCoding + "\'",
                    new UnsupportedEncodingException(preDecodeCharCoding));
            }
            String from = mNodes.getAnteCoding();
            String to = preDecodeCharCoding;
            transCoder = CoderFactory.getTrans(from, to);
        } else {
            transCoder = mNodes.getAntecoder();
        }
        boolean readOnly = false;
        bytes = transCoder.recode(input, readOnly);

        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder buf = new StringBuilder();
            buf.append("Before decoding, the recoded byte array size=");
            buf.append(bytes.length).append(" using transCoder=[");
            buf.append(transCoder);
            if (mLog.isLoggable(Level.FINEST)) {
                buf.append("], recoded bytes=[").append(Misc.printable(bytes));
            }
            buf.append("].");
            if (mLog.isLoggable(Level.FINEST)) {
                mLog.finest(buf.toString());
            } else if (mLog.isLoggable(Level.FINE)) {
                mLog.fine(buf.toString());
            }
        }

        InputStream inStream = new ByteArrayInputStream(bytes);
        InputSource inputSource = new InputSource(inStream);
        inputSource.setPublicId("byte array data");
        UnmarshalAdaptor adaptor = new UnmarshalAdaptor();
        return new SAXSource(adaptor, inputSource);
    }

    public Source decodeFromReader(Reader input)
        throws EncoderException {
        EncoderProperties encoderProps = null;
        return decodeFromReader(input, encoderProps);
    }

    public Source decodeFromReader(Reader input, EncoderProperties encoderProps)
        throws EncoderException {
        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder buf = new StringBuilder("Decode from input reader");
            if (mLog.isLoggable(Level.FINE)) {
                buf.append(", encoderProperties=[");
                buf.append(encoderProps == null ? "null" : encoderProps);
                buf.append("]");
            }
            buf.append(".");
            if (mLog.isLoggable(Level.FINER)) {
                mLog.finer(buf.toString());
            } else if (mLog.isLoggable(Level.FINE)) {
                mLog.fine(buf.toString());
            }
        }
        byte[] bytes = null;
        String str = null;
        try {
            String preDecodeCharCoding = null;
            if (encoderProps != null) {
                preDecodeCharCoding = encoderProps.getPreDecodeCharCoding();
            }
            if (preDecodeCharCoding == null) {
                preDecodeCharCoding = mEncoderProps.getPreDecodeCharCoding();
            }
            StringCoder stringCoder = null;
            if (preDecodeCharCoding != null) {
                if (!CoderFactory.hasCoder(preDecodeCharCoding)) {
                    throw new EncoderException("Unsupported encoding: \'"
                        + preDecodeCharCoding + "\'",
                        new UnsupportedEncodingException(preDecodeCharCoding));
                }
                stringCoder = CoderFactory.getCoder(preDecodeCharCoding);
            } else {
                stringCoder = mNodes.getPreDecodeCharCoder();
            }
            boolean toClose = false;
            str = readString(input, toClose);
            bytes = stringCoder.encode(str);

            if (mLog.isLoggable(Level.FINE)) {
                StringBuilder buf = new StringBuilder();
                buf.append("String length from reader=").append(str.length());
                buf.append(", encoded byte size from string=");
                buf.append(bytes.length);
                buf.append(" using stringCoder=[").append(stringCoder);
                if (mLog.isLoggable(Level.FINEST)) {
                    buf.append("], string=").append(str);
                    buf.append("], bytes=").append(Misc.printable(bytes));
                }
                buf.append("].");
                if (mLog.isLoggable(Level.FINEST)) {
                    mLog.finest(buf.toString());
                } else if (mLog.isLoggable(Level.FINE)) {
                    mLog.fine(buf.toString());
                }
            }
        } catch (IOException e) {
            throw new EncoderException(e);
        }
        InputStream inStream = new ByteArrayInputStream(bytes);
        InputSource inputSource = new InputSource(inStream);
        inputSource.setPublicId("character sequence data");
        UnmarshalAdaptor adaptor = new UnmarshalAdaptor();
        return new SAXSource(adaptor, inputSource);
    }

    public Source decodeFromStream(InputStream input) throws EncoderException {
        return decodeFromStream(input, null);
    }

    public Source decodeFromStream(InputStream inputStream,
        EncoderProperties encoderProps) throws EncoderException {
        try {
            boolean toClose = false;
            byte[] bytes = readBytes(inputStream, toClose);
            return decodeFromBytes(bytes, encoderProps);
        } catch (IOException e) {
            throw new EncoderException(e);
        }
    }

    public Source decodeFromString(String input, EncoderProperties encoderProps)
        throws EncoderException {
        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder buf = new StringBuilder("Decode from string");
            buf.append(", total string length=").append(input.length());
            if (mLog.isLoggable(Level.FINER)) {
                buf.append(", encoderProperties=[");
                buf.append(encoderProps == null ? "null" : encoderProps);
                buf.append("]");
            } else if (mLog.isLoggable(Level.FINEST)) {
                buf.append(", input string=[").append(input).append("]");
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

        String preDecodeCharCoding = null;
        if (encoderProps != null) {
            preDecodeCharCoding = encoderProps.getPreDecodeCharCoding();
        }
        if (preDecodeCharCoding == null) {
            preDecodeCharCoding = mEncoderProps.getPreDecodeCharCoding();
        }

        byte[] bytes;
        StringCoder stringCoder = null;
        if (preDecodeCharCoding != null) {
            if (!CoderFactory.hasCoder(preDecodeCharCoding)) {
                throw new EncoderException("Unsupported encoding: \'"
                    + preDecodeCharCoding + "\'",
                    new UnsupportedEncodingException(preDecodeCharCoding));
            }
            stringCoder = CoderFactory.getCoder(preDecodeCharCoding);
        } else {
            stringCoder = mNodes.getPreDecodeCharCoder();
        }
        bytes = stringCoder.encode(input);
        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder buf = new StringBuilder();
            buf.append("Encoded byte size from input string=");
            buf.append(bytes.length);
            buf.append(" using stringCoder=[").append(stringCoder);
            if (mLog.isLoggable(Level.FINEST)) {
                buf.append("], bytes=").append(Misc.printable(bytes));
            }
            buf.append("].");
            if (mLog.isLoggable(Level.FINEST)) {
                mLog.finest(buf.toString());
            } else if (mLog.isLoggable(Level.FINE)) {
                mLog.fine(buf.toString());
            }
        }

        InputStream inStream = new ByteArrayInputStream(bytes);
        InputSource inputSource = new InputSource(inStream);
        inputSource.setPublicId("character sequence data");
        UnmarshalAdaptor adaptor = new UnmarshalAdaptor();
        return new SAXSource(adaptor, inputSource);
    }

    public byte[] encodeToBytes(Source input, EncoderProperties encoderProps)
        throws EncoderException {
        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder buf = new StringBuilder("Encode to bytes from source=");
            buf.append(input.getClass().getName());
            if (mLog.isLoggable(Level.FINER)) {
                buf.append(", encoderProperties=[");
                buf.append(encoderProps == null ? "null" : encoderProps);
                buf.append("]");
            }
            buf.append(".");
            if (mLog.isLoggable(Level.FINER)) {
                mLog.finer(buf.toString());
            } else if (mLog.isLoggable(Level.FINE)) {
                mLog.fine(buf.toString());
            }
        }

        BudOutput budOutput = new BudOutput(mNodes, mNodes.getDelim());
        try {
            mTransformer.transform(input, new SAXResult(budOutput));
            byte[] output = budOutput.getBufferContentBytes();

            if (mLog.isLoggable(Level.FINE)) {
                StringBuilder buf = new StringBuilder();
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

            String postEncodeCharCoding = null;
            if (encoderProps != null) {
                postEncodeCharCoding = encoderProps.getPostEncodeCharCoding();
            }
            if (postEncodeCharCoding == null) {
                postEncodeCharCoding = mEncoderProps.getPostEncodeCharCoding();
            }

            TransCoder postCoder;
            if (postEncodeCharCoding != null) {
                String from = postEncodeCharCoding;
                String to = mNodes.getPostCoding();
                postCoder = CoderFactory.getTrans(from, to);
            } else {
                postCoder = mNodes.getPostcoder();
            }
            // recode
            boolean readOnly = false;
            byte[] bytes = postCoder.recode(output, readOnly);

            if (mLog.isLoggable(Level.FINE)) {
                StringBuilder buf = new StringBuilder();
                buf.append("Recoded byte size=").append(bytes.length);
                buf.append(" using postCoder=").append(postCoder);
                if (mLog.isLoggable(Level.FINEST)) {
                    buf.append(", Recoded bytes=[");
                    buf.append(Misc.printable(bytes)).append("]");
                }
                buf.append(".");
                if (mLog.isLoggable(Level.FINEST)) {
                    mLog.finest(buf.toString());
                } else if (mLog.isLoggable(Level.FINE)) {
                    mLog.fine(buf.toString());
                }
            }
            return bytes;
        } catch (TransformerException e) {
            throw new EncoderException(e);
        }
    }

    public void encodeToStream(Source input, OutputStream out)
        throws EncoderException {
        EncoderProperties encoderProps = null;
        encodeToStream(input, out, encoderProps);
    }

    public void encodeToStream(Source input, OutputStream out,
        EncoderProperties encoderProps) throws EncoderException {
        try {
            byte[] bytes = encodeToBytes(input, encoderProps);
            out.write(bytes);
            out.flush();
        } catch (IOException e) {
            throw new EncoderException(e);
        }
    }

    public String encodeToString(Source input, EncoderProperties encoderProps)
        throws EncoderException {
        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder buf = new StringBuilder("Encode to string from source=");
            buf.append(input.getClass().getName());
            if (mLog.isLoggable(Level.FINER)) {
                buf.append(", encoderProperties=[");
                buf.append(encoderProps == null ? "null" : encoderProps);
                buf.append("]");
            }
            buf.append(".");
            if (mLog.isLoggable(Level.FINER)) {
                mLog.finer(buf.toString());
            } else if (mLog.isLoggable(Level.FINE)) {
                mLog.fine(buf.toString());
            }
        }
        BudOutput budOutput = new BudOutput(mNodes, mNodes.getDelim());
        try {
            mTransformer.transform(input, new SAXResult(budOutput));
            byte[] bytes = budOutput.getBufferContentBytes();
            if (mLog.isLoggable(Level.FINE)) {
                StringBuilder buf = new StringBuilder();
                buf.append("Before postEncodeCharCoding, output byte size=");
                buf.append(bytes.length);
                if (mLog.isLoggable(Level.FINEST)) {
                    buf.append(", output bytes=[");
                    buf.append(Misc.printable(bytes)).append("]");
                }
                buf.append(".");
                if (mLog.isLoggable(Level.FINEST)) {
                    mLog.finest(buf.toString());
                } else if (mLog.isLoggable(Level.FINE)) {
                    mLog.fine(buf.toString());
                }
            }

            String postEncodeCharCoding = null;
            if (encoderProps != null) {
                postEncodeCharCoding = encoderProps.getPostEncodeCharCoding();
            }
            if (postEncodeCharCoding == null) {
                postEncodeCharCoding = mEncoderProps.getPostEncodeCharCoding();
            }
            StringCoder stringCoder = null;
            if (postEncodeCharCoding != null) {
                if (!CoderFactory.hasCoder(postEncodeCharCoding)) {
                    throw new EncoderException("Unsupported encoding: \'"
                        + postEncodeCharCoding + "\'",
                        new UnsupportedEncodingException(postEncodeCharCoding));
                }
                stringCoder = CoderFactory.getCoder(postEncodeCharCoding);
            } else {
                stringCoder = mNodes.getPostEncodeCharCoder();
            }
            String str = stringCoder.decode(bytes);
            if (mLog.isLoggable(Level.FINE)) {
                StringBuilder buf = new StringBuilder();
                buf.append("Coded string length=").append(str.length());
                buf.append(" using stringCoder=").append(stringCoder);
                if (mLog.isLoggable(Level.FINEST)) {
                    buf.append(", Coded string=[").append(str).append("]");
                }
                buf.append(".");
                if (mLog.isLoggable(Level.FINEST)) {
                    mLog.finest(buf.toString());
                } else if (mLog.isLoggable(Level.FINE)) {
                    mLog.fine(buf.toString());
                }
            }

            return str;
        } catch (TransformerException e) {
            throw new EncoderException(e);
        }
    }

    public void encodeToWriter(Source input, Writer out)
        throws EncoderException {
        EncoderProperties encoderProps = null;
        encodeToWriter(input, out, encoderProps);
    }

    public void encodeToWriter(Source input, Writer out,
        EncoderProperties encoderProps) throws EncoderException {
        try {
            String data = encodeToString(input, encoderProps);
            out.write(data);
            out.flush();
        } catch (IOException e) {
            throw new EncoderException(e);
        }
    }

    /**
     * Gets the properties of this encoder.
     */
    public EncoderProperties getProperties() {
        return mEncoderProps;
    }

    /**
     * Gets the type of this encoder.
     */
    public EncoderType getType() {
        return mEncoderType;
    }

    private String readString(Reader reader, boolean toClose)
        throws IOException {
        StringBuilder sb = new StringBuilder();
        char[] buf = new char[1024];
        int read;
        while ((read = reader.read(buf)) != -1) {
            sb.append(buf, 0, read);
        }
        if (toClose) {
            reader.close();
        }
        return sb.toString();
    }

    private byte[] readBytes(InputStream inStream, boolean toClose)
        throws IOException {
        ByteArrayOutputStream outStream = new ByteArrayOutputStream();
        byte[] buf = new byte[1024];
        int read;
        while ((read = inStream.read(buf)) != -1) {
            outStream.write(buf, 0, read);
        }
        if (toClose) {
            inStream.close();
        }
        return outStream.toByteArray();
    }

    /**
     * Simple XMLReader implementation that facilitates the creation of
     * SAXSource (a "push" model XML Source).
     */
    public class UnmarshalAdaptor implements XMLReader {

        private EntityResolver mEntityResolver;
        private DTDHandler mDTDHandler;
        private ContentHandler mContentHandler;
        private ErrorHandler mErrorHandler;

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
                            + " is not supported."); //I18N
                }
                return;
            }
            if ("http://xml.org/sax/features/namespace-prefixes".equals(name)) {
                if (value) {
                    throw new SAXNotSupportedException(
                            "Feature '" + name + "' with value " + value
                            + " is not supported."); //I18N
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
            //no-op
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

        public void parse(InputSource inputSource)
            throws IOException, SAXException {
            OtdDelim.OtdDelimInst inst = mNodes.getDelim().new OtdDelimInst();
            Parse parse = new Parse(inst, mNodes);
            boolean toClose = false;
            InputStream inputStream = inputSource.getByteStream();
            byte[] bytes = readBytes(inputStream, toClose);
            StringOtdInputStreamImpl stringInputStream =
                    new StringOtdInputStreamImpl(bytes);
            StringCoder decoder = mNodes.getDecoder();
            parse.parseFromInputStream(mNodes.getRootNodeIndex(),
                mContentHandler, decoder, stringInputStream);
        }

        public void parse(String systemId)
            throws IOException, SAXException {
            parse(new InputSource(systemId));
        }
    }
}
