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
 * EncodingComponent.java - ver 1.0 - 2006
 *
 * Copyright 2006-2007 Gestalt, LLC. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.gestalt.jbi.encoding;

import com.gestalt.encoding.EncoderFactory;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.servicemix.components.util.OutBinding;
import org.apache.servicemix.jbi.jaxp.BytesSource;
import org.apache.servicemix.jbi.util.ByteArrayDataSource;

import javax.activation.DataHandler;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import java.io.*;
import java.util.ArrayList;
import java.util.regex.Pattern;
import java.util.regex.Matcher;


/**
 * Receives an InOut MessageExchange expecting content to be
 * set by the consumer to be encoded. It gets the content, encodes
 * the XML, and sets the result as an attachment to the Out
 * NormalizedMessage. Attachments have to be used because JBI strictly
 * restricts content to only XML. Therefore attachments are used.
 * The out NormalizedMessage can then be passed back to the EncodingComponent
 * who then takes the attachment, decodes based off the content type,
 * and places the decoded XML back in the content of the out NormalizedMessage.
 */
public class EncodingComponent extends OutBinding {
    private static Log log = LogFactory.getLog(EncodingComponent.class);
    private static final String ENDPOINT = "encoding";
    public static final QName SERVICE = new QName("http://nettoolkit.gestalt.com/encoding",
            ENDPOINT);

    /**
     * The id used when adding an attachment
     */
    public static final String ATTACHMENT_ID = "response";

    /**
     * Use EncodingStyle enum for value.
     */
    public static final String ENCODE_PROPERTY_NAME = "com.gestalt.nettoolkit.encoding.style";

    public EncodingComponent() {
        super.setEndpoint(ENDPOINT);
        super.setService(SERVICE);
    }

    /**
     * Called when this provider receives a new InOut request. Get the "in"
     * normalized message, validate it, and then process the request.  If the
     * request has not yet been encoded, then encode the request data. If the
     * request has already been encoded with a recognizeable encoding style
     * then decode the request data. Return the message exchange to its
     * consumer with a valid "out" message or an error status.
     * Process inbound message exchange request by either encoding or
     * decoding the "in" message content.  The encoded response will
     * be added as an attachment with the id of response. The decoded
     * data will be added as Content.
     *
     * @param me - inbound message exchange
     */
    protected void process(MessageExchange me,
                           NormalizedMessage normalizedMessage) throws Exception {
        log.debug("EncodingComponent.process()");

        byte[] dataOut;
        byte[] dataIn = transformInMe(me);

        NormalizedMessage onm = me.createMessage();

        EncodingStyle encodingStyle = (EncodingStyle) me.getProperty(ENCODE_PROPERTY_NAME);

        if (encodingStyle == null) {
            encodingStyle = EncodingStyle.ZIP;
        }

        if (me.getMessage("in").getAttachment(ATTACHMENT_ID) == null) {
            dataOut = encodeInMe(me, dataIn, encodingStyle);

            ByteArrayDataSource dataSource = new ByteArrayDataSource(dataOut,
                    encodingStyle.getContentType());
            onm.addAttachment(ATTACHMENT_ID, new DataHandler(dataSource));

            // Outbound HTTP BC requires content as of version 3.0
            onm.setContent(new BytesSource("<d/>".getBytes()));
        } else {
            dataOut = decodeOutMe(me, dataIn);
            onm.setContent(new BytesSource(dataOut));
        }

        answer(me, onm);
    }

    /**
     * Extract and transform the inbound message exchange request data.
     *
     * @param me - inbound message exchange
     * @return String containing transformed "in" message data
     * @throws Exception
     */
    protected byte[] transformInMe(MessageExchange me)
        throws Exception {
        byte[] inData = new byte[0];

        // fetch "in" normalized message content
        NormalizedMessage inm = me.getMessage("in");

        if (null == inm) {
            throw new Exception("message exchange has no IN normalized message");
        }

        Source sourceIn = inm.getContent();

        if (null == sourceIn) {
            throw new Exception(
                "message exchange IN normalized message has no content");
        }

        if (log.isDebugEnabled()) {
            log.debug("sourceIn object is = " + sourceIn.getClass().getName());
        }

        if (sourceIn instanceof BytesSource) {
            byte b;
            InputStream is = ((BytesSource) sourceIn).getInputStream();
            ArrayList list = new ArrayList();

            while (-1 != (b = (byte) is.read())) {
                list.add(new Byte(b));
            }

            int len = list.size();
            byte[] bytes = new byte[len];

            for (int i = 0; i < len; i++) {
                Byte aByte = (Byte) list.get(i);
                bytes[i] = aByte.byteValue();
            }

            inData = bytes;
        } else {
            Transformer transformer = TransformerFactory.newInstance()
                                                        .newTransformer();

            StreamResult result = new StreamResult(new StringWriter());
            transformer.transform(sourceIn, result);
            inData = (result.getWriter().toString()).getBytes();
        }

        if (inm.getAttachment(ATTACHMENT_ID) != null) {
            DataHandler dh = inm.getAttachment(ATTACHMENT_ID);
            BufferedInputStream in = new BufferedInputStream(dh.getInputStream());
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            int i;

            while ((i = in.read()) != -1) {
                baos.write(i);
            }

            inData = baos.toByteArray();
        }

        if (log.isDebugEnabled()) {
            log.debug("inbound request length [" + inData.length
                + "] content String(inData) = " + inData);
        }

        return inData;
    }

    /**
     * Encode inbound message exchange request data. Choose the best algorithm
     * based upon the encoded data length. Default is ZIP.
     *
     * @param me - inbound message exchange
     * @param dataIn - inbound "in" normalized message source data
     * @param style - The EncodingStyle
     * @return String of encoded data
     * @throws Exception
     */
    protected byte[] encodeInMe(MessageExchange me, byte[] dataIn,
                                EncodingStyle style) throws Exception {
        if (log.isDebugEnabled()) {
            log.debug("Encoding data using style: " + style);
        }

        byte[] dataOut = new byte[0];

        if (style == EncodingStyle.ZIP) {
            dataOut = encodeZip(dataIn);
        } else if (style == EncodingStyle.TAG) {
            dataOut = encode(dataIn);
        }

        if (log.isDebugEnabled()) {
            log.debug("Data was encoded");
            log.debug("encoded length [" + dataOut.length + "] content: "
                + dataOut);
        }

        return dataOut;
    }

    /**
     * Encode the data using the encode factory. For the moment this translates
     * to FastInfoset.
     *
     * @param data - byte[] of data to encode
     * @return byte[] of data that has been encoded.
     * @throws Exception
     */
    protected byte[] encode(byte[] data) throws Exception {
        return EncoderFactory.getInstance(EncoderFactory.FI).encode(data);
    }

    /**
     * Encode the data using our best and bestest compression technology.
     * For the moment this applies GZIP encoding.
     *
     * @param data - byte[] of data to encode
     * @return byte[] of data that has been encoded.
     * @throws Exception
     */
    public byte[] encodeZip(byte[] data) throws Exception {
        return EncoderFactory.getInstance(EncoderFactory.GZIP).encode(data);
    }

    /**
     * Decode the content of the MessageExchange NormalizedMessage.
     * @param me - MessageExchange coming in
     * @param dataIn String containing the data to be decoded.
     * @return A String containing the decoded data.
     * @throws Exception
     */
    public byte[] decodeOutMe(MessageExchange me, byte[] dataIn)
        throws Exception {
        byte[] dataOut;

        if (log.isDebugEnabled()) {
            log.debug("Decoding data");
        }

        String contentType = me.getMessage("in").getAttachment(ATTACHMENT_ID)
                               .getDataSource().getContentType();

        if (contentType.equals(EncodingStyle.ZIP.getContentType())) {
            dataOut = decodeZip(dataIn);
        } else if (contentType.equals(EncodingStyle.TAG.getContentType())) {
            dataOut = decode(dataIn);
        } else {
            throw new Exception("invalid useEncodingStyle: " + contentType);
        }

        if (log.isDebugEnabled()) {
            log.debug("Decoded Data with a content type of: " + contentType);
            log.debug("decoded length [" + dataOut.length + "] content: "
                + dataOut);
        }

        log.debug("Checking for SOAP to remove from the decoded message.");

        String returnString = stripSoap(new String(dataOut));
        return returnString.getBytes();
    }

    /**
     * Decode the data according to the encoding rules that were used
     * when it was encoded.
     *
     * @param data - byte[] of data to decode
     * @return byte[] of data that has been decoded.
     * @throws Exception
     */
    protected byte[] decode(byte[] data) throws Exception {
        return EncoderFactory.getInstance(EncoderFactory.FI).decode(data);
    }

    /**
     * Decode the data according to the encoding rules that were used
     * when it was encoded.
     *
     * @param data - byte[] of data to decode
     * @return byte[] of data that has been decoded.
     * @throws Exception
     */
    public byte[] decodeZip(byte[] data) throws Exception {
        return EncoderFactory.getInstance(EncoderFactory.GZIP).decode(data);
    }

    /**
     * Remove the SOAP from a String and return the content only.
     * @param message
     * @return Content of the SOAP Message
     */
    protected String stripSoap(String message) {
        log.debug("EncodingComponent.stripSoap()");

        int beginIndex = 0, endIndex = message.length();
        if (message.indexOf("soap") > -1) {
            Pattern startPattern = Pattern.compile("<\\w*?:?[bB]ody>");
            Pattern endPattern = Pattern.compile("</\\w*?:?[bB]ody>");
            Matcher startMatcher = startPattern.matcher(message);
            Matcher endMatcher = endPattern.matcher(message);
            boolean found = false;

            while (found = startMatcher.find()) {
                beginIndex = startMatcher.end();
            }
            while (found = endMatcher.find()) {
                endIndex = endMatcher.start();
            }
        }

        return message.substring(beginIndex,endIndex);

    }
}
