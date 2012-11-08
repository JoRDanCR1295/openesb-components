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
 * FastInfosetEncoderImpl.java - ver 1.0 - 2006
 *
 * Copyright 2006-2007 Gestalt, LLC. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.gestalt.encoding;

import com.sun.xml.fastinfoset.sax.SAXDocumentSerializer;
import org.jvnet.fastinfoset.FastInfosetSource;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.InputStream;


/**
 * An Encoder implementation using Fast Infoset serialization.  This primarily
 * encodes the XML tag and attribute elements.  It does not encode the content
 * of the XML.
 * @author Phillip Anderson, panderson@gestalt-llc.com
 */
public class FastInfosetEncoderImpl implements Encoder {
    /**
     * Encodes the specified XML byte[] using a Fast Infoset serializer.
     * Do not convert the returning byte[] to a String.
     * For example do not do the following:
     * byte[] result = Encoder.encode("data".getBytes());
     * String str = new String(result); // do not do
     * The value for str will be unable to decode.
     * @param bytes A byte[] containing the XML structure to be encoded.
     * @return A byte[] containing the encoded XML structure.
     */
    public byte[] encode(byte[] bytes) throws Exception {
        byte[] returnVal;

        InputStream xmlDocument = new ByteArrayInputStream(bytes);
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        DataOutputStream fiDocument = new DataOutputStream(buffer);

        SAXDocumentSerializer saxDocumentSerializer = new SAXDocumentSerializer();
        saxDocumentSerializer.setOutputStream(fiDocument);

        SAXParserFactory saxParserFactory = SAXParserFactory.newInstance();
        saxParserFactory.setNamespaceAware(true);

        SAXParser saxParser = saxParserFactory.newSAXParser();
        saxParser.setProperty("http://xml.org/sax/properties/lexical-handler",
            saxDocumentSerializer);
        saxParser.parse(xmlDocument, saxDocumentSerializer);

        fiDocument.flush();
        fiDocument.close();
        returnVal = buffer.toByteArray();

        return returnVal;
    }

    /**
     * Decodes the specified encoded byte[] into the original message.
     * @param bytes A byte[] containing the encoded xml data.
     * @return A byte[] containing the decoded XML data.
     */
    public byte[] decode(byte[] bytes) throws Exception {
        byte[] returnVal;

        InputStream fiDocument = new ByteArrayInputStream(bytes);
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        DataOutputStream xmlDocument = new DataOutputStream(buffer);

        Transformer tx = TransformerFactory.newInstance().newTransformer();
        tx.transform(new FastInfosetSource(fiDocument),
            new StreamResult(xmlDocument));

        xmlDocument.flush();
        xmlDocument.close();
        returnVal = buffer.toString().replaceFirst("\015\n", "").getBytes();

        return returnVal;
    }
}
