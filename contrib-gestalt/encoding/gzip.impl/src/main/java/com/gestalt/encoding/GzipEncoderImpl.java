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
 * GzipEncoderImpl.java - ver 1.0 - 2006
 *
 * Copyright 2006-2007 Gestalt, LLC. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.gestalt.encoding;

import java.io.ByteArrayOutputStream;

import java.util.zip.Deflater;
import java.util.zip.Inflater;


/**
 * An Encoder implementation using GZIP serialization.  This primarily
 * encodes the XML tag and attribute elements.  It does not encode the content
 * of the XML.
 * @author Phillip Anderson, panderson@gestalt-llc.com
 */
public class GzipEncoderImpl implements Encoder {
    /**
     * Encodes the specified XML byte[] using a GZIP serializer.
     * Do not convert the returning byte[] to a String.
     * For example do not do the following:
     * byte[] result = Encoder.encode("data".getBytes());
     * String str = new String(result); // do not do
     * The value for str will be unable to decode.
     * @param bytes A byte[] containing the XML structure to be encoded.
     * @return A byte[] containing the encoded XML structure.
     */
    public byte[] encode(byte[] bytes) throws Exception {
        Deflater compressor = new Deflater();
        compressor.setLevel(Deflater.BEST_COMPRESSION);

        // Give the compressor the data to compress
        compressor.setInput(bytes);
        compressor.finish();

        // Create an expandable byte array to hold the compressed data.
        // You cannot use an array that's the same size as the orginal because
        // there is no guarantee that the compressed data will be smaller than
        // the uncompressed data.
        ByteArrayOutputStream bos = new ByteArrayOutputStream(bytes.length);

        // Compress the data
        byte[] buf = new byte[bytes.length + 1024];

        while (!compressor.finished()) {
            int count = compressor.deflate(buf);
            bos.write(buf, 0, count);
        }

        bos.close();

        return bos.toByteArray();
    }

    /**
     * Decodes the specified encoded byte[] into the original message.
     * @param bytes A byte[] containing the encoded xml data.
     * @return A byte[] containing the decoded XML data.
     */
    public byte[] decode(byte[] bytes) throws Exception {
        Inflater decompressor = new Inflater();
        decompressor.setInput(bytes);

        // Create an expandable byte array to hold the decompressed data
        ByteArrayOutputStream bos = new ByteArrayOutputStream(bytes.length);

        // Decompress the data
        byte[] buf = new byte[bytes.length + 1024];

        while (!decompressor.finished()) {
            int count = decompressor.inflate(buf);
            bos.write(buf, 0, count);
        }

        bos.close();

        // Get the decompressed data
        return bos.toByteArray();
    }
}
