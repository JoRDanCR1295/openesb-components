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
 * Encoder.java - ver 1.0 - 2006
 *
 * Copyright 2006-2007 Gestalt, LLC. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.gestalt.encoding;


/**
 * Interface defining common functionality for encoding and decoding
 * data being sent over low band width connections.
 *
 * @author Phillip Anderson, panderson@gestalt-llc.com
 */
public interface Encoder {
    /**
     * Common method definition for encoding or compressing data.
     * Do not convert the returning byte[] to a String.
     * For example do not do the following:
     * byte[] result = Encoder.encode("data".getBytes());
     * String str = new String(result); // do not do
     * The value for str will be unable to decode.
     * @param bytes byte[] to be encoded or compressed.
     * @return byte[] resulting from the encoding or compression of the
     * of the original byte[].
     * @throws Exception
     */
    public byte[] encode(byte[] bytes) throws Exception;

    /**
     * Common method definition for decoding or uncompressing data.
     * @param bytes byte[] to be decoded or uncompressed.
     * @return byte[] resulting from the decoding or uncompressing of the
     * of the original byte[].
     * @throws Exception
     */
    public byte[] decode(byte[] bytes) throws Exception;
}
