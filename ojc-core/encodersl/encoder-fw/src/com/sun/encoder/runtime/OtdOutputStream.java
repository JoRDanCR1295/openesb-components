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
 * @(#)OtdOutputStream.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime;

import java.io.IOException;

/**
 * Class to provide an output byte stream to an OTD unmarshal (deserialization)
 * method. The underlying transport model could be a byte array, an external
 * file, a stream over JMS, or any other number of things. The transport model
 * used may be a trade-off between speed and large-message support.
 *
 * @author Michael Libourel
 * @version 
 */
public interface OtdOutputStream {
    /**
     * Announces the start of writing the message. May be a no-op.
     *
     * @throws IOException for output problem
     */
    void begin()
        throws IOException;

    /**
     * Announces the end of writing the message.
     *
     * @throws IOException for output problem
     */
    void end()
        throws IOException;

    /**
     * Writes the given part of the buffer to the stream.
     *
     * @param buffer the bytes array to write from
     * @param offset where to start
     * @param size how many bytes to write, from start
     *
     * @throws IOException for output problem
     */
    void write(
        byte  buffer[],
        int   offset,
        int   size)
        throws IOException;

    /**
     * Writes the buffer to the stream.
     *
     * @param buffer the bytes array to write from
     *
     * @throws IOException for output problem
     */
    void write(byte buffer[])
        throws IOException;

    /**
     * Writes the given byte to the stream.
     *
     * @param value the byte to write
     *
     * @throws IOException for output problem
     */
    void write(byte value)
        throws IOException;
}
