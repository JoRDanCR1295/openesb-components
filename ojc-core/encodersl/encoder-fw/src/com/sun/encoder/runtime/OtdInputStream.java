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
 * @(#)OtdInputStream.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime;

import java.io.IOException;

/**
 * Class to provide an input byte stream to an OTD unmarshal (deserialization)
 * method. The underlying transport model could be a byte array, an external
 * file, a stream over JMS, or any other number of things. The transport model
 * used may be a trade-off between speed and large-message support.
 *
 * @author Michael Libourel, Hong Lin
 * @version 
 */
public interface OtdInputStream {
    /**
     * Announces the start of parsing a message. A rewind should return to this
     * point.
     *
     * @throws IOException for input problem
     */
    void begin ()
        throws IOException;

    /**
     * Terminates consumption of the stream by the parser.
     *
     * @return whether there was any data left in the stream
     *
     * @throws IOException for input problem
     */
    boolean end ()
        throws IOException;

    /**
     * Tests if the stream cursor has reached the end of the nput data.
     * The effect of calling this method before begin() or after end()
     * is not defined.
     *
     * @return true if end of data reached, else false
     */
    /*-
    boolean eof ()
        throws IOException;
    -*/

    /**
     * Fills given buffer with as much data as possible. May block until data
     * becomes available.
     *
     * @param buffer the array to receive the data
     *
     * @return the number of bytes added to the buffer, or -1 for EOF
     *
     * @throws IOException for input problem
     */
    int read (byte buffer[])
        throws IOException;

    /**
     * Fills given buffer with as much data as possible. May block until data
     * becomes available.
     *
     * @param buffer the array to receive the data
     * @param offset stating index in buffer for new data
     * @param len length
     * @return the number of bytes added to the buffer, or -1 for EOF
     * @throws IOException for input problem
     */
    int read (
        byte  buffer[],
        int   offset,
        int   len)
        throws IOException;

    /**
     * Returns the next available byte, else -1 for EOF
     *
     * @return a byte (range 0-255) or -1
     * @throws IOException for input errors
     */
    int read ()
        throws IOException;

    /**
     * Marks the repeatable node.
     *
     * @return cookie to return to position later
     * @throws IOException for input errors
     */
    OtdInputStreamMark mark ()
        throws IOException;

    /**
     * Returns to position marked earlier.
     *
     * @param mark cookie returned by mark()
     * @throws IOException for input problem
     */
    void seek (OtdInputStreamMark mark)
        throws IOException;

    /**
     * Returns to the very beginning of the stream, which is implicitly marked.
     *
     * @throws IOException for input problem
     */
    void rewind ()
        throws IOException;

    /**
     * Skips the given number of bytes ahead.
     * Trying to skip past the end-of-file will throw an I/O exception.
     *
     * @param count  the number of bytes to skip; non-negative number
     * @throws IOException for input problem
     */
    void skip (long count)
        throws IOException;
}
