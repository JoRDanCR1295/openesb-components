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
 * @(#)SeekInputStream.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcs.ssc;

/**
 * The SeekInputStream class is similar to the InputStream class, but has
 * the capability to return the current position as a value, and to seek
 * back to a returned position at a later point in time.  This is more
 * general than the "mark/reset" interface of InputStream, and is always
 * supported (cf. "markSupported") by this interface.
 * We need this interface as the lower layer for dynamic encoding streams.
 */
public interface SeekInputStream
{
    /**
     * A special return value for read() and readGet(), to indicate
     * the end of the input stream.
     */
    public static final int EOF = -1;

    /**
     * Returns information on position.
     * The result can be used as the argument to seek(long).
     *
     * @return the position, or -1 after close()
     */
    public long tell () throws java.io.IOException;

    /**
     * Returns stream cursor to given position.
     * The position is normally obtained by an earlier call to tell().
     *
     * @param pos  the new position
     * @throwns IOException after close() or with invalid position
     */
    public void seek (long pos) throws java.io.IOException;

    /**
     * Stream cursor skips next N bytes (negative is backward).
     *
     * @param offset  number of bytes to skip
     * @throws IOException after close() or invalid offset
     */
    public void skip (long pos) throws java.io.IOException;

    /**
     * Fetches next input byte at cursor, or EOF.
     *
     * @return byte value 0-255, or SeekInputStream.EOF
     */
    public int read () throws java.io.IOException;

    /**
     * Tries to fetch next N bytes, returns real count (which may be less).
     *
     * @param buf  the fetch buffer
     * @param offset  offset into fetch buffer, for start of result
     * @param count  number of bytes to fetch
     * @return the number of bytes really read
     */
    public int read (byte[] buf, int offset, int count)
        throws java.io.IOException;

    /**
     * Closes the stream, releases any resources held.
     *
     * @throws IOException when called twice
     */
    public void close () throws java.io.IOException;
}
