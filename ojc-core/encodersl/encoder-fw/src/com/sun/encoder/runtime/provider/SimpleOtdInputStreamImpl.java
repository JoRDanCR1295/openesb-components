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
 * @(#)SimpleOtdInputStreamImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime.provider;

import com.sun.encoder.runtime.OtdInputStream;
import com.sun.encoder.runtime.OtdInputStreamMark;
import com.sun.encoder.util.EgateConfig;

import java.io.IOException;
import java.io.InputStream;

/**
 * A simple OtdInputStream, based on a regular input stream.
 *
 * @author Hong Lin, Michael Libourel
 * @version 
 */
public class SimpleOtdInputStreamImpl
    implements OtdInputStream {
    /** DOCUMENT ME! */
    protected InputStream mIn = null;
    private long mPos = 0L;
    public static final int REWIND_SIZE =
        EgateConfig.getInteger("otd.rewind", 5000000);

    /**
     * Creates a new SimpleOtdInputStreamImpl object.
     *
     * @param in  rewindable input stream
     * @throws IOException if stream closed before
     */
    public SimpleOtdInputStreamImpl (InputStream in)
        throws IOException {
        if (in == null) {
            throw new IOException("Stream closed");
        }
        if (!in.markSupported()) {
            throw new IllegalArgumentException("input must support mark()");
        }
        in.mark(REWIND_SIZE);
        mIn = in;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public InputStream getInputStream () {
        return mIn;
    }

    /**
     * DOCUMENT ME!
     *
     * @param in DOCUMENT ME!
     */
    public void setInputStream (InputStream in) {
        mIn = in;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws IOException DOCUMENT ME!
     */
    public int available ()
        throws IOException {
        return mIn.available();
    }

    /**
     * DOCUMENT ME!
     *
     * @throws IOException DOCUMENT ME!
     */
    public void begin ()
        throws IOException {
    }

    /**
     * DOCUMENT ME!
     *
     * @throws IOException DOCUMENT ME!
     */
    public void close ()
        throws IOException {
        mIn.close();
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws IOException DOCUMENT ME!
     */
    public boolean end ()
        throws IOException {
        mIn.close();

        return true;
    }

    // Mark cookie.
    private static class Mark
        implements OtdInputStreamMark {
        public final long mPos;
        public Mark (long pos) { mPos = pos; }
        public long offset () { return mPos; }
    }

    /**
     * Marks the repeatable node.
     *
     * @return cookie to return to position later
     * @throws IOException for input errors
     */
    public synchronized OtdInputStreamMark mark ()
        throws IOException {
        return new Mark(mPos);
    }

    /**
     * Returns to position marked earlier.
     *
     * @param mark cookie returned by mark()
     * @throws IOException for input problem
     */
    public synchronized void seek (OtdInputStreamMark mark)
        throws IOException {
        if (mark == null) {
            throw new NullPointerException("no mark");
        }
        if (!(mark instanceof Mark)) {
            throw new NullPointerException("foreign mark");
        }
        Mark m = (Mark) mark;
        if (m.mPos < mPos) {
            rewind();
        }
        skip(m.mPos - mPos);
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws IOException DOCUMENT ME!
     */
    public int read ()
        throws IOException {
        int result = mIn.read();
        if (mPos++ > REWIND_SIZE) {
            throw new IOException("exceeded rewindable size: " + REWIND_SIZE);
        }
        return result;
    }

    /**
     * Tests if the stream cursor has reached the end of the nput data.
     * The effect of calling this method before begin() or after end()
     * is not defined.
     *
     * @return true if end of data reached, else false.
     * @throws IOException IOException occurred.
     */
    public boolean eof ()
        throws IOException {
        //NYI: fix for QAI #63370
        throw new IOException("eof() not yet implemented...");
    }

    /**
     * DOCUMENT ME!
     *
     * @param b DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws IOException DOCUMENT ME!
     */
    public int read (byte b[])
        throws IOException {
        return read(b, 0, b.length);
    }

    /**
     * DOCUMENT ME!
     *
     * @param b DOCUMENT ME!
     * @param off DOCUMENT ME!
     * @param len DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws IOException DOCUMENT ME!
     */
    public synchronized int read (byte b[], int off, int len)
        throws IOException {
        int result = mIn.read(b, off, len);
        mPos += len;
        if (mPos > REWIND_SIZE) {
            throw new IOException("exceeded rewindable size: " + REWIND_SIZE);
        }
        return result;
    }

    /**
     * Returns to the very beginning of the stream, which is implicitly marked.
     *
     * @throws IOException for input problem
     */
    public void rewind ()
        throws IOException {
        mIn.reset();
        mPos = 0;
    }

    /**
     * Skips the given number of bytes ahead.
     * Trying to skip past the end-of-file will throw an I/O exception.
     *
     * @param count  the number of bytes to skip; non-negative number
     * @throws IOException for input problem
     */
    public void skip (long count)
        throws IOException {
        if (count < 0) {
            throw new IllegalArgumentException("negative count");
        }
        long did = mIn.skip(count);
        if (did != count) {
            throw new IOException("tried to skip " + count + ", did " + did);
        }
        mPos += count;
        if (mPos > REWIND_SIZE) {
            throw new IOException("exceeded rewindable size: " + REWIND_SIZE);
        }
    }
}
