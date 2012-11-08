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
 * @(#)FileOtdInputStreamImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime.provider;

import com.sun.encoder.runtime.OtdInputStream;
import com.sun.encoder.runtime.OtdInputStreamMark;

import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;

/**
 * Class to implement an OTD input stream based on an existing stream. This
 * class uses an local temporary file to hold a copy of any data that may be
 * rewound.
 *
 * @author stc
 * @version 
 */
public class FileOtdInputStreamImpl
    implements OtdInputStream {
    /** DOCUMENT ME! */
    protected InputStream mIn = null;

    /** DOCUMENT ME! */
    protected boolean mMarkFlag = false;

    /** DOCUMENT ME! */
    protected int mCurTokenBegin = 0;

    /** DOCUMENT ME! */
    protected int mCurTokenEnd = -1;

    /** DOCUMENT ME! */
    protected int pos = 0;

    /** DOCUMENT ME! */
    protected String mFileName = "/temp/otd/external/output.txt";

    /** DOCUMENT ME! */
    private RandomAccessFile mRAF = null;

    /**
     * Constructs from an existing stream.
     *
     * @param in the underlying stream
     */
    public FileOtdInputStreamImpl(InputStream in) {
        this.mIn = in;
    }

    /**
     * Indicates the beginning of reading a message. Required by the
     * OtdInputStream interface.
     */
    public void begin() {
        // dummy...
    }

    /**
     * ????
     *
     * @throws IOException IO exception
     */
    public void close()
        throws IOException {
        mIn.close();
        //mRAF.close();
    }

    /**
     * Indicates the end of reading a message. Required by the OtdInputStream
     * interface.
     *
     * @return whether we are at the stream end
     *
     * @throws IOException for input error
     */
    public boolean end()
        throws IOException {
        // ...we could release mark here, truncate the copy file...
        return mIn.read() < 0;
    }

    /**
     * ???? int : position of RandomAccessFile
     *
     * @return ????
     */
    public synchronized OtdInputStreamMark mark() {
        if (!mMarkFlag) {
            mMarkFlag = true;
            /*-
            try {
                File f = new File(mFileName);
                if (f.isFile()) {
                    f.delete();
                }
                mRAF = new RandomAccessFile(mFileName, "rw");
            } catch (FileNotFoundException fnf) {
                fnf.printStackTrace();
            }
            -*/
        }
        return new FileOtdInputStreamMark(mCurTokenEnd - mCurTokenBegin);
    }

    /**
     * ????
     *
     * @return DOCUMENT ME!
     */
    public boolean markSupported() {
        return true;
    }

    /**
     * Read a single byte.
     *
     * @return the byte value (range0...255), or -1 for EOF
     *
     * @throws IOException IO exception
     */
    public synchronized int read()
        throws IOException {
        int value = mIn.read();

        if (value < 0) {
            return -1;
        }

        pos++;

        return value;
    }

    /**
     * ????
     *
     * @param b ???
     * @param off ???
     *
     * @return the number of bytes read, or -1 for EOF
     *
     * @throws IOException IO exception
     */
    public synchronized int read(
        byte  b[],
        int   off)
        throws IOException {
        int count = mIn.read(b, off, b.length - off);
        System.out.println(
            "read(byte[],int,int)...begin pos: " + pos + ", ending pos: "
            + (pos + count) + ", count: " + count);

        if (count < 0) {
            return -1;
        }

        pos += count;

        return count;
    }

    /**
     * Fills given buffer with as much data as possible. May block until data
     * becomes available.  If less data is returned that the buffer can
     * contain, this implies we are at the EOF.
     *
     * @param buffer  the array to receive the data
     * @return the number of bytes added to the buffer, or -1 for EOF
     * @throws IOException for input problem
     */
    public int read (byte buffer[])
        throws IOException {
        return read(buffer, 0, buffer.length);
    }

    /**
     * Fills given buffer with as much data as possible. May block until data
     * becomes available.  If less data is returned that the buffer can
     * contain, this implies we are at the EOF.
     *
     * @param buffer  the array to receive the data
     * @param offset  stating index in buffer for new data
     * @param length  number of bytes to read
     * @return the number of bytes added to the buffer, or -1 for EOF
     * @throws IOException for input problem
     */
    public synchronized int read (byte buffer[], int offset, int length)
        throws IOException {
        if (offset + length > buffer.length) {
            throw new IllegalArgumentException("offset+length > buffer size");
        }
        int count = mIn.read(buffer, offset, length);
        System.out.println(
            "read(byte[],int,int)...begin pos: " + pos + ", ending pos: "
            + (pos + count) + ", count: " + count);

        if (count < 0) {
            // EOF.
            return -1;
        }
        pos += count;
        return count;
    }

    /**
     * ????
     *
     * @throws IOException IO exception
     */
    public void reset()
        throws IOException {
        mIn.reset();
    }

    /**
     * Returns to the very beginning of the stream, which is implicitly marked.
     *
     * @throws IOException for input problem
     */
    public synchronized void rewind()
        throws IOException {
        //same as seek()?
    }

    /**
     * ????
     *
     * @param mark ???
     *
     * @throws IOException IO exception
     */
    public synchronized void seek(OtdInputStreamMark mark)
        throws IOException {
        long offset = ((FileOtdInputStreamMark) mark).getOffset();
        //mRAF.seek(offset);
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
            throw new IllegalArgumentException("negative offset");
        }
        mIn.skip(count);
    }

    /**
     * DOCUMENT ME!
     *
     * @param buffer byte buffer to read from
     * @param offset  intial offset
     * @param len number of bytes to write
     *
     * @throws IOException IO exception
     */
    public void writeMarkOffset(
        byte  buffer[],
        int   offset,
        int   len)
        throws IOException {
    }

    /**
     * ????
     *
     * @param bytes ???
     * @param off ???
     * @param len ???
     *
     * @throws IOException IO exception
     */
    public synchronized void writeToken(
        byte  bytes[],
        int   off,
        int   len)
        throws IOException {
        //Don't need to save token if the first mark is not set
        if (!mMarkFlag) {
            return;
        }

        //System.out.println("write bytes: " + new String(bytes));
        //always append token to the end of file
        mCurTokenBegin = (int) mRAF.length();

        mRAF.seek(mRAF.length());
        mRAF.write(
            bytes,
            off,
            len);
        mCurTokenEnd = (int) mRAF.length();
        System.out.println(
            "......................curTokenBegin: " + mCurTokenBegin + " , end: "
            + mCurTokenEnd);
    }

    /**
     * DOCUMENT ME!
     */
    private static class FileOtdInputStreamMark
        implements OtdInputStreamMark {
        /** DOCUMENT ME! */
        private long mOffset;

        /**
         * Constructs from the offset.
         *
         * @param offset
         */
        public FileOtdInputStreamMark(long offset) {
            mOffset = offset;
        }

        /**
         * Retrieves the offset again.
         *
         * @return the offset value
         */
        public long getOffset() {
            return mOffset;
        }

        /**
         * Retrieves the offset again.
         * Required by interface.
         *
         * @return the offset value
         */
        public long offset() {
            return mOffset;
        }
    }
}
