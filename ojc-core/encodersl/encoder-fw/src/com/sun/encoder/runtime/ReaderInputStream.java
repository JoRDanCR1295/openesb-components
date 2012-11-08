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
 * @(#)ReaderInputStream.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/*
 * ************************************************
 * *                 IMPORTANT !                  *
 * ************************************************
 * 
 * Most of the contents of this file was copied from Apache Software
 * Foundation's XmlBeans implementation's
 * org.apache.xmlbeans.impl.common.PushedInputStream class and 
 * org.apache.xmlbeans.impl.common.ReaderInputStream class with minor 
 * modification to include support for BASE64 encoding and the modification
 * that removes "synchronized" on all methods.  The use of the copied content
 * must be in compliance with Apache's License, Version 2.0.  A copy of that
 * license can be view at http://www.apache.org/licenses/LICENSE-2.0.
 * 
 * Sun's modification code is clearly marked and shall still be under
 * control of the Common Development and Distribution License (CDDL) as
 * declared in the header.
 */

package com.sun.encoder.runtime;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.io.Writer;

import com.sun.encoder.runtime.provider.Base64Coder;

/**
 * An implementation of InputStream that is backed up by a Reader and a known
 * character encoding.  This is kind like a reverse wrapping in terms of what
 * InputStreamReader does.
 * 
 * @version $Revision: 1.2 $
 */
public class ReaderInputStream extends InputStream {

    private static int defaultBufferSize = 2048;
    protected byte buf[];
    protected int writepos;
    protected int readpos;
    protected int markpos = -1;
    protected int marklimit;
    protected OutputStream outputStream = new InternalOutputStream();

    private Reader reader;
    private Writer writer;
    private char[] cbuf;
    //<-- Start of Sun's modification
    private boolean isBase64;
    private int cbufStart;
    //--> End of Sun's modification

    public ReaderInputStream(Reader reader, String encoding)
            throws UnsupportedEncodingException {
        this(reader, encoding, defaultBufferSize);
    }

    public ReaderInputStream(Reader reader, String encoding, int bufferSize)
            throws UnsupportedEncodingException {
        if (bufferSize <= 0)
            throw new IllegalArgumentException("Buffer size <= 0");

        this.reader = reader;
        //<-- Start of Sun's modification
        if ("base64".equals(encoding)) {
            isBase64 = true;
        } else {
        //--> End of Sun's modification
            this.writer = new OutputStreamWriter(getOutputStream(), encoding);
        //<-- Start of Sun's modification
        }
        //--> End of Sun's modification
        cbuf = new char[bufferSize];
        buf = new byte[bufferSize];
        cbufStart = 0;
    }

    /**
     * Called when more bytes need to be written into this stream
     * (as an OutputStream).
     *
     * This method must write at least one byte if the stream is
     * not ended, and it must not write any bytes if the stream has
     * already ended.
     */
    private void fill(int requestedBytes)
            throws IOException {
        do {
            int chars;
            //<-- Start of Sun's modification
            if (!isBase64) {
            //--> End of Sun's modification
                chars = reader.read(cbuf);
                if (chars < 0) {
                    return;
                }
                writer.write(cbuf, 0, chars);
                writer.flush();
            //<-- Start of Sun's modification
            } else {
                chars = reader.read(cbuf, cbufStart, cbuf.length - cbufStart);
                int count = 0;
                while (cbufStart + chars < 4 && chars >= 0) {
                    //at least needs four characters or EOF
                    count =
                        reader.read(cbuf, cbufStart + chars,
                                cbuf.length - cbufStart - chars);
                    if (count < 0) {
                        break;
                    }
                    chars += count;
                }
                if (chars >= 0 && count >= 0) {
                    count = ((int) (((cbufStart + chars) / 4))) * 4;
                } else if (chars >= 0) {
                    count = cbufStart + chars;
                } else {
                    count = cbufStart;
                }
                getOutputStream().write(
                        Base64Coder.decodeToBytes(
                                new String(cbuf, 0, count)));
                if (chars >= 0) {
                    for (int i = count; i < chars + cbufStart; i++) {
                        cbuf[i - count] = cbuf[i];
                    }
                    cbufStart = chars + cbufStart - count;
                } else {
                    cbufStart = 0;
                    break;
                }
            }
        //--> End of Sun's modification
        // Sun's modification: change from "available() <= 0"
        } while (available() <= requestedBytes); // loop for safety, in case
                                                // encoding didn't produce
                                                //any bytes yet
    }

    /**
     * Returns the linked output stream.
     *
     * This is the output stream that must be written to whenever
     * the fill method is called.
     */
    private OutputStream getOutputStream()
    {
        return outputStream;
    }

    /**
     * Makes room for cb more bytes of data
     */
    private void shift(int cb)
    {
        int savepos = readpos;
        if (markpos > 0)
        {
            if (readpos - markpos > marklimit)
                markpos = -1;
            else
                savepos = markpos;
        }

        int size = writepos - savepos;

        if (savepos > 0 && buf.length - size >= cb && size <= cb)
        {
            System.arraycopy(buf, savepos, buf, 0, size);
        }
        else
        {
            int newcount = size + cb;
            byte newbuf[] = new byte[Math.max(buf.length << 1, newcount)];
            System.arraycopy(buf, savepos, newbuf, 0, size);
            buf = newbuf;
        }

        if (savepos > 0)
        {
            readpos -= savepos;
            if (markpos > 0)
                markpos -= savepos;
            writepos -= savepos;
        }
    }

    //Sun's modification: remove the "synchronized" modifier
    public int read() throws IOException
    {
        if (readpos >= writepos)
        {
            fill(1);
            if (readpos >= writepos)
                return -1;
        }
        return buf[readpos++] & 0xff;
    }

    /**
     * Read characters into a portion of an array, reading from the underlying
     * stream at most once if necessary.
     */
    @Override
    public int read(byte[] b, int off, int len) throws IOException
    {
        int avail = writepos - readpos;
        if (avail < len)
        {
            fill(len - avail);
            avail = writepos - readpos;
            if (avail <= 0) return -1;
        }
        int cnt = (avail < len) ? avail : len;
        System.arraycopy(buf, readpos, b, off, cnt);
        readpos += cnt;
        return cnt;
    }

    @Override
    public long skip(long n) throws IOException
    {
        if (n <= 0)
            return 0;

        long avail = writepos - readpos;

        if (avail < n)
        {
            // Fill in buffer to save bytes for reset
            long req = n - avail;
            if (req > Integer.MAX_VALUE)
                req = Integer.MAX_VALUE;
            fill((int)req);
            avail = writepos - readpos;
            if (avail <= 0)
                return 0;
        }

        long skipped = (avail < n) ? avail : n;
        readpos += skipped;
        return skipped;
    }

    @Override
    public int available()
    {
        return writepos - readpos;
    }

    @Override
    public void mark(int readlimit)
    {
        marklimit = readlimit;
        markpos = readpos;
    }

    @Override
    public void reset() throws IOException
    {
        if (markpos < 0)
            throw new IOException("Resetting to invalid mark");
        readpos = markpos;
    }

    @Override
    public boolean markSupported()
    {
        return true;
    }

    private class InternalOutputStream extends OutputStream
    {
        public void write(int b) throws IOException
        {
            if (writepos + 1 > buf.length)
            {
                shift(1);
            }
            buf[writepos] = (byte)b;
            writepos += 1;
        }

        @Override
        public void write(byte b[], int off, int len)
        {
            if ((off < 0) || (off > b.length) || (len < 0) ||
                ((off + len) > b.length) || ((off + len) < 0))
                throw new IndexOutOfBoundsException();
            else if (len == 0)
                return;

            if (writepos + len > buf.length)
                shift(len);

            System.arraycopy(b, off, buf, writepos, len);
            writepos += len;
        }
    }

    @Override
    public void close() throws IOException {
        reader.close();
    }
}
