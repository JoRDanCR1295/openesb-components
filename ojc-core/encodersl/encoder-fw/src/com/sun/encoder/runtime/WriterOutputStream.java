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
 * @(#)WriterOutputStream.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;

import com.sun.encoder.runtime.provider.Base64Coder;

/**
 * Wraps a Writer instance as an OutputStream instance.  It is kind like a
 * reverse wrapping in terms of what <code>OutputStreamWriter</code> does.
 * 
 * @author Jun Xu
 * @version $Revision: 1.1 $
 */
public class WriterOutputStream extends OutputStream {

    private static final int DEFAULT_BUF_SIZE = 2048;
    
    private final int mBufferSize;
    private final Writer mWriter;
    private final CharsetDecoder mCharsetDecoder;
    private final boolean mIsBase64;
    private ByteBuffer mByteBuf;
    private CharBuffer mCharBuf;
    private byte[] mOneByte = new byte[1];
    
    public WriterOutputStream(Writer writer, String encoding)
            throws UnsupportedEncodingException {
        this(writer, encoding, DEFAULT_BUF_SIZE);
    }

    public WriterOutputStream(Writer writer, String encoding, int bufferSize)
            throws UnsupportedEncodingException {
        if (bufferSize < 1) {
            throw new IllegalArgumentException(
                    "Illegal buffer size: " + bufferSize);
        }
        mIsBase64 = "base64".equals(encoding);
        mBufferSize = bufferSize;
        mWriter = writer;
        mCharsetDecoder =
            mIsBase64 ? null : Charset.forName(encoding).newDecoder();
        mByteBuf = ByteBuffer.allocate(mBufferSize + 128);
        mByteBuf.flip();
        mCharBuf = CharBuffer.allocate(mByteBuf.capacity() * 4 / 3 + 4);
    }

    @Override
    public void close() throws IOException {
        flushToWriter(true);
        mWriter.flush();
        mWriter.close();
    }

    @Override
    public void flush() throws IOException {
        flushToWriter(false);
        mWriter.flush();
    }

    @Override
    public void write(byte[] b, int off, int len) throws IOException {
        if (len <= mBufferSize) {
            writeInternal(b, off, len);
            return;
        }
        final int count = len / mBufferSize; 
        for (int i = 0; i < count; i++) {
            writeInternal(b, off + i * mBufferSize, mBufferSize);
        }
        writeInternal(b, off + count * mBufferSize, len - count * mBufferSize);
    }

    @Override
    public void write(byte[] b) throws IOException {
        write(b, 0, b.length);
    }

    @Override
    public void write(int b) throws IOException {
        mOneByte[0] = (byte) (b & 0xff);
        write(mOneByte);
    }
    
    /**
     * Translates the bytes in the byte buffer into characters and writes
     * them to the Writer.
     * 
     * @param done if the end of stream has been reached
     * @throws IOException
     */
    public void flushToWriter(boolean done) throws IOException {
        if (!mByteBuf.hasRemaining()) {
            return;
        }
        mCharBuf.limit(mCharBuf.capacity());
        mCharBuf.rewind();
        if (!mIsBase64) {
            if (mByteBuf.remaining() > mCharBuf.remaining()) {
                mCharBuf = CharBuffer.allocate(mByteBuf.remaining());
            }
            mCharsetDecoder.decode(mByteBuf, mCharBuf, done);
        } else {
            if (!done && mByteBuf.remaining() < 3) {
                return;
            }
            if (mByteBuf.remaining() * 4 / 3 > mCharBuf.remaining()) {
                mCharBuf = CharBuffer.allocate(
                        mByteBuf.remaining() * 4 / 3 + 4);
            }
            if (done) {
                mCharBuf.put(
                        Base64Coder.encodeToString(
                                mByteBuf.array(),
                                mByteBuf.arrayOffset() + mByteBuf.position(),
                                mByteBuf.remaining()));
                mByteBuf.position(mByteBuf.limit());
            } else {
                int count = (mByteBuf.remaining() / 3) * 3;
                if (count > 0) {
                    mCharBuf.put(
                            Base64Coder.encodeToString(
                                    mByteBuf.array(),
                                    mByteBuf.arrayOffset()
                                        + mByteBuf.position(),
                                    count));
                    mByteBuf.position(mByteBuf.position() + count);
                }
            }
        }
        mCharBuf.flip();
        if (mCharBuf.hasRemaining()) {
            if (mCharBuf.hasArray()) {
                mWriter.write(
                        mCharBuf.array(),
                        mCharBuf.position() + mCharBuf.arrayOffset(),
                        mCharBuf.remaining());
            } else {
                mWriter.write(mCharBuf.toString());
            }
        }
    }
    
    /**
     * Writes the bytes to the Writer.
     * 
     * @param b the bytes to be written
     * @param off the offset of the first byte to be written
     * @param len the length of the bytes to be written
     * @throws IOException thrown if IO problems occur
     */
    private void writeInternal(byte[] b, int off, int len) throws IOException {
        makeRoom(len);
        mByteBuf.mark();
        mByteBuf.position(mByteBuf.limit());
        mByteBuf.limit(mByteBuf.capacity());
        mByteBuf.put(b, off, len);
        mByteBuf.limit(mByteBuf.position());
        mByteBuf.reset();
        flushToWriter(false);
    }
    
    /**
     * Makes room in the byte buffer to capable of accepting required number
     * of new bytes.
     * 
     * @param required the required number of bytes
     */
    private void makeRoom(int required) {
        if (required <= mByteBuf.capacity() - mByteBuf.limit()) {
            return;
        }
        if (required <= mByteBuf.capacity() - mByteBuf.remaining()) {
            mByteBuf.compact();
            mByteBuf.flip();
            return;
        }
        ByteBuffer buf = mByteBuf;
        mByteBuf = ByteBuffer.allocate(buf.remaining() + required);
        mByteBuf.put(buf);
        mByteBuf.flip();
        return;
    }
    
    public static void main(String[] argv) {
        try {
            // Trivial testing for WriterOutputStream and ReaderInputStream
            Writer writer = new StringWriter();
            OutputStream out = new WriterOutputStream(writer, "UTF-8", 64);
            InputStream in = new FileInputStream("c:/temp/utf8.txt");
            int read;
            byte[] buf = new byte[85];
            while ((read = in.read(buf)) != -1) {
                out.write(buf, 0, read);
            }
            in.close();
            out.close();
            String result = writer.toString();
            System.out.println(result);
            writer = new OutputStreamWriter(
                    new FileOutputStream("c:/temp/utf8_r1.txt"), "UTF-8");
            writer.write(result);
            writer.close();
            Reader reader = new StringReader(result);
            in = new ReaderInputStream(reader, "UTF-8", 64);
            out = new FileOutputStream("c:/temp/utf8_r2.txt");
            while ((read = in.read(buf)) != -1) {
                out.write(buf, 0, read);
            }
            in.close();
            out.close();
            
            reader =
                new InputStreamReader(
                        new FileInputStream("c:/temp/base64.txt"), "ASCII");
            in = new ReaderInputStream(reader, "base64", 96);
            out = new FileOutputStream("c:/temp/base64_r1.html");
            while ((read = in.read(buf)) != -1) {
                out.write(buf, 0, read);
            }
            in.close();
            out.close();
            in = new FileInputStream("c:/temp/base64_r1.html");
            writer = new StringWriter();
            out = new WriterOutputStream(writer, "base64", 96);
            while ((read = in.read(buf)) != -1) {
                out.write(buf, 0, read);
            }
            in.close();
            out.close();
            result = writer.toString();
            System.out.println(result);
            writer =
                new OutputStreamWriter(
                        new FileOutputStream("c:/temp/base64_r2.txt"), "ASCII");
            writer.write(result);
            writer.close();
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        } catch (Throwable t) {
            t.printStackTrace();
        }
    }
}
