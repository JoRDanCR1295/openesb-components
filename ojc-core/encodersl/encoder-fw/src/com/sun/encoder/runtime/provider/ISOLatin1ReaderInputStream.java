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
 * @(#)ISOLatin1ReaderInputStream.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime.provider;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.nio.charset.UnmappableCharacterException;

/**
 * A utility class that wraps a reader, which is assumed to be ISO-8859-1
 * (ISO Latin 1) encoded, into an input stream. The rational behind this 
 * is that ISO-8859-1 is incorporated as the first 256 code points of 
 * ISO/IEC 10646 and Unicode.
 * 
 * @author Jun Xu
 * @version $Revision: 1.1 $
 */
public class ISOLatin1ReaderInputStream extends InputStream {
    
    private final Reader mReader;
    private final char[] mBuf = new char[1024];
    
    public ISOLatin1ReaderInputStream(Reader reader) {
        mReader = reader;
    }

    @Override
    public int available() throws IOException {
        if (mReader.ready()) {
            return 1;
        }
        return 0;
    }

    @Override
    public void close() throws IOException {
        mReader.close();
    }

    @Override
    public synchronized void mark(int readlimit) {
        throw new UnsupportedOperationException("mark not supported.");
    }

    @Override
    public boolean markSupported() {
        return false;
    }

    @Override
    public int read(byte[] b, int off, int len) throws IOException {
        char[] buf = mBuf;
        if (len < buf.length) {
            int got = mReader.read(buf, 0, len);
            for (int i = 0; i < got; i++) {
                if (buf[i] < 0 || buf[i] > 255) {
                    throw new UnmappableCharacterException(1);
                }
                b[i + off] = (byte) (0xFF & buf[i]);  
            }
            return got;
        }
        int left = len;
        boolean eof = false;
        while (left > 0) {
            int readsofar = len - left;
            int got =
                mReader.read(buf, 0, left > buf.length ? buf.length : left);
            for (int i = 0; i < got; i++) {
                if (buf[i] < 0 || buf[i] > 255) {
                    throw new UnmappableCharacterException(1);
                }
                b[i + readsofar] = (byte) (0xFF & buf[i]);
            }
            if (got >= 0) {
                left -= got;
            } else {
                eof = true;
                break;
            }
        }
        if (eof && len == left) {
            //read nothing
            return -1;
        }
        return len - left;
    }

    @Override
    public int read(byte[] b) throws IOException {
        return read(b, 0, b.length);
    }

    @Override
    public synchronized void reset() throws IOException {
        throw new UnsupportedOperationException("reset not supported.");
    }

    @Override
    public long skip(long n) throws IOException {
        return mReader.skip(n);
    }

    @Override
    public int read() throws IOException {
        int c = mReader.read();
        if (c < 0 || c > 255) {
            throw new UnmappableCharacterException(1);
        }
        return c;
    }
}
