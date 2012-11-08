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
 * @(#)ISOLatin1WriterOutputStream.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime.provider;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;

/**
 * A utility class that wraps a writer, which is assumed to be ISO-8859-1
 * (ISO Latin 1) encoded, into an output stream. The rational behind this
 * is that ISO-8859-1 is incorporated as the first 256 code points of
 * ISO/IEC 10646 and Unicode.
 * 
 * @author Jun Xu
 * @version $Revision: 1.1 $
 */
public class ISOLatin1WriterOutputStream extends OutputStream {

    private final Writer mWriter;
    
    public ISOLatin1WriterOutputStream(Writer writer) {
        mWriter = writer;
    }
    
    @Override
    public void close() throws IOException {
        mWriter.close();
    }

    @Override
    public void flush() throws IOException {
        mWriter.flush();
    }

    @Override
    public void write(byte[] b, int off, int len) throws IOException {
        mWriter.write(new String(b, off, len, "ISO-8859-1"));
    }

    @Override
    public void write(byte[] b) throws IOException {
        write(b, 0, b.length);
    }

    @Override
    public void write(int b) throws IOException {
        mWriter.write(b);
    }
}
