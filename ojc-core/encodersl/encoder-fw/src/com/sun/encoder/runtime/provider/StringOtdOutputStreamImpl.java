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
 * @(#)StringOtdOutputStreamImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime.provider;

import java.io.IOException;
import java.io.ByteArrayOutputStream;

import com.sun.encoder.runtime.OtdOutputStream;

/**
 * Class to provide an output byte stream to an OTD unmarshal (deserialization)
 * method. The underlying transport model could be a byte array, an external
 * file, a stream over JMS, or any other number of things. The transport model
 * used may be a trade-off between speed and large-message support.
 *
 * @author Michael Libourel
 * @version 
 */
public class StringOtdOutputStreamImpl
    implements OtdOutputStream {
    private ByteArrayOutputStream bos = null;
    private boolean mOpen = false; // between begin() / end() ?

    /**
     * Creates fresh.
     */
    public StringOtdOutputStreamImpl () { }

    /**
     * Retrieves the contents. This also releases the buffer contents.
     *
     * @return the data as a string, assuming UTF-8 encoding
     * @throws IOException if stream not closed, or already taken
     */
    public String takeData ()
        throws IOException {
        if (mOpen) {
            throw new IOException("forgot end()");
        }
        if (bos == null) {
            throw new IOException("second takeData()");
        }
        String result = Misc.bytes2str(bos.toByteArray());
        bos = null;
        return result;
    }

    /**
     * Retrieves the contents. This also releases the buffer contents.
     *
     * @return the raw data bytes
     * @throws IOException if stream not closed, or already taken
     */
    public byte[] takeBytes ()
        throws IOException {
        if (mOpen) {
            throw new IOException("forgot end()");
        }
        if (bos == null) {
            throw new IOException("second takeData()");
        }
        byte[] result = bos.toByteArray();
        bos = null;
        return result;
    }

    /*------------------*\
    |  REQUIRED METHODS  |
    \*------------------*/

    /**
     * Announces the start of writing the message.
     *
     * @throws IOException for output problem
     */
    public void begin ()
        throws IOException {
        if (mOpen) {
            throw new IOException("second begin()");
        }
        mOpen = true;
        bos = new ByteArrayOutputStream();
    }

    /**
     * Announces the end of writing the message.
     *
     * @throws IOException for output problem
     */
    public void end()
        throws IOException {
        if (!mOpen) {
            throw new IOException("second end()");
        }
        mOpen = false;
    }

    /**
     * Writes the given part of the buffer to the stream.
     *
     * @param buffer the bytes array to write from
     * @param offset where to start
     * @param size how many bytes to write, from start
     *
     * @throws IOException for output problem
     */
    public void write (byte buffer[], int offset, int size)
        throws IOException {
        if (buffer == null) {
            throw new NullPointerException("no buffer");
        }
        if (!mOpen) {
            throw new IOException("forgot begin()");
        }
        bos.write(buffer, offset, size);
    }

    /**
     * Writes the buffer to the stream.
     *
     * @param buffer the bytes array to write from
     *
     * @throws IOException for output problem
     */
    public void write (byte buffer[])
        throws IOException {
        write(buffer, 0, buffer.length);
    }

    /**
     * Writes the given byte to the stream.
     *
     * @param value the byte to write
     *
     * @throws IOException for output problem
     */
    public void write (byte value)
        throws IOException {
        if (!mOpen) {
            throw new IOException("forgot begin()");
        }
        bos.write(value);
    }

    /**
     * Unit test, echoes command line parameters.
     *
     * @param args  command line arguments
     */
    public static void main (String[] args) {
        try {
            StringOtdOutputStreamImpl soos = new StringOtdOutputStreamImpl();
            OtdOutputStream oos = soos;
            oos.begin();
            for (int i = 0; i < args.length; i++) {
                if (i > 0) {
                    oos.write((byte) ' ');
                }
                oos.write(args[i].getBytes("UTF-8"));
            }
            oos.end();
            System.out.println("Data = '" + soos.takeData() + "'");
        } catch (Exception all) {
            all.printStackTrace();
            System.exit(1);
        }
    }
}
