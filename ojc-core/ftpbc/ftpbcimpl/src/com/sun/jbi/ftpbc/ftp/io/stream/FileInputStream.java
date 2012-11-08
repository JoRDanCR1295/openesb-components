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
 * @(#)FileInputStream.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
/******************************************************************************
 * Copyright © 2006 Sun Microsystems, Inc., 4150 Network Circle, Santa Clara,
 * California 95054, U.S.A. All rights reserved. Sun Microsystems, Inc. has
 * intellectual property rights relating to technology embodied in the product
 * that is described in this document. In particular, and without limitation,
 * these intellectual property rights may include one or more of the U.S. patents
 * listed at http://www.sun.com/patents and one or more additional patents or
 * pending patent applications in the U.S. and in other countries. THIS PRODUCT
 * CONTAINS CONFIDENTIAL INFORMATION AND TRADE SECRETS OF SUN MICROSYSTEMS, INC.
 * USE, DISCLOSURE OR REPRODUCTION IS PROHIBITED WITHOUT THE PRIOR EXPRESS WRITTEN
 * PERMISSION OF SUN MICROSYSTEMS, INC.U.S. Government Rights - Commercial
 * software.  Government users are subject to the Sun Microsystems, Inc. standard
 * license agreement and applicable provisions of the FAR and its supplements.
 * Use is subject to license terms.  This distribution may include materials
 * developed by third parties. Sun, Sun Microsystems, the Sun logo, Java
 * Composite Application Platform Suite,  SeeBeyond, eGate, eInsight, eVision, eTL,
 * eXchange, eView, eIndex, eBAM and  eWay are trademarks or registered trademarks of
 * Sun Microsystems, Inc. in the U.S. and other countries. All SPARC trademarks are
 * used under license and are trademarks or registered trademarks of SPARC
 * International, Inc. in the U.S. and other countries. Products bearing SPARC
 * trademarks are based upon architecture developed by Sun Microsystems, Inc.
 * UNIX is a registered trademark in the U.S. and other countries, exclusively
 * licensed through X/Open Company, Ltd. This product is covered and controlled by
 * U.S. Export Control laws and may be subject to the export or import laws in
 * other countries.  Nuclear, missile, chemical biological weapons or nuclear
 * maritime end uses or end users, whether direct or indirect, are strictly
 * prohibited.  Export or reexport to countries subject to U.S. embargo or to
 * entities identified on U.S. export exclusion lists, including, but not limited
 * to, the denied persons and specially designated nationals lists is strictly
 * prohibited.
 **/
package com.sun.jbi.ftpbc.ftp.io.stream;

import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.File;
import java.io.FileDescriptor;

/**
 *
 * This class extends the java.io.FileInputStream class in order to provide
 * the ability to track the current position of the input stream as data is
 * being read.  It also provides methods such as a method for getting the
 * size of the file being read and a method for determining the end of file.
 * See the class methods' JavaDoc for details.
 *
 */
public class FileInputStream extends java.io.FileInputStream {

    protected long markPos = -1;     // The marked position as a result of calling markPos
    protected long pos = 0;      // Current position in the stream.
    protected File file = null;   // Constructed with a File.
    protected String name = null;   // Constructed with a filename.

    /**
     * Constructs an FileInputStream.
     *
     * @param     file        An instance of java.io.File.
     *
     * @throws    java.io.FileNotFoundException upon error opening the file.
     *
     * @author    SeeBeyond
     */
    public FileInputStream(File file) throws FileNotFoundException {
        super(file);
        this.file = file;
    }

    /**
     * Constructs an FileInputStream.
     *
     * @param     fdObj        An instance of java.io.FileDescriptor.
     *
     * @author    SeeBeyond
     */
    protected FileInputStream(FileDescriptor fdObj) {
        super(fdObj);
    }

    /**
     * Constructs an FileInputStream.
     *
     * @param     name        The name of the file.
     *
     * @throws    java.io.FileNotFoundException upon error opening the file.
     *
     * @author    SeeBeyond
     */
    public FileInputStream(String name) throws FileNotFoundException {
        super(name);
        this.name = name;
    }

    /**
     * Marks the current position in this input stream. A subsequent call to the reset
     * method repositions this stream at the last marked position so that subsequent
     * reads re-read the same bytes.
     *
     * The readlimit arguments tells this input stream to allow that many bytes to be
     * read before the mark position gets invalidated.
     *
     * The stream remembers all the bytes read after the call to mark and stands ready
     * to supply those same bytes again if and whenever the method reset is called.
     * However, the stream is not required to remember any data at all if more than
     * readlimit bytes are read from the stream before reset is called.
     *
     * @param     readlimit        The maximum limit of bytes that can be read before
     *                             the mark position becomes invalid.
     *
     * @see       #reset
     *
     * @author    SeeBeyond
     */
    public void mark(int readlimit) {
        markPos = pos;
        super.mark(readlimit);
    }

    /**
     * Reads a byte of data from this input stream.
     * This method blocks if no input is yet available.
     *
     * @returns   The next byte of data, or -1 if the end of the file is reached.
     *
     * @throws    java.io.IOException upon error reading the stream
     *
     * @author    SeeBeyond
     */
    public int read() throws IOException {
        int i = super.read();
        if (-1 != i) {
            pos++;
        }
        return i;
    }

    /**
     * Reads up to b.length bytes of data from this input stream into an array of bytes.
     * This method blocks until some input is available.
     *
     * @param     b     The buffer into which the data is read.
     *
     * @returns   The total number of bytes read into the buffer, or -1 if there is no
     *            more data because the end of the file has been reached.
     *
     * @throws    java.io.IOException upon error reading the stream
     *
     * @author    SeeBeyond
     */
    public int read(byte[] b) throws IOException {
        int i = super.read(b);
        if (-1 != i) {
            pos = pos + i;
        }
        return i;
    }

    /**
     * Reads up to len bytes of data from this input stream into an array of bytes.
     * This method blocks until some input is available.
     *
     * @param     b     The buffer into which the data is to be read.
     * @param     off   The start offset of the buffer.
     * @param     len   The maximum number of bytes to be read.
     *
     * @returns   The total number of bytes read into the buffer, or -1 if there is no
     *            more data because the end of the file has been reached.
     *
     * @throws    java.io.IOException upon error reading the stream
     *
     * @author    SeeBeyond
     */
    public int read(byte[] b, int off, int len) throws IOException {
        int i = super.read(b, off, len);
        if (-1 != i) {
            pos = pos + i;
        }
        return i;
    }

    /**
     * Repositions this stream to the position at the time the mark method was last
     * called on this input stream
     *
     * @see       #mark
     *
     * @throws    java.io.IOException upon error reading the stream
     *
     * @author    SeeBeyond
     */
    public void reset() throws IOException {
        super.reset();
        pos = markPos;
    }

    /**
     * Skips over and discards n bytes of data from the input stream. The skip method
     * may, for a variety of reasons, end up skipping over some smaller number of bytes,
     * possibly 0. The actual number of bytes skipped is returned.  If n is negative then
     * skip will skip backwards (rewind) n number of bytes.
     *
     * @see       #mark
     *
     * @throws    java.io.IOException upon error reading the stream
     *
     * @author    SeeBeyond
     */
    public long skip(long n) throws IOException {
        long i = super.skip(n);
        pos += i;
        return i;
    }

    /**
     * Determines whether an end of file has been reached.
     *
     * @returns   true if the end of file has been reached or false
     *            otherwise.
     *
     * @throws    java.io.IOException upon error.
     *
     * @author    SeeBeyond
     */
    public boolean endOfFile() throws IOException {
        boolean eof = (super.read() == -1);
        if (!eof) {
            super.skip(-1);
        }
        return eof;
    }

    /**
     * Gets the current position in the stream.  The position is
     * is zero-based.  The position points to the next byte that
     * is to be read.
     *
     * @returns   The position of the next byte for reading.
     *
     * @throws    java.io.IOException upon error.
     *
     * @author    SeeBeyond
     */
    public long getPosition() {
        return pos;
    }

    /**
     * Gets the total size of the file being read.
     *
     * @returns   The size of the file.
     *
     * @author    SeeBeyond
     */
    public long getSize() {
        return (file == null ? (new File(this.name)).length() : this.file.length());
    }

    static public void main(String args[]) {
        try {
            String testFile;
            if (null == args || 0 == args.length) {
                testFile = "/tmp/harry.test.txt";
            } else {
                testFile = args[0];
            }

            FileInputStream fis = new FileInputStream(testFile);
            System.out.println("Size : " + fis.getSize());
            int count = 0;
            int charRead = 0;
            //System.out.println ("Current Position : " + fis.getPosition());
            //while ((charRead = fis.read()) != -1)
            while (!fis.endOfFile()) {
                //System.out.println ("Read Char : " + (char)charRead);
                System.out.println("Current Position : " + fis.getPosition());
                System.out.println("Read Char : " + (char) fis.read());
                count++;
            }
            fis.skip((int) 0 - count);
            System.out.println("Current Position : " + fis.getPosition());
            byte[] buff = new byte[5];
            int i = fis.read(buff);
            System.out.println("Current Position : " + fis.getPosition());
            System.out.println("Bytes Read : " + new String(buff, 0, i));
            fis.skip((int) 0 - i);
            buff[0] = 'z';
            i = fis.read(buff, 1, 4);
            System.out.println("Current Position : " + fis.getPosition());
            System.out.println("Bytes : " + new String(buff, 0, i + 1));
            if (fis.endOfFile()) {
                System.out.println("End of File!!");
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}
