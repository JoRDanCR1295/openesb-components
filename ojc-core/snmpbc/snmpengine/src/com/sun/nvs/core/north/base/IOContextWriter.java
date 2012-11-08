/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.north.base;

import java.io.*;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public class IOContextWriter extends java.io.Writer {
    private IOContext _ioContext;

    /**
     * Creates a new IOContextWriter object.
     *
     * @param io DOCUMENT ME!
     */
    public IOContextWriter(IOContext io) {
        super();

        _ioContext = io;
    }

    /**
     * DOCUMENT ME!
     *
     * @throws IOException DOCUMENT ME!
     */
    public void close() throws IOException {
    }

    /**
     * DOCUMENT ME!
     *
     * @throws IOException DOCUMENT ME!
     */
    public void flush() throws IOException {
    }

    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     *
     * @throws IOException DOCUMENT ME!
     */
    public void write(String s) throws IOException {
        _ioContext.showMessage(s);
    }

    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     * @param offset DOCUMENT ME!
     * @param count DOCUMENT ME!
     *
     * @throws IOException DOCUMENT ME!
     */
    public void write(String s, int offset, int count)
        throws IOException {
        this.write(s.toCharArray(), offset, count);
    }

    /**
     * DOCUMENT ME!
     *
     * @param buf DOCUMENT ME!
     *
     * @throws IOException DOCUMENT ME!
     */
    public void write(char[] buf) throws IOException {
        this.write(new String(buf));
    }

    /**
     * DOCUMENT ME!
     *
     * @param buf DOCUMENT ME!
     * @param offset DOCUMENT ME!
     * @param count DOCUMENT ME!
     *
     * @throws IOException DOCUMENT ME!
     */
    public void write(char[] buf, int offset, int count)
        throws IOException {
        this.write(new String(buf, offset, count));
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public IOContext getIOContext() {
        return _ioContext;
    }
}
