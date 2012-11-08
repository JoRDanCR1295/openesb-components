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
 * @(#)FileStreamHandler.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.ftpbc.util;

import com.sun.jbi.internationalization.Messages;

import java.io.IOException;
import java.io.InputStream;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.io.ByteArrayOutputStream;

/**
 * Input stream handler.
 *
 * Sun Microsystems
 **/
public class FTPInputStreamWrapper {

    public static final int DEFAULT_BUFFER_LENGTH = 1024;
    private static final Messages mMessages = Messages.getMessages(FTPInputStreamWrapper.class);
    private static Logger mLogger = Messages.getLogger(FTPInputStreamWrapper.class);
    private InputStream mStream;
    private boolean donotClose = false;
    private boolean closed = false;

    public FTPInputStreamWrapper(InputStream aStream) {
        if (aStream == null) {
            mLogger.log(Level.SEVERE, mMessages.getString("FTPBC-E006101"));
            throw new IllegalArgumentException(mMessages.getString("FTPBC-E006101"));
        }
        mStream = aStream;
    }

    public byte[] getAllContentsAsBytes() throws IOException {
        return getAllContentsAsBytes(mStream);
    }

    public byte[] getAllContentsAsBytes(InputStream aStream) throws IOException {
        if (aStream == null) {
            mLogger.log(Level.SEVERE, mMessages.getString("FTPBC-E006101"));
            throw new IOException(mMessages.getString("FTPBC-E006101"));
        }

        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        byte[] buffer = new byte[DEFAULT_BUFFER_LENGTH];
        int len = 0;
        try {
            while ((len = aStream.read(buffer)) > 0) {
                bos.write(buffer, 0, len);
            }
        } catch (IOException e) {
            mLogger.log(Level.SEVERE, e.getLocalizedMessage());
            throw e;
        }

        if (len >= 0) {
            throw new IOException("EOF not reached when reading all contents as bytes...");
        }
        return bos.toByteArray();
    }

    public InputStream getInpuStream() {
        return mStream;
    }

    public synchronized void close() {
        if (closed) {
            return;
        }
        try {
            mStream.close();
            closed = true;
        } catch (Throwable t) {
        }
    }

    public boolean isDonotClose() {
        return donotClose;
    }

    public void setDonotClose(boolean donotClose) {
        this.donotClose = donotClose;
    }
}
