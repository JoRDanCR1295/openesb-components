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
 * Copyright 2007-2008 ZAZ Consulting, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.zaz.ssapi.protocol.common.telnet;

import java.awt.Dimension;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import java.net.Socket;

public class Wrapper {

    protected InputStream in;
    protected OutputStream out;
    protected Socket socket;
    protected String host;
    protected int port = 23;

    public void connect(String host, int port) throws IOException {
        try {
            socket = new java.net.Socket(host, port);
            in = socket.getInputStream();
            out = socket.getOutputStream();
        } catch (Exception e) {
            System.err.println("Wrapper: " + e);
            disconnect();
            throw ((IOException) e);
        }
    }

    public void disconnect() throws IOException {
        if (socket != null) {
            socket.close();
        }
    }

    public int read(byte[] b) throws IOException {
        return -1;
    }

    public void write(byte[] b) throws IOException {
        out.write(b);
    }

    public String getTerminalType() {
        return "dumb";
    }

    public Dimension getWindowSize() {
        return new Dimension(80, 24);
    }
}
