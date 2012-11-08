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

public class TelnetWrapper extends Wrapper {

    protected TelnetProtocolHandler handler;

    public TelnetWrapper() {
        handler = new TelnetProtocolHandler() {

            /** get the current terminal type */
            public String getTerminalType() {
                return "IBM-3278-2";
            }

            /** get the current window size */
            public Dimension getWindowSize() {
                return new Dimension(80, 24);
            }

            /** notify about local echo */
            public void setLocalEcho(boolean echo) {
            }

            /** write data to our back end */
            public void write(byte[] b) throws IOException {
                out.write(b);
            }

            /** sent on IAC EOR (prompt terminator for remote access systems). */
            public void notifyEndOfRecord() {
            }
        };
    }

    public TelnetProtocolHandler getHandler() {
        return handler;
    }

    @Override
    public void connect(String host, int port) throws IOException {
        super.connect(host, port);
        handler.reset();
    }

    @Override
    public int read(byte[] b) throws IOException {
        /* process all already read bytes */
        int n;

        do {
            n = handler.negotiate(b);
            if (n > 0) {
                return n;
            }
        } while (n == 0);

        while (n <= 0) {
            do {
                n = handler.negotiate(b);
                if (n > 0) {
                    return n;
                }
            } while (n == 0);
            n = in.read(b);
            if (n < 0) {
                return n;
            }
            handler.inputfeed(b, n);
            n = handler.negotiate(b);
        }
        return n;
    }
}
