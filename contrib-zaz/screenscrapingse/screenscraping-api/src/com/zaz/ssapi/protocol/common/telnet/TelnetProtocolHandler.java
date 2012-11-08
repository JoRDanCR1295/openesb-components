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

public abstract class TelnetProtocolHandler {

    public final static String ID = "$Id: TelnetProtocolHandler.java,v 1.1 2008/09/23 01:41:09 wbzhang Exp $";
    private static byte[] one = new byte[1];
    private byte[] tempbuf = new byte[0];
    private byte[] crlf = new byte[2];
    private byte[] cr = new byte[2];

    public TelnetProtocolHandler() {
        reset();

        crlf[0] = 13;
        crlf[1] = 10;
        cr[0] = 13;
        cr[1] = 0;
    }

    protected abstract String getTerminalType();

    protected abstract Dimension getWindowSize();

    protected abstract void setLocalEcho(boolean echo);

    protected abstract void notifyEndOfRecord();

    protected abstract void write(byte[] b) throws IOException;

    private void write(byte b) throws IOException {
        one[0] = b;
        write(one);
    }

    public void reset() {
        neg_state = 0;
        receivedDX = new byte[256];
        sentDX = new byte[256];
        receivedWX = new byte[256];
        sentWX = new byte[256];
    }

    // ===================================================================
    // the actual negotiation handling for the telnet protocol follows:
    // ===================================================================
    /** state variable for telnet negotiation reader */
    private byte neg_state = 0;
    /** constants for the negotiation state */
    private final static byte STATE_DATA = 0;
    private final static byte STATE_IAC = 1;
    private final static byte STATE_IACSB = 2;
    private final static byte STATE_IACWILL = 3;
    private final static byte STATE_IACDO = 4;
    private final static byte STATE_IACWONT = 5;
    private final static byte STATE_IACDONT = 6;
    private final static byte STATE_IACSBIAC = 7;
    private final static byte STATE_IACSBDATA = 8;
    private final static byte STATE_IACSBDATAIAC = 9;
    /** What IAC SB <xx> we are handling right now */
    private byte current_sb;
    /** current SB negotiation buffer */
    private byte[] sbbuf;
    /** IAC - init sequence for telnet negotiation. */
    private final static byte IAC = (byte) 255;
    /** [IAC] End Of Record */
    private final static byte EOR = (byte) 239;
    /** [IAC] WILL */
    private final static byte WILL = (byte) 251;
    /** [IAC] WONT */
    private final static byte WONT = (byte) 252;
    /** [IAC] DO */
    private final static byte DO = (byte) 253;
    /** [IAC] DONT */
    private final static byte DONT = (byte) 254;
    /** [IAC] Sub Begin */
    private final static byte SB = (byte) 250;
    /** [IAC] Sub End */
    private final static byte SE = (byte) 240;
    /** Telnet option: binary mode */
    private final static byte TELOPT_BINARY = (byte) 0;  /* binary mode */

    /** Telnet option: echo text */
    private final static byte TELOPT_ECHO = (byte) 1;  /* echo on/off */

    /** Telnet option: sga */
    private final static byte TELOPT_SGA = (byte) 3;  /* supress go ahead */

    /** Telnet option: End Of Record */
    private final static byte TELOPT_EOR = (byte) 25;  /* end of record */

    /** Telnet option: Negotiate About Window Size */
    private final static byte TELOPT_NAWS = (byte) 31;  /* NA-WindowSize*/

    /** Telnet option: Terminal Type */
    private final static byte TELOPT_TTYPE = (byte) 24;  /* terminal type */

    private final static byte TELOPT_TANY = (byte) 25;
//    private final static byte[] IACWILL = {IAC, WILL};
//    private final static byte[] IACWONT = {IAC, WONT};
//    private final static byte[] IACDO = {IAC, DO};
//    private final static byte[] IACDONT = {IAC, DONT};
    private final static byte[] IACSB = {IAC, SB};
    private final static byte[] IACSE = {IAC, SE};
    /** Telnet option qualifier 'IS' */
    private final static byte TELQUAL_IS = (byte) 0;
    /** Telnet option qualifier 'SEND' */
    private final static byte TELQUAL_SEND = (byte) 1;
    /** What IAC DO(NT) request do we have received already ? */
    private byte[] receivedDX;
    /** What IAC WILL/WONT request do we have received already ? */
    private byte[] receivedWX;
    /** What IAC DO/DONT request do we have sent already ? */
    private byte[] sentDX;
    /** What IAC WILL/WONT request do we have sent already ? */
    private byte[] sentWX;

//    public void sendTelnetControl(byte code)
//            throws IOException {
//        byte[] b = new byte[2];
//
//        b[0] = IAC;
//        b[1] = code;
//        write(b);
//    }

//    public void setWindowSize(int columns, int rows)
//            throws IOException {
//        if (receivedDX[TELOPT_NAWS] != DO) {
//            System.err.println("not allowed to send NAWS? (DONT NAWS)");
//            return;
//        }
//        write(IAC);
//        write(SB);
//        write(TELOPT_NAWS);
//        write((byte) (columns >> 8));
//        write((byte) (columns & 0xff));
//        write((byte) (rows >> 8));
//        write((byte) (rows & 0xff));
//        write(IAC);
//        write(SE);
//    }
    private void handle_sb(byte type, byte[] sbdata)
            throws IOException {
        switch (type) {
            case TELOPT_TTYPE:
                if (sbdata.length > 0 && sbdata[0] == TELQUAL_SEND) {
                    String ttype = getTerminalType();
                    if (ttype == null) {
                        ttype = "dumb";
                    }
                    byte[] buf = new byte[ttype.length() + 6];
                    buf[0] = IACSB[0];
                    buf[1] = IACSB[1];
                    buf[2] = TELOPT_TTYPE;
                    buf[3] = TELQUAL_IS;
                    for (int i = 0; i < ttype.length(); i++) {
                        buf[i + 4] = ttype.getBytes()[i];
                    }
                    buf[ttype.length() + 4] = IACSE[0];
                    buf[ttype.length() + 5] = IACSE[1];
                    write(buf);
                }

        }
    }

    public void transpose(byte[] buf) throws IOException {
        int i;

        byte[] nbuf, xbuf;
        int nbufptr = 0;
        nbuf = new byte[buf.length * 2]; // FIXME: buffer overflows possible

        for (i = 0; i < buf.length; i++) {
            switch (buf[i]) {
                // Escape IAC twice in stream ... to be telnet protocol compliant
                // this is there in binary and non-binary mode.
                case IAC:
                    nbuf[nbufptr++] = IAC;
                    nbuf[nbufptr++] = IAC;
                    break;
                // We need to heed RFC 854. LF (\n) is 10, CR (\r) is 13
                // we assume that the Terminal sends \n for lf+cr and \r for just cr
                // linefeed+carriage return is CR LF */ 
                case 10:	// \n

                    if (receivedDX[TELOPT_BINARY + 128] != DO) {
                        while (nbuf.length - nbufptr < crlf.length) {
                            xbuf = new byte[nbuf.length * 2];
                            System.arraycopy(nbuf, 0, xbuf, 0, nbufptr);
                            nbuf = xbuf;
                        }
                        for (int j = 0; j < crlf.length; j++) {
                            nbuf[nbufptr++] = crlf[j];
                        }
                        break;
                    } else {
                        // copy verbatim in binary mode.
                        nbuf[nbufptr++] = buf[i];
                    }
                    break;
                // carriage return is CR NUL */ 
                case 13:	// \r

                    if (receivedDX[TELOPT_BINARY + 128] != DO) {
                        while (nbuf.length - nbufptr < cr.length) {
                            xbuf = new byte[nbuf.length * 2];
                            System.arraycopy(nbuf, 0, xbuf, 0, nbufptr);
                            nbuf = xbuf;
                        }
                        for (int j = 0; j < cr.length; j++) {
                            nbuf[nbufptr++] = cr[j];
                        }
                    } else {
                        // copy verbatim in binary mode.
                        nbuf[nbufptr++] = buf[i];
                    }
                    break;
                // all other characters are just copied
                default:
                    nbuf[nbufptr++] = buf[i];
                    break;
            }
        }
        xbuf = new byte[nbufptr];
        System.arraycopy(nbuf, 0, xbuf, 0, nbufptr);
        write(xbuf);
    }

    public int negotiate(byte nbuf[]) throws IOException {
        int count = tempbuf.length;
        byte[] buf = tempbuf;
        byte sendbuf[] = new byte[3];
        byte b, reply;
        int boffset = 0, noffset = 0;
        boolean dobreak = false;

        if (count == 0) // buffer is empty.
        {
            return -1;
        }
        while (!dobreak && (boffset < count) && (noffset < nbuf.length)) {
            b = buf[boffset++];
            // of course, byte is a signed entity (-128 -> 127)
            // but apparently the SGI Netscape 3.0 doesn't seem
            // to care and provides happily values up to 255
            if (b >= 128) {
                b = (byte) ((int) b - 256);
            }
            switch (neg_state) {
                case STATE_DATA:
                    if (b == IAC) {
                        byte bnext = buf[boffset];
                        if (bnext >= 128) {
                            bnext = (byte) ((int) bnext - 256);
                        }

                        if (bnext != -17) {
                            neg_state = STATE_IAC;
                            dobreak = true; // leave the loop so we can sync.

                        } else {
                            nbuf[noffset++] = b;
                        }

                    } else {
                        nbuf[noffset++] = b;
                    }
                    break;
                case STATE_IAC:
                    switch (b) {
                        case IAC:
                            neg_state = STATE_DATA;
                            nbuf[noffset++] = IAC;
                            break;
                        case WILL:
                            neg_state = STATE_IACWILL;
                            break;
                        case WONT:
                            neg_state = STATE_IACWONT;
                            break;
                        case DONT:
                            neg_state = STATE_IACDONT;
                            break;
                        case DO:
                            neg_state = STATE_IACDO;
                            break;
                        case EOR:
                            notifyEndOfRecord();
                            dobreak = true; // leave the loop so we can sync.

                            neg_state = STATE_DATA;
                            break;
                        case SB:
                            neg_state = STATE_IACSB;
                            break;
                        default:
                            neg_state = STATE_DATA;
                            break;
                    }
                    break;
                case STATE_IACWILL:
                    switch (b) {
                        case TELOPT_ECHO:
                            reply = DO;
                            setLocalEcho(false);
                            break;
                        case TELOPT_SGA:
                            reply = DO;
                            break;
                        case TELOPT_EOR:
                            reply = DO;
                            break;
                        case TELOPT_BINARY:
                            reply = DO;
                            break;
                        default:
                            reply = DONT;
                            break;
                    }
                    if (reply != sentDX[b + 128] || WILL != receivedWX[b + 128]) {
                        sendbuf[0] = IAC;
                        sendbuf[1] = reply;
                        sendbuf[2] = b;
                        write(sendbuf);
                        sentDX[b + 128] = reply;
                        receivedWX[b + 128] = WILL;
                    }
                    neg_state = STATE_DATA;
                    break;
                case STATE_IACWONT:
                    switch (b) {
                        case TELOPT_ECHO:
                            setLocalEcho(true);
                            reply = DONT;
                            break;
                        case TELOPT_SGA:
                            reply = DONT;
                            break;
                        case TELOPT_EOR:
                            reply = DONT;
                            break;
                        case TELOPT_BINARY:
                            reply = DONT;
                            break;
                        default:
                            reply = DONT;
                            break;
                    }
                    if (reply != sentDX[b + 128] || WONT != receivedWX[b + 128]) {
                        sendbuf[0] = IAC;
                        sendbuf[1] = reply;
                        sendbuf[2] = b;
                        write(sendbuf);
                        sentDX[b + 128] = reply;
                        receivedWX[b + 128] = WILL;
                    }
                    neg_state = STATE_DATA;
                    break;
                case STATE_IACDO:
                    switch (b) {
                        case TELOPT_ECHO:
                            reply = WILL;
                            setLocalEcho(true);
                            break;
                        case TELOPT_SGA:
                            reply = WILL;
                            break;
                        case TELOPT_TTYPE:
                            reply = WILL;
                            break;
                        case TELOPT_TANY:
                            reply = WILL;
                            break;
                        case TELOPT_BINARY:
                            reply = WILL;
                            break;
                        case TELOPT_NAWS:
                            Dimension size = getWindowSize();
                            receivedDX[b] = DO;
                            if (size == null) {
                                // this shouldn't happen
                                write(IAC);
                                write(WONT);
                                write(TELOPT_NAWS);
                                reply = WONT;
                                sentWX[b] = WONT;
                                break;
                            }
                            reply = WILL;
                            sentWX[b] = WILL;
                            sendbuf[0] = IAC;
                            sendbuf[1] = WILL;
                            sendbuf[2] = TELOPT_NAWS;
                            write(sendbuf);
                            write(IAC);
                            write(SB);
                            write(TELOPT_NAWS);
                            write((byte) (size.width >> 8));
                            write((byte) (size.width & 0xff));
                            write((byte) (size.height >> 8));
                            write((byte) (size.height & 0xff));
                            write(IAC);
                            write(SE);
                            break;
                        default:
                            reply = WONT;
                            break;
                    }
                    if (reply != sentWX[128 + b] || DO != receivedDX[128 + b]) {
                        if (b == TELOPT_TANY) {
                            byte[] newbuf = new byte[6];
                            newbuf[0] = IAC;
                            newbuf[1] = WILL;
                            newbuf[2] = b;
                            newbuf[3] = IAC;
                            newbuf[4] = DO;
                            newbuf[5] = b;
                            write(newbuf);
                        } else {
                            sendbuf[0] = IAC;
                            sendbuf[1] = reply;
                            sendbuf[2] = b;
                            write(sendbuf);
                        }
                        sentWX[b + 128] = reply;
                        receivedDX[b + 128] = DO;
                    }
                    neg_state = STATE_DATA;
                    break;
                case STATE_IACDONT:
                    switch (b) {
                        case TELOPT_ECHO:
                            reply = WONT;
                            setLocalEcho(false);
                            break;
                        case TELOPT_SGA:
                            reply = WONT;
                            break;
                        case TELOPT_NAWS:
                            reply = WONT;
                            break;
                        case TELOPT_BINARY:
                            reply = WONT;
                            break;
                        default:
                            reply = WONT;
                            break;
                    }
                    if (reply != sentWX[b + 128] || DONT != receivedDX[b + 128]) {
                        write(IAC);
                        write(reply);
                        write(b);
                        sentWX[b + 128] = reply;
                        receivedDX[b + 128] = DONT;
                    }
                    neg_state = STATE_DATA;
                    break;
                case STATE_IACSBIAC:
                    if (b == IAC) {
                        sbbuf = new byte[0];
                        current_sb = b;
                        neg_state = STATE_IACSBDATA;
                    } else {
                        System.err.println("(bad) " + b + " ");
                        neg_state = STATE_DATA;
                    }
                    break;
                case STATE_IACSB:
                    switch (b) {
                        case IAC:
                            neg_state = STATE_IACSBIAC;
                            break;
                        default:
                            current_sb = b;
                            sbbuf = new byte[0];
                            neg_state = STATE_IACSBDATA;
                            break;
                    }
                    break;
                case STATE_IACSBDATA:
                    switch (b) {
                        case IAC:
                            neg_state = STATE_IACSBDATAIAC;
                            break;
                        default:
                            byte[] xsb = new byte[sbbuf.length + 1];
                            System.arraycopy(sbbuf, 0, xsb, 0, sbbuf.length);
                            sbbuf = xsb;
                            sbbuf[sbbuf.length - 1] = b;
                            break;
                    }
                    break;
                case STATE_IACSBDATAIAC:
                    switch (b) {
                        case IAC:
                            neg_state = STATE_IACSBDATA;
                            byte[] xsb = new byte[sbbuf.length + 1];
                            System.arraycopy(sbbuf, 0, xsb, 0, sbbuf.length);
                            sbbuf = xsb;
                            sbbuf[sbbuf.length - 1] = IAC;
                            break;
                        case SE:
                            handle_sb(current_sb, sbbuf);
                            current_sb = 0;
                            neg_state = STATE_DATA;
                            break;
                        case SB:
                            handle_sb(current_sb, sbbuf);
                            neg_state = STATE_IACSB;
                            break;
                        default:
                            neg_state = STATE_DATA;
                            break;
                    }
                    break;
                default:
                    neg_state = STATE_DATA;
                    break;
            }
        }
        // shrink tempbuf to new processed size.
        byte[] xb = new byte[count - boffset];
        System.arraycopy(tempbuf, boffset, xb, 0, count - boffset);
        tempbuf = xb;
        return noffset;
    }

    public void inputfeed(byte[] b, int len) {
        byte[] xb = new byte[tempbuf.length + len];

        System.arraycopy(tempbuf, 0, xb, 0, tempbuf.length);
        System.arraycopy(b, 0, xb, tempbuf.length, len);
        tempbuf = xb;
    }
}
