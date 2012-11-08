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
package com.zaz.ssapi.protocol.vt100;

import com.zaz.ssapi.protocol.common.model.CustomizeOutputField;
import com.zaz.ssapi.protocol.common.model.CustomizeOutputInfo;
import com.zaz.ssapi.protocol.common.telnet.TelnetWrapper;
import com.zaz.ssapi.protocol.common.util.EncodeConvertUtil;
import com.zaz.ssapi.protocol.vt100.model.Field;
import com.zaz.ssapi.protocol.vt100.model.jaxb.Emulatorflow;
import java.io.IOException;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author liyunhai
 */
public class ProtocolHandlerVT100 {

    private static final int MAX_READ_DATA_STREAM = 10240;
    private static final int MAX_WRITE_DATA_STREAM = 10240;
    private TelnetWrapper telnet = new TelnetWrapper();
    private SessionVT100 session = new SessionVT100();
    private Emulatorflow emulatorflow = new Emulatorflow();
    private CustomizeOutputInfo cusOutputInfo = new CustomizeOutputInfo();
    private List<byte[]> screenBackup = new ArrayList<byte[]>();
    private byte[] readBuffer = new byte[MAX_READ_DATA_STREAM];
    private byte[] tempBuffer = new byte[MAX_READ_DATA_STREAM];
    private byte[] writeBuffer = new byte[MAX_WRITE_DATA_STREAM];
    private int outputScreenIndex = 0;
    private int paramIndex = 0;
    private int allScreenIndex = 0;

    public Emulatorflow getEmulatorflow() {
        return emulatorflow;
    }

    public SessionVT100 getSession() {
        return session;
    }

    public void setCusOutputInfo(CustomizeOutputInfo cusOutputInfo) {
        this.cusOutputInfo = cusOutputInfo;
    }

    public CustomizeOutputInfo getCusOutputInfo() {
        return cusOutputInfo;
    }

    public List<byte[]> getScreenBackup() {
        return screenBackup;
    }

    public byte[] getWriteBuffer() {
        return writeBuffer;
    }

    public int getOutputScreenIndex() {
        return outputScreenIndex;
    }

    public int getAllScreenIndex() {
        return allScreenIndex;
    }

    public void increaseAllScreenIndex() {
        allScreenIndex++;
    }

    public void setParamIndex(int paramIndex) {
        this.paramIndex = paramIndex;
    }

    public void increaseOutputScreenIndex() {
        outputScreenIndex++;
    }

    public void setOutputScreenIndex(int outputScreenIndex) {
        this.outputScreenIndex = outputScreenIndex;
    }

    public void connect(String host, int port) {
        try {
            this.emulatorflow.setHost(host);
            this.emulatorflow.setPort(port);
            telnet.connect(host, port);
            session.setFirstTrasFlag(true);
            session.setConnectFlag(true);
        } catch (IOException ex) {
            Logger.getLogger(SessionVT100.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    public void disconnect() {
        try {
            telnet.disconnect();
            session.setConnectFlag(false);
        } catch (IOException ex) {
            Logger.getLogger(SessionVT100.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    public int read() {
        try {
            resetReadBuffer();
            resetTempBuffer();
            int m = 0;
            while (true) {
                int i = telnet.read(tempBuffer);
                if (i == -1) {
                    disconnect();
                    clearAll();
                    return -1;
                }
                int j = 0;
                while (j < i) {
                    if (tempBuffer[j] == 0x1b) {
                        while (tempBuffer[j] != 0x6d) {
                            j++;
                        }
                        j++;
                    } else {
                        readBuffer[m++] = tempBuffer[j++];
                    }
                }
                if (checkLogoutMsg(readBuffer)) {
                    disconnect();
                    break;
                }
                if (m >= 2 && (readBuffer[m - 2] != 0x0d || readBuffer[m - 1] != 0x0a)) {
                    break;
                }
            }
            System.out.println("Print out ReadBufferData: ");
            System.out.println(EncodeConvertUtil.getASCIIStringFrombyte(readBuffer));

        } catch (IOException ex) {
            Logger.getLogger(SessionVT100.class.getName()).log(Level.SEVERE, null, ex);
        }

        return 0;
    }

    public void write() {
        try {
            int i = 0;

            while (writeBuffer[i] != 0x00) {
                i++;
            }

            byte[] buffer = new byte[i + 1];
            System.arraycopy(writeBuffer, 0, buffer, 0, i);
            buffer[i] = 0x0d;
            telnet.write(buffer);
            session.setPreAddr(session.getOutputAddr());
            session.setPreBufferAddr(session.getBufferAddr());
            System.out.println("Print out writeBuffer data: ");
            System.out.println(EncodeConvertUtil.getASCIIStringFrombyte(buffer));

            resetWriteBuffer();
            increaseAllScreenIndex();

        } catch (IOException ex) {
            Logger.getLogger(SessionVT100.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    public void backupScreen() {
        byte[] screen = session.getBytesFromBuffer();
        screenBackup.add(screen);
    }

    public void parseDataStream() {
        int i = 0;
        int size = readBuffer.length;
        while (readBuffer[i] != 0x00 && i < size) {
            i++;
        }
        i = 0;
        if (session.getBufferAddr() + i > SessionVT100.DEVICE_BUFFER_SIZE) {
            session.clear();
            if (!session.isFirstTrasFlag()) {
                while (readBuffer[i] != 0x00 && (readBuffer[i] != 0x0d || readBuffer[i + 1] != 0x0a)) {
                    i++;
                }
                i = i + 2;
                session.setPreAddr(-80);
            }
        }
        while (i < size && readBuffer[i] != 0x00) {
            byte[] dBuffer = session.getDeviceBuffer();
            int offset = session.getBufferAddr();
            dBuffer[offset] = readBuffer[i];
            session.setBufferAddr(offset + 1);
            session.setCursorAddr(offset + 1);
            i++;
        }
        FormatOutputBuffer();
        printScreen();
        session.fieldClear();
    }

    public void assembleDataStream() {
        if (session.isFirstTrasFlag()) {
            session.setFirstTrasFlag(false);
        }
        System.arraycopy(session.getDeviceBuffer(), session.getBufferAddr(), writeBuffer, 0,
                session.getCursorAddr() - session.getBufferAddr());
        Emulatorflow.Screen screen = new Emulatorflow.Screen();
        Map<Integer, List<CustomizeOutputField>> mapInfo = cusOutputInfo.getOutputInfo();
        screen.setOutput(false);
        for (int i = 0; i < mapInfo.size(); i++) {
            List<CustomizeOutputField> list = mapInfo.get(Integer.valueOf(i));
            if (list != null && list.size() > 0) {
                CustomizeOutputField field = list.get(0);
                if (field.getAllScreenIndex() == allScreenIndex) {
                    screen.setOutput(true);
                }
            }
        }
        if (session.isModifiedFieldFlag()) {
            Field field = session.getField();
            if (field != null) {
                Emulatorflow.Screen.Field xsdField = new Emulatorflow.Screen.Field();
                String tempValue = EncodeConvertUtil.getStringFromASCIIByte(writeBuffer);
                xsdField.setValue(tempValue);

                if (field.isRecordParamFlag()) {
                    xsdField.setIsparam(true);
                    xsdField.setComment(field.getRecordComment());
                    String paramName = field.getRecordParamName();
                    if ("param".equals(paramName)) {
                        paramName = String.format("param%d", paramIndex++);
                    }
                    xsdField.setParaname(paramName);
                } else {
                    xsdField.setIsparam(false);
                    xsdField.setNewvalue(field.getRecordNewValue());
                }
                screen.setField(xsdField);
            }
        }
        this.emulatorflow.getScreen().add(screen);
    }

    private boolean checkLogoutMsg(byte[] buf) {
        boolean checkFlg = true;

        byte[] logoutBuf = {0x6c, 0x6f, 0x67, 0x6f, 0x75, 0x74, 0x0d, 0x0a};
        byte[] exitBuf = {0x65, 0x78, 0x69, 0x74, 0x0d, 0x0a};
        if (buf.length < exitBuf.length) {
            return false;
        }
        for (int i = 0; i < exitBuf.length; i++) {
            if (buf[i] != exitBuf[i]) {
                checkFlg = false;
                break;
            }
        }
        if (checkFlg) {
            return true;
        }
        checkFlg = true;
        if (buf.length < logoutBuf.length) {
            return false;
        }
        for (int i = 0; i < logoutBuf.length; i++) {
            if (buf[i] != logoutBuf[i]) {
                checkFlg = false;
                break;
            }
        }
        return checkFlg;
    }

    private void resetReadBuffer() {
        for (int i = 0; i < MAX_READ_DATA_STREAM; i++) {
            readBuffer[i] = 0x00;
        }
    }

    private void resetTempBuffer() {
        for (int i = 0; i < MAX_READ_DATA_STREAM; i++) {
            tempBuffer[i] = 0x00;
        }
    }

    private void resetWriteBuffer() {
        for (int i = 0; i < MAX_WRITE_DATA_STREAM; i++) {
            writeBuffer[i] = 0x00;
        }
    }

    private void printScreen() {
        System.out.println("Print out data in device buffer: ");

        for (int i = 0; i <= session.getOutputAddr()/ 80; i++) {
            System.out.println(EncodeConvertUtil.getStringFromASCIIByte(
                    session.getOutputBytesFromBuffer(i * 80, 80)));
        }
    }

    private void FormatOutputBuffer() {
        byte[] buf = new byte[80 * 1000];
        for (int n = 0; n < buf.length; n++) {
            buf[n] = 0x00;
        }
        int i = 0;
        int j = 0;
        int rows = 0;
        byte[] bytes = session.getDeviceBuffer();
        while (true) {
            int length = i - j;
            if (bytes[i] == 0x0d && bytes[i + 1] == 0x0a) {
                if (length % 80 == 0) {
                    System.arraycopy(bytes, j, buf, rows * 80, length);
                    rows = rows + length / 80;
                } else {
                    byte[] temp = new byte[length + 80 - length % 80];
                    System.arraycopy(bytes, j, temp, 0, length);
                    byteFormat(temp, length);
                    System.arraycopy(temp, 0, buf, rows * 80, temp.length);
                    rows = rows + length / 80 + 1;
                }
                j = i + 2;
                i++;
            }
            if (bytes[i] == 0x00) {
                System.arraycopy(bytes, j, buf, rows * 80, length);
                session.setOutputAddr(rows * 80 + length);
                break;
            }
            i++;
        }
//        for (i = 1; i < rows; i++) {
//            buf[i * 80 - 1] = 0x20;
//        }
        session.setOutputBuffer(buf);
    }

    private void byteFormat(byte[] temp, int length) {
        for (int i = length; i < temp.length; i++) {
            temp[i] = 0x20;
        }
    }

    private void clearAll() {
        session.clear();
        cusOutputInfo.clear();
        screenBackup.clear();
        outputScreenIndex = 0;
        paramIndex = 0;
    }
}
