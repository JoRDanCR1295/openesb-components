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
package com.zaz.ssapi.protocol.tn3270;

import com.zaz.ssapi.protocol.common.model.CustomizeOutputField;
import com.zaz.ssapi.protocol.common.model.CustomizeOutputInfo;
import com.zaz.ssapi.protocol.common.telnet.TelnetWrapper;
import com.zaz.ssapi.protocol.common.util.EncodeConvertUtil;
import com.zaz.ssapi.protocol.tn3270.command.DefaultCommand;
import com.zaz.ssapi.protocol.tn3270.command.EraseAllUnprotectedCommand;
import com.zaz.ssapi.protocol.tn3270.command.EraseWriteAlternateCommand;
import com.zaz.ssapi.protocol.tn3270.command.EraseWriteCommand;
import com.zaz.ssapi.protocol.tn3270.command.ReadBufferCommand;
import com.zaz.ssapi.protocol.tn3270.command.ReadModifiedAllCommand;
import com.zaz.ssapi.protocol.tn3270.command.ReadModifiedCommand;
import com.zaz.ssapi.protocol.tn3270.command.WriteCommand;
import com.zaz.ssapi.protocol.tn3270.command.WriteStructuredFieldCommand;
import com.zaz.ssapi.protocol.tn3270.misc.WriteControlCharacter;
import com.zaz.ssapi.protocol.tn3270.model.Field;
import com.zaz.ssapi.protocol.tn3270.model.jaxb.Emulatorflow;
import com.zaz.ssapi.protocol.tn3270.order.DefaultOrder;
import com.zaz.ssapi.protocol.tn3270.order.EraseUnprotectedToAddressOrder;
import com.zaz.ssapi.protocol.tn3270.order.GraphicEscapeOrder;
import com.zaz.ssapi.protocol.tn3270.order.InsertCursorOrder;
import com.zaz.ssapi.protocol.tn3270.order.ModifyFieldOrder;
import com.zaz.ssapi.protocol.tn3270.order.ProgramTabOrder;
import com.zaz.ssapi.protocol.tn3270.order.RepeatToAddressOrder;
import com.zaz.ssapi.protocol.tn3270.order.SetAttributeOrder;
import com.zaz.ssapi.protocol.tn3270.order.SetBufferAddressOrder;
import com.zaz.ssapi.protocol.tn3270.order.StartFieldExtendedOrder;
import com.zaz.ssapi.protocol.tn3270.order.StartFieldOrder;
import com.zaz.ssapi.protocol.tn3270.util.AddressConvertUtil;
import com.zaz.ssapi.protocol.tn3270.util.EmulatorRubyRecord;

import java.io.IOException;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author liyunhai
 */
public class ProtocolHandler3270 {

    private static final int MAX_READ_DATA_STREAM = 4096;
    private static final int MAX_WRITE_DATA_STREAM = 2048;
    private TelnetWrapper telnet = new TelnetWrapper();
    private Session3270 session = new Session3270();
    private Emulatorflow emulatorflow = new Emulatorflow();
    private CustomizeOutputInfo cusOutputInfo = new CustomizeOutputInfo();
    private List<byte[]> screenBackup = new ArrayList<byte[]>();
    private byte[] readBuffer = new byte[MAX_READ_DATA_STREAM];
    private byte[] tempBuffer = new byte[MAX_READ_DATA_STREAM];
    private byte[] writeBuffer = new byte[MAX_WRITE_DATA_STREAM];
    private int outputScreenIndex = 0;
    private int tempPos = MAX_READ_DATA_STREAM - 1;
    private int tempLen = 0;
    private boolean changeFlag = false;
    private int paramIndex = 0;
    private int allScreenIndex = 0;

    public Emulatorflow getEmulatorflow() {
        return emulatorflow;
    }

    public Session3270 getSession() {
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

    public void connect(String host, int port) throws IOException {
        //try {
            this.emulatorflow.setHost(host);
            this.emulatorflow.setPort(port);
            telnet.connect(host, port);
            /**
        } catch (IOException ex) {
            Logger.getLogger(Session3270.class.getName()).log(Level.SEVERE, null, ex);
        }
             * **/
    }

    public void disconnect() {
        try {
            telnet.disconnect();
        } catch (IOException ex) {
            Logger.getLogger(Session3270.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    private boolean checkKeyboardUnlockState(byte[] data) {
        int i = 0;
        int size = data.length;
        while (data[i] != 0x00 && i < size) {
            if (WriteControlCharacter.checkKeyboardUnlockState(data[i + 1])) {
                return true;
            }
            i += 2;

            while ((data[i] & 0xFF) != 0xFF ||
                    (data[i + 1] & 0xFF) != 0xEF) {
                i++;
            }
            i += 2;
        }
        return false;
    }

    public int read() {
        try {
            resetReadBuffer();
            System.arraycopy(tempBuffer, tempPos, readBuffer, 0, tempLen);

            boolean endFlag = false;
            int bufferPos = tempLen;
            while (!endFlag) {
                resetTempBuffer();
                int i = telnet.read(tempBuffer);
                if (i == -1) {
                    disconnect();
                    clearAll();
                    return -1;
                }
                int pos = i - 1;
                for (; pos > 0; pos--) {
                    if (((tempBuffer[pos - 1] & 0xFF) == 0xFF) &&
                            ((tempBuffer[pos] & 0xFF) == 0xEF)) {
                        endFlag = true;
                        break;
                    }
                }

                if (endFlag) {
                    System.arraycopy(tempBuffer, 0, readBuffer, bufferPos,
                            pos + 1);
                    tempPos = pos + 1;
                    tempLen = i - pos - 1;
                    bufferPos += (pos + 1);
                    if (!checkKeyboardUnlockState(readBuffer)) {
                        System.arraycopy(tempBuffer, tempPos, readBuffer, bufferPos,
                                tempLen);
                        bufferPos += tempLen;
                        endFlag = false;
                    } else {
                        if (((readBuffer[8] & 0xFF) == 0xFF) &&
                                ((readBuffer[9] & 0xFF) == 0xEF) &&
                                ((readBuffer[10] & 0xFF) == 0x00) &&
                                ((readBuffer[11] & 0xFF) == 0x00)) {
                            endFlag = false;
                            continue;
                        } else if (((readBuffer[8] & 0xFF) == 0xFF) &&
                                ((readBuffer[9] & 0xFF) == 0xEF) &&
                                ((readBuffer[10] & 0xFF) == 0x05) &&
                                ((readBuffer[23] & 0xFF) == 0x00) &&
                                ((readBuffer[24] & 0xFF) == 0x00)) {
                            System.arraycopy(tempBuffer, tempPos, readBuffer, bufferPos,
                                    tempLen);
                            bufferPos += tempLen;
                            endFlag = false;
                        }
                    }
                } else {
                    System.arraycopy(tempBuffer, 0, readBuffer, bufferPos,
                            i);
                    bufferPos += i;
                }
            }

            System.out.println("Print out ReadBufferData: ");
            System.out.println(EncodeConvertUtil.getHexStringFrombyte(readBuffer));
        } catch (IOException ex) {
            Logger.getLogger(Session3270.class.getName()).log(Level.SEVERE, null, ex);
        }

//        EmulatorRubyRecord.recordRead();

        return 0;
    }

    public void backupScreen(boolean pageDown) {
        byte[] screen = new byte[Session3270.DEVICE_BUFFER_SIZE];
        System.arraycopy(session.getDeviceBuffer(), 0, screen, 0,
                Session3270.DEVICE_BUFFER_SIZE);

        if (pageDown) {
            byte[] lastScreen = screenBackup.get(screenBackup.size() - 1);
            byte[] newLastScreen = new byte[lastScreen.length +
                    Session3270.DEVICE_BUFFER_SIZE];
            System.arraycopy(lastScreen, 0, newLastScreen, 0, lastScreen.length);
            System.arraycopy(screen, 0, newLastScreen, lastScreen.length,
                    Session3270.DEVICE_BUFFER_SIZE);

            screenBackup.remove(screenBackup.size() - 1);
            screenBackup.add(newLastScreen);
        } else {
            screenBackup.add(screen);
        }
    }

    public void scrollScreen(boolean downUp) { //DOWN: true  UP:false

        if (EmulatorRubyRecord.isRecordFlag()) {
            changeFlag = true;
        }

        EmulatorRubyRecord.setRecordFlag(false);

        byte[] oldDeviceBuffer = new byte[Session3270.DEVICE_BUFFER_SIZE];
        boolean first = true;
        int allScreenIndexBak = allScreenIndex;

        do {
            if (downUp) {
                if (first) {
                    backupScreen(false);
                    first = false;
                } else {
                    backupScreen(true);
                }
            }

            System.arraycopy(session.getDeviceBuffer(), 0,
                    oldDeviceBuffer, 0, Session3270.DEVICE_BUFFER_SIZE);

            assemblePageDownDataStream(downUp);
            write();

            read();
            parseDataStream();
        } while (!deviceBufferCompare(session.getDeviceBuffer(), oldDeviceBuffer));

        allScreenIndex = allScreenIndexBak;

        if (changeFlag) {
            EmulatorRubyRecord.setRecordFlag(true);
            changeFlag = false;
        }
    }

    private boolean deviceBufferCompare(byte[] deviceBuffer, byte[] oldDeviceBuffer) {
        int len = Session3270.DEVICE_BUFFER_SIZE;
        for (int i = 0; i < len; i++) {
            if (deviceBuffer[i] != oldDeviceBuffer[i]) {
                return false;
            }
        }
        return true;
    }

    private void assemblePageDownDataStream(boolean downUp) {
        if (downUp) {
            writeBuffer[0] = (byte) 0xF8;
        } else {
            writeBuffer[0] = (byte) 0xF7;
        }

        byte[] screenAddr =
                AddressConvertUtil.getScreenAddressFromOffset(session.getCursorAddr());
        writeBuffer[1] = screenAddr[0];
        writeBuffer[2] = screenAddr[1];
//        writeBuffer[3] = (byte) 0x11;
//        writeBuffer[4] = (byte) 0xC2;
//        writeBuffer[5] = (byte) 0x5B;
//        writeBuffer[6] = (byte) 0xD7;
//        writeBuffer[7] = (byte) 0xC1;
//        writeBuffer[8] = (byte) 0xC7;
//        writeBuffer[9] = (byte) 0xC5;

        System.arraycopy(new byte[]{(byte) 0xFF, (byte) 0xEF}, 0, writeBuffer, 3, 2);
    }

    public void write() {
        try {
            int i = 0;
            while ((writeBuffer[i] & 0xFF) != 0xFF ||
                    (writeBuffer[i + 1] & 0xFF) != 0xEF) {
                i++;
            }

            byte[] buffer = new byte[i + 2];
            System.arraycopy(writeBuffer, 0, buffer, 0, i + 2);
            telnet.write(buffer);

            System.out.println("Print out writeBuffer data: ");
            System.out.println(EncodeConvertUtil.getHexStringFrombyte(writeBuffer));

            resetWriteBuffer();

            increaseAllScreenIndex();

//            EmulatorRubyRecord.recordWrite();
        } catch (IOException ex) {
            Logger.getLogger(Session3270.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    public void parseDataStream() {
        int i = 0;
        int size = readBuffer.length;

        while (readBuffer[i] != 0x00 && i < size) {
            callCommand(readBuffer[i]);
            i++;

            WriteControlCharacter.processWCC(session, readBuffer[i]);
            i++;

            while ((readBuffer[i] & 0xFF) != 0xFF ||
                    (readBuffer[i + 1] & 0xFF) != 0xEF) {
                if (isOrder(readBuffer[i])) {
                    i = i + callOrder(readBuffer[i], i);
                } else {
                    byte[] dBuffer = session.getDeviceBuffer();
                    int offset = session.getBufferAddr();
                    dBuffer[offset] = readBuffer[i];
                    if (offset == 1919) {
                        session.setBufferAddr(0);
                    } else {
                        session.setBufferAddr(offset + 1);
                    }
                    if (session.isFieldStartPos(offset)) {
                        session.getField().remove(session.getFieldFromScreenOffset(offset));
                    }
                    i++;
                }
            }
            i += 2;
        }

        replaceLineEnd();

        session.sortFieldList();
        session.setFieldLength();

        printScreen();
    //printField();
    }

    public void assembleDataStream() {
        writeBuffer[0] = (byte) session.getAction();
        //EmulatorRubyRecord.recordAction(session.getAction());


        byte[] screenAddr =
                AddressConvertUtil.getScreenAddressFromOffset(session.getCursorAddr());
        writeBuffer[1] = screenAddr[0];
        writeBuffer[2] = screenAddr[1];
        //EmulatorRubyRecord.recordCursorAddr(session.getCursorAddr());

        Emulatorflow.Screen screen = new Emulatorflow.Screen();
        screen.setAction(session.getAction());
        screen.setCursoraddress(session.getCursorAddr());
        Map<Integer, List<CustomizeOutputField>> mapInfo = cusOutputInfo.getOutputInfo();
        screen.setOutput(0);
        for (int i = 0; i < mapInfo.size(); i++) {
            List<CustomizeOutputField> list = mapInfo.get(Integer.valueOf(i));
            if (list != null && list.size() > 0) {
                CustomizeOutputField field = list.get(0);
                if (field.getAllScreenIndex() == allScreenIndex) {
                    screen.setOutput(field.getOutputType());
                }
            }
        }
        int bufPos = 3;

        //if (session.getAction() == 0x7D) {
        bufPos = assembleFieldData(bufPos, screen);
        //}
        this.emulatorflow.getScreen().add(screen);

        System.arraycopy(new byte[]{(byte) 0xFF, (byte) 0xEF}, 0, writeBuffer, bufPos, 2);
    }

    public int assembleFieldData(int bufPos, Emulatorflow.Screen screen) {
        List fields = session.getField();
        Iterator it = fields.iterator();
        while (it.hasNext()) {
            Field field = (Field) it.next();
            if (!field.isFProtected() && field.isModifiedDataTag()) {
                System.arraycopy(new byte[]{0x11}, 0, writeBuffer, bufPos, 1);
                bufPos++;

                byte[] addr = AddressConvertUtil.getScreenAddressFromOffset(
                        field.getStartPos() + 1);
                System.arraycopy(addr, 0, writeBuffer, bufPos, 2);
                bufPos += 2;
                String tempValue = "";
                int fIndex = session.getField().indexOf(field);
                if ((fIndex == session.getField().size() - 1) &&
                        (field.getStartPos() + field.getLength() > 1920)) {
                    byte[] fieldData = new byte[field.getLength() - 1];
                    System.arraycopy(session.getDeviceBuffer(), field.getStartPos() + 1,
                            fieldData, 0, 1920 - field.getStartPos() - 1);
                    System.arraycopy(session.getDeviceBuffer(), 0, fieldData,
                            1920 - field.getStartPos() - 1, session.getField().get(0).getStartPos());
                    byte[] trimData = trimByte(fieldData);
                    System.arraycopy(trimData, 0,
                            writeBuffer, bufPos, trimData.length);
                    bufPos += trimData.length;
                    tempValue = EncodeConvertUtil.getStringFromEBCDICByte(trimData).trim();
                //EmulatorRubyRecord.recordField(fIndex, trimData, field, outputScreenIndex);

                } else {
                    byte[] fieldData = new byte[field.getLength() - 1];
                    System.arraycopy(session.getDeviceBuffer(), field.getStartPos() + 1,
                            fieldData, 0, field.getLength() - 1);
                    byte[] trimData = trimByte(fieldData);
                    System.arraycopy(trimData, 0,
                            writeBuffer, bufPos, trimData.length);
                    bufPos += trimData.length;
                    tempValue = EncodeConvertUtil.getStringFromEBCDICByte(trimData).trim();
                //EmulatorRubyRecord.recordField(fIndex, trimData, field, outputScreenIndex);
                }
                Emulatorflow.Screen.Field xsdField = new Emulatorflow.Screen.Field();
                xsdField.setIndex(fIndex);
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
                screen.getField().add(xsdField);
            }
        }
        return bufPos;
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

    private void callCommand(byte cmdId) {
        switch (cmdId) {
            case WriteCommand.CMD_ID:
                WriteCommand.processCommand(session);
                break;
            case ReadBufferCommand.CMD_ID:
                ReadBufferCommand.processCommand(session);
                break;
            case WriteStructuredFieldCommand.CMD_ID:
                WriteStructuredFieldCommand.processCommand(session);
                break;
            case EraseWriteCommand.CMD_ID:
                EraseWriteCommand.processCommand(session);
                break;
            case ReadModifiedCommand.CMD_ID:
                ReadModifiedCommand.processCommand(session);
                break;
            case ReadModifiedAllCommand.CMD_ID:
                ReadModifiedAllCommand.processCommand(session);
                break;
            case EraseAllUnprotectedCommand.CMD_ID:
                EraseAllUnprotectedCommand.processCommand(session);
                break;
            case EraseWriteAlternateCommand.CMD_ID:
                EraseWriteAlternateCommand.processCommand(session);
                break;
            default:
                DefaultCommand.processCommand();
                break;
        }
    }

    private int callOrder(byte orderId, int bufPos) {
        int ret = 0;
        switch (orderId) {
            case ProgramTabOrder.ORDER_ID:
                ret = ProgramTabOrder.processOrder(session, readBuffer, bufPos);
                break;
            case GraphicEscapeOrder.ORDER_ID:
                ret = GraphicEscapeOrder.processOrder(session, readBuffer, bufPos);
                break;
            case SetBufferAddressOrder.ORDER_ID:
                ret = SetBufferAddressOrder.processOrder(
                        session, readBuffer, bufPos);
                break;
            case EraseUnprotectedToAddressOrder.ORDER_ID:
                ret = EraseUnprotectedToAddressOrder.processOrder(
                        session, readBuffer, bufPos);
                break;
            case InsertCursorOrder.ORDER_ID:
                ret = InsertCursorOrder.processOrder(session);
                break;
            case StartFieldOrder.ORDER_ID:
                ret = StartFieldOrder.processOrder(session, readBuffer, bufPos);
                break;
            case SetAttributeOrder.ORDER_ID:
                ret = SetAttributeOrder.processOrder(session, readBuffer, bufPos);
                break;
            case StartFieldExtendedOrder.ORDER_ID:
                ret = StartFieldExtendedOrder.processOrder(
                        session, readBuffer, bufPos);
                break;
            case ModifyFieldOrder.ORDER_ID:
                ret = ModifyFieldOrder.processOrder(session, readBuffer, bufPos);
                break;
            case RepeatToAddressOrder.ORDER_ID:
                ret = RepeatToAddressOrder.processOrder(session, readBuffer, bufPos);
                break;
            default:
                DefaultOrder.processOrder();
                break;
        }

        return ret;
    }

    private boolean isOrder(byte orderId) {
        if ((orderId == ProgramTabOrder.ORDER_ID) ||
                (orderId == GraphicEscapeOrder.ORDER_ID) ||
                (orderId == SetBufferAddressOrder.ORDER_ID) ||
                (orderId == EraseUnprotectedToAddressOrder.ORDER_ID) ||
                (orderId == InsertCursorOrder.ORDER_ID) ||
                (orderId == StartFieldOrder.ORDER_ID) ||
                (orderId == SetAttributeOrder.ORDER_ID) ||
                (orderId == StartFieldExtendedOrder.ORDER_ID) ||
                (orderId == ModifyFieldOrder.ORDER_ID) ||
                (orderId == RepeatToAddressOrder.ORDER_ID)) {
            return true;
        }

        return false;
    }

    private void printScreen() {
        System.out.println("Print out data in device buffer: ");
        for (int i = 0; i < 24; i++) {
            System.out.println(EncodeConvertUtil.getStringFromEBCDICByte(
                    session.getBytesFromBuffer(i * 80, 80)));
        }
    }

    private void replaceLineEnd() {
        byte[] buffer = session.getDeviceBuffer();
        for (int j = 0; j < 24; j++) {
            buffer[j * 80 + 79] = 0x40;
        }
    }

    private byte[] trimByte(byte[] src) {
        int i = src.length - 1;
        for (; i >= 0; i--) {
            if (src[i] != 0x40 && src[i] != 0x00) {
                break;
            }
        }

        byte[] ret = new byte[i + 1];
        System.arraycopy(src, 0, ret, 0, i + 1);
        return ret;
    }

    private void clearAll() {
        session.clear();
        cusOutputInfo.clear();
        screenBackup.clear();
        outputScreenIndex = 0;
        paramIndex = 0;
        tempPos = MAX_READ_DATA_STREAM - 1;
        tempLen = 0;
    }
}
