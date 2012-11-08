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

import com.zaz.ssapi.protocol.vt100.model.Field;

/**
 *
 * @author xliu
 */
public class SessionVT100 {

    public static final int DEVICE_BUFFER_SIZE = 80 * 1000;
    private byte[] deviceBuffer = new byte[DEVICE_BUFFER_SIZE];
    private byte[] outputBuffer = new byte[DEVICE_BUFFER_SIZE];
    private Field field = new Field();
    private int preAddr = 0;
    private int bufferAddr = 0;
    private int preBufferAddr = 0;
    private int cursorAddr = 0;
    private int outputAddr = 0;
    private boolean modifiedFieldFlag = false;
    private boolean firstTrasFlag = false;
    private boolean connectFlag = false;

    public Field getField() {
        return field;
    }

    public boolean isFirstTrasFlag() {
        return firstTrasFlag;
    }

    public void setFirstTrasFlag(boolean firstTrasFlag) {
        this.firstTrasFlag = firstTrasFlag;
    }

    public void setField(Field field) {
        this.field = field;
    }

    public int getPreAddr() {
        return preAddr;
    }

    public void setPreAddr(int preAddr) {
        this.preAddr = preAddr;
    }

    public int getPreBufferAddr() {
        return preBufferAddr;
    }

    public void setPreBufferAddr(int preBufferAddr) {
        this.preBufferAddr = preBufferAddr;
    }

    public int getOutputAddr() {
        return outputAddr;
    }

    public boolean isConnectFlag() {
        return connectFlag;
    }

    public void setConnectFlag(boolean connectFlag) {
        this.connectFlag = connectFlag;
    }

    public void setOutputAddr(int outputAddr) {
        this.outputAddr = outputAddr;
    }

    public byte[] getOutputBuffer() {
        return outputBuffer;
    }

    public void setOutputBuffer(byte[] outputBuffer) {
        this.outputBuffer = outputBuffer;
    }

    public int getCursorAddr() {
        return cursorAddr;
    }

    public void setCursorAddr(int cursorAddr) {
        this.cursorAddr = cursorAddr;
    }

    public byte[] getDeviceBuffer() {
        return deviceBuffer;
    }

    public void setDeviceBuffer(byte[] deviceBuffer) {
        this.deviceBuffer = deviceBuffer;
    }

    public boolean isModifiedFieldFlag() {
        return modifiedFieldFlag;
    }

    public void setModifiedFieldFlag(boolean modifiedFieldFlag) {
        this.modifiedFieldFlag = modifiedFieldFlag;
    }

    public byte[] getBytesFromBuffer() {
        if (firstTrasFlag) {
            return getOutputBytesFromBuffer(preAddr / 80 * 80, outputAddr);
        } else {
            return getOutputBytesFromBuffer((preAddr / 80 + 1) * 80, outputAddr);
        }
    }

    public byte[] getOutputBytesFromBuffer(int startPos, int endPos) {
        byte[] ret = new byte[endPos];
        for (int i = 0; i < endPos; i++) {
            ret[i] = outputBuffer[startPos++];
        }
        return ret;
    }

    public byte[] getPresentBuffer() {
        int startPos = preBufferAddr;
        int endPos = cursorAddr;
        byte[] ret = new byte[endPos - startPos];
        for (int i = 0; i < cursorAddr - preBufferAddr; i++) {
            ret[i] = deviceBuffer[startPos++];
        }
        return ret;
    }

    public int getBufferAddr() {
        return bufferAddr;
    }

    public void setBufferAddr(int bufferAddr) {
        this.bufferAddr = bufferAddr;
    }

    public void clear() {
        preAddr = 0;
        bufferAddr = 0;
        cursorAddr = 0;
        preBufferAddr = 0;
        deviceBufferClear();
        fieldClear();
    }

    public void deviceBufferClear() {
        for (int i = 0; i < DEVICE_BUFFER_SIZE; i++) {
            deviceBuffer[i] = 0x00;
        }
    }

    public void fieldClear() {
        modifiedFieldFlag = false;
        field = new Field();
    }
}
