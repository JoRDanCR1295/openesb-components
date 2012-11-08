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

import com.zaz.ssapi.protocol.tn3270.model.Field;
import com.zaz.ssapi.protocol.tn3270.model.FieldComparator;
import com.zaz.ssapi.protocol.tn3270.model.WCC;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 *
 * @author liyunhai
 */
public class Session3270 {

    public static final int DEVICE_BUFFER_SIZE = 1920;
    private byte[] deviceBuffer = new byte[DEVICE_BUFFER_SIZE];
    private List<Field> field = new ArrayList<Field>();
    private WCC wcc = new WCC();
    private int bufferAddr = 0;
    private int cursorAddr = 0;
    private int action = 0;

    public List<Field> getField() {
        return field;
    }

    public void setField(List<Field> field) {
        this.field = field;
    }

    public WCC getWcc() {
        return wcc;
    }

    public void setWcc(WCC wcc) {
        this.wcc = wcc;
    }

    public int getBufferAddr() {
        return bufferAddr;
    }

    public void setBufferAddr(int bufferAddr) {
        this.bufferAddr = bufferAddr;
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

    public int getAction() {
        return action;
    }

    public void setAction(int action) {
        this.action = action;
    }

    public Field getFieldFromScreenOffset(int offset) {
//        for (Iterator i = field.iterator(); i.hasNext();) {
//            Field tfield = (Field) i.next();
//            if (offset >= tfield.getStartPos() &&
//                    offset <= tfield.getStartPos() + tfield.getLength() - 1) {
//                return tfield;
//            }
//        }
        sortFieldList();
        setFieldLength();

        for (int i = 0; i < field.size(); i++) {
            Field tfield = field.get(i);
            if (offset >= tfield.getStartPos() &&
                    offset < tfield.getStartPos() + tfield.getLength()) {
                return tfield;
            }

            if (i == field.size() - 1) {
                int startPos = (field.get(0)).getStartPos();
                if (startPos != 0 && offset < startPos) {
                    return tfield;
                }
            }
        }

        return null;
    }

    public int getFieldOffsetFromScreenOffset(int offset) {
        Field tfield = getFieldFromScreenOffset(offset);

        if (tfield == null) {
            return -1;
        }

        if (field.indexOf(tfield) != field.size() - 1) {
            return offset - tfield.getStartPos();
        } else {
            if (offset >= tfield.getStartPos()) {
                return offset - tfield.getStartPos();
            } else {
                return offset + (1920 - tfield.getStartPos());
            }
        }
    }

    public int getFieldIndexFromScreenOffset(int offset) {
        Field tfield = getFieldFromScreenOffset(offset);

        if (tfield == null) {
            return -1;
        }

        return field.indexOf(tfield);
    }

    public int getModifiedFieldCount() {
        int count = 0;
        for (int i = 0; i < field.size(); i++) {
            Field tfield = field.get(i);
            if (!tfield.isFProtected() && tfield.isModifiedDataTag()) {
                count++;
            }
        }

        return count;
    }

    public Field getModifiedField(int index) {
        for (int i = 0; i < field.size(); i++) {
            Field tfield = field.get(i);
            if (!tfield.isFProtected() && tfield.isModifiedDataTag()) {
                index--;
            }
            if (index < 0) {
                return tfield;
            }
        }

        return null;
    }

    public boolean isFieldStartPos(int offset) {
        for (Iterator i = field.iterator(); i.hasNext();) {
            Field tfield = (Field) i.next();
            if (offset == tfield.getStartPos()) {
                return true;
            }
        }

        return false;
    }

    public void setFieldLength() {
        if (field == null || field.size() == 0) {
            return;
        }

        int size = field.size();
        Field curField = field.get(0);
        Field nextField = null;

        if (size == 1) {
            curField.setLength(1920 - curField.getStartPos());
            return;
        }

        for (int i = 1; i < size; i++) {
            nextField = field.get(i);
            curField.setLength(nextField.getStartPos() - curField.getStartPos());
            curField = nextField;
        }

        nextField.setLength(1920 - nextField.getStartPos() +
                (field.get(0)).getStartPos());
    }

    public void sortFieldList() {
        Collections.sort(field, new FieldComparator());
    }

    public byte[] getBytesFromBuffer(int startPos, int length) {
        byte[] ret = new byte[length];
        for (int i = 0; i < length; i++) {
            ret[i] = deviceBuffer[startPos++];
        }
        return ret;
    }

    public void clear() {
        field.clear();
        wcc.reset();
        bufferAddr = 0;
        cursorAddr = 0;
        action = 0;
        for (int i = 0; i < DEVICE_BUFFER_SIZE; i++) {
            deviceBuffer[i] = 0x40;
        }
    }
}
