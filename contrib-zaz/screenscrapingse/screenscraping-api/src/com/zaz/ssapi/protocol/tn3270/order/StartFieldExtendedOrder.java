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
package com.zaz.ssapi.protocol.tn3270.order;

import com.zaz.ssapi.protocol.tn3270.Session3270;
import com.zaz.ssapi.protocol.tn3270.misc.FieldAttribute;
import com.zaz.ssapi.protocol.tn3270.model.Field;

import java.awt.Color;

import java.util.List;

/**
 *
 * @author liyunhai
 */
public class StartFieldExtendedOrder {

    public static final byte ORDER_ID = 0x29;

    public static int processOrder(Session3270 session, byte[] buf, int bufPos) {
        int ret = 0;
        int offset = session.getBufferAddr();
        List<Field> fieldList = session.getField();

        Field field = new Field();
        field.setStartPos(offset);
        if (session.isFieldStartPos(offset)) {
            fieldList.remove(session.getFieldFromScreenOffset(offset));
        }
        fieldList.add(field);

        session.getDeviceBuffer()[offset] = 0x40;
        if (offset == 1919) {
            session.setBufferAddr(0);
        } else {
            session.setBufferAddr(offset + 1);
        }

        int pairs = buf[bufPos + 1];
        ret = (pairs + 1) * 2;
        for (int i = 1; i <= pairs; i++) {
            int j = 2 * i;
            if (buf[bufPos + j] == 0xC0) {
                byte fa = buf[bufPos + j + 1];
                FieldAttribute.processFA(session, fa);
            } else if (buf[bufPos + j] == 0x41) {
                switch ((int) buf[bufPos + j + 1] & 0xFF) {
                    case 0xF1:
                        field.getExtField().setHighLighting(0xF1);
                        break;
                    case 0xF2:
                        field.getExtField().setHighLighting(0xF2);
                        break;
                    case 0xF4:
                        field.getExtField().setHighLighting(0xF4);
                        break;
                }
            } else if (buf[bufPos + j] == 0x42) {
                switch ((int) buf[bufPos + j + 1] & 0xFF) {
                    case 0xF1:
                        field.getExtField().setColor(Color.blue);
                        break;
                    case 0xF2:
                        field.getExtField().setColor(Color.red);
                        break;
                    case 0xF3:
                        field.getExtField().setColor(Color.pink);
                        break;
                    case 0xF4:
                        field.getExtField().setColor(Color.green);
                        break;
                    case 0xF5:
                        field.getExtField().setColor(Color.cyan);
                        break;
                    case 0xF6:
                        field.getExtField().setColor(Color.yellow);
                        break;
                    case 0xF7:
                        field.getExtField().setColor(Color.white);
                        break;
                }
            }
        }

        return ret;
    }
}
