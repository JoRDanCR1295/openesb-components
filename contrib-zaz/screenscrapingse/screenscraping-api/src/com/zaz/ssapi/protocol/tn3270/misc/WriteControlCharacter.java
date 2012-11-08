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
package com.zaz.ssapi.protocol.tn3270.misc;

import com.zaz.ssapi.protocol.tn3270.Session3270;
import com.zaz.ssapi.protocol.tn3270.model.Field;
import com.zaz.ssapi.protocol.tn3270.model.WCC;

import java.util.Iterator;
import java.util.List;

/**
 *
 * @author liyunhai
 */
public class WriteControlCharacter {

    public static void processWCC(Session3270 session, byte wcc) {
        WCC writeCC = session.getWcc();

        String padZero = "00000000";
        String wccs = Integer.toBinaryString(Integer.valueOf(wcc & 0xFF).intValue());
        wccs = padZero.substring(0, padZero.length() - wccs.length()) + wccs;

        if ("1".equals(wccs.substring(1, 2))) {
            writeCC.setReset(true);
        }

        if ("1".equals(wccs.substring(5, 6))) {
            writeCC.setAlarm(true);
        }

        if ("1".equals(wccs.substring(6, 7))) {
            writeCC.setUnlockKeyboard(true);
        }

        if ("1".equals(wccs.substring(7))) {
            writeCC.setResetModifiedDataTag(true);

            List<Field> fieldList = session.getField();
            Iterator it = fieldList.iterator();

            while (it.hasNext()) {
                Field field = (Field) it.next();
                field.setModifiedDataTag(false);
            }
        }
    }

    public static boolean checkKeyboardUnlockState(byte wcc) {
        String padZero = "00000000";
        String wccs = Integer.toBinaryString(Integer.valueOf(wcc & 0xFF).intValue());
        wccs = padZero.substring(0, padZero.length() - wccs.length()) + wccs;

        if ("1".equals(wccs.substring(6, 7))) {
            return true;
        }

        return false;
    }
}
