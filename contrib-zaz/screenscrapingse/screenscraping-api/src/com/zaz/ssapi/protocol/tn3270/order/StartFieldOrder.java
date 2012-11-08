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

import java.util.List;

/**
 *
 * @author liyunhai
 */
public class StartFieldOrder {

    public static final byte ORDER_ID = 0x1D;

    public static int processOrder(Session3270 session, byte[] buf, int bufPos) {
        List<Field> fieldList = session.getField();
        int offset = session.getBufferAddr();

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

        byte fa = buf[bufPos + 1];
        FieldAttribute.processFA(session, fa);

        return 2;
    }
}
