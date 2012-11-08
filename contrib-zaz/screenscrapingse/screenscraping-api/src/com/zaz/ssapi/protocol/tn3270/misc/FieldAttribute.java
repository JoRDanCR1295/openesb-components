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
import com.zaz.ssapi.protocol.tn3270.model.ExtField;
import com.zaz.ssapi.protocol.tn3270.model.Field;

import java.awt.Color;

import java.util.List;

/**
 *
 * @author liyunhai
 */
public class FieldAttribute {

    public static void processFA(Session3270 session, byte fa) {
        List<Field> fieldList = session.getField();
        Field field = (Field) fieldList.get(fieldList.size() - 1);
        ExtField extField = field.getExtField();

        String padZero = "00000000";
        String fas = Integer.toBinaryString(Integer.valueOf(fa & 0xFF).intValue());
        fas = padZero.substring(0, padZero.length() - fas.length()) + fas;

        if ("1".equals(fas.substring(2, 3))) {
            field.setFProtected(true);
        }

        if ("1".equals(fas.substring(3, 4))) {
            field.setNumeric(true);
        }

        if ("1".equals(fas.substring(7))) {
            field.setModifiedDataTag(true);
        }

        String displayStyled = fas.substring(4, 6);
        field.setDisplayStyled(displayStyled);

        if (displayStyled.equals(Field.NORMAL_NOT_LIGHT)) {
            if (field.isFProtected()) {
                extField.setColor(Color.cyan);
            } else {
                extField.setColor(Color.green);
            }
        } else if (displayStyled.equals(Field.NORMAL_LIGHT)) {
            extField.setColor(Color.lightGray);
        } else if (displayStyled.equals(Field.INTENSIFIED_LIGHT)) {
            if (field.isFProtected()) {
                extField.setColor(Color.lightGray);
            } else {
                extField.setColor(Color.green);
            }
        } else if (displayStyled.equals(Field.NOT_DISPLAYED)) {
            extField.setColor(Color.black);
        }
    }
}
