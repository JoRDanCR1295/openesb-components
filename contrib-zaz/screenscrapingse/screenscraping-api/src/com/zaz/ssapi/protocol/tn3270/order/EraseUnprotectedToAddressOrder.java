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

/**
 *
 * @author liyunhai
 */
public class EraseUnprotectedToAddressOrder {

    public static final byte ORDER_ID = 0x12;

    public static int processOrder(Session3270 session, byte[] buf, int bufPos) {
        //        int ret = 3;
        //        byte[] stopAddress = new byte[]{buf[bufPos + 1], buf[bufPos + 2]};
        //        byte[] dBuffer = session.getDeviceBuffer();
        //        int stopOffSet = AddressConvertUtil.getOffsetFromScreenAddress(stopAddress);
        //        int cBufferAddr = session.getBufferAddr();
        //
        //        /**
        //         * Character attributes for every character changed to nulls must be reset to their defaults.
        //         * But here that is not dealed with.
        //         */
        //        if (cBufferAddr < stopOffSet) {
        //            for (int i = cBufferAddr; i < stopOffSet; i++) {
        //                if (!session.isFieldStartPos(i) && !session.getFieldFromScreenOffset(i).isFProtected()) {
        //                    dBuffer[i] = 0x00;
        //                }
        //            }
        //            cBufferAddr = stopOffSet;
        //        } else if (cBufferAddr > stopOffSet) {
        //            for (int i = cBufferAddr; i < dBuffer.length; i++) {
        //                if (!session.isFieldStartPos(i) && !session.getFieldFromScreenOffset(i).isFProtected()) {
        //                    dBuffer[i] = 0x00;
        //                }
        //            }
        //            cBufferAddr = stopOffSet;
        //        } else if (cBufferAddr == stopOffSet) {
        //            for (int i = 0; i < dBuffer.length; i++) {
        //                if (!session.isFieldStartPos(i) && !session.getFieldFromScreenOffset(i).isFProtected()) {
        //                    dBuffer[i] = 0x00;
        //                }
        //            }
        //        }
        //
        //        session.setDeviceBuffer(dBuffer);
        //        session.setBufferAddr(cBufferAddr);
        //
        //        return ret;
        throw new UnsupportedOperationException("Not supported yet.");
    }
}
