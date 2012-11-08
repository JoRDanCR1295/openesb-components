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
package com.zaz.ssapi.protocol.tn3270.util;

import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author tianlize
 */
public class AddressConvertUtil {

    private static final Map<String, String> CVT_MAP;
    

    static {
        CVT_MAP = new HashMap<String, String>();
        CVT_MAP.put("000000", "40");
        CVT_MAP.put("000001", "C1");
        CVT_MAP.put("000010", "C2");
        CVT_MAP.put("000011", "C3");
        CVT_MAP.put("000100", "C4");
        CVT_MAP.put("000101", "C5");
        CVT_MAP.put("000110", "C6");
        CVT_MAP.put("000111", "C7");
        CVT_MAP.put("001000", "C8");
        CVT_MAP.put("001001", "C9");
        CVT_MAP.put("001010", "4A");
        CVT_MAP.put("001011", "4B");
        CVT_MAP.put("001100", "4C");
        CVT_MAP.put("001101", "4D");
        CVT_MAP.put("001110", "4E");
        CVT_MAP.put("001111", "4F");
        CVT_MAP.put("010000", "50");
        CVT_MAP.put("010001", "D1");
        CVT_MAP.put("010010", "D2");
        CVT_MAP.put("010011", "D3");
        CVT_MAP.put("010100", "D4");
        CVT_MAP.put("010101", "D5");
        CVT_MAP.put("010110", "D6");
        CVT_MAP.put("010111", "D7");
        CVT_MAP.put("011000", "D8");
        CVT_MAP.put("011001", "D9");
        CVT_MAP.put("011010", "5A");
        CVT_MAP.put("011011", "5B");
        CVT_MAP.put("011100", "5C");
        CVT_MAP.put("011101", "5D");
        CVT_MAP.put("011110", "5E");
        CVT_MAP.put("011111", "5F");
        CVT_MAP.put("100000", "60");
        CVT_MAP.put("100001", "61");
        CVT_MAP.put("100010", "E2");
        CVT_MAP.put("100011", "E3");
        CVT_MAP.put("100100", "E4");
        CVT_MAP.put("100101", "E5");
        CVT_MAP.put("100110", "E6");
        CVT_MAP.put("100111", "E7");
        CVT_MAP.put("101000", "E8");
        CVT_MAP.put("101001", "E9");
        CVT_MAP.put("101010", "6A");
        CVT_MAP.put("101011", "6B");
        CVT_MAP.put("101100", "6C");
        CVT_MAP.put("101101", "6D");
        CVT_MAP.put("101110", "6E");
        CVT_MAP.put("101111", "6F");
        CVT_MAP.put("110000", "F0");
        CVT_MAP.put("110001", "F1");
        CVT_MAP.put("110010", "F2");
        CVT_MAP.put("110011", "F3");
        CVT_MAP.put("110100", "F4");
        CVT_MAP.put("110101", "F5");
        CVT_MAP.put("110110", "F6");
        CVT_MAP.put("110111", "F7");
        CVT_MAP.put("111000", "F8");
        CVT_MAP.put("111001", "F9");
        CVT_MAP.put("111010", "7A");
        CVT_MAP.put("111011", "7B");
        CVT_MAP.put("111100", "7C");
        CVT_MAP.put("111101", "7D");
        CVT_MAP.put("111110", "7E");
        CVT_MAP.put("111111", "7F");
    }

    public final static RowColAddress getRowColAddressFromOffset(int offset) {
        if (offset < 0 || offset > 1919) {
            return null;
        }

        int row = offset / 80;
        int col = offset % 80;

        return new RowColAddress(row, col);
    }

    public final static int getOffsetFromRowColAddress(RowColAddress address) {
        if (address == null) {
            return -1;
        }

        return address.getRow() * 80 + address.getCol();
    }

    public final static int getOffsetFromRowColAddress(int row, int col) {
        if (row < 0 || col < 0 || row > 23 || col > 79) {
            return -1;
        }

        return row * 80 + col;
    }

    public final static int getRowFromOffset(int offset) {
        if (offset < 0 || offset > 1919) {
            return -1;
        }

        return offset / 80;
    }

    public final static int getColFromOffset(int offset) {
        if (offset < 0 || offset > 1919) {
            return -1;
        }

        return offset % 80;
    }

    public final static int getOffsetFromScreenAddress(byte[] address) {
        int ret = -1;
        String padZero = "00000000";

        String firstByte = Integer.toBinaryString(
                Integer.valueOf(address[0] & 0xFF).intValue());
        String secondByte = Integer.toBinaryString(
                Integer.valueOf(address[1] & 0xFF).intValue());

        firstByte = padZero.substring(
                0, padZero.length() - firstByte.length()) + firstByte;
        secondByte = padZero.substring(
                0, padZero.length() - secondByte.length()) + secondByte;

        String flagStr = firstByte.substring(0, 2);
        if ("01".equals(flagStr) || "11".equals(flagStr)) {
            String retStr = firstByte.substring(2) + secondByte.substring(2);
            ret = Integer.valueOf(retStr, 2);
        } else if ("00".equals(flagStr)) {
            String retStr = firstByte + secondByte;
            ret = Integer.valueOf(retStr, 2);
        }

        return ret;
    }

    public final static byte[] getScreenAddressFromOffset(int offset) {
        byte[] ret = new byte[]{0x00, 0x00};
        String padZero = "000000000000";

        if (offset < 0 | offset > 1919) {
            return ret;
        }

        String bin = Integer.toBinaryString(offset);
        bin = padZero.substring(
                0, padZero.length() - bin.length()) + bin;

        String firstByte = bin.substring(0, 6);
        String secondByte = bin.substring(6);

        if (CVT_MAP.containsKey(firstByte)) {
            ret[0] = (byte) Integer.parseInt((String) CVT_MAP.get(firstByte), 16);
        }

        if (CVT_MAP.containsKey(secondByte)) {
            ret[1] = (byte) Integer.parseInt((String) CVT_MAP.get(secondByte), 16);
        }

        return ret;
    }

    public final static byte[] getScreenAddressFromRowColAddress(RowColAddress address) {
        return getScreenAddressFromOffset(getOffsetFromRowColAddress(address));
    }

    public final static RowColAddress getRowColAddressFromScreenAddress(byte[] address) {
        return getRowColAddressFromOffset(getOffsetFromScreenAddress(address));
    }
}
