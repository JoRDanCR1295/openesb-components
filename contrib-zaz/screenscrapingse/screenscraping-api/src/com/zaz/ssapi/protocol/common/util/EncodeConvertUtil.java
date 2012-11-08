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
package com.zaz.ssapi.protocol.common.util;

import java.io.UnsupportedEncodingException;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author tianlize
 */
public class EncodeConvertUtil {

    private final static int[] a2eG = {
        0, 1, 2, 3, 55, 45, 46, 47, 22, 5, 37, 11, 12, 13, 14, 15,
        16, 17, 18, 19, 60, 61, 50, 38, 24, 25, 63, 39, 28, 29, 30, 31,
        64, 90, 127, 123, 91, 108, 80, 125, 77, 93, 92, 78, 107, 96, 75, 97,
        240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 122, 94, 76, 126, 110, 111,
        124, 193, 194, 195, 196, 197, 198, 199, 200, 201, 209, 210, 211, 212, 213, 214,
        215, 216, 217, 226, 227, 228, 229, 230, 231, 232, 233, 74, 224, 90, 95, 109,
        121, 129, 130, 131, 132, 133, 134, 135, 136, 137, 145, 146, 147, 148, 149, 150,
        151, 152, 153, 162, 163, 164, 165, 166, 167, 168, 169, 192, 106, 208, 161, 7,
        32, 33, 34, 35, 36, 21, 6, 23, 40, 41, 42, 43, 44, 9, 10, 27,
        48, 49, 26, 51, 52, 53, 54, 8, 56, 57, 58, 59, 4, 20, 62, 225,
        65, 66, 67, 68, 69, 70, 71, 72, 73, 81, 82, 83, 84, 85, 86, 87,
        88, 89, 98, 99, 100, 101, 102, 103, 104, 105, 112, 113, 114, 115, 116, 117,
        118, 119, 120, 128, 138, 139, 140, 141, 142, 143, 144, 154, 155, 156, 157, 158,
        159, 160, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183,
        184, 185, 186, 187, 188, 189, 190, 191, 202, 203, 204, 205, 206, 207, 218, 219,
        220, 221, 222, 223, 234, 235, 236, 237, 238, 239, 250, 251, 252, 253, 254, 255
    };
    private final static int[] e2aG = {
        0, 1, 2, 3, 156, 9, 134, 127, 151, 141, 142, 11, 12, 13, 14, 15,
        16, 17, 18, 19, 157, 133, 8, 135, 24, 25, 146, 143, 28, 29, 30, 31,
        128, 129, 130, 131, 132, 10, 23, 27, 136, 137, 138, 139, 140, 5, 6, 7,
        144, 145, 22, 147, 148, 149, 150, 4, 152, 153, 154, 155, 20, 21, 158, 26,
        32, 160, 161, 162, 163, 164, 165, 166, 167, 168, 91, 46, 60, 40, 43, 33,
        38, 169, 170, 171, 172, 173, 174, 175, 176, 177, 33, 36, 42, 41, 59, 94,
        45, 47, 178, 179, 180, 181, 182, 183, 184, 185, 124, 44, 37, 95, 62, 63,
        186, 187, 188, 189, 190, 191, 192, 193, 194, 96, 58, 35, 64, 39, 61, 34,
        195, 97, 98, 99, 100, 101, 102, 103, 104, 105, 196, 197, 198, 199, 200, 201,
        202, 106, 107, 108, 109, 110, 111, 112, 113, 114, 203, 204, 205, 206, 207, 208,
        209, 126, 115, 116, 117, 118, 119, 120, 121, 122, 210, 211, 212, 213, 214, 215,
        216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231,
        123, 65, 66, 67, 68, 69, 70, 71, 72, 73, 232, 233, 234, 235, 236, 237,
        125, 74, 75, 76, 77, 78, 79, 80, 81, 82, 238, 239, 240, 241, 242, 243,
        92, 159, 83, 84, 85, 86, 87, 88, 89, 90, 244, 245, 246, 247, 248, 249,
        48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 250, 251, 252, 253, 254, 255
    };

    public static final byte[] getEBCDICByteFromString(String in) {
        byte[] ret = null;
        try {
            ret = in.getBytes("CP500");
        } catch (UnsupportedEncodingException ex) {
            Logger.getLogger(EncodeConvertUtil.class.getName()).log(Level.SEVERE, null, ex);
        }
        return ret;
    }

    public static final byte[] getASCIIByteFromString(String in) {
        byte[] ret = null;
        try {
            ret = in.getBytes("ASCII");
        } catch (UnsupportedEncodingException ex) {
            Logger.getLogger(EncodeConvertUtil.class.getName()).log(Level.SEVERE, null, ex);
        }
        return ret;
    }

    public static final String getStringFromASCIIByte(byte[] bytes) {
        String ret = null;
        int i = 0;
        if (bytes == null | bytes.length < 1) {
            return "";
        }
        int size = bytes.length;
        char[] retChars = new char[size];
        for (; i < size; i++) {
            if (bytes[i] == 0x00) {
                break;
            }
            int j = (int) bytes[i];
            retChars[i] = (char) j;
        }

        ret = new String(retChars, 0, i);
        return ret;
    }

    public static final String getStringFromEBCDICByte(byte[] bytes) {
        String ret = null;
        if (bytes == null | bytes.length < 1) {
            return "";
        }
        int size = bytes.length;
        char[] retChars = new char[size];
        for (int i = 0; i < size; i++) {
            int j = ((int) bytes[i]) & 0xFF;
            if (j == 0) {
                j = 64;
            }
            retChars[i] = (char) e2aG[j];
        }
        ret = new String(retChars);
        return ret;
    }

    public static void printBytes(byte[] bytes) {
        if (bytes == null | bytes.length < 1) {
            return;
        }
        for (int i = 0; i < bytes.length; i++) {
            if (i > 0 & i % 10 == 0) {
                System.out.println();
            }
            System.out.print((((int) bytes[i]) & 0xFF) + " ");
        }
        System.out.println();
    }

    public final static String getStringFromEBCDICString(String str) {
        if (str == null | str.equals("")) {
            return "";
        }
        String[] strArray = str.split(" ");
        char[] strIns = new char[strArray.length];
        for (int i = 0; i < strArray.length; i++) {
            strIns[i] = (char) e2aG[Integer.parseInt(strArray[i], 16)];
        }
        return new String(strIns);
    }

    public final static byte[] getEBCDICByteFromHexString(String str) {
        if (str == null | "".equals(str)) {
            return null;
        }
        String[] strArray = str.split(" ");
        byte[] ret = new byte[strArray.length];
        for (int i = 0; i < strArray.length; i++) {
            int j = Integer.valueOf(strArray[i], 16);
            ret[i] = (byte) j;
        }
        return ret;
    }

    public final static String getHexStringFrombyte(byte[] bytes) {
        String ret = "";
        if (bytes == null | bytes.length < 1) {
            return "";
        }
        int size = bytes.length;
        int i = 0;
        for (; i < size - 2; i++) {
            if ((bytes[i] & 0xFF) == 0xFF && (bytes[i + 1] & 0xFF) == 0xEF && (bytes[i + 2] & 0xFF) == 0x00) {
                break;
            }
            if (i > 0 && i % 20 == 0) {
                ret += "\n";
            }
            int j = ((int) bytes[i]) & 0xFF;
            String str = Integer.toHexString(j);
            if (str.length() < 2) {
                str = "0" + str;
            }
            ret = ret + str + " ";
        }
        if (i > 0 && i % 20 == 0) {
            ret += "\n";
        }
        ret = ret + "ff ef ";
        return ret;
    }

    public final static String getASCIIStringFrombyte(byte[] bytes) {
        String ret = "";
        if (bytes == null | bytes.length < 1) {
            return "";
        }
        int size = bytes.length;
        int i = 0;
        for (; i < size; i++) {
            if ((bytes[i] & 0xFF) == 0x00) {
                break;
            }
            if (i > 0 && i % 20 == 0) {
                ret += "\n";
            }
            int j = ((int) bytes[i]) & 0xFF;
            String str = Integer.toHexString(j);
            if (str.length() < 2) {
                str = "0" + str;
            }
            ret = ret + str + " ";
        }
        if (i > 0 && i % 20 == 0) {
            ret += "\n";
        }
        return ret;
    }
}

