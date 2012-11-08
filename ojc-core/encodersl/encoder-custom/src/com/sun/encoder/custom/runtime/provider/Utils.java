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

package com.sun.encoder.custom.runtime.provider;

/**
 *
 * @author ltang
 */
public class Utils {

    public static final int HIGH_NIBBLE = 0xF0;
    public static final int LOW_NIBBLE = 0x0F;

    /**
     * Convert a byte[] array to readable string format.
     * This makes the "hex" readable!
     * @return result String buffer in String format
     * @param inputBytes byte[] buffer to convert to string format
     */
    public static String bytesToHex(byte[] inputBytes) {
        if (inputBytes == null || inputBytes.length <= 0) {
            return "";
        }
        byte b = 0x00;
        int i = 0;
        String translation[] = {"0", "1", "2", "3", "4", "5", "6", "7",
            "8", "9", "A", "B", "C", "D", "E", "F"
        };
        StringBuffer buff = new StringBuffer(inputBytes.length * 2);

        while (i < inputBytes.length) {
            // Strip off high nibble
            b = (byte) (inputBytes[i] & HIGH_NIBBLE);
            // shift the bits down
            b = (byte) (b >>> 4);
            // must do this is high order bit is on!
            b = (byte) (b & LOW_NIBBLE);
            // convert the nibble to a String Character
            buff.append(translation[(int) b]);
            // Strip off low nibble
            b = (byte) (inputBytes[i] & LOW_NIBBLE);
            // convert the nibble to a String Character
            buff.append(translation[(int) b]);
            i++;
        }
        return buff.toString();
    }

}
