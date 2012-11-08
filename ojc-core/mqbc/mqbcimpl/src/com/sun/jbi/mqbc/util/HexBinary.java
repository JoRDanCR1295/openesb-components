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
 */
package com.sun.jbi.mqbc.util;

import java.io.ByteArrayOutputStream;

/**
 * Utility class for handling conversion to and from the hexBinary encoding
 * defined in the XML Schema specification.
 * 
 * @author Noel.Ang@sun.com
 * @see http://www.w3.org/TR/2004/REC-xmlschema-2-20041028/datatypes.html#hexBinary
 */
public final class HexBinary {
    
    private HexBinary() {}

    public static byte[] decodeString(String hexBinary)
            throws NullPointerException, NumberFormatException {

        if (hexBinary == null) {
            throw new NullPointerException("hexBinary");
        }

        if (hexBinary.length() % 2 != 0) {
            throw new IllegalArgumentException(
                    "Non-even length; value: " + hexBinary);
        }

        ByteArrayOutputStream stream = new ByteArrayOutputStream(
                hexBinary.length() / 2);

        StringBuffer inputBuffer = new StringBuffer(hexBinary);

        for (int i = 0, len = inputBuffer.length(); i <= len - 2; i += 2) {
            String hexValue = inputBuffer.substring(i, i + 2);
            int signed = Integer.parseInt(hexValue, 16);
            stream.write(signed & 0x000000ff);
        }

        return stream.toByteArray();
    }

    public static String encodeBytes(byte[] bytePayload)
            throws NullPointerException {
        
        if (bytePayload == null) {
            throw new NullPointerException("bytePayload");
        }
        
        StringBuffer resultBuffer = new StringBuffer(bytePayload.length * 2);
        StringBuffer scratch = new StringBuffer(2);
        for (byte b : bytePayload) {
            int unsignedValue = b & 0xFF;
            scratch.append(Integer.toHexString(unsignedValue));
            
            // Convert to upper case
            for (int charIx = 0; charIx < scratch.length(); ++charIx) {
                char ch = scratch.charAt(charIx);
                if (Character.isLowerCase(ch)) {
                    scratch.deleteCharAt(charIx);
                    scratch.insert(charIx, Character.toUpperCase(ch));
                }
            }
            
            // Zero-padding
            if (scratch.length() == 1) {
                scratch.insert(0, '0');
            }
            
            resultBuffer.append(scratch.toString());
            scratch.delete(0, scratch.length());
        }
        
        return resultBuffer.toString();
    }
}
