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

import junit.framework.TestCase;

public class HexBinaryTest extends TestCase {

    public void testEncodeBytesNullPointerException() {
        byte[] data = null;
        try {
            HexBinary.encodeBytes(data);
            fail("Expected null data to cause NullPointerException");
        } catch (NullPointerException e) {
            ;
        }
    }
    
    public void testEncodeBytes() throws Exception {
        byte[] data = new byte[] {
                0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
                20, 30, 40, 50, 60, 70, 80, 90, 100,
                126, 127, -1, -2, -128
        };
        String expectedResult = "000102030405060708090A0B0C0D0E0F141E28323C46505A647E7FFFFE80";
        String encoded = HexBinary.encodeBytes(data);
        assertTrue("Expected: " + expectedResult + ", got: " + encoded, expectedResult.equals(encoded));
    }

}