/*
 * 
 * =======================================================================
 * Copyright (c) 2002-2004 Axion Development Team.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above
 *    copyright notice, this list of conditions and the following
 *    disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The names "Tigris", "Axion", nor the names of its contributors may
 *    not be used to endorse or promote products derived from this
 *    software without specific prior written permission.
 *
 * 4. Products derived from this software may not be called "Axion", nor
 *    may "Tigris" or "Axion" appear in their names without specific prior
 *    written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * =======================================================================
 */

package org.axiondb.util;


/**
 * Utility to convert Ascii to EBCDIC and vice-versa
 * @author Rahul Dwivedi
 */
public class AsciiEbcdicEncoder {
    private static byte[] AToE = {
    /* 0 */0, 1, 2, 3, 55, 45, 46, 47, 22, 5, 21, 11, 12, 13, 14, 15,
    /* 16 */16, 17, 18, 63, 60, 61, 50, 38, 24, 25, 63, 39, 28, 29, 30, 31,
    /* 32 */64, 90, 127, 123, 91, 108, 80, 125, 77, 93, 92, 78, 107, 96, 75, 97,
    /* 48 */-16, -15, -14, -13, -12, -11, -10, -9, -8, -7, 122, 94, 76, 126, 110, 111,
    /* 64 */124, -63, -62, -61, -60, -59, -58, -57, -56, -55, -47, -46, -45, -44, -43, -42,
    /* 80 */-41, -40, -39, -30, -29, -28, -27, -26, -25, -24, -23, 63, 63, 63, 63, 109,
    /* 96 */-71, -127, -126, -125, -124, -123, -122, -121, -120, -119, -111, -110, -109, -108,
            -107, -106,
    /* 112 */-105, -104, -103, -94, -93, -92, -91, -90, -89, -88, -87, 63, 79, 63, 63, 7,
    /* 128 */63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63,
    /* 144 */63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63,
    /* 160 */64, 63, 74, 123, 63, 63, 63, 63, 63, 63, 63, 63, 95, 96, 63, 63,
    /* 176 */63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63,
    /* 192 */63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63,
    /* 208 */63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63,
    /* 224 */63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63,
    /* 240 */63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63};

    /**
     * EBCDIC to ASCII translate table. source code generated by com.mindprod.trt.TrtBuild
     */
    private static byte[] EToA = {
    /* 0 */0, 1, 2, 3, 26, 9, 26, 127, 26, 26, 26, 11, 12, 13, 14, 15,
    /* 16 */16, 17, 18, 26, 26, 10, 8, 26, 24, 25, 26, 26, 28, 29, 30, 31,
    /* 32 */26, 26, 28, 26, 26, 10, 23, 27, 26, 26, 26, 26, 26, 5, 6, 7,
    /* 48 */26, 26, 22, 26, 26, 30, 26, 4, 26, 26, 26, 26, 20, 21, 26, 26,
    /* 64 */32, 26, 26, 26, 26, 26, 26, 26, 26, 26, -94, 46, 60, 40, 43, 124,
    /* 80 */38, 26, 26, 26, 26, 26, 26, 26, 26, 26, 33, 36, 42, 41, 59, -84,
    /* 96 */45, 47, 26, 26, 26, 26, 26, 26, 26, 26, 26, 44, 37, 95, 62, 63,
    /* 112 */26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 58, 35, 64, 39, 61, 34,
    /* 128 */26, 97, 98, 99, 100, 101, 102, 103, 104, 105, 26, 26, 26, 26, 26, 26,
    /* 144 */26, 106, 107, 108, 109, 110, 111, 112, 113, 114, 26, 26, 26, 26, 26, 26,
    /* 160 */26, 26, 115, 116, 117, 118, 119, 120, 121, 122, 26, 26, 26, 26, 26, 26,
    /* 176 */26, 26, 26, 26, 26, 26, 26, 26, 26, 96, 26, 26, 26, 26, 26, 26,
    /* 192 */26, 65, 66, 67, 68, 69, 70, 71, 72, 73, 26, 26, 26, 26, 26, 26,
    /* 208 */26, 74, 75, 76, 77, 78, 79, 80, 81, 82, 26, 26, 26, 26, 26, 26,
    /* 224 */26, 26, 83, 84, 85, 86, 87, 88, 89, 90, 26, 26, 26, 26, 26, 26,
    /* 240 */48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 26, 26, 26, 26, 26, 26};

    public static int ASCIIToEBCDIC(int ascii) {
        return AToE[ascii & 0xff] & 0xff;
    }

    public static int EBCDICToASCII(int ebcdic) {
        return EToA[ebcdic & 0xff] & 0xff;
    }

    public static void convertAsciiToEbcdic(byte[] ascii) {        
        for (int i = 0; i < ascii.length; i++) {
            ascii[i] = (byte) ASCIIToEBCDIC(ascii[i]);
        }
    }

    public static void convertEbcdicToAscii(byte[] ascii) {
        for (int i = 0; i < ascii.length; i++) {
            ascii[i] = (byte) EBCDICToASCII(ascii[i]);
        }
    }
}
