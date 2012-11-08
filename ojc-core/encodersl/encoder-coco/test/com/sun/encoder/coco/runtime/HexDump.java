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
 * @(#)HexDump.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.coco.runtime;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Calendar;
import java.util.Random;

/**
 * Utility class for rendering byte data in hexadecimal.
 * 
 * @author nang@seebeyond.com
 * @version $Revision: 1.1 $
 */ 
public final class HexDump
{
    private HexDump()
    {
    }

    /**
     * Generate a hex dump.
     * <p/>
     * If the specified <code>data</code> or <code>buffer</code> is
     * <code>null</code>, the method fails quietly. The supplied buffer is not
     * cleared before use.
     *
     * @param data   Bytes to dump
     * @param buffer Buffer to send the output
     * @param indent Amount of indentation for the output
     */
    public static void dump(final byte[] data,
                            StringBuffer buffer,
                            int indent
                            )
    {
        if (data == null || buffer == null) {
            return;
        }

        // bytes output
        long counter = 0L;
        for (int i = 0 ; i < data.length ; i += BYTES_PER_LINE) {

            // indentation for output
            indent = Math.max(0, indent);
            for (int l = 0 ; l < indent ; ++l) {
                buffer.append(' ');
            }
        
            // offset display for the next {BYTES_PER_LINE} bytes
            writeHex(buffer, counter + i);
            buffer.append("h: ");
            
            // output bytes in hex
            for (int j = 0 ; j < BYTES_PER_LINE ; ++j) {
                int idx = j + i;
                if (idx < data.length) {
                    writeHex(buffer, data[idx]);
                    buffer.append(' ');
                }
                else {
                    buffer.append("  ").append(' ');
                }
            }
            
            // output bytes as characters
            buffer.append(" ");
            for (int j = 0 ; j < BYTES_PER_LINE ; ++j) {
                int idx = j + i;
                if (idx < data.length) {
                    if (32 <= data[idx] && data[idx] <= 127) {
                        buffer.append((char) data[idx]);
                    }
                    else {
                        buffer.append('.');
                    }
                }
            }

            buffer.append(EOL);
        }
    }

    private static void writeHex(StringBuffer buffer, long val)
    {
        for (int i = BYTESHIFTS.length - 1; i >= 0; --i) {
            buffer.append(
                    HEXCHARS.charAt(
                            ((int) (val >> BYTESHIFTS[i]))
                            & 0x0F));
        }
    }

    private static void writeHex(StringBuffer buffer, byte val)
    {
        buffer.append(HEXCHARS.charAt((val >> 4) & 0x0F));
        buffer.append(HEXCHARS.charAt(val & 0x0F));
    }

    /**
     * Bitwise right-shift displacement table. To move the (x)th byte (counting
     * from 0) of an n-byte value to the least significant position, shift-right
     * the number of bits indicated by BYTESHIFTS[x * 2].  To move the (x)th
     * nybble to the least significant nybble position, shift as indicated by
     * BYTESHIFTS[(x*2) + 1]. 
     * <p/>
     * Only up to 64 bits are currently supported.
     */
    private static final int[] BYTESHIFTS = {
        0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60
    };

    /**
     * Hex character look-up table.  HEXCHARS[x] contains the hex character
     * representation for the number x.
     */
    private static final String HEXCHARS = "0123456789ABCDEF";
    
    /**
     * Number of bytes to output per "line of display".
     */
    private static final int BYTES_PER_LINE = 16;

    /**
     * End-of-line character used in output.
     */ 
    private static final String EOL;

    static
    {
        String lEOL = System.getProperty("line.separator");
        if (lEOL != null) {
            EOL = lEOL;
        }
        else {
            EOL = "\n";
        }
    }
    
    public static void main(String[] args)
            throws IOException {
        final Random random =
            new Random(Calendar.getInstance().getTimeInMillis());
        final byte[] input = new byte[random.nextInt(1001)];
        final StringBuffer buffer = new StringBuffer(input.length * 10);
        random.nextBytes(input);
        
        // test code; dump output
        dump(input, buffer, 10);
        File file = File.createTempFile("HexDump", ".output");
        FileWriter fw = new FileWriter(file);
        fw.write(buffer.toString());
        fw.flush();
        fw.close();
        
        // dump input data into another file for auditing
        file = File.createTempFile("HexDump", ".audit");
        FileOutputStream fos = new FileOutputStream(file);
        fos.write(input);
        fos.flush();
        fos.close();
    }
}
