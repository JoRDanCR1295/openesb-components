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
 * @(#)SingleByteCoder.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime.provider;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;

import com.sun.encoder.runtime.CoderException;

/**
 * Generic coder class for 8-bit encodings configurable through a
 * code description file. The encodings do not have to be reversible.
 * See {@link com.sun.encoder.runtime.StringCoder}.
 *
 * The definition can define 3 special values: a surrogate character
 * for undecodable bytes, a surrogate byte for unencodable characters,
 * and a padding byte for fixed-length output.
 *
 * @author Michael Libourel
 * @version
 */
public class SingleByteCoder
    extends SharedCoder {

    private static final int BYTES = 0x100;
    public static final int NOCHAR = -1;
    public static final short NOBYTE = -1;

    /**
     * Mapping of byte value (as index) to Unicode character, or NOCHAR
     * if invalid.
     */
    private final int[] mByte2Char = new int[BYTES];

    /**
     * Mapping of Unicode code point to byte encoding. The index into
     * this array is the high-byte of the Unicode value, giving a
     * second tier array indexed by the low-byte (we assume no chars
     * over 0xFFFF, i.e. support for Unicode 3.0.1 at most).
     * A value of NOBYTE (or null second tier array) implies not mappable.
     */
    private final short[][] mChar2Byte = new short[BYTES][];

    /**
     * Mapping of invalid byte value to Unicode default, or NOCHAR if fatal.
     */
    private int mSurrogateChar = NOCHAR;

    /**
     * Mapping of unencodable Unicode code point, or NOBYTE if fatal.
     */
    private short mSurrogateByte = NOBYTE;

    /**
     * Byte used for padding encoded output if required.
     */
    private byte mPaddingByte = 0x20;

    /**
     * Constructs from a list of single-byte values.
     * The list maps the byte values used in the encoding to corresponding
     * Unicode code points.
     *
     * @param codes  a list of 256 Unicode code points, or -1 if not mapped
     * @param sChar  surrogate character, or NOCHAR
     * @param sByte  surrogate byte, or NOBYTE
     */
    public SingleByteCoder(int[] codes, int sChar, short sByte) {
        if (codes == null) {
            throw new NullPointerException("no codes");
        }
        if (codes.length != BYTES) {
            throw new IllegalArgumentException("wrong code list size: "
                + codes.length + ", should be " + BYTES);
        }
        for (int i = 0; i < BYTES; i++) {
            add((byte) i, codes[i]);
        }
        mSurrogateChar = sChar;
        mSurrogateByte = sByte;
    }

    /**
     * Constructs from a descriptor file.
     * The list maps the byte values used in the encoding to corresponding
     * Unicode code points.
     *
     * The descriptor file has 5 kinds of lines: empty, comment ("# ..."),
     * surrogate byte definition ("@byte X"), surrogate character ("@char C"),
     * and code definition ("X = C"), where X and C are 2-digit (byte) and
     * 4-digit (Unicode char) hexadecimal numbers respectively.
     *
     * @param input  the descriptor file
     * @throws IOException for input access or descriptor syntax problems
     */
    public SingleByteCoder(File input)
        throws IOException {
        InputStream is = new FileInputStream(input);
        add(input.getPath(), is);
        is.close();
    }

    /**
     * Constructs from a descriptor resource.
     * The list maps the byte values used in the encoding to corresponding
     * Unicode code points.
     *
     * The descriptor file has 5 kinds of lines: empty, comment ("# ..."),
     * surrogate byte definition ("@byte X"), surrogate character ("@char C"),
     * and code definition ("X = C"), where X and C are 2-digit (byte) and
     * 4-digit (Unicode char) hexadecimal numbers respectively.
     *
     * @param resource  the descriptor resource path
     * @throws IOException for input access or descriptor syntax problems
     */
    public SingleByteCoder(String resource)
        throws IOException {
        ClassLoader loader = SingleByteCoder.class.getClassLoader();
        //System.out.println("[ loading resource " + resource + " ]");
        InputStream is = loader.getResourceAsStream(resource);
        if (is == null) {
            throw new IOException("can't find code table resource '"
                + resource + "'");
        }
        add(resource, is);
        is.close();
    }

    private int getHexNumber(String path, LineNumberReader in, String s,
        String what, int max)
        throws IOException {
        try {
            s = s.trim();
            int n = Integer.parseInt(s, 16);
            if (n < 0 || max < n) {
                throw new IOException(path + " line " + in.getLineNumber()
                    + ": invalid " + what + " value '" + s + "'");
            }
            return n;
        } catch (NumberFormatException n) {
            throw new IOException(path + " line " + in.getLineNumber()
                + ": malformed " + what + " value '" + s + "'");
        }
    }

    /**
     * Constructs from a list of single-byte values, read from a stream.
     * The list maps the byte values used in the encoding to corresponding
     * Unicode code points.
     *
     * @param path  the input stream path, for use in error messages.
     * @param is  the input stream.
     * @throws IOException IOException occurred.
     */
    public void add(String path, InputStream is)
        throws IOException {
        if (is == null) {
            throw new NullPointerException("no input stream");
        }
        LineNumberReader in;
        try { in =
            new LineNumberReader(
                new BufferedReader(
                    new InputStreamReader(is, "UTF-8")));
        } catch (UnsupportedEncodingException u) {
            // JDK guarantees UTF-8.
            throw new RuntimeException("broken platform");
        }
        for (int i = 0; i < BYTES; i++) {
            mByte2Char[i] = NOCHAR;
        }
        String line;
        while ((line = in.readLine()) != null) {
            if (line.length() > 0 && !line.startsWith("#")) {
                if (line.startsWith("@byte")) {
                    // Surrogate byte.
                    mSurrogateByte = (short) getHexNumber(path, in,
                        line.substring(5), "byte", 0xFF);
                } else if (line.startsWith("@char")) {
                    // Surrogate char.
                    mSurrogateChar = getHexNumber(path, in,
                        line.substring(5), "char", 0xFFFD);
                } else if (line.startsWith("@fill")) {
                    // Padding filler.
                    mPaddingByte = (byte) getHexNumber(path, in,
                        line.substring(5), "fill", 0xFF);
                } else {
                    // Code definition: byte = char.
                    int pos = line.indexOf('=');
                    if (pos < 0) {
                        throw new IOException(path + " line "
                            + in.getLineNumber() + ": missing '='");
                    }
                    String b = line.substring(0, pos);
                    String s = line.substring(pos + 1);
                    byte nbyte = (byte) getHexNumber(path, in, b, "byte", 0xFF);
                    int nchar = getHexNumber(path, in, s, "char", 0xFFFD);
                    add(nbyte, nchar);
                }
            }
        }
        in.close();
    }

    /**
     * Adds a mapping to the list.
     * Only used during instance initialization.
     *
     * @param b  a byte value
     * @param c  the encoded character, or NOCHAR
     */
    private void add(byte b, int c) {
        if (c < -1 || 0xFFFD <= c) {
            throw new IllegalArgumentException("byte " + b
                + " maps to invalid code " + uname(c));
        }
        if ((mByte2Char[b & 0xFF] = c) >= 0) {
            int lo = (c & 0xFF), hi = ((c >> 8) & 0xFF);
            if (mChar2Byte[hi] == null) {
                // Allocate 2nd tier map.
                short[] s = (mChar2Byte[hi] = new short[BYTES]);
                for (int j = 0; j < BYTES; j++) {
                    s[j] = NOBYTE;
                }
            }
            mChar2Byte[hi][lo] = (short) (b & 0xFF);
        }
    }

    /**
     * Converts a string into a byte array of the given size.
     * If too short, the result is padded at the end with a padding
     * determined by "@fill". If too long, an exception is thrown.
     *
     * @param in  a string, or null
     * @param min  the minimum length, or -1 for no minimum
     * @param max  the maximum length, or -1 for no maximum
     * @return byte array containing the encoded string
     * @throws CoderException for unrepresentable characters
     */
    public byte[] encode(String in, int min, int max)
        throws CoderException {
        if (in == null) {
            return null;
        }
        if (0 <= min && 0 <= max && max < min) {
            // Conflicting bounds.
            throw new IllegalArgumentException("max (" + max + ") < min ("
                + min + ")");
        }
        int len = in.length(), size = len, c;
        if (size < min) {
            size = min;
        }
        if (0 <= max && max < size) {
            // Data overflow.
            throw new CoderException("too big: " + size + ", max=" + max);
        }
        short b;
        byte[] out = new byte[size];
        for (int i = size = 0; i < len; i++) {
            c = (int) in.charAt(i);
            short[] map = mChar2Byte[(c >> 8) & 0xFF];
            if ((b = ((map == null) ? NOBYTE : map[c & 0xFF])) == NOBYTE) {
                if ((b = mSurrogateByte) == NOBYTE) {
                    throw new CoderException("char #" + i + " = "
                        + uname(c) + ", not encodable");
                }
            }
            out[size++] = (byte) b;
        }
        // Append padding to end.
        while (size < out.length) {
            out[size++] = mPaddingByte;
        }
        return out;
    }

    /**
     * Converts a byte array into a string.
     * All characters will be in range U+0000 - U+00FF.
     *
     * @param in  null or a byte array containing the encoded string
     * @param from  index of first byte
     * @param length  length of sub-array to convert
     * @return the decoded string
     */
    public String decode(byte[] in, int from, int length)
        throws CoderException {
        if (in == null) {
            return null;
        }
        if (from < 0 || in.length < from || length < 0
            || in.length < from + length) {
            throw new CoderException("invalid size/from/length: "
                + in.length + "/" + from + "/" + length);
        }
        char[] out = new char[length];
        int size = 0, c, last = from + length;
        for (int i = from; i < last; i++) {
            if ((c = mByte2Char[in[i] & 0xFF]) == NOCHAR &&
                (c = mSurrogateChar) == NOCHAR) {
                throw new CoderException(i, "code = " + bname(in[i])
                    + ", not decodable");
            }
            out[size++] = (char) c;
        }
        return new String(out);
    }

    public void dump(PrintStream ps) {
        ps.println("# Single-byte encoding map dump.");
        ps.println("#-------------------------------");
        ps.println();
        if (mSurrogateByte >= 0) {
            ps.println("@byte " + hex2(mSurrogateByte));
            ps.println();
        }
        if (mSurrogateChar >= 0) {
            ps.println("@char " + hex4(mSurrogateByte));
            ps.println();
        }
        for (int i = 0; i < BYTES; i++) {
            if (mByte2Char[i] >= 0) {
                ps.println(hex2(i) + " = " + hex4(mByte2Char[i]));
            }
        }
    }

    /**
     * Runs coder through basic testing.
     * Usage: main [-r] [path].
     * Option -r loads from resource, else read code from local file.
     * @param args arguments.
     */
    public static void main(String[] args) {
        try {
            boolean res = false;
            String path = "my.code";
            int opt = 0;
            if (args.length > opt && args[opt].equals("-r")) {
                res = true;
                opt++;
            }
            if (args.length > opt) {
                path = args[opt++];
            }
            SingleByteCoder sbc = (res
                ? new SingleByteCoder(path)
                : new SingleByteCoder(new File(path)));
            sbc.dump(System.out);
        } catch (Exception all) {
            all.printStackTrace(System.err);
            System.exit(1);
        }
    }

    @Override
    public String toString() {
        StringBuffer buf = new StringBuffer("SingleByteCoder@");
        buf.append(Integer.toHexString(hashCode()));
        return buf.toString();
    }
}
