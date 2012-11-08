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
 * @(#)MultiByteCoder.java
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
 * Generic coder class for N-byte encodings configurable through a
 * code description file. The encodings do not have to be reversible.
 * See {@link com.sun.encoder.runtime.StringCoder}.
 *
 * The supported mappings assume a stateless model, mapping a specific
 * set of input byte sequences to specific single Unicode code points.
 * It does not support mapping a sequence to multiple characters,
 * decoding/encoding state, input sequences that share an initial prefix,
 * character values over U+FFFD, and multiple input sequences mapping to
 * the same character value.
 *
 * The resource file list a series of input byte sequences in hexadecimal,
 * followed by "=" and the Unicode code point in hexadecimal too. Example:
 *   00FF12 = 1234
 * The resource can also contain empty lines, and comment lines starting
 * with "#".
 *
 * The map used internally is two-level indirection with compacted
 * secondary index.  The first input byte maps to either a code point or
 * to a (negated) index in a secondary table.  The second and further
 * bytes are mapped through a compaction map (in the expectation that the
 * 2nd byte generally does not allow all 256 values), and then used as
 * offset into the secondary map from the index.
 *
 * @author Michael Libourel
 * @version
 */
public class MultiByteCoder
    extends SharedCoder {

    private static final int BYTES = 0x100;
    public static final int NOCHAR = 0xFFFF;

    /**
     * Maximum number of bytes to encode a single character.
     */
    public static final byte MAX_CODE = 4;

    /**
     * Mapping of first byte value (as index) to Unicode character, or NOCHAR
     * if invalid, or negative as secondary offset into mRest.
     */
    private final int[] mFirst = new int[BYTES];

    /**
     * Mapping of 2nd or later byte value, mapped through mIndex, to Unicode
     * character, or NOCHAR, or (negated) offset into mRest for next byte.
     * It is divided into blocks of mBlock entries.
     */
    private int[] mRest = null;

    /**
     * The size of an mRest block for a single secondary byte map.
     * Range 0...256.
     */
    private short mBlock = 0;

    /**
     * The padding sequence, based on the space character.
     */
    private byte[] mPadding = null;

    /**
     * Compaction mapping of secondary input byte to offset in mRest block.
     * This is used on all secondary bytes, to compact mRest when secondary
     * bytes are sparse.
     * Maps byte value 0...255 to new value in range 0...mBlock-1, or -1 if
     * not valid.
     */
    private final short[] mIndex = new short[BYTES];

    private final short[] mRevIndex = new short[BYTES];

    /**
     * Mapping of Unicode code point to byte encoding. The index into
     * this array is the high-byte of the Unicode value, giving a
     * second tier array indexed by the low-byte (we assume no chars
     * over 0xFFFF, i.e. support for Unicode 3.0.1 at most).
     * A value of null (or null second tier array) implies not mappable.
     */
    private final byte[][][] mChar2Bytes = new byte[BYTES][][];

    // Longest encoding found.
    private int mMaxCode = 0;

    /**
     * Constructs from a descriptor file.
     * The list maps the byte values used in the encoding to corresponding
     * Unicode code points.
     * See class description for file format.
     *
     * @param inputFile  the descriptor file
     * @throws IOException for input access or descriptor syntax problems
     */
    public MultiByteCoder(File inputFile)
        throws IOException {
        InputStream is = new FileInputStream(inputFile);
        init(inputFile.getPath(), is);
        is.close();
    }

    /**
     * Constructs from a descriptor resource.
     * The list maps the byte values used in the encoding to corresponding
     * Unicode code points.
     * See class description for file format.
     *
     * @param resource  the descriptor resource path
     * @throws IOException for input access or descriptor syntax problems
     */
    public MultiByteCoder(String resource)
        throws IOException {
        ClassLoader loader = MultiByteCoder.class.getClassLoader();
        /*-
        System.out.println("[ loading resource " + resource + " ]");
        -*/
        InputStream is = loader.getResourceAsStream(resource);
        if (is == null) {
            throw new IOException("Can't find code table resource '"
                + resource + "'");
        }
        init(resource, is);
        is.close();
    }

    /**
     * Gets hexadecimal specification byte sequence from string.
     * Reports error in given location in case of failure.
     *
     * @param path  the resource/file path, for error messages
     * @param in  the input stream, for error messages
     * @param s  the hexadecimal string
     * @param what  the purpose of the number, for error messages
     * @param buf  the byte buffer
     * @return the number of bytes
     */
    private int getHexNumber(String path, LineNumberReader in, String s,
        String what, byte[] buf)
        throws IOException {
        s = s.trim();
        int len = s.length();
        if (len == 0) {
            throw new IOException(path + " line " + in.getLineNumber()
                + ": zero-length " + what + " value");
        }
        if ((len & 1) != 0) {
            // Odd number of digits, prepend 0.
            s = "0" + s;
            len++;
        }
        if (buf.length * 2 < len) {
            // Number too big.
            throw new IOException(path + " line " + in.getLineNumber()
                + ": oversized " + what + " value '" + s + "', max is "
                + (buf.length * 2) + " digits");
        }
        for (int i = 0; i < len; i += 2) {
            int hi = "0123456789ABCDEF".indexOf(s.charAt(i));
            int lo = "0123456789ABCDEF".indexOf(s.charAt(i + 1));
            if (hi < 0 || lo < 0) {
                throw new IOException(path + " line " + in.getLineNumber()
                    + ": malformed " + what + " value '" + s + "'");
            }
            buf[i / 2] = (byte) ((hi << 4) + lo);
        }
        return len / 2;
    }

    /**
     * Gets hexadecimal value from string.
     * Reports error in given location in case of failure.
     *
     * @param path  the resource/file path, for error messages
     * @param in  the input stream, for error messages
     * @param s  the hexadecimal string
     * @param what  the purpose of the number, for error messages
     * @param max  the maximum acceptable input value
     */
    private int getHexNumber(String path, LineNumberReader in, String s,
        String what, long max)
        throws IOException {
        byte[] buf = new byte[MAX_CODE];
        s = s.trim();
        int bytes = getHexNumber(path, in, s, what, buf), n = 0;
        for (int i = 0; i < bytes; i++) {
            n = (n << 8) + (buf[i] & 0xFF);
        }
        if (n < 0 || max < n) {
            throw new IOException(path + " line " + in.getLineNumber()
                + ": invalid " + what + " value '" + s + "'");
        }
        return n;
    }

    private Object[] mTopMap = null;
    private boolean[] mSecond = null;

    /**
     * Constructs from a list of single-byte values, read from a stream.
     * The list maps the byte values used in the encoding to corresponding
     * Unicode code points.
     *
     * @param path  the input stream path, for use in error messages.
     * @param is  the input stream.
     * @throws IOException IOException occurred.
     */
    public void init(String path, InputStream is)
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
        mMaxCode = 0;
        mTopMap = new Object[BYTES];
        mSecond = new boolean[BYTES];
        for (int i = 0; i < BYTES; i++) {
            mTopMap[i] = null;
            mSecond[i] = false;
        }
        String line;
        byte[] buf = new byte[MAX_CODE];
        while ((line = in.readLine()) != null) {
            if (line.length() > 0 && !line.startsWith("#")) {
                // Code definition: byte = char.
                int pos = line.indexOf('=');
                if (pos < 0) {
                    throw new IOException(path + " line "
                        + in.getLineNumber() + ": missing '='");
                }
                String b = line.substring(0, pos);
                String s = line.substring(pos + 1);
                int nbyte = getHexNumber(path, in, b, "byte", buf);
                int nchar = getHexNumber(path, in, s, "char", 0xFFFD);
                try {
                    add(buf, nbyte, nchar);
                } catch (IllegalArgumentException ia) {
                    throw new IOException(path + " line " + in.getLineNumber()
                        + ": " + ia.getMessage());
                }
            }
        }
        in.close();

        // Convert temporary map to definitive one.
        // First, compute the compaction index for secondary bytes.
        mBlock = 0;
        for (int i = 0; i < BYTES; i++) {
            if (mSecond[i]) {
                mRevIndex[mBlock] = (short) i;
                mIndex[i] = mBlock++;
                /*-
                System.out.println("[ second " + hex2(i) + " = "
                    + mIndex[i] + " ]");
                -*/
            } else {
                mIndex[i] = -1;
            }
        }
        mSecond = null;
        /*-
        System.out.println("[ block size = " + mBlock + " ]");
        -*/

        /* Compute the secondary map. We don't need one for the top map.
         * We leave entry 0 unused, as negative indexes start at -1.
         */
        int sec = mapCount(mTopMap) - 1;
        mRestPos = 1;
        mRest = new int[sec * mBlock + mRestPos];
        for (int i = mRest.length; i-- > 0;) {
            mRest[i] = NOCHAR;
        }
        /*-
        System.out.println("[ " + sec + " secondary maps of " + mBlock + " ]");
        System.out.println("[ total secondary entries: " + mRest.length + " ]");
        -*/
        for (int i = 0; i < BYTES; i++) {
            Object o = mTopMap[i];
            mFirst[i] = ((o == null)
                ? NOCHAR
                : ((o instanceof Integer)
                    ? ((Integer) o).intValue()
                    : convertMap((Object[]) o)));
        }
        mTopMap = null;

        // Compute padding sequence: mapped to U+0020.
        if (mChar2Bytes[0] == null ||
            (mPadding = mChar2Bytes[0][0x20]) == null) {
            throw new IOException(path + ": does not map space (for padding)");
        }
    }

    // Next available mRest[] entry.
    private int mRestPos;

    /**
     * Converts non-top map to block of mBlock entries in mRest.
     * Returns the negated index of the first entry.
     *
     * @param map  the sub-map, with BYTES entries
     * @return a negative number, negated index in mRest[]
     */
    private int convertMap(Object[] map) {
        int start = mRestPos;
        mRestPos += mBlock;
        for (int i = 0; i < mBlock; i++) {
            mRest[start + i] = NOCHAR;
        }
        for (int i = 0; i < BYTES; i++) {
            Object o = map[i];
            if (o != null) {
                int n = mIndex[i];
                if (n < 0) {
                    // Broken mIndex?
                    throw new RuntimeException("index map failed for " + i);
                }
                mRest[start + n] = ((o instanceof Integer)
                    ? ((Integer) o).intValue()
                    : convertMap((Object[]) o));
            }
        }
        return -start;
    }

    /**
     * Counts the number of maps this map refers to, including itself.
     *
     * @param map  an array of null, Integer and Object[] values
     * @return a positive value
     */
    private int mapCount(Object[] map) {
        int count = 1;
        Object o;
        for (int i = 0; i < BYTES; i++) {
            if ((o = map[i]) != null && (o instanceof Object[])) {
                count += mapCount((Object[]) o);
            }
        }
        return count;
    }

    /**
     * Adds a mapping to the temporary list.
     * Only used during instance initialization.
     *
     * @param b  a byte buffer
     * @param bsize  the number initial bytes in b[] to use
     * @param c  the encoded character
     */
    private void add(byte b[], int bsize, int c) {
        if (c < 0 || 0xFFFD <= c) {
            throw new IllegalArgumentException("invalid code " + uname(c));
        }
        // Enter in temporary byte-sequence to char map.
        if (mMaxCode < bsize) {
            mMaxCode = bsize;
        }
        Object[] map = mTopMap;
        for (int i = 0; i < bsize; i++) {
            int n = (b[i] & 0xFF);
            boolean last = (i == bsize - 1);
            if (i > 0) {
                // Needed in mIndex map.
                /*-
                if (!mSecond[i])
                    System.out.println("[ second: " + n + ", byte " + i + " ]");
                -*/
                mSecond[n] = true;
            }
            Object o;
            if ((o = map[n]) != null && (last || !(o instanceof Object[]))) {
                // One encoding sequence cannot be a prefix of another.
                throw new IllegalArgumentException("duplicate encoding prefix "
                    + hex(b, 0, i + 1));
            }
            if (last) {
                map[n] = new Integer(c);
            } else {
                map = (Object[]) (o != null ? o : (map[n] = new Object[BYTES]));
            }
        }

        // Enter in char to byte-sequence map.
        int lo = (c & 0xFF), hi = ((c >> 8) & 0xFF);
        byte[] seq = new byte[bsize];
        for (int i = 0; i < bsize; i++) {
            seq[i] = b[i];
        }
        if (mChar2Bytes[hi] == null) {
            // Allocate 2nd tier map.
            byte[][] cb2 = (mChar2Bytes[hi] = new byte[BYTES][]);
            for (int j = 0; j < BYTES; j++) {
                cb2[j] = null;
            }
        }
        mChar2Bytes[hi][lo] = seq;
    }

    /**
     * Converts a string into a byte array of the given size.
     * If too short, the result is padded at the end with the encoding
     * of U+0020.  If too long, an exception is thrown.
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
        int len = in.length(), room = len * mMaxCode;
        if (room < min) {
            room = min;
        }
        int size = 0, c, lo, hi;
        byte[] buf = new byte[room], seq;
        for (int i = 0; i < len; i++) {
            c = (int) in.charAt(i);
            byte[][] map = mChar2Bytes[(c >> 8) & 0xFF];
            if (map == null || (seq = map[c & 0xFF]) == null) {
                throw new CoderException("char #" + i + " = "
                    + uname(c) + ", not encodable");
            }
            // Copy sequence to buffer.
            for (int j = 0; j < seq.length; j++) {
                buf[size++] = seq[j];
            }
        }
        if (0 <= max && max < size) {
            // Data overflow.
            throw new CoderException("too big: " + size + ", max=" + max);
        }
        while (size < min) {
            if (min < size + mPadding.length) {
                throw new CoderException("can't use " + mPadding.length
                    + "-byte padding (" + hex(mPadding) + ") for "
                    + (min - size) + " gap");
            }
            for (int i = 0; i < mPadding.length; i++) {
                buf[size++] = mPadding[i];
            }
        }
        // Create and return the filled buffer slice.
        byte[] out = new byte[size];
        while (size-- > 0) {
            out[size] = buf[size];
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
        int size = 0, c, last = from + length, n;
        for (int i = from; i < last; i++) {
            if ((c = mFirst[in[i] & 0xFF]) == NOCHAR) {
                // No mapping specified.
                throw new CoderException(i, "code = "
                    + bname(in[i]) + ", not decodable");
            } else {
                int j = i;
                while (c < 0) {
                    // Multi-byte encoding.
                    if (++j >= last) {
                        throw new CoderException(i, "unterminated sequence");
                    }
                    if ((n = mIndex[in[j] & 0xFF]) < 0) {
                        // Invalid secondary.
                        throw new CoderException(i, "part " + (j - i) + " = "
                            + bname(in[j]) + ", invalid continuation of"
                            + hex(in, i, j));
                    }
                    if ((c = mRest[n - c]) == NOCHAR) {
                        // No mapping specified.
                        throw new CoderException(i, " to " + j + ": code = "
                            + bname(in[i]) + ", not decodable");
                    }
                }
                i = j;
            }
            out[size++] = (char) c;
        }
        return new String(out, 0, size);
    }

    // Auxiliary for dump(PrintStream).
    private void dump(PrintStream ps, int block, String prefix) {
        ps.println();
        ps.println("# [" + prefix + "]: " + block);
        int c;
        for (int i = 0; i < mBlock; i++) {
            if ((c = mRest[block + i]) != NOCHAR) {
                if (c < 0) {
                    // Dump entire sub-map.
                    dump(ps, -c, prefix + hex2(mRevIndex[i]));
                } else {
                    ps.println(prefix + hex2(mRevIndex[i]) + " = " + hex4(c));
                }
            }
        }
    }

    public void dump(PrintStream ps) {
        ps.println("# Multi-byte encoding map dump.");
        ps.println("#-------------------------------");
        ps.println();
        for (int i = 0; i < BYTES; i++) {
            if (mFirst[i] != NOCHAR) {
                if (mFirst[i] < 0) {
                    dump(ps, -(mFirst[i]), hex2(i));
                } else {
                    ps.println(hex2(i) + " = " + hex4(mFirst[i]));
                }
            }
        }
        /*-
        ps.println("Rest:");
        for (int i = 0; i < mRest.length; i++) {
            ps.print(" " + (mRest[i] == NOCHAR
                ? "*"
                : (mRest[i] >= 0
                    ? uname(mRest[i])
                    : ("(" + Integer.toString(-mRest[i]) + ")"))));
        }
        -*/
        ps.println();
        for (int hi = 0; hi < BYTES; hi++) {
            if (mChar2Bytes[hi] != null) {
                for (int lo = 0; lo < BYTES; lo++) {
                    byte[] b = mChar2Bytes[hi][lo];
                    if (b != null) {
                        ps.println("# " + uname(hi * BYTES + lo) + " = "
                            + hex(b, 0, b.length));
                    }
                }
            }
        }
    }

    /**
     * Runs coder through basic testing.
     * Usage: main [-r] [path].
     * Option -r loads from resource, else read code from local file.
     *
     * @param args  command line arguments
     */
    public static void main(String[] args) {
        try {
            boolean res = false;
            String path = "mb.code";
            int opt = 0;
            if (args.length > opt && args[opt].equals("-r")) {
                res = true;
                opt++;
            }
            if (args.length > opt) {
                path = args[opt++];
            }
            MultiByteCoder sbc = (res
                ? new MultiByteCoder(path)
                : new MultiByteCoder(new File(path)));
            sbc.dump(System.out);
        } catch (Exception all) {
            all.printStackTrace(System.err);
            System.exit(1);
        }
    }

    @Override
    public String toString() {
        StringBuffer buf = new StringBuffer("MultiByteCoder@");
        buf.append(Integer.toHexString(hashCode()));
        return buf.toString();
    }
}
