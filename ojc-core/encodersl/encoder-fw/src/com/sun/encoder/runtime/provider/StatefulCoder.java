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
 * @(#)StatefulCoder.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime.provider;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.LineNumberReader;
import java.io.PrintStream;
import java.util.List;
import java.util.ArrayList;

import com.sun.encoder.runtime.CoderException;
import com.sun.encoder.util.UnicodeFile;

/**
 * Generic coder class for stateful N-byte encodings configurable through a
 * code description file. The encodings do not have to be reversible.
 * See {@link com.sun.encoder.runtime.StringCoder}.
 *
 * The supported mappings assume a stateful model, where successive input
 * byte sequences can either change the state, or translate to one or more
 * Unicode code points, or both. If the encoding is not stateful and does
 * not map single sequences to multiple Unicode values, it may be more
 * easy and efficient to use {@link SingleByteCoder} or {@link MultiByteCoder}
 * instead.
 *
 * Note that the StringCoder interface it limited to translating whole
 * sequences and strings, there is no support for streaming (cf.
 * {@link com.sun.encoder.runtime.OtdInputStream}; hence, the state only
 * exists within each single conversion call.
 *
 * The decoding model is a follows: there are a number of named input states,
 * and a series of input values: byte values 00-FF and end-of-input.
 * One state is designated as the start-state. For every state, there is a
 * list of actions, each labelled with a sequence of bytes. An action is
 * either a Unicode sequence, or a state change, or both, or an error.
 * If any of the actions in the current state is labelled with a prefix
 * of the current input position, then we execute its action and consume the
 * input. If no labels match, we have a decoding error.
 *
 * The encoding model is similar, except that the action labels are now
 * Unicode sequences, and the actions can carry byte sequences instead.
 *
 * (NYI: document resource format...)
 * The resource can also contain empty lines, and comment lines starting
 * with "#".
 *
 * "@decode" {name}
 * ["&lt;" name "&gt;"] {byte} space {code} ["&gt;" name "&lt;"] [comment]
 *
 * @author Michael Libourel
 * @version
 */
public class StatefulCoder
    extends SharedCoder {

    private static final int BYTES = 0x100;
    public static final int NOCHAR = 0xFFFF;
    public static final byte MAXLEN = 8; // max. bytes per encoding sequence

    /**
     * All the information needed for decoding.
     */
    private static class CodeInfo {

        /**
         * Container fo all decoding rules in a specific state.
         */
        private static class CodeState {

            private final String mName;
            private byte mMaxLen = 0;

            /**
             * Creates with the given state name.
             *
             * @param name  the symbolic state name
             */
            public CodeState(String name) {
                mName = name;
            }

            /**
             * Class to represent a single decoder action.
             * If the input contains the mLabel sequence, then report an error
             * if mError is present, else emit mCodes (if not empty) and switch
             * input state to mState (if defined).
             */
            public static class CodeEffect {

                public CodeEffect mNext = null; // next to match
                public final byte[] mLabel; // input to match
                public final boolean mLast; // terminates input?
                public final String mCodes; // Unicode output, or null if none
                public final byte mState; // new state, or -1 if no change
                public final String mError; // error message, or null if okay

                public CodeEffect(byte[] label, boolean last, String codes,
                    byte state, String error, CodeEffect next) {
                    mLabel = label;
                    mCodes = codes;
                    mLast = last;
                    mState = state;
                    mError = error;
                    mNext = next;
                }
            }

            /**
             * Lists of effects in this state, indexed by 1st input byte + 1,
             * or index=0 for empty input sequence.
             */
            private CodeEffect[] mEffects = new CodeEffect[0x101];

            /**
             * Adds an effect to the state.
             */
            public void addEffect(byte[] label, boolean last, String codes,
                byte state, String error) {
                if (codes == null) {
                    throw new NullPointerException("no codes");
                }
                if (MAXLEN < label.length) {
                    throw new IllegalArgumentException("label " + hex(label)
                        + " too long, maximum is " + MAXLEN + " bytes");
                }
                if (mMaxLen < label.length) {
                    mMaxLen = (byte) label.length;
                }
                int index = ((label.length == 0) ? 0 : ((label[0] & 0xFF) + 1));
                mEffects[index] = new CodeEffect(label, last, codes, state,
                    error, mEffects[index]);
            }

            /**
             * Dumps contents to given output.
             * Meant for debugging only.
             *
             * @param out  the output stream
             */
            public void dump(PrintStream out) {
                for (int i = 0; i < 0x101; i++) {
                    CodeEffect d = mEffects[i];
                    if (d != null) {
                        out.println("   Effect "
                            + ((i == 0) ? "<END>" : bname(i - 1))
                            + ":");
                        for (; d != null; d = d.mNext) {
                            out.println("    label=" + hex(d.mLabel)
                                + (d.mLast ? "<END>" : "")
                                + ", codes=" + d.mCodes
                                + (d.mState < 0 ? "" : (", state=" + d.mState))
                                + (d.mError == null
                                    ? ""
                                    : (", error=" + d.mError)));
                        }
                    }
                }
            }

            /**
             * Tries to match the given input byte sequence in this state.
             * Returns the effect matched.
             *
             * @param data  the input data buffer
             * @param from  the offset to the start of available input
             * @param end  the first data positition not available
             * @return the effect matched, or null for none
             */
            public CodeEffect match(byte[] data, int from, int end) {
                int left = end - from;
                int init = ((left <= 0) ? 0 : ((data[from] & 0xFF) + 1));
                /*-
                System.out.println("[ match " + hex(data) + " (" + from
                    + "-" + end + "): left=" + left + ", init=" + init + " ]");
                -*/
                outer: for (CodeEffect d = mEffects[init]; d != null;
                    d = d.mNext) {
                    if (d.mLast
                        ? d.mLabel.length == left
                        : d.mLabel.length <= left) {
                        for (int i = d.mLabel.length; i-- > 0;) {
                            if (d.mLabel[i] != data[from + i]) {
                                continue outer;
                            }
                        }
                    }
                    return d;
                }
                return null;
            }

            /**
             * Tries to match the given input char sequence in this state.
             * Returns the effect matched.
             *
             * @param text  the input data buffer
             * @param from  the offset to the start of available input
             * @param end  the first data positition not available
             * @return the effect matched, or null for none
             */
            public CodeEffect match(String text, int from, int end) {
                int left = end - from;
                /*-
                System.out.println("[ match " + Misc.printable(text) + " ("
                    + from + "-" + end + "): left=" + left
                    + ", init=" + init + " ]");
                -*/
                outer: for (int b = 0; b <= 0x100; b++) {
                    for (CodeEffect d = mEffects[b]; d != null;
                        d = d.mNext) {
                        if (d.mLast
                            ? d.mCodes.length() == left
                            : d.mCodes.length() <= left) {
                            for (int i = d.mCodes.length(); i-- > 0;) {
                                if (d.mCodes.charAt(i) !=
                                    text.charAt(from + i)) {
                                    continue outer;
                                }
                            }
                        }
                        return d;
                    }
                }
                return null;
            }
        }

        /**
         * Decodes given slice of data.
         *
         * @param data  the input data buffer
         * @param from  the offset to the start of available input
         * @param to  the offset to the first byte past the input
         * @return the decoded string
         * @throws CoderException for undecodable data
         */
        public String decode(byte[] data, int from, int to)
            throws CoderException {
            byte state = 0;
            int pos = from;
            StringBuffer sb = new StringBuffer(to - from);
            CodeState.CodeEffect effect;
            System.out.println("[ decode " + hex(data) + ", from " + from
                + " to " + to + " ]");
            do {
                System.out.println("[ - state=" + state + ", pos=" + pos + " ]");
                if ((effect = mStates[state].match(data, pos, to)) == null) {
                    throw new CoderException(pos - from, "cannot decode");
                }
                System.out.println("[ - got " + hex(effect.mLabel)
                    + (effect.mLast ? "<END>" : "")
                    + (effect.mState < 0 ? "" : (", state=" + effect.mState))
                    + " ]");
                if (effect.mCodes != null) {
                    sb.append(effect.mCodes);
                }
                if (effect.mState >= 0) {
                    state = effect.mState;
                }
                pos += effect.mLabel.length;
            } while (!effect.mLast);
            return sb.toString();
        }

        /**
         * Encodes given slice of text.
         *
         * @param text  the input text buffer
         * @param min  the minimum length, or -1 for no minimum
         * @param max  the maximum length, or -1 for no maximum
         * @return the encoded data
         * @throws CoderException for unencodable text
         */
        public byte[] encode(String text, int min, int max, byte pad)
            throws CoderException {
            int len = text.length(), parts = 0, size = 0;
            byte[][] part = new byte[len][];
            byte state = 0;
            int pos = 0;
            CodeState.CodeEffect effect;
            System.out.println("[ encode " + Misc.printable(text) + " ]");
            do {
                System.out.println("[ - state=" + state + ", pos=" + pos + " ]");
                if ((effect = mStates[state].match(text, pos, len)) == null) {
                    throw new CoderException(pos, "cannot encode");
                }
                System.out.println("[ - got " + Misc.printable(effect.mCodes)
                    + (effect.mLast ? "<END>" : "")
                    + (effect.mState < 0 ? "" : (", state=" + effect.mState))
                    + " ]");
                if (effect.mLabel != null) {
                    part[parts++] = effect.mLabel;
                    size += effect.mLabel.length;
                    if (0 <= max && max < size) {
                        throw new CoderException("can't encode in "
                            + max + " bytes");
                    }
                }
                if (effect.mState >= 0) {
                    state = effect.mState;
                }
                pos += effect.mCodes.length();
            } while (!effect.mLast);
            if (size < min) {
                size = min;
            }
            byte[] data = new byte[size];
            pos = 0;
            for (int i = 0; i < parts; i++) {
                for (int j = 0; j < part[i].length; j++) {
                    data[pos++] = part[i][j];
                }
            }
            while (pos < size) {
                // Add tabbing to end.
                data[pos++] = pad;
            }
            return data;
        }

        /**
         * A list of state names, indexed by state number.
         */
        private final String[] mNames;

        /**
         * The state descriptors, indexed by state number.
         */
        private final CodeState[] mStates;

        public CodeInfo(String[] states) {
            mNames = states;
            mStates = new CodeState[states.length];
            for (int i = 0; i < states.length; i++) {
                mStates[i] = new CodeState(states[i]);
            }
        }

        /**
         * Adds an effect to the given state.
         *
         * @param in  the state in which the effect works
         * @param the resulting state, or -1 for same
         */
        public void addEffect(byte in, byte[] label, boolean last,
            String codes, byte state, String error) {
            if (in < 0 || mStates.length <= in) {
                throw new IllegalArgumentException("invalid state: " + in);
            }
            mStates[in].addEffect(label, last, codes, state, error);
        }

        /**
         * Dumps contents to given output.
         * Meant for debugging only.
         *
         * @param out  the output stream
         */
        public void dump(PrintStream out, String what) {
            out.println(" " + what);
            for (int i = 0; i < mNames.length; i++) {
                out.println("  State " + i + " = " + mNames[i]);
                mStates[i].dump(out);
            }
        }
    }

    /**
     * Dumps contents to given output.
     * Meant for debugging only.
     *
     * @param out  the output stream
     */
    public void dump(PrintStream out) {
        out.println("Stateful Coder");
        if (mDecode != null) {
            mDecode.dump(out, "Decodes");
        }
        if (mEncode != null) {
            mEncode.dump(out, "Encodes");
        }
    }

    /**
     * Reader for the decoder resource file.
     */
    private static class ResourceReader {

        public String mText = null;
        private final LineNumberReader mReader;

        public ResourceReader(InputStream is)
            throws IOException {
            mReader = new LineNumberReader(UnicodeFile.makeInputReader(is));
        }

        /**
         * Pulls in next line, if any.
         *
         * @return true if line available, false on EOF
         */
        public boolean next()
            throws IOException {
            if ((mText = mReader.readLine()) == null) {
                mReader.close();
                return false;
            }
            skipSpace();
            return true;
        }

        /**
         * Reports error in input format.
         *
         * @param message  the error string
         */
        public void fail(String message)
            throws IOException {
            throw new IOException("line " + mReader.getLineNumber() + ": "
                + message);
        }

        /**
         * Skips spaces and eats comment, if any.
         */
        private void skipSpace() {
            int len = mText.length();
            for (int pos = 0; pos < len; pos++) {
                char c = mText.charAt(pos);
                if (c == '#') {
                    // Comment to end of line.
                    break;
                }
                if (c != ' ' && c != '\t') {
                    if (pos > 0) {
                        mText = mText.substring(pos);
                    }
                    return;
                }
            }
            mText = "";
        }

        /**
         * Tests if remainder of line is empty.
         */
        public boolean empty() {
            skipSpace();
            return mText.length() == 0;
        }

        /**
         * Ensures line has been processed.
         */
        public void done()
            throws IOException {
            if (!empty()) {
                fail("trailing junk: '" + mText + "'");
            }
        }


        /**
         * Eats given string name, if any.
         *
         * @return true if encountered, else false
         */
        public boolean get(String key) {
            skipSpace();
            if (!mText.startsWith(key)) {
                return false;
            }
            mText = mText.substring(key.length());
            return true;
        }

        /**
         * Eats initial bracketed state name, if any.
         *
         * @return the state, or -1 if none
         */
        public byte getState(String[] names)
            throws IOException {
            skipSpace();
            if (!mText.startsWith("<")) {
                return -1;
            }
            int pos = mText.indexOf('>');
            if (pos < 0) {
                fail("'<' not followed by '>'");
            }
            String name = mText.substring(1, pos);
            for (int i = 0; i < names.length; i++) {
                if (names[i].equals(name)) {
                    mText = mText.substring(pos + 1);
                    return (byte) i;
                }
            }
            fail("unknown state <" + name + ">");
            return -1; // never
        }

        /**
         * Eats code value, if any.
         *
         * @return the value, or -1 if none
         */
        public int getCode() {
            skipSpace();
            int result = 0, pos = 0, nibble;
            while (pos < mText.length() &&
                (nibble = "0123456789ABCDEF".indexOf(mText.charAt(pos))) >= 0) {
                result = (result << 4) + nibble;
                pos++;
            }
            if (pos > 0) {
                mText = mText.substring(pos);
                return result;
            }
            return -1;
        }

        /**
         * Eats state name, if any.
         *
         * @return the value, or -1 if none
         */
        public String getName() {
            skipSpace();
            int pos = 0;
            for (int len = mText.length(); pos < len; pos++) {
                char c = mText.charAt(pos);
                if (c < 'a' || 'z' < c) {
                    break;
                }
            }
            if (pos == 0) {
                return null;
            }
            String name = mText.substring(0, pos);
            mText = mText.substring(pos);
            return name;
        }

        // EOD marker in effect.
        private static final String END = "$";

        /**
         * Gets the end-of-data marker, if any.
         */
        public boolean getEnd() {
            skipSpace();
            if (mText.startsWith(END)) {
                mText = mText.substring(END.length());
                return true;
            }
            return false;
        }
    }

    // Longest encoding found.
    private int mMaxCode = 0;

    public CodeInfo mDecode = null;
    public CodeInfo mEncode = null;
    public byte mPad = (byte) 0x20;

    /**
     * Constructs from a descriptor file.
     * The list maps the byte values used in the encoding to corresponding
     * Unicode code points.
     * See class description for file format.
     *
     * @param input  the descriptor file
     * @throws IOException for input access or descriptor syntax problems
     */
    public StatefulCoder(File input)
        throws IOException {
        InputStream is = new FileInputStream(input);
        init(input.getPath(), is);
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
    public StatefulCoder(String resource)
        throws IOException {
        ClassLoader loader = StatefulCoder.class.getClassLoader();
        /*-
        System.out.println("[ loading resource " + resource + " ]");
        -*/
        InputStream is = loader.getResourceAsStream(resource);
        if (is == null) {
            throw new IOException("can't find code table resource '"
                + resource + "'");
        }
        init(resource, is);
        is.close();
    }

    private byte[] getBytes(ResourceReader reader) {
        byte[] buf = new byte[MAXLEN];
        int pos = 0, b;
        while (pos < MAXLEN && (b = reader.getCode()) >= 0) {
            buf[pos++] = (byte) b;
        }
        byte[] bytes = new byte[pos];
        while (pos-- > 0) {
            bytes[pos] = buf[pos];
        }
        return bytes;
    }

    private String getChars(ResourceReader reader) {
        char[] buf = new char[MAXLEN];
        int pos = 0, c;
        pos = 0;
        while (pos < MAXLEN && (c = reader.getCode()) >= 0) {
            buf[pos++] = (char) c;
        }
        return new String(buf, 0, pos);
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
    public void init(String path, InputStream is)
        throws IOException {
        if (is == null) {
            throw new NullPointerException("no input stream");
        }
        boolean decode = true;
        ResourceReader reader = new ResourceReader(is);
        String[] states = null;
        byte[] label = null;
        String codes = null;
        while (reader.next()) {
            System.out.println("[ rule <" + reader.mText + "> ]");
            if (reader.empty()) {
                continue;
            }
            if (reader.get("@pad")) {
                mPad = (byte) reader.getCode();
                continue;
            }
            if (reader.get("@decode")) {
                // Syntax: "@decode" { <state-name> }
                if (mDecode != null) {
                    reader.fail("duplicate @decode line");
                }
                List names = new ArrayList();
                String name;
                while ((name = reader.getName()) != null) {
                    names.add(name);
                }
                states = new String[names.size()];
                names.toArray(states);
                mDecode = new CodeInfo(states);
                continue;
            }
            if (reader.get("@encode")) {
                // Syntax: "@encode" { <state-name> }
                if (mEncode != null) {
                    reader.fail("duplicate @encode line");
                }
                List names = new ArrayList();
                String name;
                while ((name = reader.getName()) != null) {
                    names.add(name);
                }
                states = new String[names.size()];
                names.toArray(states);
                mEncode = new CodeInfo(states);
                decode = false;
                continue;
            }
            if ((decode ? mDecode : mEncode) == null) {
                reader.fail("missing @decode/@encode line");
            }
            byte from = reader.getState(states);
            if (decode) {
                label = getBytes(reader);
            } else {
                codes = getChars(reader);
            }
            boolean end = reader.getEnd();
            reader.skipSpace();
            if (!reader.get(":")) {
                reader.fail("expected ':', not '" + reader.mText + "'");
            }
            if (decode) {
                codes = getChars(reader);
            } else {
                label = getBytes(reader);
            }
            byte into = reader.getState(states);
            reader.done();
            if (decode) {
                mDecode.addEffect(from, label, end, codes, into, null);
            } else {
                mEncode.addEffect(from, label, end, codes, into, null);
            }
        }
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
        if (mEncode == null) {
            throw new CoderException("no encoding rules");
        }
        if (in == null) {
            return null;
        }
        if (0 <= min && 0 <= max && max < min) {
            // Conflicting bounds.
            throw new IllegalArgumentException("max (" + max + ") < min ("
                + min + ")");
        }
        return mEncode.encode(in, min, max, mPad);
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
        if (mDecode == null) {
            throw new CoderException("No decoding rules.");
        }
        if (in == null) {
            return null;
        }
        if (from < 0 || in.length < from || length < 0
            || in.length < from + length) {
            throw new CoderException("invalid size/from/length: "
                + in.length + "/" + from + "/" + length);
        }
        return mDecode.decode(in, from, from + length);
    }

    /**
     * Runs coder through basic testing.
     * Usage: main [-r] [path].
     * Option -r loads from resource, else read code from local file.
     *
     * @param args  command line arguments
     */
    public static void main (String[] args) {
        try {
            /*-
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
            StatefulCoder sfc = (res
                ? new StatefulCoder(path)
                : new StatefulCoder(new File(path)));
            -*/
            StatefulCoder sfc = new StatefulCoder(new File("sfc.code"));
            sfc.dump(System.out);
            String s = sfc.decode(new byte[] { 3, 4 });
            System.out.println("[ decoded <" + s + "> ]");
            byte[] b = sfc.encode("abc", 6, -1);
            System.out.println("[ encoded <" + Misc.printable(b) + "> ]");
        } catch (Exception all) {
            all.printStackTrace(System.err);
            System.exit(1);
        }
    }

    @Override
    public String toString() {
        StringBuffer buf = new StringBuffer("StatefulCoder@");
        buf.append(Integer.toHexString(hashCode()));
        return buf.toString();
    }
}
