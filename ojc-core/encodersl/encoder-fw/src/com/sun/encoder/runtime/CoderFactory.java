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
 * @(#)CoderFactory.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.LineNumberReader;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.sun.encoder.runtime.provider.BuiltInStringCoder;
import com.sun.encoder.runtime.provider.Latin1Coder;
import com.sun.encoder.runtime.provider.SingleByteCoder;
import com.sun.encoder.runtime.provider.MultiByteCoder;
import com.sun.encoder.runtime.provider.Utf8Coder;
import com.sun.encoder.runtime.provider.Utf16Coder;
import com.sun.encoder.util.UnicodeFile;

/**
 * Factory class to instantiate and share StringCoder objects.
 * See {@link com.sun.encoder.ud1.runtime.StringCoder}.
 *
 * The coder objects themselves do not have an intrinsic name; the names
 * exist to get them from the coder factory. The factory can test a name
 * for validity, but does not know all names in advance; there is a set
 * of "known" names that is returned by getCoderNameList(), but the factory
 * will accept names that start with certain prefixes and interpret the rest
 * of each such name depending on the prefix to construct a coder.
 * An example of a known name is "ascii", an example of a prefixed name is
 * "[cc]my.Coder" (will try to instantiate the "my.Coder" class) or
 * "[jc]UTF-8" (wraps up the Java built-in "UTF-8" coder as a StringCoder).
 *
 * @author Michael Libourel
 * @version
 */
public class CoderFactory {

    /**
     * A list of all available encoding names.
     */
    private static final List mNames = new ArrayList();

    // Maps String -&gt; String: aliased name to original encoding name.
    private static final Map mAlias = new HashMap();

    // Latin1 coder
    private static StringCoder mLatin1Coder = null;
    private static boolean mLatin1Shared = false;

    // Ascii coder
    private static StringCoder mAsciiCoder = null;

    /**
     * Code resource base directory.
     */
    private static final String CODE_DIR =
        "com/sun/encoder/runtime/provider/code/";

    private static final String CODE_MAP = CODE_DIR + "code.map";

    /**
     * Code resource suffix.
     */
    private static final String CODE_SUFFIX = ".code";

    /**
     * Maps String -&gt; StringCoder: encoding name to single-byte coder.
     */
    private static final Map mSingleCoders = new HashMap();

    /**
     * Maps String -&gt; StringCoder: encoding name to multi-byte coder.
     */
    private static final Map mMultiCoders = new HashMap();

    /**
     * Registers a single-byte resource-based coder.
     * Does not load the resource or coder instance yet.
     *
     * @param name  the encoding name, also basename of resource
     */
    private static void sbc(String name) {
        // LT: change to lower case
        if (name != null) {
            name = name.toLowerCase();
        }
        mSingleCoders.put(name, null);
        mNames.add(name);
    }

    /**
     * Registers a multi-byte resource-based coder.
     * Does not load the resource or coder instance yet.
     *
     * @param name  the encoding name, also basename of resource
     */
    private static void mbc(String name) {
        // LT: change to lower case
        if (name != null) {
            name = name.toLowerCase();
        }
        mMultiCoders.put(name, null);
        mNames.add(name);
    }

    /**
     * Registers an alias for an encoding.
     *
     * @param also  an alternative encoding name
     * @param name  the original encoding name
     */
    private static void alias(String also, String name) {
        // LT: change to lower case
        if (also != null) {
            also = also.toLowerCase();
        }
        if (name != null) {
            name = name.toLowerCase();
        }
        if (!mNames.contains(name)) {
            throw new RuntimeException("alias '" + also
                + "' refers to unknown '" + name + "'");
        }
        if (mAlias.containsKey(also)) {
            throw new RuntimeException("alias '" + also
                + "' already refers to '" + (String) mAlias.get(also) + "'");
        }
        mAlias.put(also, name);
        mNames.add(also);
    }

    /**
     * Special coder name prefix to signal use of local file.
     * This is used for testing. sb - single byte, mb - multiple bytes.
     */
    public static final String SBC_FILE = "[sb]";
    public static final String MBC_FILE = "[mb]";

    /**
     * Special coder name prefix to signal use of Java native coder.
     * This is used for testing.
     */
    public static final String BUILT_IN = "[jc]";

    /**
     * Special coder name prefix to signal use of a coder class by name.
     * This is used for testing.
     */
    public static final String FQ_CLASS = "[cc]";

    // Register all the coders.
    static { initFromResource(CODE_MAP); }

    /**
     * Tests for the existence of a string coder with the given name.
     *
     * @param name  the encoding name
     * @return true if available, else false
     * @throws IllegalArgumentException for unknown encoding
     */
    public static boolean hasCoder(String name)
        throws IllegalArgumentException {
        if (name == null) {
            throw new NullPointerException("no name");
        }
        // LT: change to lower case
        name = name.toLowerCase();
        if (name.startsWith(SBC_FILE) ||
            name.startsWith(MBC_FILE) ||
            name.startsWith(BUILT_IN) ||
            name.startsWith(FQ_CLASS)) {
            // Special hack for testing with local coder file.
            return true;
        }
        return mNames.contains(name);
    }

    /**
     * Fetches a string coder with the given name.
     *
     * @param name  the encoding name
     * @return the coder object
     * @throws IllegalArgumentException for unknown encoding.
     */
    public static StringCoder getCoder(String name)
        throws IllegalArgumentException {
        if (name == null) {
            throw new NullPointerException("No encoding name.");
        }
        // LT: change to lower case
        name = name.toLowerCase();
        String ref = (String) mAlias.get(name);
        if (ref == null) {
            ref = name;
        }
        if (ref.equals("iso-8859-1")) {
            if (mLatin1Coder == null) {
                mLatin1Coder = new Latin1Coder();
                mLatin1Shared = isShared(mLatin1Coder);
            }
            if (mLatin1Shared) {
                return mLatin1Coder;
            } else {
                return new Latin1Coder();
            }
        } else if (ref.equals("ascii")) {
            if (mAsciiCoder == null) {
                int[] ascii = new int[0x100];
                for (int i = 0; i < ascii.length; i++) {
                    ascii[i] = ((i < 0x80) ? i : SingleByteCoder.NOCHAR);
                }
                mAsciiCoder = new SingleByteCoder(ascii,
                    SingleByteCoder.NOCHAR, SingleByteCoder.NOBYTE);
            }
            return mAsciiCoder;
        } else if (ref.equals("utf-8")) {
            //NYI: share...
            return new Utf8Coder(false, false);
        } else if (ref.equals("utf-8-strict")) {
            //NYI: share...
            return new Utf8Coder(true, false);
        } else if (ref.equals("utf-8-valid")) {
            //NYI: share...
            return new Utf8Coder(false, true);
        } else if (ref.equals("utf-8-strict-valid")) {
            //NYI: share...
            return new Utf8Coder(true, true);
        } else if (ref.equals("utf-16")) {
            //NYI: share...
            return new Utf16Coder(false, false);
        } else if (ref.equals("utf-16-little")) {
            //NYI: share...
            return new Utf16Coder(true, false);
        } else if (ref.equals("utf-16-valid")) {
            //NYI: share...
            return new Utf16Coder(false, true);
        } else if (ref.equals("utf-16-little-valid")) {
            //NYI: share...
            return new Utf16Coder(true, true);
        } else if (mSingleCoders.containsKey(ref)) {
            // We assume sharability.
            StringCoder coder = (StringCoder) mSingleCoders.get(ref);
            if (coder == null) {
                synchronized (mSingleCoders) {
                    String res = CODE_DIR + ref + CODE_SUFFIX;
                    try {
                        coder = new SingleByteCoder(res);
                    } catch (IOException e) {
                        throw new RuntimeException("can't get encoding '" + ref
                            + "' using resource '" + res + "': "
                            + e.getMessage());
                    }
                    mSingleCoders.put(ref, coder);
                }
            }
            return coder;
        } else if (mMultiCoders.containsKey(ref)) {
            // We assume sharability.
            StringCoder coder = (StringCoder) mMultiCoders.get(ref);
            if (coder == null) {
                synchronized (mMultiCoders) {
                    String res = CODE_DIR + ref + CODE_SUFFIX;
                    try {
                        coder = new MultiByteCoder(res);
                    } catch (IOException e) {
                        throw new RuntimeException("can't get encoding '" + ref
                            + "' using resource '" + res + "': "
                            + e.getMessage());
                    }
                    mMultiCoders.put(ref, coder);
                }
            }
            return coder;
        } else if (ref.startsWith(SBC_FILE)) {
            // Creates encoding from local file, for testing.
            // We assume sharability.
            StringCoder coder = (StringCoder) mSingleCoders.get(ref);
            if (coder == null) {
                synchronized (mSingleCoders) {
                    String file = ref.substring(SBC_FILE.length());
                    try {
                        coder = new SingleByteCoder(new File(file));
                    } catch (IOException e) {
                        throw new RuntimeException("can't get SBC encoding '"
                            + ref + "' using file '" + file + "': "
                            + e.getMessage());
                    }
                    mSingleCoders.put(ref, coder);
                }
            }
            return coder;
        } else if (ref.startsWith(MBC_FILE)) {
            // Creates encoding from local file, for testing.
            // We assume sharability.
            StringCoder coder = (StringCoder) mMultiCoders.get(ref);
            if (coder == null) {
                synchronized (mMultiCoders) {
                    String file = ref.substring(MBC_FILE.length());
                    try {
                        coder = new MultiByteCoder(new File(file));
                    } catch (IOException e) {
                        throw new RuntimeException("can't get MBC encoding '"
                            + ref + "' using file '" + file + "': "
                            + e.getMessage());
                    }
                    mMultiCoders.put(ref, coder);
                }
            }
            return coder;
        } else if (ref.startsWith(BUILT_IN)) {
            // Creates encoding from Java built-in encoding.
            // We assume non-sharability.
            String jtag = ref.substring(BUILT_IN.length());
            try {
                return new BuiltInStringCoder(jtag);
            } catch (UnsupportedEncodingException e) {
                throw new RuntimeException("can't get built-in encoding '"
                    + ref + "' using name '" + jtag + "': "
                    + e.getMessage());
            }
        } else if (ref.startsWith(FQ_CLASS)) {
            // Creates coder class instance.
            String fqName = ref.substring(FQ_CLASS.length());
            try {
                Object obj = CoderFactory.class.getClassLoader()
                    .loadClass(fqName).newInstance();
                if (obj instanceof StringCoder) {
                    return (StringCoder) obj;
                }
                throw new RuntimeException("non-coder class for encoding '"
                    + ref + "' using name '" + fqName + "'");
            } catch (ClassNotFoundException e) {
                throw new RuntimeException("can't load class for encoding '"
                    + ref + "' using name '" + fqName + "': "
                    + e.getMessage());
            } catch (IllegalAccessException e) {
                throw new RuntimeException("can't access class for encoding '"
                    + ref + "' using name '" + fqName + "': "
                    + e.getMessage());
            } catch (InstantiationException e) {
                throw new RuntimeException("can't instantiate for encoding '"
                    + ref + "' using name '" + fqName + "': "
                    + e.getMessage());
            } catch (ExceptionInInitializerError e) {
                throw new RuntimeException("can't initialize for encoding '"
                    + ref + "' using name '" + fqName + "': "
                    + e.getMessage());
            } catch (SecurityException e) {
                throw new RuntimeException("can't secure class for encoding '"
                    + ref + "' using name '" + fqName + "': "
                    + e.getMessage());
            }
        }
        throw new IllegalArgumentException("unknown encoding name '"
            + name + "'");
    }

    /**
     * Fetches an encoding transformer between the given two encodings.
     *
     * @param from  the input encoding name
     * @param to  the output encoding name
     * @return the transcoder object
     */
    public static TransCoder getTrans(String from, String to) {
        if (from == null) {
            throw new NullPointerException("No input encoding name.");
        }
        if (to == null) {
            throw new NullPointerException("No output encoding name.");
        }
        // LT: change to lower case
        from = from.toLowerCase();
        to = to.toLowerCase();

        String iref = (String) mAlias.get(from);
        String oref = (String) mAlias.get(to);
        if (iref == null) {
            iref = from;
        }
        if (oref == null) {
            oref = to;
        }
        if (iref.equals(oref)) {
            return mSame;
        } else {
            StringCoder inCoder = getCoder(from);
            StringCoder outCoder = getCoder(to);
            return new SimpleTransCoder(inCoder, outCoder);
        }
    }

    /**
     * Fetches an encoding transformer that does nothing.
     * This is useful from cases where we syntactically need a transformer,
     * but we do not want any change in the data.
     *
     * @return the transcoder object
     */
    public static TransCoder getEchoCoder() {
        return mSame;
    }

    private static final TransCoder mSame = new SameTransCoder();

    /**
     * A singleton class for dummy transcoding, where input and output
     * are identical.
     */
    private static class SameTransCoder
        implements TransCoder {

        /**
         * Return the input byte array if "readOnly" is true, or return
         * a copy of input byte array if "readOnly" is false.
         *
         * @param data  a byte array, or null
         * @param readOnly  if true, will not change or return "data"
         * @return original or a copy of data
         */
        public byte[] recode(byte[] data, boolean readOnly)
            throws CoderException {
            byte[] copy = data;
            if (readOnly && data != null) {
                copy = new byte[data.length];
                System.arraycopy(data, 0, copy, 0, data.length);
            }
            return copy;
        }

        @Override
        public String toString() {
            StringBuffer buf = new StringBuffer("SameTransCoder@");
            buf.append(Integer.toHexString(hashCode()));
            return buf.toString();
        }
    }

    /**
     * A simple class for transcoding, not optimized for possible
     * in-situ replacement.
     */
    private static class SimpleTransCoder
        implements TransCoder {

        private final StringCoder mInCoder;
        private final StringCoder mOutCoder;

        /**
         * Constructs from two coders.
         *
         * @param in  input coder
         * @param out  output coder
         */
        public SimpleTransCoder(StringCoder in, StringCoder out) {
            mInCoder = in;
            mOutCoder = out;
        }

        /**
         * Transforms the input to the output encoding.
         *
         * @param data  a byte array, or null
         * @param readOnly  if true, will not change or return "data"
         * @return byte array containing the recoded data, or null
         * @throws CoderException for decoding or encoding problems
         */
        public byte[] recode(byte[] data, boolean readOnly)
            throws CoderException {
            return mOutCoder.encode(mInCoder.decode(data));
        }

        @Override
        public String toString() {
            StringBuffer buf = new StringBuffer("SimpleTransCoder@");
            buf.append(Integer.toHexString(hashCode()));
            buf.append(" inCoder=").append(mInCoder);
            buf.append(" outCoder=").append(mOutCoder);
            return buf.toString();
        }
    }

    /**
     * Tests if given coder is safe to share.
     *
     * @param coder  the coder to test
     * @return true if SHARE property set to non-false, else false
     */
    public static boolean isShared(StringCoder coder) {
        if (coder == null) {
            throw new NullPointerException("No StringCoder.");
        }
        try {
            Object share = coder.getProperty(StringCoder.SHARE);
            return ((Boolean) share).booleanValue();
        } catch (IllegalArgumentException ia) {
            // Coder does not support "share" property, assume unsafe.
            return false;
        } catch (ClassCastException cc) {
            // Non-boolean "share" value, assume unsafe.
            return false;
        }
    }

    /**
     * Returns a list of recognized encoding names.
     *
     * @return a list of names
     */
    public static String[] getCoderNameList () {
        return (String[]) mNames.toArray(new String[mNames.size()]);
    }

    /**
     * Reads in the coder map defining the known names and aliases.
     * This sets mNames.
     *
     * @param path  the input file or reosurce path
     * @param in  the input stream
     */
    private static void init(String path, InputStream in)
        throws IOException {
        LineNumberReader r =
            new LineNumberReader(UnicodeFile.makeInputReader(in));
        String line;
        while ((line = r.readLine()) != null) {
            line = line.trim();
            if (!(line.equals("") || line.startsWith("#"))) {
                String s1 = line.substring(1).trim();
                switch (line.charAt(0)) {
                case 'a':
                    // Simple alias.
                    int pos = s1.indexOf(' ');
                    if (pos < 0) {
                        throw new RuntimeException(path + " line "
                            + r.getLineNumber() + ": no aliased name: "
                            + line);
                    }
                    String s2 = s1.substring(pos + 1).trim();
                    s1 = s1.substring(0, pos).trim();
                    // LT: change to lower case
                    s1 = s1.toLowerCase();
                    // LT: change to lower case
                    s2 = s2.toLowerCase();
                    alias(s1, s2);
                    break;
                case 'k':
                    // Known name.
                    // LT: change to lower case
                    s1 = s1.toLowerCase();
                    mNames.add(s1);
                    break;
                case 's':
                    // Single-byte coder.
                    // LT: change to lower case
                    s1 = s1.toLowerCase();
                    sbc(s1);
                    break;
                case 'm':
                    // Multi-byte coder.
                    // LT: change to lower case
                    s1 = s1.toLowerCase();
                    mbc(s1);
                    break;
                default:
                    throw new RuntimeException(path + " line "
                        + r.getLineNumber() + ": weird line: " + line);
                }
            }
        }
        in.close();
    }

    /**
     * Reads in the coder map defining the known names and aliases
     * from a resource {@link #CODE_MAP}.
     *
     * @param path  the input file or reosurce path
     * @param in  the input stream
     */
    private static void initFromResource(String path) {
        ClassLoader loader = CoderFactory.class.getClassLoader();
        InputStream is = loader.getResourceAsStream(path);
        if (is == null) {
            throw new RuntimeException("can't find code map resource '"
                + path + "'");
        }
        try {
            init(path, is);
        } catch (IOException ie) {
            // Not allowed to throw IOException from static init.
            throw new RuntimeException("can't read code map '" + path + "': "
                + ie.getMessage());
        }
    }

    /**
     * Test facility.
     * Usage: CoderFactory [code [input [output]]].
     * @param args arguments.
     */
    public static void main(String[] args) {
        try {
            if (args.length == 0) {
                // Print available encodings.
                String[] codes = getCoderNameList();
                for (int i = 0; i < codes.length; i++) {
                    System.out.println(codes[i]);
                }
            } else {
                String name = args[0];
                StringCoder coder = getCoder(name);
                System.out.println("coder class " + coder.getClass().getName());
                System.out.println("coder " + (isShared(coder) ? "is" : "not")
                    + " shared");
                if (args.length > 1) {
                    File input = new File(args[1]);
                    byte[] data = new byte[(int) input.length()];
                    FileInputStream fis = new FileInputStream(input);
                    fis.read(data);
                    fis.close();
                    String s = coder.decode(data);
                    System.out.println("decoded " + data.length + " bytes to "
                        + s.length() + " chars");
                    byte[] made = coder.encode(s);
                    System.out.println("encoded " + s.length() + " chars to "
                        + made.length + " bytes");
                    if (args.length > 2) {
                        File output = new File(args[2]);
                        FileOutputStream fos = new FileOutputStream(output);
                        fos.write(made);
                        fos.close();
                    }
                    if (data.length != made.length) {
                        throw new RuntimeException("length differs: "
                            + data.length + " bytes in, out " + made.length);
                    }
                    for (int i = 0; i < data.length; i++) {
                        if (data[i] != made[i]) {
                            throw new RuntimeException("data differs, byte #"
                                + i + ": " + data[i] + " versus " + made[i]);
                        }
                    }
                }
            }
        } catch (Exception all) {
            all.printStackTrace(System.err);
            System.exit(1);
        }
    }
}
