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
 * @(#)NamespaceUtility.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.bpel.xml;

import java.io.ByteArrayInputStream;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.Charset;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Utility class
 * XML uses inband encoding detection - this class obtains it.
 *
 * this implementation is based on dt UnicodeReader version
 *
 * @author Vitaly Bychkov
 */
public class EncodingUtil {

    private EncodingUtil(){
    }

    // heuristic constant guessing max prolog length
    private static final int EXPECTED_PROLOG_LENGTH = 1000;
    private static final String UTF8 = "UTF8";
    private static final Logger logger = Logger.getLogger(EncodingUtil.class.getName());

    public static Reader getUnicodeReader(InputStream in) throws IOException {
        String enc = detectEncoding(in);
        if (enc == null) {
            enc = UTF8;
        }
        return getUnicodeReader(in, enc);
    }

    /**
     * Returns a specialized reader that allows to skip BOM marks.
     * See http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4508058.
     * See https://open-esb.dev.java.net/issues/show_bug.cgi?id=1128.
     * @param in
     * @param encoding
     * @return a specialized reader that allows to skip BOM marks.
     */
    public static Reader getUnicodeReader(InputStream in, String encoding) {
        return new UnicodeReader(in, encoding);
    }

    /**
     * Detect input stream encoding.
     * The stream stays intact.
     * @return java encoding names ("UTF8", "ASCII", etc.) or null
     * if the stream is not markable or enoding cannot be detected.
     */
    public static String detectEncoding(InputStream in) throws IOException {
        return doDetectEncoding(in);
    }

    /**
     * Checks the validity of an encoding string.
     */
    public static boolean isValidEncoding(String encoding) {
        boolean valid = true;
        try {
            Charset.forName(encoding);
        } catch (Exception ex) {
            valid = false;
        }
        return valid;
    }

    /**
     * Returns the Java encoding name for the specified IANA encoding name.
     *
     * @param ianaEncoding
     * @return
     */
    public static String getIANA2JavaMapping(String ianaEncoding) {
        String java = (String) encodingIANA2JavaMap.get (ianaEncoding.toUpperCase ());
        return java == null ? ianaEncoding : java;
}

    /**
     * Returns the IANA encoding name for the specified Java encoding name.
     *
     * @param ianaEncoding
     * @return
     */
    public static String getJava2IANAMapping(String javaEncoding) {
        String iana = (String) encodingJava2IANAMap.get (javaEncoding);
        return iana == null ? javaEncoding : iana;
    }

    /** Detect input stream encoding.
    * The stream stays intact.
    * @return java encoding names ("UTF8", "ASCII", etc.) or null
    * if the stream is not markable or enoding cannot be detected.
    */
    private static String doDetectEncoding(InputStream in) throws IOException {

        if (! in.markSupported()) {
            logger.log(Level.WARNING, "EncodingHelper got unmarkable stream: " + in.getClass()); // NOI18N
            return null;
        }

        try {
            in.mark(EXPECTED_PROLOG_LENGTH);

            byte[] bytes = new byte[EXPECTED_PROLOG_LENGTH];
            for (int i = 0; i<bytes.length; i++) {
                try {
                    int datum = in.read();
                    if (datum == -1) break;
                    bytes[i] = (byte) datum;
                } catch (EOFException ex) {
                }
            }

            String enc = autoDetectEncoding(bytes);
            if (enc == null) return null;

            enc = detectDeclaredEncoding(bytes, enc);
            if (enc == null) return null;

            return getIANA2JavaMapping(enc);
        } finally {
            in.reset();
        }
    }


    /**
     * @return Java encoding family identifier or <tt>null</tt> for unrecognized
     */
    static String autoDetectEncoding(byte[] buf) throws IOException {


        if (buf.length >= 4) {
            switch (buf[0]) {
                case 0:
                    // byte order mark of (1234-big endian) or (2143) USC-4
                    // or '<' encoded as UCS-4 (1234, 2143, 3412) or UTF-16BE
                    if (buf[1] == (byte)0x3c && buf[2] == (byte)0x00 && buf[3] == (byte)0x3f) {
                        return "UnicodeBigUnmarked";
                    }
                    // else it's probably UCS-4
                    break;

                case 0x3c:
                    switch (buf[1]) {
                        // First character is '<'; could be XML without
                        // an XML directive such as "<hello>", "<!-- ...", // NOI18N
                        // and so on.

                        // 3c 00 3f 00 UTF-16 little endian
                        case 0x00:
                            if (buf [2] == (byte)0x3f && buf [3] == (byte)0x00) {
                                return  "UnicodeLittleUnmarked";
                            }
                            break;

                        // 3c 3f 78 6d == ASCII and supersets '<?xm'
                        case '?':
                            if (buf [2] == 'x' && buf [3] == 'm') {
                                return  "UTF8"; // NOI18N
                            }
                            break;
                    }
                    break;

                // 4c 6f a7 94 ... some EBCDIC code page
                case 0x4c:
                    if (buf[1] == (byte)0x6f && buf[2] == (byte)0xa7 && buf[3] == (byte)0x94) {
                        return "Cp037"; // NOI18N
                    }
                    break;

                // UTF-16 big-endian marked
                case (byte)0xfe:
                    if (buf[1] == (byte)0xff && (buf[2] != 0 || buf[3] != 0)) {
                        return  "UnicodeBig"; // NOI18N
                    }
                    break;

                // UTF-16 little-endian marked
                case (byte)0xff:
                    if (buf[1] == (byte)0xfe && (buf[2] != 0 || buf[3] != 0)) {
                        return "UnicodeLittle"; // NOI18N
                    }
                    break;

                // UTF-8 byte order mark
                case (byte)0xef:
                    if (buf[1] == (byte)0xbb && buf[2] == (byte)0xbf) {
                        return "UTF8";  //NOI18N
                    }
                    break;

            }
        }

        return null;
    }

    /**
     * Look for encoding='' anyway stop at <tt>?></tt>
     * @return found encoding or null if none declared
     */
    static String detectDeclaredEncoding(byte[] data, String baseEncoding) throws IOException {

        StringBuffer buf = new StringBuffer();
        Reader r;
        char delimiter = '"';

        r = new InputStreamReader(new ByteArrayInputStream(data), baseEncoding);
        try {
            for (int c = r.read(); c != -1; c = r.read()) {
                buf.append((char)c);
            }
        } catch (IOException ex) {
            // EOF of data out of boundary
            // dont care try to guess from given data
        }

        String s = buf.toString();

        int iend = s.indexOf("?>");
        iend = iend == -1 ? s.length() : iend;

        int iestart = s.indexOf("encoding");
        if (iestart == -1 || iestart > iend) return null;

        char[] chars = s.toCharArray();

        int i = iestart;

        for (; i<iend; i++) {
            if (chars[i] == '=') break;
        }

        for (; i<iend; i++) {
            if (chars[i] == '\'' || chars[i] == '"') {
                delimiter = chars[i];
                break;
            }

        }

        i++;

        int ivalstart = i;
        for (; i<iend; i++) {
            if (chars[i] == delimiter) {
                return new String(chars, ivalstart, i - ivalstart);
            }
        }

        return null;
    }

    /**
     * IANA to Java encoding mappings
     */
    final static Map<String, String> encodingIANA2JavaMap = new TreeMap<String, String>();
    final static Map<String, String> encodingIANAAliasesMap = new TreeMap<String, String>();
    final static Map<String, String> encodingJava2IANAMap = new TreeMap<String, String>();

    /**
     * Static initialization
     */
    static {
        encodingIANA2JavaMap.put("BIG5", "Big5"); // NOI18N
        encodingIANAAliasesMap.put("BIG5", "BIG5"); // NOI18N

        encodingIANA2JavaMap.put("IBM037", "CP037");  // NOI18N
        encodingIANAAliasesMap.put("IBM037", "IBM037"); // NOI18N
        encodingIANAAliasesMap.put("EBCDIC-CP-US", "IBM037"); // NOI18N
        encodingIANAAliasesMap.put("EBCDIC-CP-CA", "IBM037"); // NOI18N
        encodingIANAAliasesMap.put("EBCDIC-CP-NL", "IBM037"); // NOI18N
        encodingIANAAliasesMap.put("EBCDIC-CP-WT", "IBM037"); // NOI18N

        encodingIANA2JavaMap.put("IBM277", "CP277");  // NOI18N
        encodingIANAAliasesMap.put("IBM277", "IBM277"); // NOI18N
        encodingIANAAliasesMap.put("EBCDIC-CP-DK", "IBM277"); // NOI18N
        encodingIANAAliasesMap.put("EBCDIC-CP-NO", "IBM277"); // NOI18N

        encodingIANA2JavaMap.put("IBM278", "CP278");  // NOI18N
        encodingIANAAliasesMap.put("IBM278", "IBM278"); // NOI18N
        encodingIANAAliasesMap.put("EBCDIC-CP-FI", "IBM278"); // NOI18N
        encodingIANAAliasesMap.put("EBCDIC-CP-SE", "IBM278"); // NOI18N

        encodingIANA2JavaMap.put("IBM280", "CP280");  // NOI18N
        encodingIANAAliasesMap.put("IBM280", "IBM280"); // NOI18N
        encodingIANAAliasesMap.put("EBCDIC-CP-IT", "IBM280"); // NOI18N

        encodingIANA2JavaMap.put("IBM284", "CP284");  // NOI18N
        encodingIANAAliasesMap.put("IBM284", "IBM284"); // NOI18N
        encodingIANAAliasesMap.put("EBCDIC-CP-ES", "IBM284"); // NOI18N

        encodingIANA2JavaMap.put("IBM285", "CP285");  // NOI18N
        encodingIANAAliasesMap.put("IBM285", "IBM285"); // NOI18N
        encodingIANAAliasesMap.put("EBCDIC-CP-GB", "IBM285"); // NOI18N

        encodingIANA2JavaMap.put("IBM297", "CP297");  // NOI18N
        encodingIANAAliasesMap.put("IBM297", "IBM297"); // NOI18N
        encodingIANAAliasesMap.put("EBCDIC-CP-FR", "IBM297"); // NOI18N

        encodingIANA2JavaMap.put("IBM424", "CP424");  // NOI18N
        encodingIANAAliasesMap.put("IBM424", "IBM424"); // NOI18N
        encodingIANAAliasesMap.put("EBCDIC-CP-HE", "IBM424"); // NOI18N

        encodingIANA2JavaMap.put("IBM500", "CP500");  // NOI18N
        encodingIANAAliasesMap.put("IBM500", "IBM500"); // NOI18N
        encodingIANAAliasesMap.put("EBCDIC-CP-CH", "IBM500"); // NOI18N
        encodingIANAAliasesMap.put("EBCDIC-CP-BE", "IBM500"); // NOI18N

        encodingIANA2JavaMap.put("IBM870", "CP870");  // NOI18N
        encodingIANAAliasesMap.put("IBM870", "IBM870"); // NOI18N
        encodingIANAAliasesMap.put("EBCDIC-CP-ROECE", "IBM870"); // NOI18N
        encodingIANAAliasesMap.put("EBCDIC-CP-YU", "IBM870"); // NOI18N

        encodingIANA2JavaMap.put("IBM871", "CP871");  // NOI18N
        encodingIANAAliasesMap.put("IBM871", "IBM871"); // NOI18N
        encodingIANAAliasesMap.put("EBCDIC-CP-IS", "IBM871"); // NOI18N

        encodingIANA2JavaMap.put("IBM918", "CP918");  // NOI18N
        encodingIANAAliasesMap.put("IBM918", "IBM918"); // NOI18N
        encodingIANAAliasesMap.put("EBCDIC-CP-AR2", "IBM918"); // NOI18N

        encodingIANA2JavaMap.put("EUC-JP", "EUCJIS"); // NOI18N
        encodingIANAAliasesMap.put("EUC-JP", "EUC-JP"); // NOI18N

        encodingIANA2JavaMap.put("EUC-KR", "KSC5601"); // NOI18N
        encodingIANAAliasesMap.put("EUC-KR", "EUC-KR");  // NOI18N

        encodingIANA2JavaMap.put("GB2312", "GB2312"); // NOI18N
        encodingIANAAliasesMap.put("GB2312", "GB2312"); // NOI18N

        encodingIANA2JavaMap.put("ISO-2022-JP", "JIS");  // NOI18N
        encodingIANAAliasesMap.put("ISO-2022-JP", "ISO-2022-JP"); // NOI18N

        encodingIANA2JavaMap.put("ISO-2022-KR", "ISO2022KR");   // NOI18N
        encodingIANAAliasesMap.put("ISO-2022-KR", "ISO-2022-KR"); // NOI18N

        encodingIANA2JavaMap.put("ISO-8859-1", "8859_1");     // NOI18N
        encodingIANAAliasesMap.put("ISO-8859-1", "ISO-8859-1"); // NOI18N
        encodingIANAAliasesMap.put("LATIN1", "ISO-8859-1"); // NOI18N
        encodingIANAAliasesMap.put("L1", "ISO-8859-1"); // NOI18N
        encodingIANAAliasesMap.put("IBM819", "ISO-8859-1"); // NOI18N
        encodingIANAAliasesMap.put("CP819", "ISO-8859-1"); // NOI18N

        encodingIANA2JavaMap.put("ISO-8859-2", "8859_2");     // NOI18N
        encodingIANAAliasesMap.put("ISO-8859-2", "ISO-8859-2"); // NOI18N
        encodingIANAAliasesMap.put("LATIN2", "ISO-8859-2"); // NOI18N
        encodingIANAAliasesMap.put("L2", "ISO-8859-2"); // NOI18N

        encodingIANA2JavaMap.put("ISO-8859-3", "8859_3");     // NOI18N
        encodingIANAAliasesMap.put("ISO-8859-3", "ISO-8859-3"); // NOI18N
        encodingIANAAliasesMap.put("LATIN3", "ISO-8859-3"); // NOI18N
        encodingIANAAliasesMap.put("L3", "ISO-8859-3"); // NOI18N

        encodingIANA2JavaMap.put("ISO-8859-4", "8859_4");     // NOI18N
        encodingIANAAliasesMap.put("ISO-8859-4", "ISO-8859-4"); // NOI18N
        encodingIANAAliasesMap.put("LATIN4", "ISO-8859-4"); // NOI18N
        encodingIANAAliasesMap.put("L4", "ISO-8859-4"); // NOI18N

        encodingIANA2JavaMap.put("ISO-8859-5", "8859_5");     // NOI18N
        encodingIANAAliasesMap.put("ISO-8859-5", "ISO-8859-5"); // NOI18N
        encodingIANAAliasesMap.put("CYRILLIC", "ISO-8859-5"); // NOI18N

        encodingIANA2JavaMap.put("ISO-8859-6", "8859_6");     // NOI18N
        encodingIANAAliasesMap.put("ISO-8859-6", "ISO-8859-6"); // NOI18N

        encodingIANA2JavaMap.put("ISO-8859-7", "8859_7");     // NOI18N
        encodingIANAAliasesMap.put("ISO-8859-7", "ISO-8859-7"); // NOI18N
        encodingIANAAliasesMap.put("GREEK", "ISO-8859-7"); // NOI18N
        encodingIANAAliasesMap.put("GREEK8", "ISO-8859-7"); // NOI18N

        encodingIANA2JavaMap.put("ISO-8859-8", "8859_8");     // NOI18N
        encodingIANAAliasesMap.put("ISO-8859-8", "ISO-8859-8"); // NOI18N
        encodingIANAAliasesMap.put("HEBREW", "ISO-8859-8"); // NOI18N

        encodingIANA2JavaMap.put("ISO-8859-9", "8859_9");     // NOI18N
        encodingIANAAliasesMap.put("ISO-8859-9", "ISO-8859-9"); // NOI18N
        encodingIANAAliasesMap.put("LATIN5", "ISO-8859-9"); // NOI18N
        encodingIANAAliasesMap.put("L5", "ISO-8859-9"); // NOI18N

        encodingIANA2JavaMap.put("KOI8-R", "KOI8_R"); // NOI18N
        encodingIANAAliasesMap.put("KOI8-R", "KOI8-R"); // NOI18N

        encodingIANAAliasesMap.put("ASCII", "US-ASCII");  // NOI18N
        encodingIANAAliasesMap.put("US-ASCII", "US-ASCII");  // NOI18N
        encodingIANAAliasesMap.put("ISO646-US", "US-ASCII");  // NOI18N
        encodingIANAAliasesMap.put("IBM367", "US-ASCII");  // NOI18N
        encodingIANAAliasesMap.put("CP367", "US-ASCII");  // NOI18N

        encodingIANA2JavaMap.put("UTF-8", "UTF8");  // NOI18N
        encodingIANAAliasesMap.put("UTF-8", "UTF-8"); // NOI18N

        encodingIANA2JavaMap.put("UTF-16", "Unicode"); // NOI18N
        encodingIANAAliasesMap.put("UTF-16", "UTF-16");  // NOI18N

        Iterator<String> iter = encodingIANA2JavaMap.keySet().iterator();
        String key;
        while (iter.hasNext()) {
            key = iter.next();
            encodingJava2IANAMap.put(encodingIANA2JavaMap.get(key), key);
        }
        encodingIANA2JavaMap.put("US-ASCII", "8859_1"); // NOI18N
    }

}
