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
 * @(#)StringUtil.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.text.StringCharacterIterator;
import java.util.HashMap;
import java.util.Map;

public class StringUtil {
    
    private static final int STREAM_READ_CHUNK_SIZE = 2048;

    private static Map<Character, String> XMLEntitiesToNamesMap =
            new HashMap<Character, String>();
    static {
        XMLEntitiesToNamesMap.put('<', "&lt;");
        XMLEntitiesToNamesMap.put('>', "&gt;");
        XMLEntitiesToNamesMap.put('\'', "&apos;");
        XMLEntitiesToNamesMap.put('"', "&quot;");
        XMLEntitiesToNamesMap.put('&', "&amp;");
    }
    
    private StringUtil() {
    }

    public static String replaceAll(String s, String match, String replacement) {
        StringBuffer sb = new StringBuffer();
        String temp = s;
        while (true) {
            int i = temp.indexOf(match);
            if (i < 0) {
                sb.append(temp);
                return sb.toString();
            }
            sb.append(temp.substring(0, i));
            sb.append(replacement);
            temp = temp.substring(i + match.length());
        }
    }
    
    /**
     * Produce a string composed of characters read from the input stream.
     * 
     * @param stream   Source of the data
     * @param encoding Encoding to use to decode the source bytes to characters
     * 
     * @return Contents of the input stream as character data
     * 
     * @throws UnsupportedEncodingException if the <code>encoding</code>
     *         argument specifies an unsupported or unrecognized charset
     *         encoding.
     * 
     * @throws IOException if any I/O-related exceptions occur while reading
     *         from the stream.
     */
    public static String streamToString(InputStream stream, String encoding)
            throws UnsupportedEncodingException, IOException {
        
        InputStreamReader isr = new InputStreamReader(stream, encoding);
        StringBuffer sb = new StringBuffer();
        char[] cb = new char[STREAM_READ_CHUNK_SIZE];
        int cnt = 0;
        // Cannot rely on ready(); some stream implementations do not
        // implement it correctly, notably the Coyote stream instances
        // the HTTP BC receives via JAX-WS.
        //if (isr.ready()) {
            while ((cnt = isr.read(cb)) != -1) {
                sb.append(cb, 0, cnt);
            }
        //}
        return sb.toString();
    }
    
    /**
     * Converts characters that represent XML predefined entities into their
     * entity names.  This "escaping" of XML data is needed before data that
     * is potentially XML data (in part or in whole), is stitched into another
     * XML body.  XML composition is ideally done using a DOM, but in some cases
     * this is too expensive (e.g., embedding  JBI MessageExchange fault
     * detail that may OR MAY NOT contain XML into a SOAP Fault, which is
     * its own XML body).
     * <p/>
     * The five XML predefined entities are: double quotation mark (&quot;),
     * ampersand (&amp;), apostrophe (&apos;), the less-than symbol (&lt;),
     * and the greater-than symbol (&gt;).  Where encountered, these characters
     * are replaced respectively with: <code>&quot;</code>, <code>&amp;</code>,
     * <code>&apos;</code>, <code>&lt;</code> and <code>&gt;</code>.
     * 
     * @param str Character data that may be, or partly, XML.
     * 
     * @return The supplied character data with all XML predefined entities
     *         that were found in it, escaped.
     */
    public static String escapeXML(String str) {
        String result = null;
        boolean foundEntity = false;
        
        if (str != null) {
            StringBuffer buffer = new StringBuffer(str.length());
            StringCharacterIterator iter = new StringCharacterIterator(str);
            for (char ch = iter.first(); ch != StringCharacterIterator.DONE; ch = iter.next()) {
                String name = XMLEntitiesToNamesMap.get(Character.valueOf(ch));
                buffer.append(name != null ? name : ch);
                foundEntity = foundEntity || (name != null);  // This is a once-up-always-up flag used later outside the loop
            }
            
            // If we never EVER found an XML entity to escape, don't create
            // a new string; reuse the original one.
            if (!foundEntity) {
                result = str;
            } else {
                result = buffer.toString();
            }
        }
        
        return result;
    }
}
