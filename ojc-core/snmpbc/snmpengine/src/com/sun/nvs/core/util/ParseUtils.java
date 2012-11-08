/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.util;

import java.io.BufferedReader;
import java.io.StringReader;

import java.util.ArrayList;
import java.util.StringTokenizer;
import java.util.Vector;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public class ParseUtils {
    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     * @param delims DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String[] split(String s, String delims) {
        StringTokenizer st = new StringTokenizer(s, delims);
        int count = st.countTokens();
        String[] tokens = new String[count];
        int i = 0;

        while (st.hasMoreTokens()) {
            tokens[i++] = st.nextToken();
        }

        return tokens;
    }

    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     * @param delims DOCUMENT ME!
     * @param returnDelims DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String[] split(String s, String delims, boolean returnDelims) {
        StringTokenizer st = new StringTokenizer(s, delims, returnDelims);
        int count = st.countTokens();
        String[] tokens = new String[count];
        int i = 0;

        while (st.hasMoreTokens()) {
            tokens[i++] = st.nextToken();
        }

        return tokens;
    }

    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String[] split(String s) {
        return split(s, " \t");
    }

    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static ParseToken[] toParseTokens(String s) {
        Vector v = new Vector();
        ParseBuffer pb = new ParseBuffer(s);
        ParseToken token = null;

        while ((token = pb.nextToken()) != null) {
            v.addElement(token);
        }

        ParseToken[] toks = new ParseToken[v.size()];
        v.toArray(toks);
        v = null;

        return toks;
    }

    /**
     * DOCUMENT ME!
     *
     * @param toks DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String[] toStringArray(ParseToken[] toks) {
        String[] tokens = new String[toks.length];

        for (int i = 0; i < toks.length; i++) {
            tokens[i] = toks[i].getValue();
        }

        return tokens;
    }

    // Splits raw data between two delmiters into string of lines
    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     * @param delims DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String splitDatatoLines(String s, String[] delims) {
        String line = null;
        BufferedReader r = new BufferedReader(new StringReader(s));
        StringBuffer rb = new StringBuffer();

        boolean log = false;

        try {
            while ((line = r.readLine()) != null) {
                if (log) {
                    if (line.indexOf(delims[1]) >= 0) {
                        break;
                    }

                    rb.append(line);
                    rb.append("\n");

                    continue;
                }

                if (line.indexOf(delims[0]) >= 0) {
                    log = true;
                    rb.append(line);
                    rb.append("\n");
                }
            }

            return rb.toString();
        } catch (Exception e) {
            e.printStackTrace();
        }

        return rb.toString();
    }

    /**
     * DOCUMENT ME!
     *
     * @param str DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String compressString(String str) {
        return compressString(str, false);
    }

    /**
     * DOCUMENT ME!
     *
     * @param str DOCUMENT ME!
     * @param removeEndSlash DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String compressString(String str, boolean removeEndSlash) {
        boolean beginsWithSlash = false;
        boolean endsWithSlash = false;

        //convert "\" to "/"
        str = str.replace('\\', '/');
        //ignore any "."s
        str = str.replaceAll("/\\./", "/");

        //remove any ".."s
        String[] pwdString = ParseUtils.split(str, "/");
        Vector outString = new Vector();
        int j = 0;

        for (int i = 0; i < pwdString.length; i++) {
            if (pwdString[i].equals("..")) {
                if (outString.size() > 0) {
                    outString.removeElementAt(outString.size() - 1);
                }
            } else {
                outString.addElement(pwdString[i]);
            }
        }

        if (str.endsWith("/")) {
            endsWithSlash = true;
        }

        if (str.startsWith("/")) {
            str = "/";
            beginsWithSlash = true;
        } else {
            str = "";
        }

        int sz = outString.size();

        for (int i = 0; i < sz; i++) {
            if (i < (sz - 1)) {
                str += ((String) outString.elementAt(i) + "/");
            } else {
                str += (String) outString.elementAt(i);
            }
        }

        if (endsWithSlash && !removeEndSlash) {
            //to remove the occurance of "//"
            if (!(beginsWithSlash && (str.length() == 1))) {
                str += "/";
            }
        }

        return str;
    }

    /**
     * DOCUMENT ME!
     *
     * @param str DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getParentString(String str) {
        String[] pwdString = ParseUtils.split(str, "/");

        if (str.startsWith("/")) {
            str = "/";
        } else {
            str = "";
        }

        for (int i = 0; i < (pwdString.length - 1); i++) {
            if (i < (pwdString.length - 2)) {
                str += (pwdString[i] + "/");
            } else {
                str += pwdString[i];
            }
        }

        return str;
    }

    /**
     * DOCUMENT ME!
     *
     * @param str DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getChildString(String str) {
        String[] pwdString = ParseUtils.split(str, "/");

        if (pwdString.length == 0) {
            return str;
        } else {
            return pwdString[pwdString.length - 1];
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param list DOCUMENT ME!
     * @param pattern DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getCommonPart(String[] list, String pattern) {
        if (list == null) {
            return "";
        }

        ArrayList a = new ArrayList();

        for (int i = 0; i < list.length; i++) {
            if (list[i].startsWith(pattern)) {
                a.add(list[i]);
            }
        }

        if (a.size() == 0) {
            return "";
        }

        String[] ret = new String[a.size()];
        a.toArray(ret);

        return getCommonPart(ret);
    }

    /**
     * DOCUMENT ME!
     *
     * @param list DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getCommonPart(String[] list) {
        if (list == null) {
            return "";
        }

        if (list.length == 0) {
            return "";
        }

        if (list.length == 1) {
            return list[0];
        }

        int slen = list[0].length();
        StringBuffer sb = new StringBuffer();

        String oldStr = "";

        for (int i = 0; i < slen; i++) {
            sb.append(list[0].charAt(i));

            for (int j = 1; j < list.length; j++) {
                if (!list[j].startsWith(sb.toString())) {
                    return oldStr;
                }
            }

            oldStr = sb.toString();
        }

        return oldStr;
    }
}
