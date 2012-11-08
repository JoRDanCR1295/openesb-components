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

package com.sun.jbi.engine.iep.core.runtime.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * StringUtil.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class StringUtil {
    private static final Messages mMessages = Messages.getMessages(StringUtil.class);
    
    public static boolean isEmpty(String str) {
        return (str == null || str.trim().length() == 0);   
    }

    // This is similar to replacePrefix except that x.y with y->z won't change x.y to x.z
    // because y starts with dot.
    public static String replacePrefixNotStartWithDot(String s, Map<String, String> replacement) {
        Set<Map.Entry<String, String>> entries = replacement.entrySet();
        Pattern p;
        Matcher matcher;
        for (Map.Entry<String, String> e : entries) {
            String k = e.getKey();
            String r = e.getValue();
            
            // replace "...k..." with "...r..."
            p = Pattern.compile("([^a-zA-Z_0-9\\.])" + k + "(\\W)", Pattern.CASE_INSENSITIVE);
            matcher = p.matcher(s);
            s = matcher.replaceAll("$1" + r + "$2");
            
            // replace "k..." with "r..."
            p = Pattern.compile("^" + k + "(\\W)", Pattern.CASE_INSENSITIVE);
            matcher = p.matcher(s);
            s = matcher.replaceAll(r + "$1");
        }
        return s;
    }
    
    public static String replacePrefix(String s, Map<String, String> replacement) {
        Set<Map.Entry<String, String>> entries = replacement.entrySet();
        Pattern p;
        Matcher matcher;
        for (Map.Entry<String, String> e : entries) {
            String k = (String)e.getKey();
            String r = (String)e.getValue();
            
            // replace "...k..." with "...r..."
            p = Pattern.compile("(\\W)" + k + "(\\W)", Pattern.CASE_INSENSITIVE);
            matcher = p.matcher(s);
            s = matcher.replaceAll("$1" + r + "$2");
            
            // replace "k..." with "r..."
            p = Pattern.compile("^" + k + "(\\W)", Pattern.CASE_INSENSITIVE);
            matcher = p.matcher(s);
            s = matcher.replaceAll(r + "$1");
        }
        return s;
    }
    
    public static String replacePostfix(String s, Map<String, String> replacement) {
        Set<Map.Entry<String, String>> entries = replacement.entrySet();
        Pattern p;
        Matcher matcher;
        for (Map.Entry<String, String> e : entries) {
            String k = (String)e.getKey();
            String r = (String)e.getValue();
            
            // replace "...k..." with "...r..."
            p = Pattern.compile("(\\W)" + k + "(\\W)", Pattern.CASE_INSENSITIVE);
            matcher = p.matcher(s);
            s = matcher.replaceAll("$1" + r + "$2");
            
            // replace "...k" with "...r"
            p = Pattern.compile("(\\W)" + k + "$", Pattern.CASE_INSENSITIVE);
            matcher = p.matcher(s);
            s = matcher.replaceAll("$1" + r);
        }
        return s;
    }

    public static String replaceWord(String s, Map<String, String> replacement) {
        Set<Map.Entry<String, String>> entries = replacement.entrySet();
        Pattern p;
        Matcher matcher;
        for (Map.Entry<String, String> e : entries) {
            String k = (String)e.getKey();
            String r = (String)e.getValue();
            
            // replace "...k..." with "...r..."
            p = Pattern.compile("(\\W)" + k + "(\\W)", Pattern.CASE_INSENSITIVE);
            matcher = p.matcher(s);
            s = matcher.replaceAll("$1" + r + "$2");
            
            // replace "k..." with "r..."
            p = Pattern.compile("^" + k + "(\\W)", Pattern.CASE_INSENSITIVE);
            matcher = p.matcher(s);
            s = matcher.replaceAll(r + "$1");

            // replace "...k" with "...r"
            p = Pattern.compile("(\\W)" + k + "$", Pattern.CASE_INSENSITIVE);
            matcher = p.matcher(s);
            s = matcher.replaceAll("$1" + r);

            // replace "k" with "r"
            p = Pattern.compile("^" + k + "$", Pattern.CASE_INSENSITIVE);
            matcher = p.matcher(s);
            s = matcher.replaceAll(r);
        }
        return s;
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
    
    public static List<String> getTokenList(String s, String delim) {
        StringTokenizer st = new StringTokenizer(s, delim);
        ArrayList<String> list = new ArrayList<String>();
        while (st.hasMoreTokens()) {
            String t = st.nextToken();
            if (t.trim().equals("")) {
                continue;
            }
            list.add(t);
        }
        return list;
    }    

    public static void main(String[] args) {
        String s = args[0];
        Map<String, String> replacement = new HashMap<String, String>();
        for (int i = 1; i < args.length -1; i+=2) {
            replacement.put(args[i], args[i+1]);
        }
        mMessages.log(Level.INFO, "StringUtil.Result", replacePrefixNotStartWithDot(s, replacement));
    }
    
}
