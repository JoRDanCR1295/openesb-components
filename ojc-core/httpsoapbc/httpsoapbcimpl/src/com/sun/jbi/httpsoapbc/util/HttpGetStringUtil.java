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
 * @(#)HttpGetStringUtil.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.util;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class HttpGetStringUtil {
    
    /**
     * Given a url-replacement encoded URL, and a pattern, creates a mapping
     * between each search term from the pattern, and a corresponding
     * value from the URL.
     *
     * <p/>For example, given a URL <code>http://example.com/o1/A1B2/3</code>,
     * and given a pattern <code>o1/A(part1)B(part2)/(part3)</code>, this map
     * is created:
     *
     * <p/>part1=1<br/>
     *     part2=2<br/>
     *     part3=3
     *
     * @param queryString The url-replacement encoded URL
     * @param urlPattern  The search pattern
     *
     * @returns A map between names and values extracted from the URL.
     *
     * @throws PatternMatchException if the query string cannot be reconciled
     *         with the specified pattern.
     */
    public static Map<String, String> extractNameValuePairs(String queryString,
                                                            String urlPattern)
        throws PatternMatchException {
        
        Map<String, String> nvMap = new HashMap<String, String>();
        
        // First I match the pattern specification against itself, so I can
        // obtain the *names* of the search patterns themselves:
        // I store these names in the name-value map as values, with their
        // order of discovery as keys, temporarily.
        int previousMatches = 0;
        {
            String regex = convertUrlPathToNameCaptureRegex(urlPattern);
            Pattern searchPattern = Pattern.compile(regex);
            StringBuffer namebuf = new StringBuffer();
            Matcher matcher = searchPattern.matcher(urlPattern);
            boolean gotMatch = matcher.find();
            if (gotMatch) {
                int matches = matcher.groupCount();
                for (int match = 1; match <= matches; ++match) {
                    namebuf.append(matcher.group(match));
                    if (namebuf.length() >= 3) {
                        String name = namebuf.substring(1, namebuf.length() - 1);
                        nvMap.put(String.valueOf(match), name);
                    }
                    namebuf.delete(0, namebuf.length());
                }
                previousMatches = matches;
            }
        }
        
        // Next, match the pattern specification against the actual input now, to
        // obtain the values corresponding to each search pattern.
        // As I populate the map with these values, I take out the mappings I
        // created in the previous step.
        {
            String regex = convertUrlPathToRegex(urlPattern);
            Pattern searchPattern = Pattern.compile(regex);
            Matcher matcher = searchPattern.matcher(queryString);
            boolean gotMatch = matcher.find();
            if (!gotMatch) {
                throw new PatternMatchException(queryString, urlPattern);
            }
            if (previousMatches != matcher.groupCount()) {
                throw new PatternMatchException(queryString, urlPattern);
            }
            previousMatches = matcher.groupCount();
            for (int match = 1; match <= previousMatches; ++match) {
                String value = matcher.group(match);
                String oldKey = String.valueOf(match);
                String newKey = nvMap.get(oldKey);
                nvMap.put(newKey, value);
                nvMap.remove(oldKey);
            }
        }
        return nvMap;
    }
    
    private static String convertUrlPathToRegex(String urlPattern) {
        // Replace all spans of non-whitespace characters delimited by parentheses.
        // Replace each occurence of such span with "(\S+?)", the regular expression
        // needed to capture such span.
        StringBuffer regexBuf = new StringBuffer(urlPattern.replaceAll("\\(\\S+?\\)", "(\\\\S+?)"));
        return regexBuf.append('$').toString();
    }

    private static String convertUrlPathToNameCaptureRegex(String urlPattern) {
        // Replace all spans of non-whitespace characters delimited by parentheses.
        // Replace each occurence of such span with "(\S+?)", the regular expression
        // needed to capture such span.
        StringBuffer regexBuf = new StringBuffer(urlPattern.replaceAll("\\(\\S+?\\)", "(\\\\(\\\\S+?\\\\))"));
        return regexBuf.toString();
    }
    
    public static class PatternMatchException extends Exception {
        private final String mInput;
        private final String mPattern;
        
        public PatternMatchException(String input, String pattern) {
            mPattern = pattern;
            mInput = input;
        }
        
        public String getPattern() {
            return mPattern;
        }
        
        public String getInput() {
            return mInput;
        }
    }
}
