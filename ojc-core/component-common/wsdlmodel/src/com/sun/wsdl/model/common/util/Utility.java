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
 * @(#)Utility.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.common.util;

import java.util.ArrayList;
import java.util.List;

/**
 * This class has Utility Methods 
 * 
 * @author Sun Microsystems
 * @version 1.0
 */
public class Utility {
    
    /** Test if a string is empty.
     * @param   s   String to test
     * @return  <code>true</code> if string is empty
     */
    public static boolean isEmpty(String s) {
        return ((null == s) || (s.trim().length() == 0));
    }

    /** Test if two strings are equal.
     * @param   s1  First string.
     * @param   s2  Second string.
     * @return  <code>true</code> if strings are equal.
     */
    public static boolean areEqual(String s1, String s2) {
        
        return (s1 == null ? s2 == null : s1.equals(s2)); 
    }
    
    /** Test if two strings are equal in XML.
     * @param   s1  First string.
     * @param   s2  Second string.
     * @return  <code>true</code> if strings are equal.
     */
    public static boolean areEqualXMLValues(String s1, String s2) {

        return ((s1 == null && s2 == null)
            || (s1 == null && isEmpty(s2))
            || (isEmpty(s1) && s2 == null)
            || (s1 != null && s1.equals(s2)));
    }
    
    /** Wraps a string into lines of specified character width.  Imbedded newlines will
     * be honored.
     * @param   text    Text string to wrap.
     * @param   width   Character width of each line.
     * @return  One string with appropriate line.separators inserted.
     * @author Sun Microsystems
     *          Criterion, Inc.   (http://www.criterioninc.com)
     * @author Sun Microsystems
     */
    public static String lineWrap(String text, int width) {
        int fromIndex = 0;
        int pos = 0;
        int bestpos;
        String largestString;
        List lines = new ArrayList();
        String s = text;
        String retStr = text;
        
        // while we haven't run past the end of the string...
        while (fromIndex != -1) {
            // Automatically skip any spaces at the beginning of the line
            while ((fromIndex < text.length()) && (text.charAt(fromIndex) == ' ')) {
                ++fromIndex;
                // If we hit the end of line
                // while skipping spaces, we're done.
                if (fromIndex >= text.length()) {
                    break;
                }
            }
            
            // fromIndex represents the beginning of the line
            pos = fromIndex;
            bestpos = -1;
            largestString = null;
            
            while (pos >= fromIndex) {
                boolean bHardNewline = false;
                int newlinePos = text.indexOf('\n', pos);
                int spacePos   = text.indexOf(' ', pos);
                
                if ((newlinePos != -1)          // there is a newline and either
                        && ((spacePos == -1)    // 1. there is no space, or
                            || (spacePos != -1
                                && (newlinePos < spacePos)))) {
                                                // 2. the newline is first
                    pos = newlinePos;
                    bHardNewline = true;
                } else {
                    pos = spacePos;
                    bHardNewline = false;
                }
                
                // Couldn't find another space?
                if (pos == -1) {
                    s = text.substring(fromIndex);
                } else {
                    s = text.substring(fromIndex, pos);
                }
                
                // If the string fits, keep track of it.
                if (s.length() < width) {
                    largestString = s;
                    bestpos = pos;
                    
                    // If we've hit the end of the
                    // string or a newline, use it.
                    if (bHardNewline)
                        bestpos++;
                    if (pos == -1 || bHardNewline) break;
                } else {
                    break;
                }
                
                ++pos;
            }
            
            if (largestString == null) {
                // Couldn't wrap at a space, so find the largest line
                // that fits and print that.  Note that this will be
                // slightly off -- the width of a string will not necessarily
                // be the sum of the width of its characters, due to kerning.
                int totalWidth = 0;
                int oneCharWidth = 0;
                
                pos = fromIndex;
                
                while (pos < text.length()) {
                    oneCharWidth = 1;
                    if ((totalWidth + oneCharWidth) >= width) break;
                    totalWidth += oneCharWidth;
                    ++pos;
                }
                
                lines.add(text.substring(fromIndex, pos));
                fromIndex = pos;
            } else {
                // Disregard trailing newlines
                if ((largestString.length() > 0) || (bestpos != -1)) {
                    lines.add(largestString);
                }
                fromIndex = bestpos;
            }
        }
        
        if ((lines != null) && (lines.size() > 0)) {
            StringBuffer wrapped = new StringBuffer(text.length());
            for (int i = 0; i < lines.size(); i++) {
                if ((i > 0) && !((String) lines.get(i - 1)).endsWith("\n")) {
                    wrapped.append(System.getProperty("line.separator"));
                }
                wrapped.append((String) lines.get(i));
            }
            retStr = wrapped.toString();
        }
        
        return retStr;
    }
}
