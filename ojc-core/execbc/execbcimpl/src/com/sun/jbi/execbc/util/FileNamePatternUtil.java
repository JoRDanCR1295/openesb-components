/*
 * FileNamePatternUtil.java
 * 
 * Created on May 17, 2007, 3:07:24 PM
 * 
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.execbc.util;

import com.sun.jbi.internationalization.Messages;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

/**
 *
 * @author jfu
 */
public class FileNamePatternUtil {

    private static final Messages mMessages = Messages.getMessages(FileNamePatternUtil.class);

    public static final String NUMBER_MARKER = "%d"; // transient sequence
    public static final String TIMESTAMP_MARKER = "%t";
    public static final String GUID_MARKER = "%u";
    public static final String SEQ_REF_EXP = "%\\{([a-zA-Z0-9\\.\\-\\_]+)\\}"; // named sequence

    public static final String UUID_REGEX="[0-9[abcdef]]{8}-[0-9[abcdef]]{4}-[0-9[abcdef]]{4}-[0-9[abcdef]]{4}-[0-9[abcdef]]{12}";

    public static final Pattern SEQ_REF_EXP_PATT = Pattern.compile(SEQ_REF_EXP);

    /** Utility to get the regular expression to match
     * the date format.
     * Reason this is in its own method is that we can
     * localize the date format recognition pattern in
     * this method only.
     * The pattern we currently support is UTC
     */
    public static final String TIMESTAMP_REGEX = "[0-9]{4,4}(0[1-9]|1[0-2])([0-2][0-9]|3[0-1])(\\-(1[0-9]|2[0-3])\\-[0-5][0-9]\\-[0-5][0-9]\\-[0-9]{0,3})?";

    public static FileNamePatternType validatePattern(String pattern, String[] decomposed) throws Exception {
        // decomposed array must be a 3 element string array
        // with:
        // decomposed[0] as string before the pattern
        // decomposed[1] as the pattern it self
        // decomposed[2] as string after the pattern
        if(pattern == null || pattern.trim().length() == 0 ) {
            throw new Exception(mMessages.getString("FNP_Invalid_pattern_blank", pattern));
        }

        FileNamePatternType type = FileNamePatternType.NAME_INVALID;
        int index1 = 0;
        int index2 = 0;
        int pcnt = 0;
        String prefix = "";
        String suffix = "";
        String patternFound = null;

        if ( (index1 = pattern.indexOf(NUMBER_MARKER)) >= 0 ) {
            // has at least 1 %d
            index2 = pattern.lastIndexOf(NUMBER_MARKER);
            if ( index1 < index2 ) {
                // more than one occurrence
                throw new Exception(mMessages.getString("FNP_Invalid_pattern_only_1_seq_allowed", pattern));
            }
            pcnt++;
            type = FileNamePatternType.NAME_WITH_SEQ;
            patternFound = NUMBER_MARKER;
            prefix = index1 == 0 ? "" : pattern.substring(0, index1);
            suffix = index1 + 2 >= pattern.length() ? "" : pattern.substring(index1 + 2);
        }
        
        if ( (index1 = pattern.indexOf(GUID_MARKER)) >= 0 ) {
            // has at least 1 %u
            index2 = pattern.lastIndexOf(GUID_MARKER);
            if ( index1 < index2 ) {
                // more than one occurrence
                throw new Exception(mMessages.getString("FNP_Invalid_pattern_only_1_uuid_allowed", pattern));
            }
            if ( pcnt > 0 ) {
                // more than one pattern mark
                throw new Exception(mMessages.getString("FNP_Invalid_pattern_only_1_pattern_allowed", pattern));
            }
            pcnt++;
            type = FileNamePatternType.NAME_WITH_UUID;
            patternFound = GUID_MARKER;
            prefix = index1 == 0 ? "" : pattern.substring(0, index1);
            suffix = index1 + 2 >= pattern.length() ? "" : pattern.substring(index1 + 2);
        }

        if ( (index1 = pattern.indexOf(TIMESTAMP_MARKER)) >= 0 ) {
            // has at least 1 %t
            index2 = pattern.lastIndexOf(TIMESTAMP_MARKER);
            if ( index1 < index2 ) {
                // more than one occurrence
                throw new Exception(mMessages.getString("FNP_Invalid_pattern_only_1_timestamp_allowed", pattern));
            }
            if ( pcnt > 0 ) {
                // more than one pattern mark
                throw new Exception(mMessages.getString("FNP_Invalid_pattern_only_1_pattern_allowed", pattern));
            }
            pcnt++;
            type = FileNamePatternType.NAME_WITH_TIMESTAMP;
            patternFound = TIMESTAMP_MARKER;
            prefix = index1 == 0 ? "" : pattern.substring(0, index1);
            suffix = index1 + 2 >= pattern.length() ? "" : pattern.substring(index1 + 2);
        }

        // added for named sequence (persisted sequence)
        if ( (index1 = pattern.indexOf("%{")) >= 0 ) {
            Matcher m = SEQ_REF_EXP_PATT.matcher(pattern);
            if ( m.find() ) {
                patternFound = m.group(1);
                String wholeMatch = m.group();
                if ( pcnt > 0 || m.find() ) {
                    // more than one pattern detected
                    throw new Exception(mMessages.getString("FNP_Invalid_pattern_only_1_pattern_allowed", pattern));
                }
                index1 = pattern.indexOf(wholeMatch);
                pcnt++;
                type = FileNamePatternType.NAME_WITH_PERSISTED_SEQ;
                prefix = index1 == 0 ? "" : pattern.substring(0, index1);
                suffix = index1 + wholeMatch.length() >= pattern.length() ? "" : pattern.substring(index1 + wholeMatch.length());
            }
        }

        if ( pcnt == 0 ) {
            throw new Exception(mMessages.getString("FNP_Invalid_pattern_no_marker_found", pattern));
        }
        
        decomposed[0] = prefix;
        decomposed[1] = patternFound;
        decomposed[2] = suffix;
        
        return type;
    }
}
