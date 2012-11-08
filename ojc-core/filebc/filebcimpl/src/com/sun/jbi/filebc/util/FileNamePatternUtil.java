/*
 * FileNamePatternUtil.java
 * 
 * Created on May 17, 2007, 3:07:24 PM
 * 
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */
package com.sun.jbi.filebc.util;

import com.sun.jbi.filebc.Endpoint.EndpointType;
import com.sun.jbi.filebc.extensions.FileOperation;
import com.sun.jbi.internationalization.Messages;
import java.util.UUID;
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
    // Inbound UUID regex - match anything.
    public static final String IB_UUID_REGEX = ".*";
    // OutBound UUID regex - match a UUID.
    public static final String UUID_REGEX = "[0-9[abcdef]]{8}-[0-9[abcdef]]{4}-[0-9[abcdef]]{4}-[0-9[abcdef]]{4}-[0-9[abcdef]]{12}";
    public static final String WORK_DIR_NAME_REGEX = "[I|O]B[0-9[abcdef]]{8}-[0-9[abcdef]]{4}-[0-9[abcdef]]{4}-[0-9[abcdef]]{4}-[0-9[abcdef]]{12}";
    public static final String SEQ_NO_REGEX = "[0-9]+";
    public static final Pattern SEQ_REF_EXP_PATT = Pattern.compile(SEQ_REF_EXP);
    public static final Pattern UUID_REGEX_PATT = Pattern.compile(UUID_REGEX);
    /** Utility to get the regular expression to match
     * the date format.
     * Reason this is in its own method is that we can
     * localize the date format recognition pattern in
     * this method only.
     * The pattern we currently support is UTC
     */
    public static final String TIMESTAMP_REGEX = "[0-9]{4,4}(0[1-9]|1[0-2])([0-2][0-9]|3[0-1])(\\-(0[0-9]|1[0-9]|2[0-3])\\-[0-5][0-9]\\-[0-5][0-9]\\-[0-9]{0,3})?";

    private FileNamePatternUtil() {}
    
    /*
     * Check if given file name patterns overlap i.e would result in match the same set of files.
     * 
     */
    public static boolean checkOverlap(String filename1, int epType1, String verb1,
            String filename2, int epType2, String verb2) throws Exception {
        String[] decomposed1 = new String[3];

        // check if only one of the file names is patterned
        if (!isNamePatterned(filename1) && isNamePatterned(filename2)) {
            //get decompose form of the patterned filename
            String[] decomposed = new String[3];
            FileNamePatternType patternType = FileNamePatternUtil.validatePattern(filename2, decomposed);
            String prefix = decomposed[0];
            String suffix = decomposed[2];
            String regex = getRegex(filename2, patternType, epType2, verb2);
            String strPattern = prefix + regex + parseSuffixPattern(suffix);
            Pattern pattern = Pattern.compile(strPattern);
            return pattern.matcher(filename1).matches();

//            return checkOverlap(filename1, prefix, regex, suffix);
        }

        if (!isNamePatterned(filename2) && isNamePatterned(filename1)) {
            //get decompose form of the patterned filename
            String[] decomposed = new String[3];
            FileNamePatternType patternType = FileNamePatternUtil.validatePattern(filename1, decomposed);
            String prefix = decomposed[0];
            String suffix = decomposed[2];
            String regex = getRegex(filename1, patternType, epType1, verb1);
            String strPattern = prefix + regex + parseSuffixPattern(suffix);
            Pattern pattern = Pattern.compile(strPattern);
            return pattern.matcher(filename2).matches();

//            return checkOverlap(filename2, prefix, regex, suffix);
        }

        // both file names contain patterns

        FileNamePatternType type1 = FileNamePatternUtil.validatePattern(filename1, decomposed1);
        String prefix1 = decomposed1[0];
        String suffix1 = decomposed1[2];

        String regex1 = getRegex(filename1, type1, epType1, verb1);
        String pattern1 = prefix1 + regex1 + parseSuffixPattern(suffix1);

        String[] decomposed2 = new String[3];
        FileNamePatternType type2 = FileNamePatternUtil.validatePattern(filename2, decomposed2);
        String prefix2 = decomposed2[0];
        String suffix2 = decomposed2[2];

        String regex2 = getRegex(filename2, type2, epType2, verb2);
        String pattern2 = prefix2 + regex2 + parseSuffixPattern(suffix2);

        if (pattern1.equals(pattern2)) {
            return true;
        } else if (IB_UUID_REGEX.equals(regex1) && !IB_UUID_REGEX.equals(regex2)) {
            if (prefix2.startsWith(prefix1) && ("".equals(suffix1) || suffix1.equals(suffix2))) {
                return true;
            } else {
                return false;
            }
        } else if (IB_UUID_REGEX.equals(regex2) && !IB_UUID_REGEX.equals(regex1)) {
            if (prefix1.startsWith(prefix2) && ("".equals(suffix2) || suffix2.equals(suffix1))) {
                return true;
            } else {
                return false;
            }
        } else if (IB_UUID_REGEX.equals(regex2) && IB_UUID_REGEX.equals(regex1)) {
            if ((prefix1.startsWith(prefix2) || prefix2.startsWith(prefix1)) && ("".equals(suffix1) || "".equals(suffix2) || suffix1.equals(suffix2))) {
                return true;
            } else {
                return false;
            }
        }

        return false;
    }

    /*
     * Check overlap between given actual filename (which does not contain any pattern), with 
     * pattern composed of specified prefix + regex + suffix.
     */
    public static boolean checkOverlap(String filename, String prefix, String regex, String suffix) {
        int i = filename.indexOf(".");
        String filenamePrefix = filename.substring(0, i);
        String filenameSuffix = filename.substring(i);
        if (filenamePrefix.startsWith(prefix) && IB_UUID_REGEX.equals(regex) && filenameSuffix.equals(suffix)) {
            return true;
        }
        return false;
    }

    private static String getRegex(String filename, FileNamePatternType type, int epType, String verb) throws Exception {
        String regex1 = "";
        switch (type) {
            case NAME_WITH_TIMESTAMP:
                regex1 = TIMESTAMP_REGEX;
                break;
            case NAME_WITH_UUID:
                if (EndpointType.INBOUND == epType || FileOperation.VERB_READ.equals(verb)) {
                    regex1 = IB_UUID_REGEX;
                } else {
                    regex1 = UUID_REGEX;
                }
                break;
            case NAME_WITH_SEQ:
                regex1 = SEQ_NO_REGEX;
                break;
            case NAME_WITH_PERSISTED_SEQ:
                regex1 = SEQ_NO_REGEX;
                break;
            default:
                throw new Exception(mMessages.getString("FILEBC-E00640.IFF_Invalid_pattern", filename));
        }

        return regex1;

    }

    public static String parseSuffixPattern(String suffix) {
        String suffixPattern = suffix;
        int index = suffix.indexOf(".");

        if (index >= 0) {
            suffixPattern = suffix.substring(0, index) + "\\." + suffix.substring(index + 1);
        }

        return suffixPattern;
    }

    public static FileNamePatternType validatePattern(String pattern, String[] decomposed) throws Exception {
        // decomposed array must be a 3 element string array
        // with:
        // decomposed[0] as string before the pattern
        // decomposed[1] as the pattern it self
        // decomposed[2] as string after the pattern
        if (pattern == null || pattern.trim().length() == 0) {
            throw new Exception(mMessages.getString("FILEBC-E00601.FNP_Invalid_pattern_blank", pattern));
        }

        FileNamePatternType type = FileNamePatternType.NAME_INVALID;
        int index1 = 0;
        int index2 = 0;
        int pcnt = 0;
        String prefix = "";
        String suffix = "";
        String patternFound = null;

        if ((index1 = pattern.indexOf(NUMBER_MARKER)) >= 0) {
            // has at least 1 %d
            index2 = pattern.lastIndexOf(NUMBER_MARKER);
            if (index1 < index2) {
                // more than one occurrence
                throw new Exception(mMessages.getString("FILEBC-E00602.FNP_Invalid_pattern_only_1_seq_allowed", pattern));
            }
            pcnt++;
            type = FileNamePatternType.NAME_WITH_SEQ;
            patternFound = NUMBER_MARKER;
            prefix = index1 == 0 ? "" : pattern.substring(0, index1);
            suffix = index1 + 2 >= pattern.length() ? "" : pattern.substring(index1 + 2);
        }

        if ((index1 = pattern.indexOf(GUID_MARKER)) >= 0) {
            // has at least 1 %u
            index2 = pattern.lastIndexOf(GUID_MARKER);
            if (index1 < index2) {
                // more than one occurrence
                throw new Exception(mMessages.getString("FILEBC-E00603.FNP_Invalid_pattern_only_1_uuid_allowed", pattern));
            }
            if (pcnt > 0) {
                // more than one pattern mark
                throw new Exception(mMessages.getString("FILEBC-E00604.FNP_Invalid_pattern_only_1_pattern_allowed", pattern));
            }
            pcnt++;
            type = FileNamePatternType.NAME_WITH_UUID;
            patternFound = GUID_MARKER;
            prefix = index1 == 0 ? "" : pattern.substring(0, index1);
            suffix = index1 + 2 >= pattern.length() ? "" : pattern.substring(index1 + 2);
        }

        if ((index1 = pattern.indexOf(TIMESTAMP_MARKER)) >= 0) {
            // has at least 1 %t
            index2 = pattern.lastIndexOf(TIMESTAMP_MARKER);
            if (index1 < index2) {
                // more than one occurrence
                throw new Exception(mMessages.getString("FILEBC-E00605.FNP_Invalid_pattern_only_1_timestamp_allowed", pattern));
            }
            if (pcnt > 0) {
                // more than one pattern mark
                throw new Exception(mMessages.getString("FILEBC-E00604.FNP_Invalid_pattern_only_1_pattern_allowed", pattern));
            }
            pcnt++;
            type = FileNamePatternType.NAME_WITH_TIMESTAMP;
            patternFound = TIMESTAMP_MARKER;
            prefix = index1 == 0 ? "" : pattern.substring(0, index1);
            suffix = index1 + 2 >= pattern.length() ? "" : pattern.substring(index1 + 2);
        }

        // added for named sequence (persisted sequence)
        if ((index1 = pattern.indexOf("%{")) >= 0) {
            Matcher m = SEQ_REF_EXP_PATT.matcher(pattern);
            if (m.find()) {
                patternFound = m.group(1);
                String wholeMatch = m.group();
                if (pcnt > 0 || m.find()) {
                    // more than one pattern detected
                    throw new Exception(mMessages.getString("FILEBC-E00604.FNP_Invalid_pattern_only_1_pattern_allowed", pattern));
                }
                index1 = pattern.indexOf(wholeMatch);
                pcnt++;
                type = FileNamePatternType.NAME_WITH_PERSISTED_SEQ;
                prefix = index1 == 0 ? "" : pattern.substring(0, index1);
                suffix = index1 + wholeMatch.length() >= pattern.length() ? "" : pattern.substring(index1 + wholeMatch.length());
            }
        }

        if (pcnt == 0) {
            throw new Exception(mMessages.getString("FILEBC-E00606.FNP_Invalid_pattern_no_marker_found", pattern));
        }

        decomposed[0] = prefix;
        decomposed[1] = patternFound;
        decomposed[2] = suffix;

        return type;
    }

    /**
     * locate a UUID in a given name and replace it with a newly generated one
     * @param fileName - the orignal name
     * @return - the name with UUID replaced if there is a sub-string matching
     * UUID regex
     */
    public static String replaceUUID(String fileName) {
        String result = null;
        if (fileName != null) {
            Matcher m = UUID_REGEX_PATT.matcher(fileName);
            if (m.find()) {
                result = fileName.replace(m.group(), UUID.randomUUID().toString());
            } else {
                // original name does not contain UUID
                // then append one
                result = fileName.concat(UUID.randomUUID().toString());
            }
        }
        return result;
    }

    public static FileNamePatternType validateRegExPattern(String pattern) throws Exception {
        FileNamePatternType type = FileNamePatternType.NAME_INVALID;
        try {
            Pattern.compile(pattern);
            type = FileNamePatternType.NAME_IS_REGEX;
        } catch (Exception e) {
            throw e;
        }
        return type;
    }

    public static boolean isGUIDPatterned(String filename) {
        return filename.contains(GUID_MARKER);
    }

    public static boolean isPersistedSeqPatterned(String filename) {
        return filename.contains("%{");
    }

    private static boolean isNamePatterned(String filename) {
        if (filename.contains(NUMBER_MARKER) || filename.contains(GUID_MARKER) || filename.contains(TIMESTAMP_MARKER) || filename.contains("%{")) {
            return true;
        }
        return false;
    }
}
