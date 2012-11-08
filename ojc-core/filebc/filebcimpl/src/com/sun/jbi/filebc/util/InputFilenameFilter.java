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
 * @(#)InputFilenameFilter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc.util;

import com.sun.jbi.internationalization.Messages;

import java.io.File;
import java.io.FilenameFilter;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/**
 *
 * This class implements the FilenameFilter interface.
 * The interface is used to filter directory listings
 * in the list method of File class.
 *
 * @author Sherry Weng
 * @author Qian Fu jfu@sun.com
 * 
 */
public class InputFilenameFilter implements FilenameFilter {

    private static final Messages mMessages =
            Messages.getMessages(InputFilenameFilter.class);
    private Pattern mRegex;
    // regex as the matching criteria
    private Pattern mExcludeRegex;
    // regex as the ignore criteria
    private AtomicInteger mMaxCount;
    // original file name spec
    private String mFileNameSpec;
    
    //
    // a reference to a global
    // maximum match count from the
    // top level caller
    //
    // introduced meant to cut
    // short the directory entry scanning
    // process when the max count
    // has been hit
    //

    public InputFilenameFilter() {
    }

    /**
     * for literal file name still need to apply exclude if there is one specified
     * @param filename
     * @param excludeRegex
     * @param maxCount
     */
    public InputFilenameFilter(String filename, String excludeRegex, AtomicInteger maxCount) {
        mMaxCount = maxCount;
        mFileNameSpec = filename;
        try {
            mExcludeRegex = (excludeRegex != null && excludeRegex.trim().length() > 0) ? Pattern.compile(excludeRegex.trim()) : null;
        } catch (PatternSyntaxException pse) {
            throw new IllegalArgumentException(mMessages.getString("FILEBC-E00641.IFF_Invalid_Exclude_Regex", excludeRegex), pse);
        }
    }

    public InputFilenameFilter(String filename, String excludeRegex, AtomicInteger maxCount, boolean isRegex) {
        mMaxCount = maxCount;
        mFileNameSpec = filename;
        try {
            mExcludeRegex = (excludeRegex != null && excludeRegex.trim().length() > 0) ? Pattern.compile(excludeRegex.trim()) : null;
        } catch (PatternSyntaxException pse) {
            throw new IllegalArgumentException(mMessages.getString("FILEBC-E00641.IFF_Invalid_Exclude_Regex", excludeRegex), pse);
        }
        if (isRegex) {
            try {
                mRegex = Pattern.compile(filename);
            } catch (PatternSyntaxException pse) {
                throw new IllegalArgumentException(mMessages.getString("FILEBC-E00642.IFF_Invalid_Filtering_Regex", filename), pse);
            }
        } else {
            setFilterExpression(filename);
        }
    }

    public void setFilterExpression(String filename) {
        String[] decomposed = new String[3];
        FileNamePatternType type = null;

        try {
            type = FileNamePatternUtil.validatePattern(filename, decomposed);
        } catch (Exception ex) {
            throw new IllegalArgumentException(mMessages.getString("FILEBC-E00640.IFF_Invalid_pattern", filename), ex);
        }

        String prefix = decomposed[0];
        String suffix = decomposed[2];
        String pattern = "";

        switch (type) {
            case NAME_WITH_TIMESTAMP:
                pattern = prefix + FileNamePatternUtil.TIMESTAMP_REGEX + FileNamePatternUtil.parseSuffixPattern(suffix);
                break;
            case NAME_WITH_UUID:
                //pattern = prefix + FileNamePatternUtil.UUID_REGEX + parseSuffixPattern(suffix);
                pattern = prefix + FileNamePatternUtil.IB_UUID_REGEX + FileNamePatternUtil.parseSuffixPattern(suffix);
                break;
            case NAME_WITH_SEQ:
                pattern = prefix + "[0-9]+" + FileNamePatternUtil.parseSuffixPattern(suffix);
                break;
            case NAME_WITH_PERSISTED_SEQ:
                // for use as inbound filter, it is treated the same as %d
                pattern = prefix + "[0-9]+" + FileNamePatternUtil.parseSuffixPattern(suffix);
                break;
            default:
                throw new IllegalArgumentException(mMessages.getString("FILEBC-E00640.IFF_Invalid_pattern", filename));
        }
        try {
            mRegex = Pattern.compile(pattern);
        } catch (PatternSyntaxException pse) {
            throw new IllegalArgumentException(mMessages.getString("FILEBC-E00640.IFF_Invalid_pattern", pattern), pse);
        }
    }

    public String getFilterExpression() {
        return mRegex != null ? mRegex.pattern() : null;
    }

    public boolean accept(File dir, String name) {
        File aEntry = new File(dir, name);
        String aEntryName = aEntry.getName();
        boolean accepted = true;
        // any entry that met the exclude criteria
        // is skipped
        if (mExcludeRegex != null && mExcludeRegex.matcher(aEntryName).matches()) {
            accepted = false;
        } else {
            // return first N matching entries
            if (mMaxCount.intValue() > 0) {
                // skip any dir
                if (aEntry.exists() && aEntry.isDirectory()) {
                    accepted = false;
                } else {
                    // further check for pattern/regex matching,
                    if ( mRegex != null ) {
                        accepted = mRegex.matcher(aEntryName).matches();
                        if (accepted) {
                            mMaxCount.decrementAndGet();
                        }
                    }
                    else {
                        // a literal file name spec
                        accepted = name.equals(mFileNameSpec);
                    }
                }
            } else {
                // dir listing already found first N matching files
                // just return false to by passing all the rest
                accepted = false;
            }
        }
        return accepted;
    }
}
