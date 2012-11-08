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
 * @(#)InputDirFilter.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc.util;

import com.sun.jbi.internationalization.Messages;

import java.io.File;
import java.io.FilenameFilter;
import java.util.regex.Pattern;

/**
 *
 * This class implements the FilenameFilter interface.
 * The interface is used to filter directory listings
 * in the list method of File class.
 *
 * this filter only accepts directory
 *
 * @author Qian Fu jfu@sun.com
 * 
 */
public class InputDirFilter implements FilenameFilter {

    private static final Messages mMessages =
            Messages.getMessages(InputDirFilter.class);
    private Pattern mExcludeRegex;
    // regex as the ignore criteria
    private static final Pattern WORK_DIR_NAME_PATT = Pattern.compile(FileNamePatternUtil.WORK_DIR_NAME_REGEX);
    // enhancement #752: open esb issue tracker
    // some entry names are considered
    // to be reserved as file bc working
    // directory name, and should not be
    // polled into (avoid messing up working files
    // such as lock files, staged files - files in processing, etc.)
    // this pattern is used to test if an entry is a working dir
    // of the BC.
    //

    public InputDirFilter(String excludeRegex) {
        mExcludeRegex = (excludeRegex != null && excludeRegex.trim().length() > 0) ? Pattern.compile(excludeRegex.trim()) : null;
    }

    public boolean accept(File dir, String name) {
        File aEntry = new File(dir, name);
        String aEntryName = aEntry.getName();
        boolean accepted = true;
        // any entry that met the exclude criteria
        // is skipped
        // accept all dirs except working dirs
        if (aEntry.exists() && aEntry.isDirectory()) {
            // if the entry name matches work dir pattern
            // it is not accepted
            accepted = !WORK_DIR_NAME_PATT.matcher(aEntryName).matches();
            if ( accepted ) {
                // further check exclude regex
                if (mExcludeRegex != null && mExcludeRegex.matcher(aEntryName).matches()) {
                    accepted = false;
                }
            }
        } else {
            // reject non-dir entries
            accepted = false;
        }
//        if (mExcludeRegex != null && mExcludeRegex.matcher(aEntryName).matches()) {
//            accepted = false;
//        } else {
//            // accept all dirs except working dirs
//            if (aEntry.exists() && aEntry.isDirectory()) {
//                // if the entry name matches work dir pattern
//                // it is not accepted
//                accepted = !WORK_DIR_NAME_PATT.matcher(aEntryName).matches();
//            } else {
//                // reject non-dir entries
//                accepted = false;
//            }
//        }
        return accepted;
    }
}
