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

package com.sun.jbi.execbc.util;

import com.sun.jbi.internationalization.Messages;

import java.io.File;
import java.io.FilenameFilter;
import java.util.regex.Pattern;


/** This class implements the FilenameFilter interface.
 * The interface is used to filter directory listings
 * in the list method of File class.
 *
 * @author Sherry Weng
 * @author Qian Fu jfu@sun.com
 */
public class InputFilenameFilter implements FilenameFilter {
    private static final Messages mMessages =
            Messages.getMessages(InputFilenameFilter.class);
    private Pattern mRegex;
    
    // default constructor
    public InputFilenameFilter() {}
    
    public InputFilenameFilter(String filename) throws Exception {
        setFilterExpression(filename);
    }
    
    public void setFilterExpression(String filename) throws Exception {
        String[] decomposed = new String[3];
        FileNamePatternType type = FileNamePatternUtil.validatePattern(filename, decomposed);
        String prefix = decomposed[0];
        String suffix = decomposed[2];
        String pattern = "";

        switch (type) {
        case NAME_WITH_TIMESTAMP:
            pattern = prefix + FileNamePatternUtil.TIMESTAMP_REGEX + parseSuffixPattern(suffix);
            break;
        case NAME_WITH_UUID:
            pattern = prefix + FileNamePatternUtil.UUID_REGEX + parseSuffixPattern(suffix);
            break;
        case NAME_WITH_SEQ:
            pattern = prefix + "[0-9]+" + parseSuffixPattern(suffix);
            break;
        case NAME_WITH_PERSISTED_SEQ:
            // for use as inbound filter, it is treated the same as %d
            pattern = prefix + "[0-9]+" + parseSuffixPattern(suffix);
            break;
        default:
            throw new Exception(mMessages.getString("IFF_Invalid_pattern", filename));
        }
        try {
            mRegex = Pattern.compile(pattern);
        } catch(Exception e) {
            throw new Exception(mMessages.getString("IFF_Invalid_pattern", pattern));
        }
    }
    
    public String getFilterExpression() {
        return mRegex.pattern();
    }
    
    public boolean accept(File dir, String name) {
        File aFile = new File(name);
        String aFileName = aFile.getName();
        return mRegex.matcher(aFileName).matches();
    }
    
    private String parseSuffixPattern(String suffix) {
        String suffixPattern = suffix;
        int index = suffix.indexOf(".");
        
        if (index >= 0) {
            suffixPattern = suffix.substring(0, index) + "\\." + suffix.substring(index + 1);
        }
        
        return suffixPattern;
    }
    
}
