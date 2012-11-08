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
 * @(#)FtpHeuristicsProperties.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/******************************************************************************
 * Copyright © 2006 Sun Microsystems, Inc., 4150 Network Circle, Santa Clara,
 * California 95054, U.S.A. All rights reserved. Sun Microsystems, Inc. has
 * intellectual property rights relating to technology embodied in the product
 * that is described in this document. In particular, and without limitation,
 * these intellectual property rights may include one or more of the U.S. patents
 * listed at http://www.sun.com/patents and one or more additional patents or
 * pending patent applications in the U.S. and in other countries. THIS PRODUCT
 * CONTAINS CONFIDENTIAL INFORMATION AND TRADE SECRETS OF SUN MICROSYSTEMS, INC.
 * USE, DISCLOSURE OR REPRODUCTION IS PROHIBITED WITHOUT THE PRIOR EXPRESS WRITTEN
 * PERMISSION OF SUN MICROSYSTEMS, INC.U.S. Government Rights - Commercial
 * software.  Government users are subject to the Sun Microsystems, Inc. standard
 * license agreement and applicable provisions of the FAR and its supplements.
 * Use is subject to license terms.  This distribution may include materials
 * developed by third parties. Sun, Sun Microsystems, the Sun logo, Java
 * Composite Application Platform Suite,  SeeBeyond, eGate, eInsight, eVision, eTL,
 * eXchange, eView, eIndex, eBAM and  eWay are trademarks or registered trademarks of
 * Sun Microsystems, Inc. in the U.S. and other countries. All SPARC trademarks are
 * used under license and are trademarks or registered trademarks of SPARC
 * International, Inc. in the U.S. and other countries. Products bearing SPARC
 * trademarks are based upon architecture developed by Sun Microsystems, Inc.
 * UNIX is a registered trademark in the U.S. and other countries, exclusively
 * licensed through X/Open Company, Ltd. This product is covered and controlled by
 * U.S. Export Control laws and may be subject to the export or import laws in
 * other countries.  Nuclear, missile, chemical biological weapons or nuclear
 * maritime end uses or end users, whether direct or indirect, are strictly
 * prohibited.  Export or reexport to countries subject to U.S. embargo or to
 * entities identified on U.S. export exclusion lists, including, but not limited
 * to, the denied persons and specially designated nationals lists is strictly
 * prohibited.
 **/
package com.sun.jbi.ftpbc.ftp;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.NoSuchElementException;
import java.util.Properties;
import java.util.StringTokenizer;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;

/**
 * A Properties-like class with the ability to load an
 * FtpHeuristics file for the Batch eWay.
 * @author Harry Liu
 * @version cvs revision:    Last Modified: 
 */

//class FtpHeuristicsProperties extends EBobConnectorProperties {
// This class is copied from EBobConnectorProperties.
// The only intention change is in method loadConfiguration() marked as
// "change STARTS here". Because the class EBobConnectorProperties was
// changed between 451GA and 453GA, to support 451GA, 452GA and 453GA,
// we cannot depend on a specific version super class EBobConnectorProperties,
// so we copy instead of extend EBobConnectorProperties here. Otherwise, we
// get runtime exception IncompatibleClassChangeError when we run it upon
// 451GA core.

class FtpHeuristicsProperties extends Properties {
    private static final Messages mMessages =
            Messages.getMessages(FtpHeuristicsProperties.class);
    private static final Logger mLogger =
            Messages.getLogger(FtpHeuristicsProperties.class);
    
    String style = null;
    
    /**
     * A constructor with the parameter "ftp directory listing style."
     *
     * @param   dirListingStyle    The FTP directory-listing style.
     */
    public FtpHeuristicsProperties(String dirListingStyle) {
        super();
        this.style = dirListingStyle.replace(' ', '_') + ".";
    }
    
    /**
     * Overwrites the super method and compress some log entries.
     *
     * @param       fin           An input stream.
     * @exception   IOException   If some error occurs.
     */
    void loadConfiguration(InputStream fin) throws IOException {
        BufferedReader cfg_buf_input;
        StringTokenizer st;
        String field;
        String sub_field;
        final int SEARCH_FOR_DELIM = 0;
        final int SEARCH_FOR_ITEM = SEARCH_FOR_DELIM + 1;
        int state = SEARCH_FOR_DELIM;
        int line_num;
        String cur_item_line;
        String[] config_delims = new String[4];
        
        // Open the configuration file
        cfg_buf_input = new BufferedReader(new InputStreamReader(fin, "ISO-8859-1"));
        
        for (line_num = 1;
        (cur_item_line = cfg_buf_input.readLine()) != null;
        line_num++) {
            try {
                // Process only non-comments (which start with a #)
                if (!cur_item_line.startsWith("#")) {
                    switch (state) {
                        case SEARCH_FOR_DELIM :
                            if (cur_item_line.startsWith("File/CFG")) {
                                st = new StringTokenizer(cur_item_line, "/");
                                
                                if (st.countTokens() < 7) {
                                    throw new IOException(mMessages.getString("FTPBC-E006049.ERR_EXT_FTP_HEURISTICS_NO_DELIM"));
                                }
                                
                                for (int i = 0; i < 7; i++) {
                                    field = st.nextToken();
                                    
                                    // Delimiter fields
                                    if (3 <= i && i <= 6) {
                                        try {
                                            int oct_posn = field.indexOf("\\o");
                                            
                                            if (-1 == oct_posn) {
                                                throw new IOException(mMessages.getString("FTPBC-E006049.ERR_EXT_FTP_HEURISTICS_NO_DELIM"));
                                            }
                                            oct_posn += 2;
                                            
                                            // Convert the octal number to a character string
                                            int char_val = Integer.parseInt(field.substring(oct_posn), 8);
                                            if (Character.MIN_VALUE <= char_val && char_val <= Character.MAX_VALUE) {
                                                config_delims[i - 3] = "" + (char) char_val;
                                            } else {
                                                throw new IOException(mMessages.getString("FTPBC-E006049.ERR_EXT_FTP_HEURISTICS_NO_DELIM"));
                                            }
                                        } catch (NumberFormatException e) {
                                            throw new IOException(mMessages.getString("FTPBC-E006049.ERR_EXT_FTP_HEURISTICS_NO_DELIM"));
                                        }
                                    }
                                }
                                
                                // Save delimiters as delim1, delim2, delim3, delim4
                                for (int i = 0; i < config_delims.length; i++) {
                                    this.put("delim" + (i + 1), config_delims[i]);
                                }
                                state = SEARCH_FOR_ITEM;
                            }
                            break;
                            
                        case SEARCH_FOR_ITEM :
                            if (!cur_item_line.startsWith("File/CFG")) {
                                st = new StringTokenizer(cur_item_line, config_delims[1]);
                                if (st.countTokens() < 3) {
                                    throw new IOException(mMessages.getString("FTPBC-E006050.ERR_EXT_FTP_HEURISTICS_INCOMPLETE_LINE", new Object[] {line_num}));
                                }
                                
                                // Put into properties list
                                String key =
                                        filterInvalidChars(st.nextToken().trim() + "." + st.nextToken().trim());
                                String value = st.nextToken();
                                value = value.substring(value.indexOf(config_delims[2]) + 1);
                                
                                // Substitute all the octal escapes in the key and value with the real characters
                                key = unescapeOctals(key);
                                value = unescapeOctals(value);
                                
                                // Both key and value must be both non-empty and non-blanks
                                if (!key.equals(".") && (value.length() > 0 && value.trim().length() > 0)) {
                                    this.put(key, value);
                                    if (key.startsWith(this.style)) {
                                        if (mLogger.isLoggable(Level.INFO)) {
                                            mLogger.log(Level.INFO, mMessages.getString("FTPBC-D006040.DBG_EXT_FTP_HEURISTICS_PROP", new Object[] {
                                            		key, value
                                            }));
                                        }
                                    }
                                }
                            }
                            break;
                    }
                }
            } catch (NoSuchElementException e) {
                // hasMoreTokens() lied!
            }
        }
    }
    
    // Tester
    public static void main(String[] args) {
        if (null == args || args.length != 2) {
            System.out.println(
                    "\nUsage: java com.sun.jbi.ftpbc.ftp.FtpHeuristicsProperties <file name> <style>");
            return;
        }
        
        try {
            FtpHeuristicsProperties p = new FtpHeuristicsProperties(args[1]);
            p.load(args[0]);
            p.list(System.out);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    /**
     * Filters out or changes invalid characters in the original string. These characters
     * can cause the <code>java.util.Properties.load()</code> to malfunction.
     *
     * @param     raw       The original string
     * @return    A clean string
     */
    private String filterInvalidChars(String raw) {
        int lraw = raw.length();
        StringBuffer sb = new StringBuffer(lraw);
        char ch;
        for (int i = 0; i < lraw; i++) {
            switch ((ch = raw.charAt(i))) {
                case ' ' :
                    sb.append('_');
                    break;
                case ':' :
                    break;
                case '=' :
                    break;
                default :
                    sb.append(ch);
                    break;
            }
        }
        return sb.toString();
    }
    
    /**
     * Reads a properties list from a file. If the file type ends with a
     * <code>.cfg</code> extension, it is interpreted as an e*Gate
     * configuration file and converted into a properties list.
     * NOTE: All embedded spaces in the section and/or parameter names are replaced
     * by underscores (_), while colons (:) and equal signs (=) are removed from the
     * property key name.
     *
     * @param     fname     The file name of a <code>.cfg</code> or a properties file.
     * @exception java.io.FileNotFoundException
     *              When the configuration or properties file is not found.
     * @exception java.io.IOException
     *              When the file cannot be accessed.
     */
    void load(String resName) throws java.io.IOException,
            java.io.FileNotFoundException {
        InputStream is = getClass().getResourceAsStream(resName);
        if (is != null) {
            loadConfiguration(is);
            is.close();
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006041.DBG_EXT_FTP_HEURISTICS_LOAD", new Object[] {resName}));
            }
        } else {
            String msg = mMessages.getString("FTPBC-E006051.ERR_EXT_FTP_HEURISTICS_NOT_FOUND", new Object[] {resName});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new java.io.FileNotFoundException(msg);
        }
    }
    
    /**
     * Unescapes octal sequences from a given string.
     *
     * @param     s     A string to scan.
     * @return    The string with escaped octal sequences converted.
     */
    private String unescapeOctals(String s) {
        int oct_posn;
        int char_val;
        int start = 0;
        
        if (null == s)
            return s;
        
        while ((oct_posn = s.indexOf("\\o", start)) != -1) {
            try {
                // Convert the octal number to a character string
                if ((oct_posn + 5) <= s.length()) {
                    char_val = Integer.parseInt(s.substring(oct_posn + 2, oct_posn + 5), 8);
                    
                    if (Character.MIN_VALUE <= char_val && char_val <= Character.MAX_VALUE) {
                        s = s.substring(0, oct_posn) + ((char) char_val) + s.substring(oct_posn + 5);
                    }
                } else {
                    throw new NumberFormatException();
                }
            } catch (NumberFormatException e) {
                // Not an octal escape sequence but perhaps a DOS directory name
            }
            start = oct_posn + 1;
        }
        return s;
    }
}
