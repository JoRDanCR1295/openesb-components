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
 * @(#)FtpDirRegExp.java 
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

import java.util.ArrayList;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;
import org.apache.commons.net.ftp.FTPFile;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 *  Provides support for FTP directory name regular expressions. The input is a full
 *  directory name made up of some regular expressions, while the output is
 *  a set of directories that are qualified against the regular expressions.
 *
 *  Use the following rules for FTP directory regular expressions:
 *
 *  <pre>
 *  <p>(1). The directory root, the directory separators should be expressed
 *          exclusively, don't express them in regexp. Only directory names
 *          are expected to appear as regexp.
 *  <p>(2). Regexp shouldn't go over the directory separators. So between any
 *          two directory separators, it would be one whole regexp.
 *  <p>(3). Escape all directory separators in directory pattern if the separator
 *          conflicts with regexp special character (one of "*[]()|+{}:.^$?\").
 *          According to FtpHeuristics, the possible directory separators are
 *          '\', '/' and '.'. Among them, '\' and '.' are special characters used
 *          in regexp. So '\' and '.' should be escaped as "\\" and "\.".
 *  <p>(4). For some ftp servers (for example, MVS PDS, MVS Sequential and MVS GDG), no directory
 *          root indicator. In fact, the directory concept is not very clear, a dataset
 *          has prefix and name. We will not know if the directory is absolute or
 *          relative directory. We will always assume the directory is absolute.
 *          So the directory root (the part before the first directory separator)
 *          should be expressed exclusively. For example, root\.regexp1\.regexp2.
 *
 *  <p>The expected directory pattern would be like
 *  <p>(1). /regexp1/regexp2/regexp3... (Unix), for example
 *          /abc\d/def/ghi (\d means any digit char)
 *          /^PRE[0-9]{5}\.dat$/... (Begin with "PRE" followed by a 5 digit number,
 *                                   with a "dat" extension. "\." means a real char '.'
 *                                   instead of any char. So PRE12345.dat will match,
 *                                   PRE123456dat will not match.)
 *  <p>(2). root\.regexp1\.regexp2... (MVS) (\. is escaped dir separator), for example
 *          EGATEX\.STC\.SAMPLE ("EGATEX" is not regexp, "STC" and "SAMPLE" are regexps).
 *  <p>(3). \\regexp1\\regexp2\\regexp3... (NT), for example
 *          \\abc.efg\\123.456 ('.' means any char).
 *  <p>(4). [regexp1\.regexp2\.regexp3... (VMS) (\. is escaped dir separator), for example
 *          [a.b\.c.d\.efg ('.'. in "a.b" or "c.d" is regexp char, means any char).
 *
 *  </pre>
 *
 *  @author Harry Liu
 *  @author jfu
 *  @version cvs revision:    Last Modified: 
 */

// In fact, the dir separators are only used to separate the regexps,
// so we can consider to accept an unique separator for all platforms by convention,
// for example, use / as separator, once we separate the input and get the regexps,
// we can construct the dirs using the native dir separator (get them from FtpHeuristics).

// If user doesn't specify the absolute dir, then the dir is relative
// to the user's current working directory (where the dir when user logs on the ftp server),
// that is, the starting point is the user's current working dir.

public class FtpDirRegExp implements DirRegExp {
    private static final Messages mMessages =
            Messages.getMessages(FtpDirRegExp.class);
    private static final Logger mLogger =
            Messages.getLogger(FtpDirRegExp.class);
    
    // the starting point (dir root) to search/list the dirs
    private String startingPoint = null;
    
    // the regular expressions separated by dir separator
    private ArrayList regExps = new ArrayList();
    
    // the original input
    private String dirPattern = null;
    
    private FtpFileProvider ftp = null;
    
    
    
    /**
     * Get the directory names.
     * @return  A list of directory names.
     * @exception     Exception If some error occurs.
     */
    public ArrayList getDirs() throws Exception {
        String msg = null;
        ArrayList tempDirs = new ArrayList();
        ArrayList tempSubDirs = new ArrayList();
        String regExp = null;
        String tempDir = null;
        String tempSubDir = null;
        FTPFile [] files = null;
        int pos = 0;
        
        String dirSeparator = this.ftp.getHeuristics().getInterDirDelimiter();
        if (dirSeparator == null) {
            dirSeparator = "";
        }
        
        try {
            this.separatePattern();
        } catch (Exception e) {
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {e, "getDirs()"}));
            }
            throw e;
        }
        
        // specify the starting point (dir root)
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006001.DBG_EXT_FTP_ROOT_DIR", new Object[] {startingPoint}));
        }
        
        tempDirs.add(this.startingPoint);
        
        for (int i = 0; i < this.regExps.size(); i++){
            if (tempDirs.isEmpty()) {
                break;
            }
            // get one level of the regexps
            regExp = (String) this.regExps.get(i);
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006002.DBG_EXT_FTP_REGEX", new Object[] {regExp}));
            }
            
            // loop through all dirs under same level,
            // store the qualified sub dirs (next level), then do the same loop.
            for (int j = 0; j < tempDirs.size(); j++){
                tempDir = (String) tempDirs.get(j);
                files = this.ftp.listFiles(tempDir, regExp);
                if (files == null || files.length == 0) {
                    continue;
                }
                for (int k = 0; k < files.length; k++) {
                    if (files[k].isDirectory()) {
                        
                        tempSubDir = files[k].getName();
                        // For some ftp server (MVS Sequential), listFiles() will return full
                        // sub-names. For example, a dataset is "DATA6.HLIU.FILE.MONK",
                        // when we are in dir "DATA6", we'll get file "HLIU.FILE.MONK";
                        // when we are in dir "DATA6.HLIU", we'll get file "FILE.MONK";
                        // when we are in dir "DATA6.HLIU.FILE", we'll get file "MONK".
                        // So we will try to get the correct part for next loop.
                        if (!dirSeparator.equals("")) {
                            if ((pos = tempSubDir.indexOf(dirSeparator)) > 0) {
                                tempSubDir = tempSubDir.substring(0, pos);
                            }
                        }
                        if (tempDir.endsWith(dirSeparator)) {
                            // For example, the tempDir is root dir /
                            tempSubDir = tempDir + tempSubDir;
                        } else {
                            tempSubDir = tempDir + dirSeparator + tempSubDir;
                        }
                        
                        // Avoid duplicates for some cases like above MVS Sequential dataset "DATA6.HLIU.FILE.MONK"
                        if (!tempSubDirs.contains(tempSubDir)) {
                            tempSubDirs.add(tempSubDir);
                        }
                    }
                }
            }
            tempDirs.clear();
            tempDirs.addAll(tempSubDirs);
            tempSubDirs.clear();
        }
        
        return tempDirs;
    }
    
    /**
     * Separates the directory pattern and get the list of directory regular expressions.
     * @exception     Exception If some error occurs.
     */
    private void separatePattern() throws Exception {
    // This method will separate original input,
    // it sets property this.startingPoint and this.regExps.
        String msg = null;
        if (this.dirPattern == null || this.dirPattern.trim().equals("")) {
            return;
        }
        
        String dir = this.dirPattern.trim();
        
        // Use forward slash '/' as the universal unique dir regexp separator
        // (because '/' is not special char for regexp).
        
        // If the separator is a special regexp char, it is supposed to
        // be escaped with back slash '\' (it becomes back slash '\' (one char)
        // plus separator (one char)), we replace all escaped regexp
        // separator (two characters) with the forward slash '/' (one character).
        // Otherwise, replace the separator (one char) with '/'.
        
        String dirRootPrefix = this.ftp.getHeuristics().getInitDirDelimiter();
        if (dirRootPrefix == null) {
            dirRootPrefix = "";
        }
        
        // Note: some ftp servers (MVS PDS, MVS Sequential, MVS GDG and AS400) don't
        //       have root dir character (initial dir character).
        if (!dirRootPrefix.equals("")) {
            if (DirRegExp.regExpChars.indexOf(dirRootPrefix) >= 0) {
                // Replace back slash '\' (one char) plus dir root prefix (one char)
                // with one forward slash '/' (one char) if any.
                if (dir.startsWith("\\" + dirRootPrefix)) {
                    dir = "/" + dir.substring(2);
                }
            } else {
                if (dir.startsWith(dirRootPrefix)) {
                    dir = "/" + dir.substring(1);
                }
            }
        }
        
        String dirSeparator = this.ftp.getHeuristics().getInterDirDelimiter();
        if (dirSeparator == null) {
            dirSeparator = "";
        }
        
        if (!dirSeparator.equals("")) {
            if (DirRegExp.regExpChars.indexOf(dirSeparator) >= 0) {
                // Replace back slash '\' (one char) plus separator (one char)
                // with one forward slash '/' (one char).
                // Note: class RE requires escape too, so '\' is escaped twice:
                //       one is for java language, another is for RE class,
                //       so following RE class substituteAll will not work.
                // dir = (new RE("\\" + dirSeparator)).substituteAll(dir, "/"); // not work
                int pos = 0;
                while ((pos = dir.indexOf("\\" + dirSeparator)) >= 0) {
                    dir = dir.substring(0, pos) + "/" + dir.substring(pos + 2);
                }
            } else {
                Pattern p = Pattern.compile(dirSeparator);
                Matcher match = p.matcher(dir);
                dir = match.replaceAll("/");
            }
        }
        
        // get user's current working directory
        String workingDir = this.ftp.pwd();
        
        // Remove the absolute path envelopes if any
        String absPathEnvelope = this.ftp.getHeuristics().getAbsPathEnvelope();
        if (absPathEnvelope == null) {
            absPathEnvelope = "";
        }
        
        if (!absPathEnvelope.equals("")) {
            absPathEnvelope = absPathEnvelope.substring(0, 1);
            if (workingDir.startsWith(absPathEnvelope)) {
                workingDir = workingDir.substring(1);
            }
            if (workingDir.endsWith(absPathEnvelope)) {
                workingDir = workingDir.substring(0, workingDir.length() - 1);
            }
        }
        
        // Remove the trailing dir separator if any
        if (!dirSeparator.equals("") &&
                workingDir.endsWith(dirSeparator)) {
            workingDir = workingDir.substring(0, workingDir.length() - 1);
        }
        
        // get the starting point (dir root)
        int regStartPos = 0;
        if (dir.startsWith("/")) {
            regStartPos = 1;
            this.startingPoint = dirRootPrefix;
        } else {
            if (dirRootPrefix.equals("")) {
                // Note: some ftp servers (MVS PDS, MVS Sequential, MVS GDG and AS400) don't
                //       have root dir character (initial dir character). So we don't
                //       know the dir is absolute or relative path. We always assume
                //       the dir is the absolute.
                
                // For MVS PDS, MVS Sequential, MVS GDG and AS400, the first part is root dir,
                // and it is required not to be regexp. Get it as starting point.
                regStartPos = dir.indexOf("/");
                if (regStartPos < 0) {
                    this.startingPoint = dir;
                    regStartPos = dir.length();
                } else {
                    this.startingPoint = dir.substring(0, regStartPos);
                    regStartPos++;
                }
                // In case of ^ROOT$/regexp1/regexp2/...   ,
                // though it is not expected (it is invalid actually),
                // let's try to translate and accept it because it might
                // be a common mistake that user may make.
                if (this.startingPoint.startsWith("^")) {
                    this.startingPoint =
                            this.startingPoint.substring(1);
                }
                if (this.startingPoint.endsWith("$")) {
                    this.startingPoint =
                            this.startingPoint.substring(0, this.startingPoint.length() - 1);
                }
            } else {
                // relative path is used (like regexp1/regexp2).
                regStartPos = 0;
                this.startingPoint = workingDir;
            }
        }
        
        // Separate the input string into a set of regexps using char "/"
        dir = dir.substring(regStartPos);
        
        // remove trailing "/"
        while (dir.endsWith("/")) {
            dir = dir.substring(0, dir.length() - 1);
        }
        
        if (dir.equals("")) {
            return;
        }
        
        StringTokenizer st = new StringTokenizer(dir, "/");
        while (st.hasMoreTokens()) {
            this.regExps.add(st.nextToken());
        }
    }
    
    /**
     * Constructs an object based on the FtpDirRegExp class.
     * @param   dirPattern    The directory pattern that is made up of directory regular expressions.
     * @param          ftp    An instance of the FtpFileProvider object.
     */
    public FtpDirRegExp(String dirPattern, FtpFileProvider ftp) {
        this.dirPattern = dirPattern;
        // Note: This ftp instance should be ready for listing files.
        //       That is, it is connected and logged in.
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"FtpDirRegExp(String dirPattern, FtpFileProvider ftp)"}));
        }
        this.ftp = ftp;
    }
}
