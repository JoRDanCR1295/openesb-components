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
 * @(#)LocalDirRegExp.java 
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
package com.sun.jbi.batchext.local;

import com.sun.jbi.batchext.DirRegExp;
import java.io.File;
import java.util.ArrayList;
import java.util.StringTokenizer;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 *  Provides supports for local directory name regular expression.
 *  The input is a full directory name that is composed of some regular expressions, and
 *  the output is a set of directories that are qualified against the regular expressions.
 *
 *  Use the following rules for local directory regular expressions:
 *  <pre>
 *  <p>(1). The directory root, the driver name, directory separators should be
 *          expressed exclusively. That is, don't express them as regexp, only
 *          folder names are expected to appear as regexp.
 *  <p>(2). Regexp shouldn't go over the directory separators. So between two
 *          directory separators, it would be one whole regexp.
 *  <p>(3). Escape all directory separators in directory pattern if the separator
 *          conflicts with regexp special character (one of "*[]()|+{}:.^$?\").
 *          Back slash ('\') is a special character in regexp (it is used to escape
 *          other special characters). For Windows platforms, the directory separator
 *          is back slash, so it should be escaped as "\\".
 *  <p>(4). For Win32 UNC (Universal Naming Convention), the directory root (including
 *          computer name and the shared root folder name) should be expressed exclusively.
 *          That is, don't express the computer name and shared root folder as regexp.
 *
 *  <p>The expected directory pattern would be like
 *  <p>(1). driver:\\regexp1\\regexp2\\regexp3... (Win32 platforms), for example
 *          c:\\eGate$\\^client\\collab\D\\...   (\D means any non-digit char)
 *          d:\\a.b\\c.d\\e.f\\g.h\\[0-9]\\... ('.' means any char)
 *  <p>(2). /regexp1/regexp2/regexp3... (Unix platforms including mounted directory), for example
 *          /abc\d/def/ghi/... (\d means any digit char)
 *          /^PRE[0-9]{5}\.dat$/... (Begin with "PRE" followed by a 5 digit number,
 *                                   with a "dat" extension. "\." means a real char '.'
 *                                   instead of any char. So PRE12345.dat will match,
 *                                   PRE123456dat will not match.)
 *  <p>(3). \\\\machineName\\shared_folder\\regexp1\\regexp2\\regexp3...
 *          (Win32 UNC platforms, prefix for Win32 UNC platforms is \\,
 *          after escaping, it becomes \\\\), for example
 *          \\\\My_Machine\\public\\xyz$\\^abc
 *
 *  <p>NOTE:  If user doesn't specify the absolute directory, then the directory is relative
 *            to the user's current working directory. That is,
 *            the starting point is the user's current working directory.
 *  </pre>
 *
 *  @author Harry Liu
 *  @version cvs revision:    Last Modified: 
 */

// In fact, the dir separators are only used to separate the regexps,
// we can get the native dir separator using File.separator,
// so we can consider to accept an unique separator for all platforms by convention,
// for example, use / as separator, once we separate the input and get the regexps,
// we can construct the dirs using the native File.separator.
// This way, the followings patterns also can be accepted
// (1). driver:/regexp1/regexp2/regexp3 (Win32 platforms)
// (2). \\\\machineName/regexp1/regexp2/regexp3 (Win32 UNC platforms)
// (3). //machineName/regexp1/regexp2/regexp3 (Win32 UNC platforms)

// If user doesn't specify the absolute dir, then the dir is relative
// to the user's current working directory (System.getProperty("user.dir")),
// that is, the starting point is the user's current working dir.

// notes: java.io.File.listRoots() can only list mapped drivers or mounted dirs.

// Change to public for test package even this class is not supposed to be public. Confused?
// Note the similar class FtpDirRegExp is non-public still.
//class LocalDirRegExp implements DirRegExp {
public class LocalDirRegExp implements DirRegExp {
    //private static final Messages mMessages =
    //        Messages.getMessages(LocalDirRegExp.class);
    private static final Logger mLogger =
            Messages.getLogger(LocalDirRegExp.class);
    
    // the starting point (dir root) to search/list the dirs
    private String startingPoint = null;
    
    // the regular expressions separated by dir separator
    private ArrayList regExps = new ArrayList();
    
    // the original input
    private String dirPattern = null;
    
    // future usages
    /*
    private String driver = null;
    private String computerName = null;
    private String dirRootPrefix = null;
    private String dirSeparator = null;
     */
    public LocalDirRegExp(String dirPattern) {
        String msg = null;
        //if (BatchTrace.isTraceMore()) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "LocalDirRegExp.LocalDirRegExp() ...");
        }
        //}
        this.dirPattern = dirPattern;
    }
    // get dirs based on dir regexps
    public ArrayList getDirs() throws Exception {
        String msg = null;
        ArrayList tempDirs = new ArrayList();
        ArrayList tempSubDirs = new ArrayList();
//      RE re = null;
        Pattern re = null;
        Matcher match = null;
        File tempDir = null;
        File [] files = null;
        
        try {
            this.separatePattern();
        } catch (Exception e) {
            msg = "LocalDirRegExp.getDirs(): got exception when calling separatePattern() - ["
                    + e.toString() + "].";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw e;
        }
        
        // specify the starting point (dir root)
        if (mLogger.isLoggable(Level.FINE)) {
            msg = "LocalDirRegExp.getDirs(): The path root is [" + this.startingPoint + "].";
            mLogger.log(Level.FINE, msg);
        }
        //if (this.regExps.isEmpty()) {
        //  return tempDirs;
        //}
        
        tempDirs.add(this.startingPoint);
        
        for (int i = 0; i < this.regExps.size(); i++){
            if (tempDirs.isEmpty()) {
                break;
            }
            // get one level of the regexps
            if (mLogger.isLoggable(Level.FINE)) {
                msg = "LocalDirRegExp.getDirs(): The working regular expression is ["
                        + (String) this.regExps.get(i) + "].";
                mLogger.log(Level.FINE, msg);
            }
            if (System.getProperty("os.name", "").toUpperCase().indexOf("WINDOWS") >= 0) {
                // Case insensitive
//              re = new RE((String) this.regExps.get(i), RE.REG_ICASE);
                re = Pattern.compile((String) this.regExps.get(i), Pattern.CASE_INSENSITIVE);
            } else {
                // Case sensitive
//              re = new RE((String) this.regExps.get(i));
                re = Pattern.compile((String) this.regExps.get(i));
            }
            
            // loop through all dirs under same level,
            // store the qualified sub dirs (next level), then do the same loop.
            for (int j = 0; j < tempDirs.size(); j++){
                tempDir = new File((String) tempDirs.get(j));
                files = tempDir.listFiles();
                if (files == null) {
                    continue;
                }
                for (int k = 0; k < files.length; k++) {
                    if (files[k].isDirectory() ) {
                        match = re.matcher(files[k].getName());
                        //~if ( match.matches() ) {
                        if ( match.find() ) {
                            tempSubDirs.add(files[k].getPath());
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
     * Used to do stand-alone testing.
     * @param       args  Command line parameters.
     * @exception   Exception  If some error occurs.
     */
    public static void main(String args[]) {
        
        if (args.length != 1) {
            System.out.println("Usage: java com.sun.jbi.batchext.local.LocalDirRegExp dirRegExp");
            return;
        }
        
        try {
            DirRegExp dirRegExp = new LocalDirRegExp(args[0]);
            ArrayList al = dirRegExp.getDirs();
            System.out.println("The qualified dirs are: ");
            for (int i = 0; i < al.size(); i++){
                System.out.println(al.get(i));
            }
        } catch (Exception e) {
            e.printStackTrace();
            return;
        }
        return;
    }
    // This method will separate original input,
    // it sets property this.startingPoint and this.regExps.
    private void separatePattern() throws Exception {
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
        
        String dirSeparator = File.separator;
        //if (dirSeparator != null && !dirSeparator.equals("")) {
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
//              dir = (new RE(dirSeparator)).substituteAll(dir, "/");
        }
        //}
        
        // get user's current working directory
        String workingDir = System.getProperty("user.dir");
        
        // get user's current working driver letter if any
        String workingDriver = null;
        if (workingDir.charAt(1) == ':') {
            workingDriver = workingDir.substring(0,1);
        }
        
        // get the starting point (dir root)
        int regStartPos = 0;
        if (dir.startsWith("/")) {
            if (dir.startsWith("//")) {
                // Win32 UNC (like //computerName/shared_folder/regexp1/regexp2)
                // we will get //computerName/shared_folder
                regStartPos = dir.indexOf("/", 2);
                if (regStartPos < 0 || dir.substring(regStartPos).equals("/")) {
                    // only computerName is specified (//computerName or //computerName/)
                    msg = "LocalDirRegExp.separatePattern(): For Win32 UNC, "
                            + "shared folder name is required. You only specified computer name.";
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, msg);
                    }
                    throw new Exception(msg);
                }
                regStartPos++;
                regStartPos = dir.indexOf("/", regStartPos);
                if (regStartPos < 0) {
                    this.startingPoint = "//" + dir.substring(2);
                    regStartPos = dir.length();
                } else {
                    this.startingPoint = "//" + dir.substring(2, regStartPos);
                    regStartPos++;
                }
                // In case of //^computerName$/^shared_folder$/... or
                //            \\\\^computerName$\\^shared_folder$\\...,
                // though they are not expected (they are invalid actually),
                // let's try to translate and accept them because they might
                // be a common mistake that user may make.
//              this.startingPoint = (new RE("/\\^")).substituteAll(this.startingPoint, "/");
//              this.startingPoint = (new RE("\\$/")).substituteAll(this.startingPoint, "/");
                
                Pattern p = Pattern.compile("/\\^");
                Pattern p2 = Pattern.compile("\\$/");
                
                Matcher m1 = p.matcher(this.startingPoint);
                this.startingPoint = m1.replaceAll("/");
                
                Matcher m2 = p2.matcher(this.startingPoint);
                this.startingPoint = m2.replaceAll("/");
                
                if (this.startingPoint.endsWith("$")) {
                    this.startingPoint =
                            this.startingPoint.substring(0, this.startingPoint.length() - 1);
                }
                
                // Replace back to native dir separator
//              this.startingPoint = (new RE("/")).substituteAll(this.startingPoint, dirSeparator);
                Pattern p3 = Pattern.compile("/");
                Matcher m3 = p3.matcher(this.startingPoint);
                this.startingPoint = m3.replaceAll(dirSeparator);
            } else {
                // Win32 or Unix (like /regexp1/regexp2).
                // For Win32, driver is the user's current woring driver (don't need to specify).
                regStartPos = 1;
                this.startingPoint = dirSeparator;
            }
        } else {
            if (dir.charAt(1) == ':') {
                // Win32 driver (like c:/regexp1/regexp2 or c:regexp1/regexp2)
                if (dir.charAt(2) == '/') {
                    // from driver root (like c:/regexp1/regexp2)
                    regStartPos = 3;
                    this.startingPoint = dir.substring(0,2) + dirSeparator;
                } else {
                    // like c:regexp1/regexp2
                    regStartPos = 2;
                    if (dir.substring(0,1).equalsIgnoreCase(workingDriver)) {
                        // treat regexp1/regexp2 as relative path
                        this.startingPoint = workingDir;
                    } else {
                        // we have to use the driver root as starting point (c:/)
                        this.startingPoint = dir.substring(0,2) + dirSeparator;
                    }
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
    
}
