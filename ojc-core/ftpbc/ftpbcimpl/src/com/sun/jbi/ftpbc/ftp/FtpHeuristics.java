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
 * @(#)FtpHeuristics.java 
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

import java.io.FileInputStream;
import java.util.Enumeration;
import java.util.Properties;
import java.util.StringTokenizer;
import java.util.Vector;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * This class represents the FtpHeuristics configuration file.
 * @author Harry Liu
 * @version cvs revision:    Last Modified: 
 */
public class FtpHeuristics {
    private static final Messages mMessages =
            Messages.getMessages(FtpHeuristics.class);
    private static final Logger mLogger =
            Messages.getLogger(FtpHeuristics.class);
    
    public static final String UNIX_DIR_LISTING_STYLE = "UNIX";
    public static final String EUC_JP_UNIX_DIR_LISTING_STYLE = "UNIX (EUC-JP)";
    public static final String SJIS_UNIX_DIR_LISTING_STYLE = "UNIX (SJIS)";
    private String workingLine = null;
    
    // for performance sake, keep the original heuristics static
    // props points to either user_defined_props or built_in_props
    private Properties props = null;
    private Properties user_defined_props = null;
    
    // only built-in heuristics is static and global
    // all the providers that use built-in heuristics refer to this instance
    private static Properties built_in_props = null;
    
    private String dirListingStyle;
    
    private String supportCmdMask;
    private int headerSkipLines;
    private String headerSkipRegex;
    private int trailerSkipLines;
    private String trailerSkipRegex;
    private String directoryRegex;
    private boolean fileLinkRealData;
    private String fileLinkRegex;
    private String linkSymbolRegex;
    private String listLineFormat;
    private int validFileLineMinPosn;
    private boolean fileNameIsLastObject;
    private int fileNamePosn;
    private int fileNameLength;
    private int fileExtPosn;
    private int fileExtLength;
    private boolean fileSizeVerifiable;
    private int fileSizePosn;
    private int fileSizeLength;
    private String absPathEnvelope;
    private boolean listDirYieldsAbsPath;
    private String absPathDelims;
    private boolean cdBeforeListing;
    private boolean dirRequireTerminator;
    private String initDirDelimiter;
    private String interDirDelimiter;
    private String initFileDelimiter;
    private String endingFileDelimiter;
    
    private String cfgFilePath;
    
    /**
     * Constructs a new FtpHeuristics object using default directory listing style.
     * @exception  Exception   If some error occurs.
     */
    public FtpHeuristics() throws Exception {
        this(UNIX_DIR_LISTING_STYLE);
    }
    
    public FtpHeuristics(String style, String cfgFilePath) throws Exception {
        if (mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"FtpHeuristics.FtpHeuristics()"}));
        
        String msg = null;
        this.cfgFilePath = cfgFilePath;
        if ( cfgFilePath == null && cfgFilePath.trim().length() == 0 ) {
            throw new Exception(mMessages.getString("FTPBC-E006021.ERR_EXT_FTP_MISS_OR_BAD_PARAM", 
                    new Object[] {"UserHeuristicsLocation", ""}));
        }
        
        if ( style == null && style.trim().length() == 0 ) {
            throw new Exception(mMessages.getString("FTPBC-E006021.ERR_EXT_FTP_MISS_OR_BAD_PARAM", 
                    new Object[] {"UserDefinedDirectoryListingStyle", ""}));
        }
        
        FtpHeuristicsProperties tempProp = new FtpHeuristicsProperties(style);
        
        FileInputStream is = null;

        try {
            is = new FileInputStream(cfgFilePath);
            tempProp.loadConfiguration(is);
            is.close();
            this.props = this.user_defined_props = (java.util.Properties) tempProp;
        } catch (Exception e) {
            msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION",
                    new Object[] {"FtpHeuristics(String style, String cfgFilePath)", e});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg, e);
            }
            throw new Exception(msg, e);
        } finally {
            if ( is != null ) {
                try {
                    is.close();
                } catch (Exception e) {
                }
            }
        }
        setProps(style);
    }
    
    /**
     * Constructs a new FtpHeuristics object using specified directory listing style.
     * @param      style  The directory listing style.
     * @exception  Exception   If some error occurs.
     */
    public FtpHeuristics(String style) throws Exception {
        if (mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"FtpHeuristics.FtpHeuristics(String style)"}));
        
        String msg = null;

        if (FtpHeuristics.built_in_props == null) {
            FtpHeuristicsProperties tempProp = new FtpHeuristicsProperties(style);
            try {
                tempProp.load(FtpHeuristics.getHeuristicsCfgFile());
                FtpHeuristics.built_in_props = (java.util.Properties) tempProp;
            } catch (Exception e) {
                msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION",
                        new Object[] {"FtpHeuristics(String style)", e});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg, e);
                }
                throw new Exception(msg, e);
            }
        } else {
            String s = style.replace(' ', '_') + ".";
            Enumeration enumer = FtpHeuristics.built_in_props.propertyNames();
            String key = null;
            while (enumer.hasMoreElements()) {
                key = (String) enumer.nextElement();
                if (key.startsWith(s)) {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, key + "=" + FtpHeuristics.built_in_props.getProperty(key));
                    }
                }
            }
        }
        // this is a heuristics using built-in
        this.props = FtpHeuristics.built_in_props;
        setProps(style);
    }
    
    /**
     * Accessor - Get Absolute Path Delimiters.
     * @return    Absolute Path Delimiters.
     */
    public String getAbsPathDelims() {
        return absPathDelims;
    }
    /**
     * Gets Absolute Path Envelope.
     * @return    The Absolute Path Envelope.
     */
    public String getAbsPathEnvelope() {
        return absPathEnvelope;
    }
    /**
     * Gets Directory Regular Expression.
     * @return    The Directory Regular Expression.
     */
    public String getDirectoryRegex() {
        return directoryRegex;
    }
    /**
     * Gets Dir Listing Style.
     * @return    The Dir Listing Style.
     */
    public String getDirListingStyle() {
        return dirListingStyle;
    }
    /**
     * Gets File Ext Length.
     * @return    The File Ext Length.
     */
    public int getFileExtLength() {
        return fileExtLength;
    }
    /**
     * Gets File Ext Position.
     * @return    The File Ext Position.
     */
    public int getFileExtPosn() {
        return fileExtPosn;
    }
    /**
     * Gets File Link Regular Expression.
     * @return    The File Link Regular Expression.
     */
    public String getFileLinkRegex() {
        return fileLinkRegex;
    }
    /**
     * Gets File Name Length.
     * @return    The File Name Length.
     */
    public int getFileNameLength() {
        return fileNameLength;
    }
    /**
     * Gets File Name Position.
     * @return    The File Name Position.
     */
    public int getFileNamePosn() {
        return fileNamePosn;
    }
    /**
     * Gets File Size Length.
     * @return    The File Size Length.
     */
    public int getFileSizeLength() {
        return fileSizeLength;
    }
    /**
     * Gets File Size Position.
     * @return    The File Size Position.
     */
    public int getFileSizePosn() {
        return fileSizePosn;
    }
    /**
     * Gets Header Skip Lines.
     * @return    The Header Skip Lines.
     */
    public int getHeaderSkipLines() {
        return headerSkipLines;
    }
    /**
     * Gets Header Skip Regular Expression.
     * @return    The Header Skip Regular Expression.
     */
    public String getHeaderSkipRegex() {
        return headerSkipRegex;
    }
    /**
     * Gets Link Symbol Regular Expression.
     * @return    The Link Symbol Regular Expression.
     */
    public String getLinkSymbolRegex() {
        return linkSymbolRegex;
    }
    /**
     * Gets List Line Format.
     * @return    The List Line Format.
     */
    public String getListLineFormat() {
        return listLineFormat;
    }
    /**
     * Gets Properties.
     * @return    The Properties.
     */
    public Properties getProps() {
        return props;
    }
    /**
     * Gets Support Cmd Mask.
     * @return    The Support Cmd Mask.
     */
    public String getSupportCmdMask() {
        return supportCmdMask;
    }
    /**
     * Gets Trailer Skip Lines.
     * @return    The Trailer Skip Lines.
     */
    public int getTrailerSkipLines() {
        return trailerSkipLines;
    }
    /**
     * Gets Trailer Skip Regular Expression.
     * @return    The Trailer Skip Regular Expression.
     */
    public String getTrailerSkipRegex() {
        return trailerSkipRegex;
    }
    /**
     * Gets Valid File Line Min Position.
     * @return    The Valid File Line Min Position.
     */
    public int getValidFileLineMinPosn() {
        return validFileLineMinPosn;
    }
    /**
     * Gets the status of Change Dir Before Listing.
     * @return    The status of Change Dir Before Listing.
     */
    public boolean isCdBeforeListing() {
        return cdBeforeListing;
    }
    /**
     * Gets the status of Dir Require Terminator.
     * @return    The status of Dir Require Terminator.
     */
    public boolean isDirRequireTerminator() {
        return dirRequireTerminator;
    }
    /**
     * Gets the status of File Link Real Data.
     * @return    The status of File Link Real Data.
     */
    public boolean isFileLinkRealData() {
        return fileLinkRealData;
    }
    /**
     * Gets the status of File Name Is Last Object.
     * @return    The status of File Name Is Last Object.
     */
    public boolean isFileNameIsLastObject() {
        return fileNameIsLastObject;
    }
    /**
     * Gets the status of File Size Verifiable.
     * @return    The status of File Size Verifiable.
     */
    public boolean isFileSizeVerifiable() {
        return fileSizeVerifiable;
    }
    /**
     * Gets the status of List Dir Yields Absolute Path.
     * @return    The status of List Dir Yields Absolute Path.
     */
    public boolean isListDirYieldsAbsPath() {
        return listDirYieldsAbsPath;
    }
    
    /**
     * Get the directory/file delimiter set.
     * @param     delimiterString  Dlimiters.
     * @return    The directory/file delimiters,
     *            it returns empty string entry "" if the delimiters are not defined.
     */
    private static String[] getDlimiters(String delimiterString) {
        String delimiterArray[] = {"", "", "", ""};
        
        if (delimiterString == null || delimiterString.equals("")) {
            return delimiterArray;
        }
        
        int i = 0;
        for (int pos = 0; pos < delimiterString.length(); pos++) {
            // skip escape character
            if (delimiterString.substring(pos, pos + 1).equals("\\")) {
                pos++;
                // "\0" means place holder - skip it.
                if (delimiterString.substring(pos, pos + 1).equals("0")) {
                    i++;
                    continue;
                }
            }
            delimiterArray[i++] = delimiterString.substring(pos, pos + 1);
        }
        return delimiterArray;
    }
    
    /**
     * Get FtpHeuristics resource name.
     * @return    The full name of the FtpHeuristics resource.
     */
    private static String getHeuristicsCfgFile() {
        return "/com/sun/jbi/ftpbc/ftp/FtpHeuristics.cfg";
    }
    
    /**
     * Creates a remote path name taking into consideration
     * any special enveloping characters for the absolute path name and
     * absolute path-name delimiters on the remote FTP host.
     * @param        dirName  The remote directory name.
     * @param   baseFileName  The remote base file name.
     * @return  The full remote file name.
     */
    public String buildFullFileName(String dirName, String baseFileName) {
        if (mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"buildFullFileName(String dirName, String baseFileName)"}));
        
        if (dirName == null) {
            dirName = "";
        }
        
        if (baseFileName == null) {
            baseFileName = "";
        }
        
        dirName = dirName.trim();
        baseFileName = baseFileName.trim();
        int envLen = 0;
        String fullFileName = "";
        
        // Filter out illegal cases
        if (dirName.equals("") && baseFileName.equals("")) {
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006014.WRN_EXT_FTP_INVALID_PATH", new Object[] {"buildFullFileName(String dirName, String baseFileName)", dirName, baseFileName}));
            }
            return null;
        }
        
        // note: for NT3.5 and NT 4.0 ftp servers, the delimiter can be either "\" or "/".
        //       In FtpHeuristics, it's "\". So we can replace "/" with "\" first.
        if (this.dirListingStyle.equals("NT 4.0") ||
                this.dirListingStyle.equals("NT 3.5")) {
            dirName = dirName.replace('/', '\\');
        }
        
        // Build the proper remote directory name first, including opening
        // Absolute Path Envelope (if any), and Initial Filename Delimiter
        if (this.absPathEnvelope != null && !this.absPathEnvelope.equals("")) {
            envLen = this.absPathEnvelope.length() / 2;
            if (!(!dirName.equals("") ? dirName : baseFileName)
            .startsWith(this.absPathEnvelope.substring(0, envLen))) {
                fullFileName = this.absPathEnvelope.substring(0, envLen);
            }
        }
        
        // A real base name and/or just a remote directory name is given
        if (!dirName.equals("")) {
            if (!initDirDelimiter.equals("") &&
                    !dirName.startsWith(initDirDelimiter)) {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006030.DBG_EXT_FTP_HEURISTICS_RELATIVE_PATH", new Object[] {"buildFullFileName(String dirName, String baseFileName)"}));
                }
            }
            fullFileName += dirName;
            // remove end envelope if any
            if (this.absPathEnvelope != null && !this.absPathEnvelope.equals("")) {
                if (fullFileName.endsWith(this.absPathEnvelope.substring(envLen))) {
                    fullFileName = fullFileName.substring(0, fullFileName.length() - envLen);
                }
            }
            
            // A real base name
            if (!baseFileName.equals("")) {
                if (!fullFileName.endsWith(initFileDelimiter)
                && !fullFileName.endsWith(interDirDelimiter)
                && !baseFileName.startsWith(interDirDelimiter)
                && !baseFileName.startsWith(initFileDelimiter)) {
                    // Special handling for MVS GDG (based on C logic - QAI30015 ?)
                    // Construct both stc.sample.gdg.g0001v00 and stc.sample.gdg(0) - for MVS GDG,
                    // baseFileName may be "g0001v00", "(0)",  "(-2)", "(+0)" or "(+1)"
                    if (dirListingStyle.equals("MVS GDG")) {
                        try {
                            Pattern pattern = Pattern.compile("^\\(([+-])?([0-9])+\\)$");
                            Matcher match = pattern.matcher(baseFileName);
                            if ( !match.find() ) {
                                fullFileName += initFileDelimiter;
                            }
                        } catch (Exception e) {
                            fullFileName += initFileDelimiter;
                            // don't throw exception
                        }
                    } else {
                        fullFileName += initFileDelimiter;
                    }
                }
                
                // Attach the file name, including Ending Filename Delimiter
                fullFileName += baseFileName;
                // For VMS the endingFileDelimiter is optional and is not at end
                if (!this.dirListingStyle.equals("VMS") &&
                        !fullFileName.endsWith(endingFileDelimiter)) {
                    fullFileName += endingFileDelimiter;
                }
            }
            // Just a remote directory name
            else {
                if (this.dirRequireTerminator && !fullFileName.endsWith(initFileDelimiter)) {
                    fullFileName += initFileDelimiter;
                }
            }
        }
        // An otherwise fully-qualified remote pathname is given
        else {
            fullFileName += baseFileName;
            // For VMS the endingFileDelimiter is optional and is not at end
            if (!this.dirListingStyle.equals("VMS") &&
                    !fullFileName.endsWith(endingFileDelimiter)) {
                fullFileName += endingFileDelimiter;
            }
        }
        
        // Add the Closing Absolute Pathname Envelope
        if (this.absPathEnvelope != null && !this.absPathEnvelope.equals("")) {
            if (!fullFileName.endsWith(this.absPathEnvelope.substring(envLen))) {
                fullFileName += this.absPathEnvelope.substring(envLen);
            }
        }
        
        // In case we want to know whether the correct full file name is built or not.
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006031.DBG_EXT_FTP_HEURISTICS_RESOLVED_PATH", new Object[] {"buildFullFileName(String dirName, String baseFileName)", fullFileName}));
        }
        
        return fullFileName;
    }
    
    /**
     * Strips off the base name of a fully qualified remote
     * file name, pursuant to any special enveloping characters for the
     * absolute path name and absolute path-name delimiters on the remote FTP host.
     * @param    fullFileName  The full remote file name.
     * @return   The base part of the remote file name.
     */
    public String getBaseFileName(String fullFileName) {
        if (mLogger.isLoggable(Level.FINE)) 
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"FtpHeuristics.getBaseFileName(String fullFileName)"}));
        
        fullFileName = fullFileName.trim();
        // Check inputs
        if (fullFileName == null || fullFileName.equals("")) {
            return null;
        }
        
        // note: for NT3.5 and NT 4.0 ftp servers, the delimiter can be either "\" or "/".
        //       In FtpHeuristics, it's "\". So we can replace "/" with "\" first.
        if (this.dirListingStyle.equals("NT 4.0") ||
                this.dirListingStyle.equals("NT 3.5")) {
            fullFileName = fullFileName.replace('/', '\\');
        }
        
        String baseFileName = fullFileName;
        int envLen;
        
        // Remove end envelope if any
        if (this.absPathEnvelope != null && !this.absPathEnvelope.equals("")) {
            envLen = this.absPathEnvelope.length() / 2;
            if (baseFileName.endsWith(this.absPathEnvelope.substring(envLen))) {
                baseFileName = baseFileName.substring(0, baseFileName.length() - envLen);
            }
        }
        
        // Get base file name (right to left)
        // Check ending file delimiter
        if (!endingFileDelimiter.equals("")) {
            // Remarks: For VMS file "node1::device1:[dir1.dir2]file1.ext;2", the version number "2" will
            //          be stripped, we will get base file name as "file1.ext", it is by old Monk design. ??
            //          In case we'd like to get "file1.ext;2" as base file name,
            //          probably (?) we can perform one of following options:
            //          1. Change the FtpHeuristics.def/cfg, "Absolute Pathname Delimiter Set" from
            //             "[.];" to "[.]".
            //          2. Replace following if-condition (1 line java code) with following line:
            //             if (baseFileName.endsWith(endingFileDelimiter)) {  // option #2; actually VMS is skipped
            //          3. Skip the processing for "VMS" explicitly - add one more condition (hardcode) like
            //             if (not VMS)
            //
            //          Note we cannot simply remove this process block because that would make MVS
            //          file "dir001.dir002(file001.dat)" stripped wrongly.
            //
            //if (baseFileName.lastIndexOf(endingFileDelimiter) >= 0) {  // old Monk logic - sometimes not work for VMS
            if (baseFileName.endsWith(endingFileDelimiter)) {  // option #2; actually VMS is skipped
                baseFileName = baseFileName.substring(0, baseFileName.lastIndexOf(endingFileDelimiter));
            }
        }
        
        // Check init file delimiter (or intermediate dir delimiter)
        if (!initFileDelimiter.equals("") || !interDirDelimiter.equals("")) {
            String delim =
                    (!initFileDelimiter.equals("") ? initFileDelimiter : interDirDelimiter);
            if (baseFileName.lastIndexOf(delim) >= 0) {
                baseFileName = baseFileName.substring(baseFileName.lastIndexOf(delim) + 1);
            }
            //? no directory part
            //? else {
            //? baseFileName = "";
            //? }
        }
        
        // In case we want to know whether the correct base file name is got.
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006032.DBG_EXT_FTP_HEURISTICS_RESOLVED_BASENAME", new Object[] {"FtpHeuristics.getBaseFileName(String fullFileName)", baseFileName}));
        }
        
        return baseFileName;
    }
    
    /**
     * Gets Ending File Delimiter.
     * @return    The Ending File Delimiter.
     */
    public String getEndingFileDelimiter() {
        return endingFileDelimiter;
    }
    
    /**
     * Gets Initial Dir Delimiter.
     * @return    The Initial Dir Delimiter.
     */
    public String getInitDirDelimiter() {
        return initDirDelimiter;
    }
    
    /**
     * Gets Initial File Delimiter.
     * @return    The Initial File Delimiter.
     */
    public String getInitFileDelimiter() {
        return initFileDelimiter;
    }
    
    /**
     * Gets Inter Dir Delimiter.
     * @return    The Inter Dir Delimiter.
     */
    public String getInterDirDelimiter() {
        return interDirDelimiter;
    }
    
    /**
     * Gets the status of Blank Delimited.
     * @return    The status of Blank Delimited.
     */
    public boolean isBlankDelimited() {
        return this.listLineFormat.equals("Blank Delimited");
    }
    
    /**
     * Gets the status of Fixed Format.
     * @return    The status of Fixed Format.
     */
    public boolean isFixedFormat() {
        return this.listLineFormat.equals("Fixed");
    }
    
    /**
     * Parses a raw line the comes from a socket.
     * @param    aLine  A line to be parsed.
     * @return   An FtpHeuristicsFile object.
     */
    FtpHeuristicsFile parseALine(String aLine) {
        if (mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"parseALine(String aLine)"}));
        
        String msg = null;

        if (aLine == null || aLine.equals(""))
            return null;
        
        FtpHeuristicsFile aFile;
        String[] tokens = null;
        int[] tokenPos = null;
        
        // Tokenize the line for "Blank Delimited"
        if (this.isBlankDelimited()) {
            // workingLine holds the unprocessed line(s).
            // one file entry may occupy more than one lines.
            // a wrapped line needs to be re-constructed (for example, VMS).
            if (this.workingLine != null) {
                // concat the adjacent lines
                aLine = this.workingLine + aLine;
                this.workingLine = null;
            }
            
            Vector v = new Vector();
            StringTokenizer st = new StringTokenizer(aLine, " ");
            while (st.hasMoreTokens()) {
                v.add(st.nextToken());
            }
            tokens = new String[v.size()];
            tokenPos = new int[v.size()];
            v.copyInto(tokens);
            int tmpPos = 0;
            for (int i = 0; i < tokens.length; i++) {
                tmpPos = aLine.indexOf(tokens[i], tmpPos);
                tokenPos[i] = tmpPos;
                tmpPos += tokens[i].length();
            }
        }
        
        // Skip line without Minimum Number of Positions
        // Need to consider a wrap line (a long file name)
                /*
                 * -- Test if a long File Name field has caused the list line to
                 *    wrap to the next line.  If so, join both lines and retokenize
                 */
        if (this.isBlankDelimited()) {
            if (tokens.length < this.validFileLineMinPosn) {
                if (this.fileNamePosn > 0 &&
                        tokens.length == this.fileNamePosn) {
                    //?this condition is too strict for wrapping lines, don't check it.
                    // In current VMS heuristics, fileNameLength is defined as 21,
                    // it may be wrong for some version's VMS ftp sites (it looks not a fix value,
                    // 21, 20, 19, etc), so don't rely on it for safety.
                    //?if (this.fileNameLength > 0 &&
                    //?	   tokens[this.fileNamePosn - 1].length() >= this.fileNameLength) {
                    this.workingLine = aLine;
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006033.DBG_EXT_FTP_HEURISTICS_RAW_LINE_WRAPPED", new Object[] {
                            "parseALine(String aLine)",
                            aLine
                        }));
                    }
                    return null;
                    //?}
                }

                if (mLogger.isLoggable(Level.INFO)) 
                    mLogger.log(Level.INFO, mMessages.getString("FTPBC-D006034.DBG_EXT_FTP_HEURISTICS_RAW_LINE_TOO_FEW_TOKEN", new Object[] {"parseALine(String aLine)", aLine}));
                
                return null;
            }
        } else {
            if (aLine.length() < this.validFileLineMinPosn) {
                if (mLogger.isLoggable(Level.INFO))
                    mLogger.log(Level.INFO, mMessages.getString("FTPBC-D006035.DBG_EXT_FTP_HEURISTICS_RAW_LINE_TOO_FEW_POS", new Object[] {"parseALine(String aLine)", aLine}));
                
                return null;
            }
        }
        
        String rawListing = aLine;
        boolean dataAvailable = FtpHeuristicsFile.REAL_DATA_AVAILABLE;
        int type = FtpHeuristicsFile.UNKNOWN_TYPE;
        String link = null;
        String name = null;
        long size = 0;
        try {
            //RE regExp;
            
            Pattern regExp;
            Matcher match;
            
            aFile = new FtpHeuristicsFile();
            
            // set rawListing
            aFile.setRawListing(rawListing);
            
            // set dataAvailable - file link real data available?
            aFile.setDataAvailable(this.fileLinkRealData);

            
            // set type
            
            // Some ftp heuristics (MVS PDS, MVS Sequential, MVS GDG) have not given
            // directoryRegex (empty), that doesn't mean no dir.
            // So some dir entries may be treated as file type always.
            // We can determine according to the listed name. For MVS PDS, MVS Sequential
            // and MVS GDG, listFiles() will return full sub-names. For example,
            // a dataset is "DATA6.HLIU.FILE.MONK",
            // when we are in dir "DATA6", we'll get file "HLIU.FILE.MONK";
            // when we are in dir "DATA6.HLIU", we'll get file "FILE.MONK";
            // when we are in dir "DATA6.HLIU.FILE", we'll get file "MONK".
            
            // So we will adjust type according to name.
            if (this.directoryRegex != null && !this.directoryRegex.equals("")) {
                //regExp = new RE(this.directoryRegex);
                
                regExp = Pattern.compile(this.directoryRegex);
                match = regExp.matcher(aLine);
                if ( match.find() ) {
                    type = FtpHeuristicsFile.DIRECTORY_TYPE;
                }
            }
            if (this.fileLinkRegex != null 
                    && !this.fileLinkRegex.equals("") 
                    && type == FtpHeuristicsFile.UNKNOWN_TYPE) {
                regExp = Pattern.compile(this.fileLinkRegex);
                match = regExp.matcher(aLine);
                if (match.find())
                    type = FtpHeuristicsFile.SYMBOLIC_LINK_TYPE;
            }
            
            if (type == FtpHeuristicsFile.UNKNOWN_TYPE) {
                type = FtpHeuristicsFile.FILE_TYPE;
            }
            
            aFile.setType(type);
            
            // set link/name/size
            // Blank Delimited
            if (this.isBlankDelimited()) {
                // Get the base name
                if (this.fileNamePosn > 0) {
                    // Check if this is the last object and for imbedded blanks
                    if (this.fileNameIsLastObject && tokens.length > this.fileNamePosn) {
                        name = aLine.substring(tokenPos[this.fileNamePosn - 1] - 1);
                        //? get rid of absolute path and special envelop if there is any
                        // MVS may return full file name if you list it using absolute path name
                        if (this.absPathEnvelope != null && !this.absPathEnvelope.equals("") &&
                                name.startsWith(this.absPathEnvelope.substring(0,1))) {
                            name = this.getBaseFileName(name);
                        }
                        
                        // See if this is a link, if so parse out the link symbol
                        if (aFile.getType() == FtpHeuristicsFile.SYMBOLIC_LINK_TYPE
                                && this.linkSymbolRegex != null && !this.linkSymbolRegex.equals("")) {
                            
                            regExp = Pattern.compile(this.linkSymbolRegex);
                            match = regExp.matcher(name);
                            if ( match.find() ) {
                                int posStart = match.start();
                                int posEnd = match.end();
                                link = name.substring(posEnd);
                                name = name.substring(0, posStart);
                            }
                        }
                    } else {
                        name = tokens[this.fileNamePosn - 1];
                        //? get rid of absolute path and special envelop if there is any
                        // MVS may return full file name if you list it using absolute path name
                        if (this.absPathEnvelope != null && !this.absPathEnvelope.equals("") &&
                                name.startsWith(this.absPathEnvelope.substring(0,1))) {
                            name = this.getBaseFileName(name);
                        }
                    }
                }
                
                // Get the extension if any
                if (this.fileExtPosn > 0) {
                    name += "." + tokens[this.fileExtPosn - 1];
                }
                
                // Get the File Size if available
                if (this.fileSizeVerifiable &&
                        this.fileSizePosn > 0 && tokens.length > this.fileSizePosn &&
                        type != FtpHeuristicsFile.DIRECTORY_TYPE &&
                        type != FtpHeuristicsFile.UNKNOWN_TYPE) {
                    try {
                        size = Integer.parseInt(tokens[this.fileSizePosn - 1]);
                    } catch (NumberFormatException e) {
                        if (mLogger.isLoggable(Level.FINE))
                            mLogger.log(Level.FINE, mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION",
                                    new Object[] {"parseALine(String aLine)", e}));
                    }
                }
                
            } // Blank Delimited
            
            // Fixed Line
            else if (this.isFixedFormat()) {
                // Get the base name
                if (this.fileNamePosn > 0) {
                    int nameLen = this.fileNameLength;
                    
                    // If Name field length is not given and field is the last
                    // one, calculate it and make it left justified (-ve)
                    
                    if (nameLen == 0 && this.fileNameIsLastObject) {
                        nameLen = - (aLine.length() - this.fileNamePosn + 1);
                    }
                    
                    if (nameLen < 0) {
                        nameLen = -nameLen;
                    }
                    if (nameLen != 0) {
                        name =
                                aLine.substring(this.fileNamePosn - 1, this.fileNamePosn - 1 + nameLen).trim();
                        //? get rid of absolute path and special envelop if there is any
                        // MVS may return full file name if you list it using absolute path name
                        if (this.absPathEnvelope != null && !this.absPathEnvelope.equals("") &&
                                name.startsWith(this.absPathEnvelope.substring(0,1))) {
                            name = this.getBaseFileName(name);
                        }
                        
                        // See if this is a link, if so parse out the link symbol
                        if (aFile.getType() == FtpHeuristicsFile.SYMBOLIC_LINK_TYPE
                                && this.linkSymbolRegex != null && !this.linkSymbolRegex.equals("")) {
                            regExp = Pattern.compile(this.linkSymbolRegex);
                            match = regExp.matcher(name);
                            if ( match.find() ) {
                                int posStart = match.start();
                                int posEnd = match.end();
                                link = name.substring(posEnd).trim();
                                name = name.substring(0, posStart).trim();
                            }
                        }
                    }
                }
                
                // Get the extension if any
                if (this.fileExtPosn > 0 && this.fileExtLength != 0) {
                    int extLen = this.fileExtLength;
                    if (extLen < 0) {
                        extLen = -extLen;
                    }
                    name += "."
                            + aLine.substring(this.fileExtPosn - 1, this.fileExtPosn - 1 + extLen).trim();
                }
                
                // Get the File Size if available
                if (this.fileSizeVerifiable && this.fileSizePosn > 0
                        && aLine.length() > this.fileSizePosn
                        && this.fileSizeLength != 0
                        && type != FtpHeuristicsFile.DIRECTORY_TYPE
                        && type != FtpHeuristicsFile.UNKNOWN_TYPE) {
                    int sizeLen = this.fileSizeLength;
                    if (sizeLen < 0) {
                        sizeLen = -sizeLen;
                    }
                    try {
                        size =
                                Integer.parseInt(
                                aLine.substring(this.fileSizePosn - 1, this.fileSizePosn - 1 + sizeLen).trim());
                    } catch (NumberFormatException e) {
                        if (mLogger.isLoggable(Level.FINE)) 
                            mLogger.log(Level.FINE, mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION",
                                    new Object[] {"parseALine(String aLine)", e}));
                    }
                }
                
            } // Fixed
            
            // set link
            aFile.setLink(link);
            
            // set name
            aFile.setName(name);
            
            // set size
            aFile.setSize(size);
            
            
            // Adjust type according to name.
            
            // Some ftp heuristics (MVS PDS, MVS Sequential, MVS GDG) have not given
            // directoryRegex (empty), that doesn't mean no dir.
            // So some dir entries may be treated as file type always.
            // We can determine according to the listed name. For MVS PDS, MVS Sequential
            // and MVS GDG, listFiles() will return full sub-names. For example,
            // a dataset is "DATA6.HLIU.FILE.MONK",
            // when we are in dir "DATA6", we'll get file "HLIU.FILE.MONK";
            // when we are in dir "DATA6.HLIU", we'll get file "FILE.MONK";
            // when we are in dir "DATA6.HLIU.FILE", we'll get file "MONK".
            if (type == FtpHeuristicsFile.FILE_TYPE) {
                if (this.directoryRegex == null || this.directoryRegex.equals("")) {
                    // Check whether a "file" type needs to be adjusted to "dir" type
                    if (this.interDirDelimiter != null &&
                            !this.interDirDelimiter.equals("") &&
                            name.indexOf(this.interDirDelimiter) >= 0) {
                        type = FtpHeuristicsFile.DIRECTORY_TYPE;
                        // adjust type
                        aFile.setType(type);
                    }
                }
            }
        }
        
        catch (Exception e) {
            if (mLogger.isLoggable(Level.WARNING))
                mLogger.log(Level.WARNING, mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION",
                        new Object[] {"parseALine(String aLine)", e}));
            return null;
        }
        return aFile;
    }
    
    /**
     * Sets FTP Heuristics properties.
     * @param       style  The Directory Listing Style.
     * @exception   Exception   If some error occurs.
     */
    private void setProps(String style) throws Exception {
        String msg = null;
        if (mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"FtpHeuristics.setProps(String style)"}));
        
        style = style == null ? "" : style.trim();
        
        if (this.dirListingStyle != null && this.dirListingStyle.equals(style))
            return;
        
        try {
            String s = style.replace(' ', '_');
            this.dirListingStyle = style;
            this.supportCmdMask =
                    this.props.getProperty(s + ".Commands_Supported_By_FTP_Server", "");
            if (this.supportCmdMask == null || this.supportCmdMask.equals("")) {
                if ( this.cfgFilePath == null )
                    msg = mMessages.getString("FTPBC-E006048.ERR_EXT_FTP_INVALID_STYLE",
                            new Object[] {"setProps(String style)", style});
                else
                    msg = mMessages.getString("FTPBC-E006048.ERR_EXT_FTP_INVALID_STYLE",
                            new Object[] {"setProps(String style)", style, this.cfgFilePath});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new Exception(msg);
            }
            this.headerSkipLines =
                    Integer.parseInt(this.props.getProperty(s + ".Header_Lines_To_Skip", "0"));
            this.headerSkipRegex =
                    this.props.getProperty(s + ".Header_Indication_Regex_Expression", "");
            this.trailerSkipLines =
                    Integer.parseInt(this.props.getProperty(s + ".Trailer_Lines_To_Skip", "0"));
            this.trailerSkipRegex =
                    this.props.getProperty(s + ".Trailer_Indication_Regex_Expression", "");
            this.directoryRegex =
                    this.props.getProperty(s + ".Directory_Indication_Regex_Expression", "");
            if (this.props.getProperty(s + ".File_Link_Real_Data_Available", "")
            .equalsIgnoreCase("Yes")) {
                this.fileLinkRealData = true;
            } else {
                this.fileLinkRealData = false;
            }
            this.fileLinkRegex =
                    this.props.getProperty(s + ".File_Link_Indication_Regex_Expression", "");
            this.linkSymbolRegex =
                    this.props.getProperty(s + ".File_Link_Symbol_Regex_Expression", "");
            this.listLineFormat = this.props.getProperty(s + ".List_Line_Format", "");
            this.validFileLineMinPosn =
                    Integer.parseInt(
                    this.props.getProperty(s + ".Valid_File_Line_Minimum_Position", "0"));
            if (this.props.getProperty(s + ".File_Name_Is_Last_Entity", "")
            .equals("Yes")) {
                this.fileNameIsLastObject = true;
            } else {
                this.fileNameIsLastObject = false;
            }
            this.fileNamePosn =
                    Integer.parseInt(this.props.getProperty(s + ".File_Name_Position", "0"));
            this.fileNameLength =
                    Integer.parseInt(this.props.getProperty(s + ".File_Name_Length", "0"));
            this.fileExtPosn =
                    Integer.parseInt(
                    this.props.getProperty(s + ".File_Extension_Position", "0"));
            this.fileExtLength =
                    Integer.parseInt(this.props.getProperty(s + ".File_Extension_Length", "0"));
            this.fileSizeVerifiable = false;
            if (this.props.getProperty(s + ".File_Size_Verifiable", "").equals("Yes"))
                this.fileSizeVerifiable = true;
            this.fileSizePosn = Integer.parseInt(this.props.getProperty(s + ".File_Size_Position", "0"));
            this.fileSizeLength = Integer.parseInt(this.props.getProperty(s + ".File_Size_Length", "0"));
            this.absPathEnvelope = this.props.getProperty(s + ".Special_Envelope_For_Absolute_Pathname", "");
            this.listDirYieldsAbsPath = false;
            if (this.props.getProperty(s + ".Listing_Directory_Yields_Absolute_Pathnames", "").equals("Yes"))
                this.listDirYieldsAbsPath = true;
            this.absPathDelims = this.props.getProperty(s + ".Absolute_Pathname_Delimiter_Set", "");
            this.cdBeforeListing = false;
            if (this.props.getProperty(s + ".Change_Directory_Before_Listing", "").equals("Yes"))
                this.cdBeforeListing = true;
            this.dirRequireTerminator = false;
            if (this.props.getProperty(s + ".Directory_Name_Requires_Terminator", "").equals("Yes"))
                this.dirRequireTerminator = true;
        } catch (java.lang.NumberFormatException e) {
            msg = "FtpHeristics.setProps(): Number format exception! "
                    + "Exception is ["
                    + e.toString() + "].";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg, e);
            }
            throw new Exception(msg, e);
        }
        
        // Set dir/file delimiters
        String[] delimiterArray = FtpHeuristics.getDlimiters(this.absPathDelims);
        this.initDirDelimiter = delimiterArray[0];
        this.interDirDelimiter = delimiterArray[1];
        this.initFileDelimiter = delimiterArray[2];
        this.endingFileDelimiter = delimiterArray[3];
        
    }
    
    /**
     * Parses raw input lines.
     * These input lines are raw lines from FTP LIST command.
     * This method skips header lines and trailer lines, and returns a solid-line string array.
     * @param    lines  The raw lines to be parsed.
     * @return   A string array.
     */
    public String[] skipLines(String[] lines) {
        String msg = null;
        if (mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"FtpHeuristics.skipLines()"}));

        if (lines == null)
            return null;
        
        int lineHeader = this.getHeaderSkipLines();
        int lineTrailer = this.getTrailerSkipLines();
        
        // no lines need to be skipped
        if (lineHeader + lineTrailer == 0) {
            return lines;
        }
        
        int lineIdx = 0;
        int lineTot = lines.length;
        Vector files = new Vector();
        //RE re = null;
        
        Pattern re = null;
        Matcher match = null;
        
        // skip header lines if any
        for (lineIdx = 0; lineIdx < lineHeader && lineIdx < lineTot; lineIdx++) {
            if (lineIdx == 0) {
                try {
                    re = Pattern.compile(this.getHeaderSkipRegex());
                    match = re.matcher(lines[lineIdx]);
                } catch (Exception e) {
                    if (mLogger.isLoggable(Level.WARNING)) {
                        mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006015.WRN_EXT_FTP_INVALID_HEADER_REGEX", new Object[] {"FtpHeuristics.skipLines()",
                                getHeaderSkipRegex(),
                                e
                        }));
                    }
                    re = null;
                    match = null;
                }
            }
            
            if ( match == null || match.find() ) {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006036.DBG_EXT_FTP_HEADER_LINE_SKIPPED", new Object[] {"FtpHeuristics.skipLines()",
                            lines[lineIdx]
                    }));
                }
                continue;
            }
            
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006037.DBG_EXT_FTP_HEADER_REGEX_UNMATCHED", new Object[] {"FtpHeuristics.skipLines()",
                        lines[lineIdx]
                }));
            }
            // try to parse this line (don't skip it simply).
            files.addElement(lines[lineIdx]);
        }
        
        // process file/dir entries
        for (lineIdx = lineHeader; lineIdx < lineTot - lineTrailer; lineIdx++) {
            files.addElement(lines[lineIdx]);
        }
        
        // skip trailer lines if any
        for (lineIdx = lineTot - lineTrailer; lineIdx < lineTot; lineIdx++) {
            if (lineIdx == lineTot - lineTrailer) {
                try {
                    re = Pattern.compile(this.getTrailerSkipRegex());
                    match = re.matcher(lines[lineIdx]);
                } catch (Exception e) {
                    if (mLogger.isLoggable(Level.WARNING)) {
                        mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006016.WRN_EXT_FTP_INVALID_TRAILER_REGEX", new Object[] {"FtpHeuristics.skipLines()",
                                getTrailerSkipRegex(),
                                e
                        }));
                    }
                    re = null;
                }
            }
            if ( match == null || match.find() ) {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006038.DBG_EXT_FTP_TRAILER_LINE_SKIPPED", new Object[] {"FtpHeuristics.skipLines()",
                            lines[lineIdx]
                    }));
                }
                continue;
            }
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006039.DBG_EXT_FTP_TRAILER_REGEX_UNMATCHED", new Object[] {"FtpHeuristics.skipLines()",
                        lines[lineIdx]
                }));
            }
            // try to parse this line (don't skip it simply).
            files.addElement(lines[lineIdx]);
        }
        
        // clean up and finish
        if (files.size() == 0) {
            return null;
        } else {
            String[] f = new String[files.size()];
            files.copyInto(f);
            return f;
        }
        
    }
    
    /**
     * Strips off the base name of a fully qualified remote
     * file name, pursuant to any special enveloping characters for the
     * absolute path name and absolute path-name delimiters on the remote FTP host.
     * @param    fullFileName  The full remote file name.
     * @return   The directory part of the full remote file name.
     */
    public String getDirectoryName(String fullFileName) {
        if (mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"FtpHeuristics.getDirectoryName(String fullFileName)"}));
        
        fullFileName = fullFileName.trim();
        
        // Check inputs
        if (fullFileName == null || fullFileName.equals("")) {
            return null;
        }
        
        // note: for NT3.5 and NT 4.0 ftp servers, the delimiter can be either "\" or "/".
        //       In FtpHeuristics, it's "\". So we can replace "/" with "\" first.
        if (this.dirListingStyle.equals("NT 4.0") ||
                this.dirListingStyle.equals("NT 3.5")) {
            fullFileName = fullFileName.replace('/', '\\');
        }
        
        String directoryName = fullFileName;
        int envLen;
        
        // Remove start envelope if any
        if (this.absPathEnvelope != null && !this.absPathEnvelope.equals("")) {
            envLen = this.absPathEnvelope.length() / 2;
            if (directoryName.startsWith(this.absPathEnvelope.substring(envLen))) {
                directoryName = directoryName.substring(envLen);
            }
        }
        
        // Get directory name (left to right)
        // Check init file delimiter (or intermediate dir delimiter)
        if (!initFileDelimiter.equals("") || !interDirDelimiter.equals("")) {
            String delim =
                    (!initFileDelimiter.equals("") ? initFileDelimiter : interDirDelimiter);
            if (directoryName.lastIndexOf(delim) >= 0) {
                directoryName = directoryName.substring(0, directoryName.lastIndexOf(delim));
            }
        }
        
        // in case the dir terminator is required (e.g. VMS)
        if (this.dirRequireTerminator && !directoryName.endsWith(initFileDelimiter)) {
            directoryName += initFileDelimiter;
        }
        
        return directoryName;
    }
    
    /**
     * Allows you to escape all regular expression special characters in an input string.
     * @param       input  Original input non-escaped string.
     * @return      Escaped output string.
     */
    public String escapeRegExpChars(String input) {
        if (mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"FtpHeuristics.escapeRegChars(String input)"}));
        
        if (input == null) {
            return null;
        }
        
        if (this.dirListingStyle.equals("MVS GDG")) {
            try {
                // For cases like (-2), (0) or (+1), don't escape it.
                Pattern re = Pattern.compile("^\\(([+-])?([0-9])+\\)$");
                Matcher match = re.matcher(input);
                
                //~if (match.matches()) {
                if (match.find()) {
                    return input;
                }
            } catch (Exception e) {
                return input;
            }
        }
        
        String output = "";
        for (int i = 0; i < input.length(); i++){
            if (DirRegExp.regExpChars.indexOf(input.substring(i, i + 1)) >= 0) {
                output += "\\";
            }
            output += input.substring(i, i + 1);
        }
        return output;
    }
}
