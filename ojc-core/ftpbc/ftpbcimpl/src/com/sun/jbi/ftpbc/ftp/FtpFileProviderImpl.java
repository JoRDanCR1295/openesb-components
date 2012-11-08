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
 * @(#)FtpFileProviderImpl.java 
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
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.SocketException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.net.SocketFactory;
import org.apache.commons.net.ftp.FTPClient;
import org.apache.commons.net.ftp.FTPFile;
import org.apache.commons.net.ftp.FTPFileListParser;

import com.sun.jbi.ftpbc.ftp.exception.FtpFileException;
import com.sun.jbi.ftpbc.ftp.namepattern.NameMatcher;
import com.sun.jbi.internationalization.Messages;
import java.nio.charset.Charset;
import org.apache.commons.net.ftp.FTP.FTPType;

/**
 * Impl of FtpFileProvider
 * @author jfu (jim.fu@sun.com)
 * @author Harry Liu
 * @version cvs revision:    Last Modified: 
 */
public class FtpFileProviderImpl implements FtpFileProvider {
    private static final Messages mMessages =
            Messages.getMessages(FtpFileProviderImpl.class);
    private static final Logger mLogger =
            Messages.getLogger(FtpFileProviderImpl.class);
    
    private FTPClient delegate = null;
    private FTPFileListParser defaultFileListParser = null;
    
    // current heuristics - refer to either built-in or user-defined
    private FtpHeuristics heuristics;
    
    // built-in heuristics
    private FtpHeuristics built_in_heuristics;
    // user defined heuristics
    private FtpHeuristics user_defined_heuristics;
    private String userDefinedHeuristicsCfgFile;
    
    // if print replyString for each ftp raw command
    private boolean traceRawCommand = false;
    
    private boolean useRegexAsMatcher = false;
    
    // Default timeouts for data and command connection sockets
    public static final int DEFAULT_TIMEOUT_DATA_CONNECTION = 45000;
    public static final int DEFAULT_TIMEOUT_COMMAND_CONNECTION = 0;
    
    // Some reply codes
    public static final int ACTION_ABORTED = 451;
    public static final int BAD_COMMAND_SEQUENCE = 503;
    public static final int CANNOT_OPEN_DATA_CONNECTION = 425;
    public static final int CLOSING_DATA_CONNECTION = 226;
    public static final int COMMAND_IS_SUPERFLUOUS = 202;
    public static final int COMMAND_NOT_IMPLEMENTED = 502;
    public static final int COMMAND_NOT_IMPLEMENTED_FOR_PARAMETER = 504;
    public static final int COMMAND_OK = 200;
    public static final int DATA_CONNECTION_ALREADY_OPEN = 125;
    public static final int DATA_CONNECTION_OPEN = 225;
    public static final int DIRECTORY_STATUS = 212;
    public static final int ENTERING_PASSIVE_MODE = 227;
    public static final int FILE_ACTION_NOT_TAKEN = 450;
    public static final int FILE_ACTION_OK = 250;
    public static final int FILE_ACTION_PENDING = 350;
    public static final int FILE_NAME_NOT_ALLOWED = 553;
    public static final int FILE_STATUS = 213;
    public static final int FILE_STATUS_OK = 150;
    public static final int FILE_UNAVAILABLE = 550;
    public static final int HELP_MESSAGE = 214;
    public static final int INSUFFICIENT_STORAGE = 452;
    public static final int NAME_SYSTEM_TYPE = 215;
    public static final int NEED_ACCOUNT = 332;
    public static final int NEED_ACCOUNT_FOR_STORING_FILES = 532;
    public static final int NEED_PASSWORD = 331;
    public static final int NOT_LOGGED_IN = 530;
    public static final int PAGE_TYPE_UNKNOWN = 551;
    public static final int PATHNAME_CREATED = 257;
    public static final int RESTART_MARKER = 110;
    public static final int SERVICE_CLOSING_CONTROL_CONNECTION = 221;
    public static final int SERVICE_NOT_AVAILABLE = 421;
    public static final int SERVICE_NOT_READY = 120;
    public static final int SERVICE_READY = 220;
    public static final int STORAGE_ALLOCATION_EXCEEDED = 552;
    public static final int SYNTAX_ERROR_IN_ARGUMENTS = 501;
    public static final int SYSTEM_STATUS = 211;
    public static final int TRANSFER_ABORTED = 426;
    public static final int UNRECOGNIZED_COMMAND = 500;
    public static final int USER_LOGGED_IN = 230;
    
    private boolean noWarning;
    
    public FtpFileProviderImpl() {
    }
    
    /**
     * Using the default FileListParser , obtains a list of file information
     * for the current working directory.
     * @return     An array of FTPFile objects.
     * @exception  IOException   If some error occurs.
     */
    public FTPFile[] listFiles() throws IOException {
        return this.listFiles(this.defaultFileListParser);
    }
    
    /**
     * Using default FileListParser, obtains a list of file information
     * for the given working directory.
     * @param      pathName  The directory name.
     * @return     An array of FTPFile objects.
     * @exception  IOException   If some error occurs.
     */
    public FTPFile[] listFiles(String pathName) throws IOException {
        return this.listFiles(this.defaultFileListParser, pathName, "*");
    }
    
    /**
     * Filters files by type. The type can bre any of the following: FTPFile.FILE_TYPE, FTPFile.DIRECTORY_TYPE,
     * FTPFile.SYMBOLIC_LINK_TYPE or FTPFile.UNKNOWN_TYPE.
     * @param         files  An array of FTP file descriptor.
     * @param      fileType  The file type.
     * @return     An array of FTP file descriptor objects.
     * @exception  IOException   If some error occurs.
     */
    public FTPFile[] listFiles(FTPFile[] files, int fileType) throws IOException {
        if (files == null) {
            return null;
        }
        
        if (fileType != FTPFile.FILE_TYPE
                && fileType != FTPFile.DIRECTORY_TYPE
                && fileType != FTPFile.SYMBOLIC_LINK_TYPE
                && fileType != FTPFile.UNKNOWN_TYPE) {
            return files;
        }
        
        Vector lists = new Vector();
        for (int i = 0; i < files.length; i++) {
            if (files[i].getType() == fileType) {
                lists.addElement(files[i]);
            }
        }
        if (lists.size() == 0)
            return null;
        else {
            FTPFile[] f = new FTPFile[lists.size()];
            lists.copyInto(f);
            return f;
        }
    }
    
    /**
     * Using a programmer-specified FileListParser , obtains a list of file information
     * for the current working directory.
     * @param      fileListParser  The file list parser object.
     * @return     An array of FTP file descriptor objects.
     * @exception  IOException   If some error occurs.
     */
    private FTPFile[] listFiles(FTPFileListParser fileListParser)
    throws IOException {
        return this.listFiles(fileListParser, null);
    }
    
    /**
     * Using a programmer-specified FileListParser , obtains a list of file information
     * for the given working directory.
     * @param      fileListParser  The file list parser object.
     * @param            pathName  The directory name.
     * @return     An array of FTP file descriptor objects.
     * @exception  IOException   If some error occurs.
     */
    private FTPFile[] listFiles(FTPFileListParser fileListParser, String pathName)
    throws IOException {
        if (pathName == null || pathName.equals("") || pathName.equals(".")) {
            return delegate.listFiles(fileListParser, pathName);
        } else {
            return this.listFiles(fileListParser, pathName, "*");
        }
    }
    
    /**
     * Using a programmer-specified FileListParser , obtains a list of file information
     * for the given working directory and regular expression.
     * @param      fileListParser  The file list parser object.
     * @param            pathName  The directory name.
     * @param              regExp  The file name regular expression.
     * @return     An array of FTP file descriptor objects.
     * @exception  IOException   If some error occurs.
     */
    private FTPFile[] listFiles(
            FTPFileListParser fileListParser,
            String pathName,
            String regExp)
            throws IOException {
        
        Pattern pattern = null;
        Matcher matcher = null;
        String msg = null;
        
        //? For MVS GDG, don't check regular expression (based on the C logic - QAI30015) ?
        // If the regexp is like (0), (-1), don't do any filtering,
        // send the exact version to ftp server directly.
        boolean GDGSpecial = false;
        if ( this.heuristics.getDirListingStyle().equals("AS400-UNIX"))
            this.delegate.sendSiteCommand("NAMEFMT 1");
        if (this.heuristics.getDirListingStyle().equals("MVS GDG")) {
            try {
                // For cases like ^(-1)$, remove envelope '^' and '$' first.
                pattern = Pattern.compile("^\\^\\([+-]?[0-9]+\\)\\$$");
                matcher = pattern.matcher(regExp);
                if (matcher.find()) {
                    regExp = regExp.substring(1, regExp.length() - 1);
                }
                
                pattern = Pattern.compile("^\\([+-]?[0-9]+\\)$");
                matcher = pattern.matcher(regExp);
                
                if (matcher.find()) {
                    GDGSpecial = true;
                }
                
            } catch (Exception e) {
                // do nothing
            }
        }
        
        // We will always change directory before listing. (Don't depend on heuristics parameter).
        FTPFile files[] = null;
        if (pathName == null || pathName.equals("") || pathName.equals(".")) {
            files = this.listFiles();
            if (!this.isPositiveCompletion(this.getReplyCode())) {
                msg = mMessages.getString("FTPBC-E006034.ERR_EXT_FTP_LIST_FILE");
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new IOException(msg);
            }
        } else {
            // change directory and then list
            String currDirSave = this.pwd();
            if (!this.isPositiveCompletion(this.getReplyCode())) {
                msg = mMessages.getString("FTPBC-E006007.ERR_EXT_FTP_ACTION_FAIL", 
                		new Object[] {
                		"FTPFile[] listFiles(FTPFileListParser fileListParser,String pathName,String regExp)", "pwd", getReplyString()});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new IOException(msg);
            }
            if (null == currDirSave) {
                if (mLogger.isLoggable(Level.WARNING)) {
                    mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006007.WRN_EXT_FTP_NULL_WK_DIR"));
                }
                currDirSave = "";
            }
            if (this.heuristics.getAbsPathEnvelope() != null) {
                pathName = this.heuristics.buildFullFileName(pathName, null);
            }
            if(!heuristics.getDirListingStyle().equals("VOSK (Hitachi)") ) { // see QAI53881
                // For MVS PDS, MVS Sequential, MVS GDG, cd() will always succeed even the dir does not exist,
                // in fact, the dir represents the prefix of the dataset.
                if (!this.cd(pathName)) {
                    if ( !noWarning && mLogger.isLoggable(Level.WARNING)) {
                        mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W006008.WRN_EXT_FTP_DIR_NOT_EXIST", new Object[] {"FTPFile[] listFiles(FTPFileListParser fileListParser,String pathName,String regExp)", pathName}));
                    }
                    // We treat this case as a normal case so we don't throw exception
                    // because the directory might exist / not exist
                    // depending on some runtime conditions.
                    //throw new IOException(msg);
                    return null;
                }
            }
            // list files in the new directory
            //? For MVS GDG, don't check regular expression (based on the C logic - QAI30015) ?
            // If the regexp is like (0), (-1), don't do any filtering,
            // send the exact version to ftp server directly.
            if (GDGSpecial) {
                String fp = heuristics.buildFullFileName(pathName, regExp);
                files = delegate.listFiles(defaultFileListParser, fp);
            } else {
                files = this.listFiles();
            }
            // For MVS PDS, MVS Sequential, MVS GDG, listFiles() will return "550 No data sets found."
            // if the dir is empty. It is not an error, but the return code is 550 instead of 226,
            // so we need to treat it differently.
            if (!this.isPositiveCompletion(this.getReplyCode())) {
                if ((this.heuristics.getDirListingStyle().equals("MVS") ||             // for backward compatibility
                        this.heuristics.getDirListingStyle().equals("MVS PS") ||          // for backward compatibility
                        this.heuristics.getDirListingStyle().equals("MVS PDS") ||         // the old "MVS"
                        this.heuristics.getDirListingStyle().equals("MVS Sequential") ||  // the old "MVS PS"
                        this.heuristics.getDirListingStyle().equals("MVS GDG")) &&
                        this.getReplyString().equalsIgnoreCase("550 No data sets found.")) {
                    // do nothing
                    ;
                } else {
                    msg = mMessages.getString("FTPBC-E006035.ERR_EXT_FTP_LIST_FILE_FAIL",
                    		new Object[] {"FTPFile[] listFiles(FTPFileListParser fileListParser,String pathName,String regExp)", 
                    		pathName, regExp, getReplyString()});
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, msg);
                    }
                    throw new IOException(msg);
                }
            }
            // Change back working directory
            if (!this.cd(currDirSave)) {
                msg = mMessages.getString("FTPBC-E006036.ERR_EXT_FTP_CD_BACK_FAIL",
                		new Object[] {"FTPFile[] listFiles(FTPFileListParser fileListParser,String pathName,String regExp)", 
                		currDirSave});
                if (null == currDirSave ||
                        0 == currDirSave.length()) {
                    if (mLogger.isLoggable(Level.WARNING)) {
                        mLogger.log(Level.WARNING, msg);
                    }
                } else {
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, msg);
                    }
                    throw new IOException(msg);
                }
            }
        }
        
        //? For MVS GDG, don't check regular expression (based on the C logic - QAI30015) ?
        // If the regexp is like (0), (-1), don't do any filtering,
        // send the exact version to ftp server directly.
        if (GDGSpecial) {
            return files;
        }
        
        // skip for some common usages
        if (files == null
                || regExp == null
                || regExp.equals("")
                || regExp.equals("*")
                || regExp.equals("*.*")) {
            return files;
        }
        
        // gnu.regexp.RE may not work if *, +, ? is the first character of the regular expression,
        // just put an extra . before them.
        if (regExp.startsWith("*")
        || regExp.startsWith("+")
        || regExp.startsWith("?")) {
            regExp = "." + regExp;
        }
        
        // for a*.txt will not match ab.txt and a0.txt, it only match aa.txt or aaa.txt ...
        // because * means repetition of the proceeding expression.
        // Do we need to change the behavior? Right now, just keep it unchanged.
        
        //RE re = null;
        
        Pattern re = null;
        
        NameMatcher nm = null;
        try {
            if (this.heuristics.getDirListingStyle().equalsIgnoreCase("NT 3.5") ||
                    this.heuristics.getDirListingStyle().equalsIgnoreCase("NT 4.0") ||
                    this.heuristics.getDirListingStyle().equalsIgnoreCase("VMS") ||
                    this.heuristics.getDirListingStyle().equalsIgnoreCase("MVS") ||             // for backward compatibility
                    this.heuristics.getDirListingStyle().equalsIgnoreCase("MVS PS") ||          // for backward compatibility
                    this.heuristics.getDirListingStyle().equalsIgnoreCase("MVS PDS") ||         // the old "MVS"
                    this.heuristics.getDirListingStyle().equalsIgnoreCase("MVS Sequential") ||  // the old "MVS PS"
                    this.heuristics.getDirListingStyle().equalsIgnoreCase("MVS GDG") ||
                    this.heuristics.getDirListingStyle().equalsIgnoreCase("VM/ESA")) {
                // Case insensitive
                if (this.isUseRegexAsMatcher()) {
                    re = Pattern.compile(regExp, Pattern.CASE_INSENSITIVE);
                } else {
                    nm = new NameMatcher(regExp, NameMatcher.CASE_INSENSITIVE);
                }
            } else {
                // Case sensitive
                if (this.isUseRegexAsMatcher()) {
                    re = Pattern.compile(regExp);
                } else {
                    nm = new NameMatcher(regExp);
                }
            }
        } catch (Exception e) {
            msg = mMessages.getString("FTPBC-E006037.ERR_EXT_FTP_CREATE_REGEX_PROCESSOR_EXCEPTION",
            		new Object[] {"FTPFile[] listFiles(FTPFileListParser fileListParser,String pathName,String regExp)", 
            		regExp, e});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg, e);
            }
            throw new IOException(msg);
        }
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006025.DBG_EXT_FTP_FILTERING", new Object[] {"FTPFile[] listFiles(FTPFileListParser fileListParser,String pathName,String regExp)", 
            		pathName, regExp}));
        }
        
        Vector lists = new Vector();
        boolean match = false;
        for (int i = 0; i < files.length; i++) {
            matcher = isUseRegexAsMatcher() ? re.matcher(files[i].getName()) : nm.getRegexMatcher(files[i].getName());
            if ( (match = matcher.matches() ) )
                lists.addElement(files[i]);
            
            if (mLogger.isLoggable(Level.FINE))
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006026.DBG_EXT_FTP_MATCH_ENTRY", new Object[] {"FTPFile[] listFiles(FTPFileListParser fileListParser,String pathName,String regExp)", 
                		files[i].getRawListing(), regExp, new Boolean(match)}));
        }
        if (lists.size() == 0) {
            return null;
        } else {
            FTPFile[] f = new FTPFile[lists.size()];
            lists.copyInto(f);
            return f;
        }
    }
    
    /**
     * Using the default FileListParser, obtains a list of file information
     * for the given working directory and regular expression.
     * @param      pathName  The directory name.
     * @param        regExp  The file name regular expression.
     * @return     An array of FTP file descriptor objects.
     * @exception  IOException   If some error occurs.
     */
    public FTPFile[] listFiles(String pathName, String regExp) throws IOException {
        return this.listFiles(this.defaultFileListParser, pathName, regExp);
    }
    
    /**
     * Appends to a file on the remote FTP server with the given file name,
     * taking the input from the given file name.
     * The FileInputSteam related to the localFileName is closed in this method.
     * @param      remoteFileName  The remote file name.
     * @param       localFileName  The local file name.
     * @return     true if the operation is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean appendFile(String remoteFileName, String localFileName)
    throws IOException {
        FileInputStream fis = new FileInputStream(localFileName);
        boolean retValue = delegate.appendFile(remoteFileName, fis);
        fis.close();
        return retValue;
    }
    
    /**
     * Returns an OutputStream through which data can be written. This data is used to append
     * to a file with the given file name, on the remote FTP server. If the current file type
     * is ASCII, the returned OutputStream converts line separators in
     * the file to the NETASCII format (therefore, do not
     * create a special OutputStream to do this operation). You must close the
     * OutputStream when you finish writing to it. The OutputStream itself
     * takes care of closing the parent data connection socket upon being
     * closed. To finalize the file transfer, you must call the method
     * completePendingCommand() and check its return value to verify its success.
     * @param           dirName  The remote directory name.
     * @param      baseFileName  The remote base file name.
     * @return An OutputStream through which the remote file can be appended.
     *      If the data connection cannot be opened (for example, if the file does not
     *      exist), a null is returned (in which case you can check the reply
     *      code to determine the exact reason for the failure).
     * @exception  IOException   If some error occurs.
     */
    // Note: After calling this method, you have to call method completePendingCommand()
    //       to read out/update the reply and finish the whole action. completePendingCommand()
    //       will communicate on command connection socket. For some ftp server (e.x. Serv-U SSL
    //       ftp server), it may give us completion reply message only after the data has been
    //       write out on data connection socket; otherwise, completePendingCommand() will always
    //       fail. That is, the order of procedure may be:
    //          1. Call this method.
    //          2. Some action to write out to the returned OutputStream.
    //          3. Call completePendingCommand().
    //       If you encounter problem when using this method, please try above scenario.
    public OutputStream appendFileStream(String dirName, String baseFileName)
    throws IOException {
        return this.appendFileStream(heuristics.buildFullFileName(dirName, baseFileName));
    }
    
    /**
     * Deletes a file on the remote FTP server.
     * @param           dirName  The remote directory name of the file.
     * @param      baseFileName  The base name of the file.
     * @return     true if the operation is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean deleteFile(String dirName, String baseFileName)
    throws IOException {
        return this.deleteFile(heuristics.buildFullFileName(dirName, baseFileName));
    }
    
    /**
     * Check on whether the trace flag is on.
     * @return     true if trace flag is on, but false if not.
     */
    public boolean isTraceRawCommand() {
        return traceRawCommand;
    }
    
    /**
     * Renames a remote file.
     * @param           dirNameFrom  The remote directory name.
     * @param      baseFileNameFrom  The remote base file name.
     * @param             dirNameTo  The remote directory name.
     * @param        baseFileNameTo  The remote base file name.
     * @return     true if the operation is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean rename(
            String dirNameFrom,
            String baseFileNameFrom,
            String dirNameTo,
            String baseFileNameTo)
            throws IOException {
        return this.rename(
                heuristics.buildFullFileName(dirNameFrom, baseFileNameFrom),
                heuristics.buildFullFileName(dirNameTo, baseFileNameTo));
    }
    
    /**
     * Retrieves a named file from the remote FTP server and writes it to the given local file.
     * The FileOutputSteam related to the localFileName is closed in this method.
     * @param      remoteFileName  The remote file name.
     * @param       localFileName  The local file name.
     * @return     true if the operation is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean retrieveFile(String remoteFileName, String localFileName)
    throws IOException {
        FileOutputStream fos = new FileOutputStream(localFileName);
        boolean retValue = delegate.retrieveFile(remoteFileName, fos);
        fos.close();
        return retValue;
    }
    
    /**
     * Returns an InputStream from which a named file from the remote FTP server
     * can be read. If the current file type is ASCII, the returned
     * InputStream converts the  line separators in the file to
     * the local representation. You must close the InputStream when you
     * finish reading from it. The InputStream itself takes care of
     * closing the parent data connection socket upon being closed. To
     * finalize the file transfer you must call the method completePendingCommand()
     * and check its return value to verify your success.
     * @param           dirName  The remote directory name.
     * @param      baseFileName  The remote base file name.
     * @return   An InputStream from which the remote file can be read. If
     *      the data connection cannot be opened (for example, if the file does not
     *      exist), a null is returned, in which case you can check the reply
     *      code to determine the exact reason for failure.
     * @exception  IOException   If some error occurs.
     */
    // Note: After calling this method, you have to call method completePendingCommand()
    //       to read out/update the reply and finish the whole action. completePendingCommand()
    //       will communicate on command connection socket. For some ftp server (e.x. Serv-U SSL
    //       ftp server), it may give us completion reply message only after the data has been
    //       read out on data connection socket; otherwise, completePendingCommand() will always
    //       fail. That is, the order of procedure may be:
    //          1. Call this method.
    //          2. Some action to read out from the returned InputStream.
    //          3. Call completePendingCommand().
    //       If you encounter problem when using this method, please try above scenario.
    public InputStream retrieveFileStream(String dirName, String baseFileName)
    throws IOException {
        return this.retrieveFileStream(heuristics.buildFullFileName(dirName, baseFileName));
    }
    
    /**
     * Sends an FTP command to the server. The method then
     * waits for a reply and returns the numerical response code.
     * @param      command  The FTP command to be executed.
     * @param         parm  The parameter of the FTP command.
     * @return     The reply code from FTP command.
     * @exception  IOException   If some error occurs.
     */
    public int sendCommand(String command, String parm) throws IOException {
        return this.delegate.sendCommand(command, parm);
    }
    
    /**
     * Allows you to turn the trace flag on and off.
     * @param      newTraceRawCommand  true or false.
     */
    public void setTraceRawCommand(boolean newTraceRawCommand) {
        traceRawCommand = newTraceRawCommand;
    }
    
    /**
     * Stores a file on the server using the given name and taking its input from the given local file.
     * The FileInputSteam related to the localFileName is closed in this method.
     * @param      remoteFileName  The remote file name.
     * @param       localFileName  The local file name.
     * @return     true if the operation is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean storeFile(String remoteFileName, String localFileName)
    throws IOException {
        FileInputStream fis = new FileInputStream(localFileName);
        boolean retValue = delegate.storeFile(remoteFileName, fis);
        fis.close();
        return retValue;
    }
    
    /**
     * Returns an OutputStream through which data can be written. This data is used to append
     * to a file with the given file name, on the remote FTP server. If the current file type
     * is ASCII, the returned OutputStream converts line separators in
     * the file to the NETASCII format (therefore, do not
     * create a special OutputStream to do this operation). You must close the
     * OutputStream when you finish writing to it. The OutputStream itself
     * takes care of closing the parent data connection socket upon being
     * closed. To finalize the file transfer, you must call the method
     * completePendingCommand() and check its return value to verify its success.
     * @param      remoteFileName  The remote file name.
     * @param      baseFileName  The remote base file name.
     * @return An OutputStream through which the remote file can be appended.
     *      If the data connection cannot be opened (for example, if the file does not
     *      exist), a null is returned (in which case you can check the reply
     *      code to determine the exact reason for the failure).
     * @exception  IOException   If some error occurs.
     */
    // Note: After calling this method, you have to call method completePendingCommand()
    //       to read out/update the reply and finish the whole action. completePendingCommand()
    //       will communicate on command connection socket. For some ftp server (e.x. Serv-U SSL
    //       ftp server), it may give us completion reply message only after the data has been
    //       write out on data connection socket; otherwise, completePendingCommand() will always
    //       fail. That is, the order of procedure may be:
    //          1. Call this method.
    //          2. Some action to write out to the returned OutputStream.
    //          3. Call completePendingCommand().
    //       If you encounter problem when using this method, please try above scenario.
    public OutputStream storeFileStream(String dirName, String baseFileName)
    throws IOException {
        return this.storeFileStream(heuristics.buildFullFileName(dirName, baseFileName));
    }
    
    /**
     * Appends to a file on the server with the given directory and file name,
     * taking its input from the given file name.
     * The FileInputSteam related to the localFileName is closed in this method.
     * @param           remoteDirName  The remote directory name.
     * @param      remoteBaseFileName  The remote base file name.
     * @param           localFileName  The local file name.
     * @return     true if the operation is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean appendFile(
            String remoteDirName,
            String remoteBaseFileName,
            String localFileName)
            throws IOException {
        return this.appendFile(
                heuristics.buildFullFileName(remoteDirName, remoteBaseFileName),
                localFileName);
    }
    
    /**
     * Retrieves a named file from the remote FTP server and writes it to the given local file.
     * The FileOutputSteam related to the localFileName is closed in this method.
     * @param           remoteDirName  The remote directory name.
     * @param      remoteBaseFileName  The remote base file name.
     * @param           localFileName  The local file name.
     * @return     true if the operation is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean retrieveFile(
            String remoteDirName,
            String remoteBaseFileName,
            String localFileName)
            throws IOException {
        return this.retrieveFile(
                heuristics.buildFullFileName(remoteDirName, remoteBaseFileName),
                localFileName);
    }
    
    /**
     * Stores a file on the remote FTP server using the given name and taking its input from the given local file.
     * The FileInputSteam related to the localFileName is closed in this method.
     * @param           remoteDirName  The remote directory name.
     * @param      remoteBaseFileName  The remote file name.
     * @param           localFileName  The local file name.
     * @return     true if the operation is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean storeFile(
            String remoteDirName,
            String remoteBaseFileName,
            String localFileName)
            throws IOException {
        return this.storeFile(
                heuristics.buildFullFileName(remoteDirName, remoteBaseFileName),
                localFileName);
    }
    
    /**
     * Gets all file entries that can be used to get real data,
     * when the parameter listRealData is true.
     * The file entries contain all files with FILE_TYPE and/or
     * files with SYMBOLIC_LINK_TYPE, if they are linked to real data that is available
     * (in FtpHeuristics) the parameter is true. However, if the list only contains files
     * with FILE_TYPE , if they are linked to real data that is available, the parameter is false.
     * This method filters out directory entries and pure symbolic link entries.
     * @param             files  An array of FTPFile objects.
     * @param      listRealData  Indicates whether the files listed contain real data.
     * @return     An array of FTPFile objects.
     * @exception  IOException   If some error occurs.
     */
    public FTPFile[] listFiles(FTPFile[] files, boolean listRealData)
    throws IOException {
        if (files == null) {
            return null;
        }
        
        Vector lists = new Vector();
        for (int i = 0; i < files.length; i++) {
            if (listRealData) {
                if (files[i].getType() == FTPFile.FILE_TYPE) {
                    lists.addElement(files[i]);
                    continue;
                }
                if (heuristics.isFileLinkRealData()
                && files[i].getType() == FTPFile.SYMBOLIC_LINK_TYPE) {
                    lists.addElement(files[i]);
                }
            } else {
                if (files[i].getType() == FTPFile.FILE_TYPE) {
                    continue;
                }
                if (heuristics.isFileLinkRealData()
                && files[i].getType() == FTPFile.SYMBOLIC_LINK_TYPE) {
                    continue;
                }
                lists.addElement(files[i]);
            }
        }
        
        if (lists.size() == 0)
            return null;
        else {
            FTPFile[] f = new FTPFile[lists.size()];
            lists.copyInto(f);
            return f;
        }
    }
    
    /**
     * Returns the integer value of the reply code of the last FTP reply.
     * @return    The reply code.
     */
    public int getReplyCode() {
        return delegate.getReplyCode();
    }
    
    /**
     * Returns the entire text of the last FTP server response exactly as it was received.
     * @return    The reply string.
     */
    public String getReplyString() {
        if (delegate.getReplyString() == null) {
            return null;
        } else {
            return delegate.getReplyString().trim();
        }
    }
    
    /**
     * Returns the lines of text from the last FTP server response as an array of strings,
     * with one entry per line.
     * @return    The reply string lines.
     */
    public String[] getReplyStrings() {
        return delegate.getReplyStrings();
    }
    
    /**
     * Appends to a file on the remote FTP server with the given name,
     * taking its input from the given InputStream.
     * The InputSteam is NOT closed in this method.
     * @param      remoteFileName  The remote file name.
     * @param               local  The local input stream.
     * @return     true if the operation is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean appendFile(String remoteFileName, InputStream local)
    throws IOException {
        return delegate.appendFile(remoteFileName, local);
    }
    
    /**
     * Appends to a file on the remote FTP server with the given name,
     * taking its input from the given InputStream.
     * The InputSteam is NOT closed in this method.
     * @param           remoteDirName  The remote directory name.
     * @param      remoteBaseFileName  The remote base file name.
     * @param                   local  The local input stream.
     * @return     true if the operation is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean appendFile(String remoteDirName, String remoteBaseFileName, InputStream local)
    throws IOException {
        return this.appendFile(
                heuristics.buildFullFileName(remoteDirName, remoteBaseFileName),
                local);
    }
    
    /**
     * Returns an OutputStream through which data can be written. This data is used to append
     * to a file with the given file name, on the remote FTP server. If the current file type
     * is ASCII, the returned OutputStream converts line separators in
     * the file to the NETASCII format (therefore, do not
     * create a special OutputStream to do this operation). You must close the
     * OutputStream when you finish writing to it. The OutputStream itself
     * takes care of closing the parent data connection socket upon being
     * closed. To finalize the file transfer, you must call the method
     * completePendingCommand() and check its return value to verify its success.
     * @param      remoteFileName  The remote file name.
     * @return An OutputStream through which the remote file can be appended.
     *      If the data connection cannot be opened (for example, if the file does not
     *      exist), a null is returned (in which case you can check the reply
     *      code to determine the exact reason for the failure).
     * @exception  IOException   If some error occurs.
     */
    // Note: After calling this method, you have to call method completePendingCommand()
    //       to read out/update the reply and finish the whole action. completePendingCommand()
    //       will communicate on command connection socket. For some ftp server (e.x. Serv-U SSL
    //       ftp server), it may give us completion reply message only after the data has been
    //       write out on data connection socket; otherwise, completePendingCommand() will always
    //       fail. That is, the order of procedure may be:
    //          1. Call this method.
    //          2. Some action to write out to the returned OutputStream.
    //          3. Call completePendingCommand().
    //       If you encounter problem when using this method, please try above scenario.
    public OutputStream appendFileStream(String remoteFileName)
    throws IOException {
        return delegate.appendFileStream(remoteFileName);
    }
    
    /**
     * Sets the file type of the file to be transferred, to ASCII.
     * @return     true if the operation is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean ascii()
    throws IOException {
        return delegate.setFileType(delegate.ASCII_FILE_TYPE);
    }
    
    /**
     * Sets the file type of the file to be transferred, to binary.
     * @return     true if the operation is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean binary()
    throws IOException {
        return delegate.setFileType(delegate.BINARY_FILE_TYPE);
    }
    
    /**
     * Changes the current working directory of the FTP session.
     * @param      dirName  The new directory name.
     * @return     true if the operation is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean cd(String dirName)
    throws IOException {
        return delegate.changeWorkingDirectory(dirName);
    }
    
    /**
     * Opens a socket connected to a remote FTP host at the specified port and
     * originating from the current host at a system-assigned port.
     * @param      host  The FTP host name.
     * @param      port  The FTP server port.
     * @exception  SocketException   If some socket error occurs.
     * @exception  IOException   If some input-output error occurs.
     */
    public void connect(String host, int port)
    throws SocketException, IOException {
        connect(host, port, FtpFileConfigConstants.DEFAULT_CNTRL_ENCODING);
    }
    
    /**
     * Opens a socket connected to a remote FTP host at the specified port and
     * originating from the current host at a system-assigned port.
     * @param      host  The FTP host name.
     * @param      port  The FTP server port.
     * @param      encoding The encoding for the server.
     * @exception  SocketException   If some socket error occurs.
     * @exception  IOException   If some input-output error occurs.
     */
    public void connect(String host, int port, String encoding)
    throws SocketException, IOException {
        this.setDefaultTimeout(45000);
        
        delegate.setControlEncoding(encoding);
        delegate.connect(host, port);
        
        this.setSoTimeout(DEFAULT_TIMEOUT_COMMAND_CONNECTION);
        this.setDataSocketTimeout(DEFAULT_TIMEOUT_DATA_CONNECTION);
    }
    
    /**
     * Deletes a file on the remote FTP server.
     * @param      remoteFileName  The name of the remote file to be deleted.
     * @return     true if the operation is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean deleteFile(String remoteFileName)
    throws IOException {
        return delegate.deleteFile(remoteFileName);
    }
    
    
    
    /**
     * Returns the current data connection mode (one of the _DATA_CONNECTION_MODE constants).
     * For example, PASSIVE or ACTIVE.
     * @return  The data connection mode.
     */
    public int getDataConnectionMode() {
        return delegate.getDataConnectionMode();
    }
    
    /**
     * Retrieves the system type name from the server and returns the string.
     * @return    The system name.
     * @exception  IOException   If some error occurs.
     */
    public String getSystemName()
    throws IOException {
        return delegate.getSystemName();
    }
    
    /**
     * Sets the file type of the file to be transferred, to "Image."
     * @return     true if the operation is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean image()
    throws IOException {
        return delegate.setFileType(delegate.IMAGE_FILE_TYPE);
    }
    
    /**
     * Checks on whether the client is currently connected to a server.
     * @return     true if the client is connected, but false if not.
     */
    public boolean isConnected() {
        return delegate.isConnected();
    }
    
    /**
     * Checks on whether the FTP operation is "Negative Permanent;" the return code can be any integer from 500 through 599.
     * @param      replyCode  The reply code of the FTP operation.
     * @return     Whether the FTP operation is "Negative Permanent."
     */
    public boolean isNegativePermanent(int replyCode) {
        return replyCode >= 500 && replyCode < 600;
    }
    
    /**
     * Checks on whether the FTP operation is "Negative Transient;" the return code can be any integer from 400 through 499.
     * @param      replyCode  The reply code of the FTP operation.
     * @return     Whether the FTP operation is "Negative Transient."
     */
    public boolean isNegativeTransient(int replyCode) {
        return replyCode >= 400 && replyCode < 500;
    }
    
    /**
     * Checks on whether the FTP operation is "Positive Completion;" the return code can be any integer from 200 through 299.
     * @param      replyCode  The reply code of the FTP operation.
     * @return     Whether the FTP operation is "Positive Completion."
     */
    public boolean isPositiveCompletion(int replyCode) {
        return replyCode >= 200 && replyCode < 300;
    }
    
    /**
     * Checks on whether the FTP operation is "Positive Intermediate;" the return code can be any integer from 300 through 399.
     * @param      replyCode  The reply code of the FTP operation.
     * @return     Whether the FTP operation is "Positive Intermediate."
     */
    public boolean isPositiveIntermediate(int replyCode) {
        return replyCode >= 300 && replyCode < 400;
    }
    
    /**
     * Checks on whether the FTP operation is "Positive Preliminary;" the return code can be any integer from 100 through 199.
     * @param      replyCode  The reply code of the FTP operation.
     * @return     Whether the FTP operation is "Positive Preliminary."
     */
    public boolean isPositivePreliminary(int replyCode) {
        return replyCode >= 100 && replyCode < 200;
    }
    
    /**
     * Retrieves the system help information from the remote FTP server and returns the full string.
     * @return     The system help information.
     * @exception  IOException   If some error occurs.
     */
    public String listHelp()
    throws IOException {
        return delegate.listHelp();
    }
    
    /**
     * Retrieves the help information for a given command from the remote FTP server and returns the full string.
     * @param      command  The command given.
     * @return     The system help information.
     * @exception  IOException   If some error occurs.
     */
    public String listHelp(String command)
    throws IOException {
        return delegate.listHelp(command);
    }
    
    /**
     * Logs into the remote FTP server using the provided user name and password.
     * @param          user  The user name.
     * @param      password  The password.
     * @return     true if the operation is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean login(String user, String password)
    throws IOException {
        return delegate.login(user, password);
    }
    
    /**
     * Logs out of the remote FTP server by sending the QUIT command.
     * @return     true if the operation is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean logout()
    throws IOException {
        return delegate.logout();
    }
    
    /**
     * Returns the path name of the current working directory.
     * @return     The current working path name.
     * @exception  IOException   If some error occurs.
     */
    public String pwd()
    throws IOException {
        return delegate.printWorkingDirectory();
    }
    
    /**
     * Renames a remote file.
     * @param      remoteFileNameFrom  The old remote file name.
     * @param        remoteFileNameTo  The new remote file name.
     * @return     true if the operation is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean rename(
            String remoteFileNameFrom,
            String remoteFileNameTo)
            throws IOException {
        return delegate.rename(remoteFileNameFrom, remoteFileNameTo);
    }
    
    /**
     * Retrieves a named file from the remote FTP server and writes it to the given OutputStream.
     * The OutputSteam is NOT closed in this method.
     * @param      remoteFileName  The remote file name.
     * @param               local  The local output stream.
     * @return     true if the operation is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean retrieveFile(String remoteFileName, OutputStream local)
    throws IOException {
        return delegate.retrieveFile(remoteFileName, local);
    }
    
    /**
     * Retrieves a named file from the remote FTP server and writes it to the given OutputStream.
     * The OutputSteam is NOT closed in this method.
     * @param           remoteDirName  The remote directory name.
     * @param      remoteBaseFileName  The remote base file name.
     * @param                   local  The local output stream.
     * @return     true if the operation is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean retrieveFile(String remoteDirName, String remoteBaseFileName, OutputStream local)
    throws IOException {
        return this.retrieveFile(
                heuristics.buildFullFileName(remoteDirName, remoteBaseFileName),
                local);
    }
    
    /**
     * Returns an InputStream from which a named file from the remote FTP server
     * can be read. If the current file type is ASCII, the returned
     * InputStream converts the  line separators in the file to
     * the local representation. You must close the InputStream when you
     * finish reading from it. The InputStream itself takes care of
     * closing the parent data connection socket upon being closed. To
     * finalize the file transfer you must call the method completePendingCommand()
     * and check its return value to verify your success.
     * @param      remoteFileName  The remote file name.
     * @return   An InputStream from which the remote file can be read. If
     *      the data connection cannot be opened (for example, if the file does not
     *      exist), a null is returned, in which case you can check the reply
     *      code to determine the exact reason for failure.
     * @exception  IOException   If some error occurs.
     */
    // Note: After calling this method, you have to call method completePendingCommand()
    //       to read out/update the reply and finish the whole action. completePendingCommand()
    //       will communicate on command connection socket. For some ftp server (e.x. Serv-U SSL
    //       ftp server), it may give us completion reply message only after the data has been
    //       read out on data connection socket; otherwise, completePendingCommand() will always
    //       fail. That is, the order of procedure may be:
    //          1. Call this method.
    //          2. Some action to read out from the returned InputStream.
    //          3. Call completePendingCommand().
    //       If you encounter problem when using this method, please try above scenario.
    public InputStream retrieveFileStream(String remoteFileName)
    throws IOException {
        return delegate.retrieveFileStream(remoteFileName);
    }
    
    /**
     * Sends an FTP command without a parameter to the remote FTP server.
     * The method then waits for a reply and returns the numerical response code.
     * @param      command  The FTP command to be executed.
     * @return     The reply code from the FTP command.
     * @exception  IOException   If some error occurs.
     */
    public int sendCommand(String command)
    throws IOException {
        return this.sendCommand(command, null);
    }
    
    /**
     * send a site command to FTP server
     * @param command - site command
     * @return - true, success, false, failure;
     * @throws IOException
     */
    public boolean sendSiteCommand(String command)
    throws IOException {
        return this.delegate.sendSiteCommand(command);
    }
    
    /**
     * Stores a file on the remote FTP server using the given name and taking its input from the given InputStream.
     * The InputSteam is NOT closed in this method.
     * @param      remoteFileName  The remote file name.
     * @param               local  The local input stream.
     * @return     true if the operation is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean storeFile(String remoteFileName, InputStream local)
    throws IOException {
        return delegate.storeFile(remoteFileName, local);
    }
    
    /**
     * Stores a file on the remote FTP server using the given name and taking its input from the given InputStream.
     * The InputSteam is NOT closed in this method.
     * @param           remoteDirName  The remote directory name.
     * @param      remoteBaseFileName  The remote file name.
     * @param                   local  The local input stream.
     * @return     true if the operation is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean storeFile(String remoteDirName, String remoteBaseFileName, InputStream local)
    throws IOException {
        return this.storeFile(
                heuristics.buildFullFileName(remoteDirName, remoteBaseFileName),
                local);
    }
    
    /**
     * Returns an OutputStream through which data can be written to store
     * a file on the server using the given name. If the current file type
     * is ASCII, the returned OutputStream converts line separators in
     * the file to the NETASCII format , that is, you do not have to
     * create a special OutputStream to do this operation. You must close the
     * OutputStream when you finish writing to it. The OutputStream itself
     * takes care of closing the parent data connection socket upon being
     * closed. To finalize the file transfer, you must call the method
     * completePendingCommand() and check its return value to verify your success.
     * @param      remoteFileName  The remote file name.
     * @return An OutputStream through which the remote file can be written. If
     *      the data connection cannot be opened (for example, the file does not
     *      exist), a null is returned. In such cases, you can check the reply
     *      code to determine the exact reason for failure.
     * @exception  IOException   If some error occurs.
     */
    // Note: After calling this method, you have to call method completePendingCommand()
    //       to read out/update the reply and finish the whole action. completePendingCommand()
    //       will communicate on command connection socket. For some ftp server (e.x. Serv-U SSL
    //       ftp server), it may give us completion reply message only after the data has been
    //       write out on data connection socket; otherwise, completePendingCommand() will always
    //       fail. That is, the order of procedure may be:
    //          1. Call this method.
    //          2. Some action to write out to the returned OutputStream.
    //          3. Call completePendingCommand().
    //       If you encounter problem when using this method, please try above scenario.
    public OutputStream storeFileStream(String remoteFileName)
    throws IOException {
        return delegate.storeFileStream(remoteFileName);
    }
    
    /**
     * Sets the current data connection mode to ACTIVE_LOCAL_DATA_CONNECTION_MODE.
     */
    public void useActive() {
        delegate.enterLocalActiveMode();
    }
    
    /**
     * Sets the current data connection mode to PASSIVE_LOCAL_DATA_CONNECTION_MODE.
     */
    public void usePassive() {
        delegate.enterLocalPassiveMode();
    }
    
    /**
     * Renames a remote file. The difference between this method and rename() is that
     * this method can also create the target path if it does not already exist.
     * @param           dirNameFrom  The remote directory name.
     * @param      baseFileNameFrom  The remote base file name.
     * @param             dirNameTo  The remote directory name.
     * @param        baseFileNameTo  The remote base file name.
     * @return     true if the operation is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean archiveFile(
            String dirNameFrom,
            String baseFileNameFrom,
            String dirNameTo,
            String baseFileNameTo)
            throws IOException {
        
        try {
            this.mkdirs(dirNameTo);
        } catch (Exception e) {
            // do nothing
        }
        
        return this.rename(
                heuristics.buildFullFileName(dirNameFrom, baseFileNameFrom),
                heuristics.buildFullFileName(dirNameTo, baseFileNameTo));
    }
    
    /**
     * Receives an FTPClient method's completion reply
     * from the server and verifies the success of the entire transaction,
     * after the programmer's code completes its actions.
     * There are a few FTPClient methods that do not complete
     * the entire sequence of FTP commands when completing a transaction.
     * These commands require action by the programmer, using this method,
     * after the reception of a positive intermediate command.
     * @return     true if the operation is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean completePendingCommand()
    throws IOException {
        return delegate.completePendingCommand();
    }
    
    /**
     * Gets the name for all file entries that are related to real file data.
     * This method filters out directory entries and pure symbolic link entries.
     * @param      files  An array of FTP ETD objects.
     * @return     An array of full file names.
     * @exception  IOException   If some error occurs.
     */
    public String[] listFileNames(FTPFile[] files)
    throws IOException {
        FTPFile[] realFiles = this.listFiles(files, true);
        if (realFiles == null) {
            return null;
        }
        
        Vector lists = new Vector();
        for (int i = 0; i < realFiles.length; i++) {
            // Note: In org.apache.commons.net.ftp.DefaultFTPFileListParser.parseFTPEntry(),
            //       there is potential problem (mentioned in that method), it assumes
            //       between datetime and file name, only one space exists. It is not
            //       always true. If more than one spaces, the file name returned by
            //       FTPFile.getName() will have some space prefix like " fileNameA",
            //       " fileNameB". I found this situation on aix ftp servers (circe and
            //       ate box) - "UNIX" listing style. Because I delegate all "UNIX"
            //       listing style to that third-party class DefaultFTPFileListParser,
            //       let's remove the leading spaces from file name if any. So this
            //       problem is fixed easily. But, if more other problems are found in
            //       that class later, I'll consider not to delegate to
            //       DefaultFTPFileListParser for whatever listing style, that is,
            //       I'll always use my class FtpHeuristicsFileListParser.
            
            lists.addElement(realFiles[i].getName().trim());
        }
        
        if (lists.size() == 0)
            return null;
        else {
            String[] f = new String[lists.size()];
            lists.copyInto(f);
            return f;
        }
    }
    
    
    /**
     * Retrieves an FtpHeuristics object.
     * @return    The FtpHeuristics object.
     */
    public FtpHeuristics getHeuristics() {
        return heuristics;
    }
    
    /**
     * Creates a new directory if the directory does not exist.
     * For directory dir1/dir2, if the parent directory dir1 does not exist, the method fails.
     * There is another method, mkdirs(), that creates all necessary parent directories.
     * @param      dir  The directory name to be created.
     * @return     true if the action is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean mkdir(String dir)
    throws IOException {
        boolean ret = true;
        boolean newDir = false;
        String msg = null;
        // save current dir
        String currDirSave = this.pwd();
        if (!this.isPositiveCompletion(this.getReplyCode())) {
            msg = mMessages.getString("FTPBC-E006007.ERR_EXT_FTP_ACTION_FAIL", 
            		new Object[] {
            		"mkdir(String dir)", "pwd", getReplyString()});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new IOException(msg);
        }
        try {
            newDir = !this.cd(dir);
        } catch (Exception e) {
            newDir = true;
        }
        if (newDir) {
            // Note: For some UNIX ftp server (e.g atlas), if the dir name is invalid (such as
            //       "\dir" instead of "/dir"), following function call may return true, and an
            //       invalid directory entry may be created. This should be treated as a bug of
            //       ftp server or a bug of third party ftp package. So we can't simply depend
            //       on the return of makeDirectory(dir), let's double check the result of
            //       function call of this.delegate.makeDirectory(dir).
            //ret = this.delegate.makeDirectory(dir);
            if (!this.delegate.makeDirectory(dir)) {
                // mkd racing condition, i.e., more than one client
                // attempting MKD with same name, there is only one
                // winner, others fail, but if the named dir
                // is there, then it should be deemed as OK,
                // delay 300 milli before return false.
                try {
                    Thread.sleep(300);
                }
                catch (Exception e) {
                    // ignore on purpose
                }
                try {
                    ret = this.cd(dir);
                } catch (Exception e) {
                    ret = false;
                }
            } else {
                // Double check
                try {
                    ret = this.cd(dir);
                } catch (Exception e) {
                    ret = false;
                }
            }
        } else {
            ret = true;
        }
        
        // Change back working directory
        if (!this.cd(currDirSave)) {
            msg = mMessages.getString("FTPBC-E006036.ERR_EXT_FTP_CD_BACK_FAIL",
            		new Object[] {"mkdir", 
            		currDirSave});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new IOException(msg);
        }
        
        return ret;
    }
    
    /**
     * Creates a new directory if the directory does not exist.
     * This method also creates all necessary parent directories.
     * @param      dir  The directory name to be created.
     * @return     true if the action is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean mkdirs(String dir)
    throws IOException {
        String msg = null;
        boolean retValue = true;
        int pos = 0;
        int nextPos = 0;
        String workDir = null;
        String dirDelimiter = this.heuristics.getInterDirDelimiter();
        String endingDirDelimiter = this.heuristics.getInitFileDelimiter();
        // Note: for NT3.5 and NT 4.0 ftp servers, the delimiter can be either "\" or "/".
        //       In FtpHeuristics, it's "\". So we can replace "/" with "\" first for
        //       convenience.
        if (this.heuristics.getDirListingStyle().equals("NT 4.0") ||
                this.heuristics.getDirListingStyle().equals("NT 3.5")) {
            dir = dir.replace('/', '\\');
        }
        
        while (nextPos >= 0) {
            nextPos = dir.indexOf(dirDelimiter, pos + 1);
            if (nextPos < 0) {
                workDir = dir;
            } else {
                pos = nextPos;
                workDir = dir.substring(0, pos);
            }
            // Append ending dir delimiter (same as initial file delimiter) if any (for example, VMS)
            if (!"".equals(endingDirDelimiter) &&
                    this.heuristics.isDirRequireTerminator() &&
                    !workDir.endsWith(endingDirDelimiter)) {
                workDir += endingDirDelimiter;
            }
            try {
                if (!this.mkdir(workDir)) {
                    retValue = false;
                    break;
                }
            } catch (Exception e) {
                msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
                		new Object[] {
                		"mkdirs(String dir)", e});
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg, e);
                }
                throw new IOException(msg);
            }
        }
        
        return retValue;
    }
    
    /**
     * Sets the SocketFactory used by the SocketClient to open socket
     * connections. If the factory value is a null, a default
     * factory value is used. Only use this method to reset the factory after you have
     * previously altered it.
     * <p>
     * @param factory  The new SocketFactory the SocketClient must use.
     */
    
    // If class SocksServerSocket has not been implemented completely,
    // we will not use method factory.createServerSocket().
    // In com.oroinc.ftp.FTPClient, factory.createServerSocket() is used
    // only for active local connection mode (in method
    // FTPClient.__openDataConnection()), we'll not need create
    // ServerSocket if we don't use active local connection mode.
    
    // So we can apply following logic to avoid the use of method
    // factory.createServerSocket():
    // In FtpInterface, before invoke this method setSocketFactory(factory),
    // always enter passive local connection mode first. See method
    // FtpInterface.configureSocks(factory).
    
    public void setSocketFactory(SocketFactory factory) {
        this.delegate.setSocketFactory(factory);
    }
    
    /**
     * Closes the connection to the remote FTP server and restores the configuration parameters
     * to the default values.
     * @exception  IOException   If some error occurs.
     */
    public void disConnect()
    throws IOException {
        delegate.disconnect();
    }
    
    /**
     * Sets the file type of the file to be transferred, to EBCDIC.
     * @return     true if the operation is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean ebcdic()
    throws IOException {
        // Note: org.apache.commons.net.ftp package doesn't support EBCDIC mode,
        //       see documentation of class org.apache.commons.net.ftp.FTPClient.
        //       Actually, FTPClient.EBCDIC_FILE_TYPE (an int) maps to an
        //       invalid type char, the class uses this way to disable the
        //       EBCDIC file type.
        //       So we cannot use setFileType(delegate.EBCDIC_FILE_TYPE).
        //       We can send raw command to ftp server directly instead.
        
        //return delegate.setFileType(delegate.EBCDIC_FILE_TYPE);
        
        if (this.isPositiveCompletion(this.sendCommand("TYPE", "E"))) {
            return true;
        } else {
            return false;
        }
    }
    
    /**
     * Returns the current SO_LINGER timeout (in seconds) of the currently opened socket.
     * A -1 return means that the option is disabled. The setting only affects socket closing.
     * <p>
     * @return The current SO_LINGER timeout.  If SO_LINGER is disabled, the return is -1.
     * @exception SocketException If the operation fails.
     */
    // Get SoLinger for the command connection socket instead of data connection socket.
    // We didn't provide the SoLinger getter for data connection socket.
    public int getSoLinger() throws SocketException {
        return delegate.getSoLinger();
    }
    
    /**
     * Returns the timeout, in milliseconds, of the currently opened socket.
     * <p>
     * @return The timeout, in milliseconds, of the currently opened socket.
     * @exception SocketException If the operation fails.
     */
    // Get SoTimeout for the command connection socket instead of data connection socket.
    // We didn't provide the SoTimeout getter for data connection socket.
    public int getSoTimeout() throws SocketException {
        return delegate.getSoTimeout();
    }
    
    /**
     * Allows you to determine whether Nagle's algorithm is enabled on the currently opened
     * socket.
     * <p>
     * @return true if Nagle's algorithm is enabled on the currently opened
     *        socket, but false if otherwise.
     * @exception SocketException If the operation fails.
     */
    // Get TcpNoDelay for the command connection socket instead of data connection socket.
    // We didn't provide the TcpNoDelay getter for data connection socket.
    public boolean getTcpNoDelay() throws SocketException {
        return delegate.getTcpNoDelay();
    }
    
    /**
     * Allows you to determine whether verification of the remote FTP host participating
     * in data connections is enabled. The default is for
     * verification to be enabled.
     * <p>
     * @return true if verification is enabled, but false if not.
     */
    public boolean isRemoteVerificationEnabled() {
        return delegate.isRemoteVerificationEnabled();
    }
    
    /**
     * Allows you to enable or disable verification that the remote FTP host taking part
     * in a given data connection is the same as the host to which the control
     * connection is attached. The default is for verification to be
     * enabled. You can set this value at any time, whether the
     * FTP client is currently connected or not.
     * <p>
     * @param enable true enables verification, and false disables it.
     */
    public void setRemoteVerificationEnabled(boolean enable) {
        delegate.setRemoteVerificationEnabled(enable);
    }
    
    /**
     * Sets the SO_LINGER timeout (in seconds) on the currently opened command socket.
     * The maximum timeout value is platform-specific. The setting only affects the closing of the socket.
     * <p>
     * @param on  true if the timeout is to be enabled, and false if not.
     * @param val The timeout in seconds (in hundredths of a second?)
     * @exception SocketException If the operation fails.
     */
    // Set SoLinger for the command connection socket instead of data connection socket.
    // We didn't provide the SoLinger setter for data connection socket.
    public void setSoLinger(boolean on, int val) throws SocketException {
        delegate.setSoLinger(on, val);
    }
    
    /**
     * Sets the timeout, in milliseconds, of a currently open command connection.
     * Only call this method after a command connection has been opened
     * by the connect() method. A timeout of zero is interpreted as an infinite timeout.
     * To set the timeout before using connect() and avoiding the possible
     * blocking of the connect() method, instead use the method setDefaultTimeout().
     * The timeout set by setDefaultTimeout() can be overwritten by the timeout
     * set by setSoTimeout().
     * <p>
     * @param timeout  The timeout, in milliseconds, to use for the currently
     *                 open command connection socket.
     * @exception SocketException If the operation fails.
     */
    // Set SoTimeout for the command connection socket instead of data connection socket.
    // It can be called only after method connect(), another method setDefaultTimeout()
    // that also is used for command connection socket can take effect only after method
    // connect().
    // For data connection socket, use method setDataSocketTimeout().
    public void setSoTimeout(int timeout) throws SocketException {
        delegate.setSoTimeout(timeout);
    }
    
    /**
     * Allows you to enable or disable the Nagle's algorithm (TCP_NODELAY) on the
     * currently opened command socket.
     * <p>
     * @param on  true to enable Nagle's algorithm, and false to disable.
     * @exception SocketException If the operation fails.
     */
    // Set TcpNoDelay for the command connection socket instead of data connection socket.
    // We didn't provide the TcpNoDelay setter for data connection socket.
    public void setTcpNoDelay(boolean on) throws SocketException {
        delegate.setTcpNoDelay(on);
    }
    
    /**
     * Removes a directory on the FTP server, if it is empty.
     * <p>
     * @param dirName  The name of the remote directory to remove.
     * @return     true if the operation is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean rmdir(String dirName) throws IOException {
        return delegate.removeDirectory(dirName);
    }
    
    /**
     * Returns the default timeout, in milliseconds, that is used when
     * opening a command connection socket.
     * <p>
     * @return The default timeout, in milliseconds, that is used when
     *         opening a command connection socket.
     */
    // Change from public to private - prevent user from using it.
    private int getDefaultTimeout() {
        return delegate.getDefaultTimeout();
    }
    
    /**
     * Sets the timeout, in milliseconds, to use when reading from the
     * data connection. This timeout is set immediately after
     * the data connection is opened.
     * NOTE: This connection is not the command connection.
     * <p>
     * @param  timeout The timeout, in milliseconds, that is used when
     *         opening a data connection socket.
     */
    // Set SoTimeout for the data connection socket instead of command connection socket.
    // This method can be called before or after connect(). It will affect ftp data transfer
    // commands like LIST, RETR, STOR, APPE.
    // For command connection socket, use method setDefaultTimeout() or setSoTimeout().
    public void setDataSocketTimeout(int timeout) {
        delegate.setDataTimeout(timeout);
    }
    
    /**
     * Set the default timeout, in milliseconds, to use when opening a command socket.
     * This value is only used before calling the connect() method and must not be
     * confused with setSoTimeout(), which operates on the currently opened command
     * socket.
     * <p>
     * @param timeout  The timeout, in milliseconds, to use for the command connection socket.
     */
    // Set SoTimeout for the command connection socket instead of data connection socket.
    // For data connection socket, use method setDataSocketTimeout().
    // Change from public to private - prevent user from using it.
    private void setDefaultTimeout(int timeout) {
        delegate.setDefaultTimeout(timeout);
    }
    
    /**
     * Initializes this ftp client wrapper.
     * @param      intf - Batch OTD interface to application code.
     * @exception  FtpFileException - error occurs.
     */
    public void initialize(FtpInterface intf) throws FtpFileException {
        if ( delegate != null )
            throw new IllegalStateException("Initialize a ftp client provider which is already initialized.");
        try {
            String userDirListingStyle = intf.getConfiguration().getUserDefinedDirectoryListingStyle();
            String userHeuristicCfgPath = intf.getConfiguration().getUserHeuristicsLocation();
            String cntrlEncoding = intf.getConfiguration().getControlChannelEncoding();
            // null or blank user style will trun off user defined heuristics
            this.setUserDefinedHeuristicsInfo(userDirListingStyle, userHeuristicCfgPath);
            // set built-in style will not affect the user defined style if user defined style
            // is specified;
            this.setDirListingStyle(intf.getConfiguration().getDirectoryListingStyle());
            
            delegate = new FTPClient();
            
            String ftpSecType = intf.getConfiguration().getSecureFTPType();
            
            if ( cntrlEncoding != null && cntrlEncoding.trim().length() > 0 ) {
                if ( Charset.isSupported(cntrlEncoding) ) {
                    delegate.setControlEncoding(cntrlEncoding);
                }
                else {
                    throw new IllegalArgumentException("Unsupported charset name [" + cntrlEncoding + "] encountered when init FtpFileProvider");
                }
            }
            
            if ( ftpSecType != null && !ftpSecType.equals(FtpFileConfigConstants.FTP_SECURE_NONE) ) {
                delegate.setFTPType( ftpSecType.equalsIgnoreCase(FtpFileConfigConstants.FTP_SECURE_IMPLICITSSL) ? FTPType.FTP_OVER_IMPLICIT_SSL : FTPType.FTP_OVER_EXPLICIT_SSL); 
                delegate.setKeyStoreLoc(intf.getConfiguration().getKeyStoreLoc());
                delegate.setKeyStorePassword(intf.getConfiguration().getKeyStorePassword());
                delegate.setTrustStoreLoc(intf.getConfiguration().getTrustStoreLoc());
                delegate.setTrustStorePassword(intf.getConfiguration().getTrustStorePassword());
                delegate.setClientAlias(intf.getConfiguration().getKeyAlias());
                delegate.setAliasPass(intf.getConfiguration().getKeyPassword());
            }
            else {
                delegate.setFTPType(FTPType.REGULAR_FTP);
            }
        } catch (Exception e) {
            throw new FtpFileException(mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
            		new Object[] {
            		"initialize()", e}), e);
        }
    }
    
    /**
     * Initializes and updates the FTP heuristics using a specified directory-listing style.
     * @param      dirListingStyle  The directory-listing style.
     * @exception  Exception   If some error occurs.
     */
    public void setDirListingStyle(String dirListingStyle) throws Exception {
        if (mLogger.isLoggable(Level.FINE)) 
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"setDirListingStyle(String dirListingStyle)"}));
        
        boolean changeCurrentHeuristics = false;
        if (this.built_in_heuristics != null
                && this.built_in_heuristics.getDirListingStyle().equals(dirListingStyle)) {
            // no change for the selected built-in heuristics,
            // don't need to update heuristics.
            return;
        }
        
        // selected built-in heuristics changed
        // check if it is used as the current heuristics
        FtpHeuristics tempHeuristics = new FtpHeuristics(dirListingStyle);
        if ( this.built_in_heuristics != null
                && this.heuristics != null ) {
            // current heuristics exist and built-in heuristics selected
            // if they are not equal -> the current heuristics is a user defined one
            if ( this.built_in_heuristics == this.heuristics ) {
                // current heuristics is built-in heuristics - change it accordingly
                changeCurrentHeuristics = true;
                this.heuristics = tempHeuristics;
            }
            this.built_in_heuristics = tempHeuristics;
        } else {
            if ( this.built_in_heuristics == null && this.heuristics == null ) {
                // first time provider is set for its dir listing style
                changeCurrentHeuristics = true;
                this.built_in_heuristics = this.heuristics = tempHeuristics;
            } else if ( this.built_in_heuristics == null ) {
                // only built-in change, do not touch existing heuristics
                this.built_in_heuristics = tempHeuristics;
            } else {
                // heuristics is null
                changeCurrentHeuristics = true;
                this.heuristics = this.built_in_heuristics = tempHeuristics;
            }
        }

//        
// jim.fu@sun.com - changes for incoporate FTP/TLS support
// quoted the following FTPClient sub classing - now apache code is in-coporated, 
// non-compliant return code can be handled there
        
//         
//        if ( this.delegate == null ) {
//            this.delegate = new FTPClient() {
//                public int sendCommand(String command, String parm)
//                throws IOException {
//                    int replyCode = super.sendCommand(command, parm);
//                    if (isTraceRawCommand() || isNegativePermanent(replyCode)
//                        || isNegativeTransient(replyCode)) {
//                        String replyString = super.getReplyString();
//                        if (replyString != null) {
//                            replyString = replyString.trim();
//                        }
//                        if (mLogger.isLoggable(Level.FINE)) {
//                            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006027.DBG_EXT_FTP_LOG_FTP_REPLY", new Object[] {
//                            		"sendCommand()", command, replyString
//                            }));
//                        }
//                    }
//                    
//                    // interpret special reply codes for some non-standard
//                    // cases (not spec-compliant cases)
//                    if ("MSP PDS (Fujitsu)".equals(heuristics.getDirListingStyle()) ||
//                            "MSP PS (Fujitsu)".equals(heuristics.getDirListingStyle())) {
//                        if ("PWD".equalsIgnoreCase(command) && (251 == replyCode)) {
//                            replyCode = 257;
//                            if (mLogger.isLoggable(Level.FINE)) {
//                                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006028.DBG_EXT_FTP_LOG_FTP_REPLY_NON_COMPLY", new Object[] {
//                                		"sendCommand()",
//                                		command, replyCode
//                                }));
//                            }
//                        }
//                    }
//                    
//                    return replyCode;
//                }
//                public int sendCommand(String command)
//                throws IOException {
//                    return this.sendCommand(command, null);
//                }
//            };
//        }
        
        if ( changeCurrentHeuristics ) {
            this.defaultFileListParser = new FtpHeuristicsFileListParser(this.heuristics);
        }
    }
    
    /**
     * Initializes and updates the FTP heuristics using a user specified directory-listing style.
     * @param      userDefinedDirListingStyle  The user defined directory-listing style.
     * @param      userDefinedHeuristicsCfgFile  The heuristics configuration file path on logical host
     * containing user defined heuristics information.
     * @exception  Exception   If error occurs.
     */
    public void setUserDefinedHeuristicsInfo(String userDefinedDirListingStyle, String userDefinedHeuristicsCfgFile) throws Exception {
        if ( mLogger.isLoggable(Level.FINE) )
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"setUserDefinedHeuristicsInfo()"}));
        
        if ( !isEmpty(userDefinedDirListingStyle) && isEmpty(userDefinedHeuristicsCfgFile) )
            throw new Exception(mMessages.getString("FTPBC-E006019.ERR_EXT_FTP_MISS_UD_HEURISTICS_FILE",
                    new Object[] {userDefinedDirListingStyle}));
        
        if (this.user_defined_heuristics != null
                && !isEmpty(userDefinedDirListingStyle)
                && this.user_defined_heuristics.getDirListingStyle().equals(userDefinedDirListingStyle)
                && !isEmpty(this.userDefinedHeuristicsCfgFile)
                && this.userDefinedHeuristicsCfgFile.equals(userDefinedHeuristicsCfgFile)) {
            // no change for the selected user defined heuristics,
            // don't need to update heuristics.
            return;
        }
        
        // selected user defined heuristics changed to null or blank - this means turning off
        // user defined and use built-in;
        if ( isEmpty(userDefinedDirListingStyle) ) {
            // setting user defined heuristics to null or blank
            // makes built-in heuristics the current
            this.heuristics = this.built_in_heuristics;
            this.user_defined_heuristics = null;
            if ( this.built_in_heuristics != null ) {
                // set the parser
                this.defaultFileListParser = new FtpHeuristicsFileListParser(this.heuristics);
            }
            return;
        }
        
        // otherwise, it is a user defined heuristics init or change
        FtpHeuristics tempHeuristics = new FtpHeuristics(userDefinedDirListingStyle, userDefinedHeuristicsCfgFile);
        this.heuristics = this.user_defined_heuristics = tempHeuristics;
        
//        if ( this.delegate == null ) {
//            this.delegate = new FTPClient() {
//                public int sendCommand(String command, String parm)
//                throws IOException {
//                    int replyCode = super.sendCommand(command, parm);
//                    if (isTraceRawCommand() || isNegativePermanent(replyCode)
//                        || isNegativeTransient(replyCode)) {
//                        String replyString = super.getReplyString();
//                        if (replyString != null) {
//                            replyString = replyString.trim();
//                        }
//                        if (mLogger.isLoggable(Level.FINE)) {
//                            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006027.DBG_EXT_FTP_LOG_FTP_REPLY", new Object[] {
//                            		"sendCommand()", command, replyString
//                            }));
//                        }
//                    }
//                    
//                    // interpret special reply codes for some non-standard
//                    // cases (not spec-compliant cases)
//                    if ("MSP PDS (Fujitsu)".equals(heuristics.getDirListingStyle()) ||
//                            "MSP PS (Fujitsu)".equals(heuristics.getDirListingStyle())) {
//                        if ("PWD".equalsIgnoreCase(command) && (251 == replyCode)) {
//                            replyCode = 257;
//                            if (mLogger.isLoggable(Level.FINE)) {
//                                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006028.DBG_EXT_FTP_LOG_FTP_REPLY_NON_COMPLY", new Object[] {
//                                		"sendCommand()",
//                                		command, replyCode
//                                }));
//                            }
//                        }
//                    }
//                    
//                    return replyCode;
//                }
//                public int sendCommand(String command)
//                throws IOException {
//                    return this.sendCommand(command, null);
//                }
//            };
//        }
        
        // no default for a user defined directory listing style - even it is called UNIX
        this.defaultFileListParser = new FtpHeuristicsFileListParser(this.heuristics);
    }
    
    /**
     * Get the first matching remote file name based on the remote directory
     * regular expression and the remote file regular expression.
     * @param          dir  The directory name.
     * @param   isDirRegex  Determines whether the directory name is a regular expression.
     * @param         file  The file name.
     * @param  isFileRegex  Determines whether the file name is a regular expression.
     * @return      The full file name.
     * @exception   IOException  If some error occurs.
     */
    public String getFirstFileName(String dir, boolean isDirRegex, String file, boolean isFileRegex) throws IOException {
        if (mLogger.isLoggable(Level.FINE)) 
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"getFirstFileName(String dir, boolean isDirRegex, String file, boolean isFileRegex)"}));
        
        String [] files = this.listFileNames(dir, isDirRegex, file, isFileRegex, true);
        if (files == null || files.length == 0) {
            return "";
        } else {
            return files[0];
        }
    }
    
    /**
     * Lists the remote file names based on the remote directory and
     * remote file regular expression.
     * @param        remoteDir   The remote directory name.
     * @param remoteFileRegExp   The file regular expression.
     * @param          oneFile   true asks for only one file, false asks for a list all qualified files.
     * @return      The full file names.
     * @exception   IOException  If some error occurs.
     */
    private String[] listFileNames(String remoteDir, String remoteFileRegExp, boolean oneFile)
    throws IOException {
        String msg = null;
        // list remote file names
        String[] files = null;
        try {
            if (mLogger.isLoggable(Level.FINE)) 
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006029.DBG_EXT_FTP_START_LIST_FILES", new Object[] {"listFileNames(String remoteDir, String remoteFileRegExp, boolean oneFile)", remoteDir, remoteFileRegExp}));
            
            files = this.listFileNames(this.listFiles(remoteDir, remoteFileRegExp));
            msg = "";
            if (files != null && files.length > 0) {
                // sort files in ascending order
                Arrays.sort(files);
                for (int i = 0; i < files.length; i++) {
                    if (i != 0) {
                        msg += ", ";
                    }
                    msg += files[i];
                    // Build full file name
                    files[i] = this.getHeuristics().buildFullFileName(remoteDir, files[i]);
                    if (oneFile) {
                        break;
                    }
                }
            }
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, msg);
            }
        } catch (Exception e) {
            msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
            		new Object[] {
            		"listFileNames(String remoteDir, String remoteFileRegExp, boolean oneFile)", e});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg, e);
            }
            throw new IOException(msg);
        }
        
        return files;
    }
    
    /**
     * Lists the remote file names based on the remote directory regular expression and
     * remote file regular expression.
     * @param          dir  The directory name.
     * @param   isDirRegex  Determines whether the directory name is a regular expression.
     * @param         file  The file name.
     * @param  isFileRegex  Determines whether the file name is a regular expression.
     * @param          oneFile   true asks for only one file, false asks for a list all qualified files.
     * @return      The full file names.
     * @exception   IOException  If some error occurs.
     */
    private String[] listFileNames(String dir, boolean isDirRegex, String file, boolean isFileRegex, boolean oneFile) throws IOException {
        String fileRegExp = null;
        
        if (!isFileRegex && this.isUseRegexAsMatcher()) {
            fileRegExp = this.heuristics.escapeRegExpChars(file);
        } else {
            fileRegExp = file;
        }
        
        if (!isDirRegex) {
            return this.listFileNames(dir, fileRegExp, oneFile);
        } else {
            DirRegExp dirRegExp = new FtpDirRegExp(dir, this);
            ArrayList list = null;
            try {
                list = dirRegExp.getDirs();
            } catch (Exception e) {
                throw new IOException(e.toString());
            }
            return this.listFileNames(list, fileRegExp, oneFile);
        }
    }
    
    /**
     * Lists the remote file names based on the remote directory list and
     * remote file regular expression.
     * @param             dirs   The directory regular expression list.
     * @param remoteFileRegExp   The file regular expression.
     * @param          oneFile   true asks for only one file, false asks for a list all qualified files.
     * @return      The full file names.
     * @exception   IOException  If some error occurs.
     */
    private String[] listFileNames(ArrayList dirs, String remoteFileRegExp, boolean oneFile)
    throws IOException {
        
        String[] dstFiles = new String[0];
        String[] srcFiles = new String[0];
        String[] tmpFiles = new String[0];
        int tmpLen = 0;
        
        if (dirs == null || dirs.size() == 0) {
            return dstFiles;
        }
        
        for (int i = 0; i < dirs.size(); i++) {
            srcFiles = this.listFileNames((String) (dirs.get(i)), remoteFileRegExp, oneFile);
            if (srcFiles == null || srcFiles.length == 0) {
                continue;
            }
            tmpLen = dstFiles.length;
            // store old dst to tmp
            tmpFiles = new String[tmpLen];
            System.arraycopy(dstFiles, 0, tmpFiles, 0, tmpLen);
            
            // create a new dst
            dstFiles = new String[tmpLen + srcFiles.length];
            System.arraycopy(tmpFiles, 0, dstFiles, 0, tmpLen);
            System.arraycopy(srcFiles, 0, dstFiles, tmpLen, srcFiles.length);
            
            if (oneFile) {
                break;
            }
        }
        return dstFiles;
    }
    
    /**
     * Lists the remote full file names based on the remote directory regular expression and
     * remote file regular expression.
     * @param          dir  The directory name.
     * @param   isDirRegex  Determines whether the directory name is a regular expression.
     * @param         file  The file name.
     * @param  isFileRegex  Determines whether the file name is a regular expression.
     * @return      The full file names.
     * @exception   IOException  If some error occurs.
     */
    public String[] listFileNames(String dir, boolean isDirRegex, String file, boolean isFileRegex) throws IOException {
        return this.listFileNames(dir, isDirRegex, file, isFileRegex, false);
    }
    
    // add following accessors for more flexible extensibility
    /**
     * Retrieves the defaultFileListParser object
     * @return FTPFileListParser
     */
    public FTPFileListParser getDefaultFileListParser() {
        return this.defaultFileListParser;
    }
    
    /**
     * Retrieves the delegate object
     * @return FTPClient
     */
    public FTPClient getDelegate() {
        return this.delegate;
    }
    
    /**
     * Sets the defaultFileListParser object
     * @param parser FTPFileListParser instance
     */
    public void setDefaultFileListParser(FTPFileListParser parser) {
        this.defaultFileListParser = parser;
    }
    
    /**
     * Sets the delegate object
     * @param client FTPClient instance
     */
    public void setDelegate(FTPClient client) {
        this.delegate = client;
    }
    
    /**
     * Sets the heuristics object
     * @param heuristics FtpHeuristics instance
     */
    public void setHeuristics(FtpHeuristics heuristics) {
        this.heuristics = heuristics;
    }
    
    private boolean isEmpty(String s) {
        if ( s == null || s.trim().length() == 0 )
            return true;
        return false;
    }

    public void setWarningOff(boolean b) {
        noWarning = b;
    }

    public boolean getWarningOff() {
        return noWarning;
    }

    public boolean isUseRegexAsMatcher() {
        return this.useRegexAsMatcher;
    }

    public void setUseRegexAsMatcher(boolean useRegexAsMatcher) {
        this.useRegexAsMatcher = useRegexAsMatcher;
    }

    public void clearCommandChannel() throws IOException {
        delegate.doCCC();
    }
}
