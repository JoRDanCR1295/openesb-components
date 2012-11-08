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
 * @(#)FtpFileProvider.java 
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

import com.sun.jbi.ftpbc.ftp.exception.FtpFileException;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;

import java.net.SocketException;

import org.apache.commons.net.SocketFactory;
import org.apache.commons.net.ftp.FTPFile;

/**
 * A wrapper class of a ftp client
 * expose API for remote file operations.
 * @author jfu (jim.fu@sun.com)
 * @author Harry Liu
 * @version cvs revision:    Last Modified: 
 */
public interface FtpFileProvider {
    /**
     * Appends to a file onto the remote FTP server with the given file name, 
     * taking its input from the given file name.
     * The FileInputSteam related to the localFileName is closed in this method.
     * @param      remoteFileName  The remote file name.
     * @param       localFileName  The local file name.
     * @return     true if the action is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean appendFile(String remoteFileName, String localFileName) 
        throws IOException;

    /**
     * Appends to a file onto the remote FTP server with the given directory and file name, 
     * taking its input from the given file name.
     * The FileInputSteam related to the localFileName is closed in this method.
     * @param           remoteDirName  The remote directory name.
     * @param      remoteBaseFileName  The remote base file name.
     * @param           localFileName  The local file name.
     * @return     true if the action is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean appendFile(
        String remoteDirName, 
        String remoteBaseFileName, 
        String localFileName)
        throws IOException;

    /**
     * Appends to a file onto the remote FTP server with the given file name, 
     * taking its input from the given InputStream. 
     * The InputSteam is NOT closed in this method.
     * @param      remoteFileName  The remote file name.
     * @param               local  The local input stream.
     * @return     true if the action is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean appendFile(String remoteFileName, InputStream local)
        throws IOException;

    /**
     * Appends to a file onto the remote FTP server with the given file name, 
     * taking its input from the given InputStream. 
     * The InputSteam is NOT closed in this method.
     * @param      remoteFileName  The remote file name.
     * @param               local  The local input stream.
     * @return     true if the action is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean appendFile(String remoteDirName, String remoteBaseFileName, InputStream local)
        throws IOException;

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
        throws IOException;

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
    public OutputStream appendFileStream(String dirName, String baseFileName)
        throws IOException;

    /**
     * Renames a remote file. This method creates the target path if it does not already exist 
     * (the difference between this method and the rename() method).
     * @param           dirNameFrom  The old remote directory name.
     * @param      baseFileNameFrom  The old remote base file name.
     * @param             dirNameTo  The new remote directory name.
     * @param        baseFileNameTo  The new remote base file name.
     * @return     true if the action is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean archiveFile(
        String dirNameFrom, 
        String baseFileNameFrom, 
        String dirNameTo, 
        String baseFileNameTo)
        throws IOException;

    /**
     * Sets the file type (to be transferred) to ASCII.
     * @return     true if the action is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean ascii()
        throws IOException;

    /**
     * Sets the file type (to be transferred) to binary.
     * @return     true if the action is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean binary()
        throws IOException;

    /**
     * Changes the current working directory of the FTP session.
     * @param      dirName  The new directory name.
     * @return     true if the action is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean cd(String dirName)
        throws IOException;

    /**
     * Finishes the pending command. There are a few FTPClient methods that do not complete 
     * the entire sequence of FTP commands to complete a transaction. 
     * These commands require some action by the programmer 
     * after the reception of a positive intermediate command. 
     * After the programmer's code completes its actions, 
     * if necessary, the current command must call this method to receive the completion reply 
     * from the remote FTP server and verify the success of the entire transaction.
     * @return     true if the action is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean completePendingCommand()
        throws IOException;

    /**
     * Opens a socket connected to a remote FTP host at the specified port and 
     * originating from the current host at a system-assigned port.
     * @param      host  The FTP host name.
     * @param      port  The FTP server port.
     * @exception  SocketException   If some socket error occurs.
     * @exception  IOException   If some IO error occurs.
     */
    public void connect(String host, int port)
        throws SocketException, IOException;

    /**
     * Opens a socket connected to a remote FTP host at the specified port and 
     * originating from the current host at a system-assigned port.
     * @param      host  The FTP host name.
     * @param      port  The FTP server port.
     * @param      encoding The encoding for the server.
     * @exception  SocketException   If some socket error occurs.
     * @exception  IOException   If some IO error occurs.
     */
    public void connect(String host, int port, String encoding)
        throws SocketException, IOException;

    
    /**
     * Deletes a file on the remote FTP server.
     * @param      remoteFileName  The remote file name to be deleted.
     * @return     true if the action is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean deleteFile(String remoteFileName)
        throws IOException;

    /**
     * Deletes a file on the remote FTP server.
     * @param           dirName  The remote directory name of the file.
     * @param      baseFileName  The base name of the file.
     * @return     true if the action is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean deleteFile(String dirName, String baseFileName)
        throws IOException;

    /**
     * Closes the connection to the FTP server and restores the configuration parameters 
     * to the default values.
     * @exception  IOException   If some error occurs.
     */
    public void disConnect()
        throws IOException;

    /**
     * Sets the file type (to be transferred) to EBCDIC.
     * @return     true if the action is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean ebcdic()
        throws IOException;

    /**
     * Returns the current data connection mode (one of the _DATA_CONNECTION_MODE constants), 
     * for example, PASSIVE or ACTIVE.
     * @return  The data connection mode.
     */
    public int getDataConnectionMode();



    /**
     * Accessor - Get the FtpHeuristics object.
     * @return    The FtpHeuristics object.
     */
    public FtpHeuristics getHeuristics();

    /**
     * Returns the integer value of the reply code of the last FTP reply.
     * @return    The reply code.
     */
    public int getReplyCode();

    /**
     * Returns the entire text of the last FTP server response exactly as it was received.
     * @return    The reply string.
     */
    public String getReplyString();

    /**
     * Returns the lines of text from the last FTP server response as an array of strings, 
     * one entry per line.
     * @return    The reply string lines.
     */
    public String[] getReplyStrings();

    /**
     * Returns the current SO_LINGER timeout (in seconds) of the currently opened socket.
     * A -1 return means that the option is disabled. The setting only affects socket closing.
     * <p>
     * @return The current SO_LINGER timeout.  If SO_LINGER is disabled, the return is -1.
     * @exception SocketException If the operation fails.
     */
    // Get SoLinger for the command connection socket instead of data connection socket.
    // We didn't provide the SoLinger getter for data connection socket.
    public int getSoLinger() throws SocketException;

    /**
     * Returns the timeout, in milliseconds, of the currently opened socket.
     * <p>
     * @return The timeout, in milliseconds, of the currently opened socket.
     * @exception SocketException If the operation fails.
     */
    // Get SoTimeout for the command connection socket instead of data connection socket.
    // We didn't provide the SoTimeout getter for data connection socket.
    public int getSoTimeout() throws SocketException;

    /**
     * Retrieves the system type name from the remote FTP server and returns the string.
     * @return    The system name.
     * @exception  IOException   If some error occurs.
     */
    public String getSystemName()
        throws IOException;

    /**
     * Allows you to determine whether Nagle's algorithm is enabled on the currently opened 
     * socket.
     * <p>
     * @return true if Nagle's algorithm is enabled on the currently opened 
     *        socket, but false if otherwise.
     * @exception SocketException If the operation fails.
     */
    public boolean getTcpNoDelay() throws SocketException;

    /**
     * Sets the file type of the file to be transferred, to "Image."
     * @return     True if successfully completed, false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean image()
        throws IOException;

    /**
     * Initializes the provider using ftp interface object.
     * @param      intf  The FTP interface object.
     * @exception  FtpFileException   If some error occurs.
     */
    public void initialize(FtpInterface intf) throws FtpFileException;


    /**
     * Checks on whether the client is currently connected to a server.
     * @return     true if it is connected, but false if not.
     */
    public boolean isConnected();

    /**
     * Checks on whether the FTP operation is "Negative Permanent;" the return code can be any integer from 500 through 599.
     * @param      replyCode  The reply code of the FTP operation.
     * @return     Whether the FTP operation is "Negative Permanent."
     */
    public boolean isNegativePermanent(int replyCode);

    /**
     * Checks on whether the FTP operation is "Negative Transient;" the return code can be any integer from 400 through 499.
     * @param      replyCode  The reply code of the FTP operation.
     * @return     Whether the FTP operation is "Negative Transient."
     */
    public boolean isNegativeTransient(int replyCode);

    /**
     * Checks on whether the FTP operation is "Positive Completion;" the return code can be any integer from 200 through 299.
     * @param      replyCode  The reply code of the FTP operation.
     * @return     Whether the FTP operation is "Positive Completion."
     */
    public boolean isPositiveCompletion(int replyCode);

    /**
     * Checks on whether the FTP operation is "Positive Intermediate;" the return code can be any integer from 300 through 399.
     * @param      replyCode  The reply code of the FTP operation.
     * @return     Whether the FTP operation is "Positive Intermediate."
     */
    public boolean isPositiveIntermediate(int replyCode);

    /**
     * Checks on whether the FTP operation is "Positive Preliminary;" the return code can be any integer from 100 through 199.
     * @param      replyCode  The reply code of the FTP operation.
     * @return     Whether the FTP operation is "Positive Preliminary."
     */
    public boolean isPositivePreliminary(int replyCode);

    /**
     * Allows you to determine whether verification of the remote FTP host participating 
     * in data connections is enabled. The default is for
     * verification to be enabled.
     * <p>
     * @return true if verification is enabled, but false if not.
     */
    public boolean isRemoteVerificationEnabled();

    /**
     * Check if the trace flag is on.
     * @return     True if trace flag is on, false if not.
     */
    public boolean isTraceRawCommand();

    /**
     * Gets the names of all file entries that are related to real file data.
     * This method filters out directory entries and pure symbolic link entries.
     * @param      files  An array of FTPFile objects.
     * @return     An array of String objects.
     * @exception  IOException   If some error occurs.
     */
    public String[] listFileNames(FTPFile[] files)
        throws IOException;
    /**
     * Using the default FileListParser , obtain a list of file information 
     * for the current working directory.
     * @return     An array of FTPFile objects.
     * @exception  IOException   If some error occurs.
     */
    public FTPFile[] listFiles() throws IOException;

    /**
     * Filters files by type. The file types can be any of the following: FTPFile.FILE_TYPE, FTPFile.DIRECTORY_TYPE,
     * FTPFile.SYMBOLIC_LINK_TYPE or FTPFile.UNKNOWN_TYPE.
     * @param         files  An array of FTPFile objects.
     * @param      fileType  The file type.
     * @return     An array of FTPFile objects.
     * @exception  IOException   If some error occurs.
     */
    public FTPFile[] listFiles(FTPFile[] files, int fileType) throws IOException;
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
        throws IOException;
        
    /**
     * Using the default FileListParser, obtains a list of file information 
     * for the given working directory.
     * @param      pathName  The directory name.
     * @return     An array of FTPFile objects.
     * @exception  IOException   If some error occurs.
     */
    public FTPFile[] listFiles(String pathName) throws IOException;

    /**
     * Using default FileListParser, obtains a list of file information 
     * for the given working directory and regular expression.
     * @param      pathName  The directory name.
     * @param        regExp  The file name regular expression.
     * @return     An array of FTPFile objects.
     * @exception  IOException   If some error occurs.
     */
    public FTPFile[] listFiles(String pathName, String regExp) throws IOException;

    /**
     * Retrieves the system help information from the remote FTP server and returns the full string.
     * @return     The system help information.
     * @exception  IOException   If some error occurs.
     */
    public String listHelp()
        throws IOException;

    /**
     * Retrieves the help information for a given command from the remote FTP server and returns the full string.
     * @param      command  The command given.
     * @return     The system help information.
     * @exception  IOException   If some error occurs.
     */
    public String listHelp(String command)
        throws IOException;

    /**
     * Logs into the remote FTP server using the provided user name and password.
     * @param          user  The user name.
     * @param      password  The password.
     * @return     true if the action is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean login(String user, String password)
        throws IOException;

    /**
     * Logs out of the remote FTP server by sending the QUIT command.
     * @return     true if the action is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean logout()
        throws IOException;

    /**
     * Creates a new directory if the directory does not exist.
     * For directory dir1/dir2, if the parent directory dir1 does not exist, the method fails.
     * There is another method, mkdirs(), that creates all necessary parent directories.
     * @param      dir  The directory name to be created.
     * @return     true if the action is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean mkdir(String dir)
        throws IOException;
    /**
     * Creates a new directory if the directory does not exist.
     * This method also creates all necessary parent directories.
     * @param      dir  The directory name to be created.
     * @return     true if the action is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean mkdirs(String dir)
        throws IOException;
    /**
     * Returns the path name of the current working directory.
     * @return     The current working directory path name.
     * @exception  IOException   If some error occurs.
     */
    public String pwd()
        throws IOException;

    /**
     * Renames a remote file.
     * @param      remoteFileNameFrom  The old remote file name.
     * @param        remoteFileNameTo  The new remote file name.
     * @return     true if the action is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean rename(
        String remoteFileNameFrom, 
        String remoteFileNameTo)
        throws IOException;

    /**
     * Renames a remote file.
     * @param           dirNameFrom  The old remote directory name.
     * @param      baseFileNameFrom  The old remote base file name.
     * @param             dirNameTo  The new remote directory name.
     * @param        baseFileNameTo  The new remote base file name.
     * @return     true if the action is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean rename(
        String dirNameFrom, 
        String baseFileNameFrom, 
        String dirNameTo, 
        String baseFileNameTo)
        throws IOException;
        
    /**
     * Retrieves a named file from the remote FTP server and writes it to the given local file.
     * The FileOutputSteam related to the localFileName is closed in this method.
     * @param      remoteFileName  The remote file name.
     * @param       localFileName  The local file name.
     * @return     true if the action is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean retrieveFile(String remoteFileName, String localFileName)
        throws IOException;

    /**
     * Retrieves a named file from the remote FTP server and writes it to the given local file.
     * The FileOutputSteam related to the localFileName is closed in this method.
     * @param           remoteDirName  The remote directory name.
     * @param      remoteBaseFileName  The remote base file name.
     * @param           localFileName  The local file name.
     * @return     true if the action is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean retrieveFile(
        String remoteDirName, 
        String remoteBaseFileName, 
        String localFileName)
        throws IOException;

    /**
     * Retrieves a named file from the remote FTP server and writes it to the given OutputStream.
     * The OutputSteam is NOT closed in this method.
     * @param      remoteFileName  The remote file name.
     * @param               local  The local output stream.
     * @return     true if the action is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean retrieveFile(String remoteFileName, OutputStream local)
        throws IOException;

    /**
     * Retrieves a named file from the remote FTP server and writes it to the given OutputStream.
     * The OutputSteam is NOT closed in this method.
     * @param           remoteDirName  The remote directory name.
     * @param      remoteBaseFileName  The remote base file name.
     * @param                   local  The local output stream.
     * @return     true if the action is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean retrieveFile(String remoteDirName, String remoteBaseFileName, OutputStream local)
        throws IOException;

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
        throws IOException;

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
        throws IOException;

    /**
     * Removes a directory on the remote FTP server, if it is empty.
     * <p>
     * @param dirName  The name of the remote directory to remove.
     * @return     true if the action is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean rmdir(String dirName) throws IOException;

    /**
     * Sends an FTP command without a parameter to the server. The method then 
     * waits for a reply and returns the numerical response code.
     * @param      command  The FTP command to be executed.
     * @return     The reply code from the FTP command.
     * @exception  IOException   If some error occurs.
     */
    public int sendCommand(String command)
        throws IOException;

    /**
     * Sends an FTP command to the server. The method then 
     * waits for a reply and returns the numerical response code.
     * @param      command  The FTP command to be executed.
     * @param         parm  The parameter of the FTP command.
     * @return     The reply code from FTP command.
     * @exception  IOException   If some error occurs.
     */
    public int sendCommand(String command, String parm) throws IOException;

    /**
     * send a site command to FTP server
     * @param command - site command
     * @return - true, success, false, failure;
     * @throws IOException
     */
    public boolean sendSiteCommand(String command) throws IOException;

    /**
     * Sets the timeout, in milliseconds, to use when reading from the 
     * data connection. This timeout is set immediately after 
     * the data connection is opened.
     * NOTE: This connection is not the command connection.
     * <p>
     * @param  timeout The timeout, in milliseconds, that is used when
     *         opening a data connection socket.
     */
    // Sets the SoTimeout value for the data connection socket instead of command connection socket.
    // This method can be called before or after connect(). It will affect ftp data transfer
    // commands like LIST, RETR, STOR, APPE.
    // For command connection socket, use method setDefaultTimeout() or setSoTimeout().
    public void setDataSocketTimeout(int timeout);

    /**
     * Allows you to enable or disable verification that the remote FTP host taking part 
     * in a given data connection is the same as the host to which the control 
     * connection is attached. The default is for verification to be 
     * enabled. You can set this value at any time, whether the 
     * FTP client is currently connected or not.
     * <p>
     * @param enable true enables verification, and false disables it.
     */
    public void setRemoteVerificationEnabled(boolean enable);

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

    public void setSocketFactory(SocketFactory factory);

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
    public void setSoLinger(boolean on, int val) throws SocketException;

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
    public void setSoTimeout(int timeout) throws SocketException;

    /**
     * Allows you to enable or disable the Nagle's algorithm (TCP_NODELAY) on the 
     * currently opened command socket.
     * <p>
     * @param on  true to enable Nagle's algorithm, and false to disable.
     * @exception SocketException If the operation fails.
     */
    // Set TcpNoDelay for the command connection socket instead of data connection socket.
    // We didn't provide the TcpNoDelay setter for data connection socket.
    public void setTcpNoDelay(boolean on) throws SocketException;

    /**
     * Allows you to turn the trace flag on or off.
     * @param      newTraceRawCommand  true or false.
     */
    public void setTraceRawCommand(boolean newTraceRawCommand);

    /**
     * Stores a file on the remote FTP server using the given name and takes input from the given local file.
     * The FileInputSteam related to the localFileName is closed in this method.
     * @param      remoteFileName  The remote file name.
     * @param       localFileName  The local file name.
     * @return     true if the operation is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean storeFile(String remoteFileName, String localFileName)
        throws IOException;

    /**
     * Stores a file on the remote FTP server using the given name and takes input from the given local file.
     * The FileInputSteam related to the localFileName is closed in this method.
     * @param           remoteDirName  The remote directory name.
     * @param      remoteBaseFileName  The remote file name.
     * @param           localFileName  The local file name.
     * @return     True if successfully completed, false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean storeFile(
        String remoteDirName, 
        String remoteBaseFileName, 
        String localFileName)
        throws IOException;

    /**
     * Stores a file on the server using the given name and takes input from the given InputStream. 
     * The InputSteam is NOT closed in this method.
     * @param      remoteFileName  The remote file name.
     * @param               local  The local input stream.
     * @return     true if the action is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean storeFile(String remoteFileName, InputStream local)
        throws IOException;

    /**
     * Stores a file on the server using the given name and takes input from the given InputStream. 
     * The InputSteam is NOT closed in this method.
     * @param           remoteDirName  The remote directory name.
     * @param      remoteBaseFileName  The remote file name.
     * @param                   local  The local input stream.
     * @return     true if the action is successfully completed, but false if not.
     * @exception  IOException   If some error occurs.
     */
    public boolean storeFile(String remoteDirName, String remoteBaseFileName, InputStream local)
        throws IOException;

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
        throws IOException;

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
     * @param           dirName  The remote directory name.
     * @param      baseFileName  The remote base file name.
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
    public OutputStream storeFileStream(String dirName, String baseFileName)
        throws IOException;

    /**
     * Sets the current data connection mode to ACTIVE_LOCAL_DATA_CONNECTION_MODE.
     */
    public void useActive();

    /**
     * Sest the current data connection mode to PASSIVE_LOCAL_DATA_CONNECTION_MODE.
     */
    public void usePassive();

    /**
     * Gets the first matching remote file name based on the remote directory 
     * regular expression and remote file regular expression.
     * @param          dir  The directory name.
     * @param   isDirRegex  Tells whether the directory name is a regular expression.
     * @param         file  The file name.
     * @param  isFileRegex  Tells whether the file name is a regular expression.
     * @return      The full file name.
     * @exception   IOException  If some error occurs.
     */
    public String getFirstFileName(String dir, boolean isDirRegex, String file, boolean isFileRegex) throws IOException;

    /**
     * Lists the remote full file names based on the remote directory regular expression and 
     * remote file regular expression.
     * @param          dir  The directory name.
     * @param   isDirRegex  Tells whether the directory name is a regular expression.
     * @param         file  The file name.
     * @param  isFileRegex  Tells whether the file name is a regular expression.
     * @return      The full file names.
     * @exception   IOException  If some error occurs.
     */
    public String[] listFileNames(String dir, boolean isDirRegex, String file, boolean isFileRegex) throws IOException;

    /**
     * initialize and update the FTP heuristics using the specified user defined 
     * heuristics information; 
     * @param userDirListingStyle - the name of the user defined directory listing style
     * must be the name of the an entry in the user defined heuristics configuration file - as 
     * specified by <code>userHeuristicsCfgFile</code>
     * @param userHeuristicsCfgFile - a path pointing to the user defined heuristics configuration file;
     * @throws Exception
     */
    public void setUserDefinedHeuristicsInfo(String userDirListingStyle, String userHeuristicsCfgFile) throws Exception;

    /**
     * Initializes and update the FTP heuristics using the specified directory listing style.
     * @param      dirListingStyle  The directory listing style.
     * @exception  Exception   If some error occurs.
     */
    public void setDirListingStyle(String dirListingStyle) throws Exception;

    public void setWarningOff(boolean b);
    public boolean getWarningOff();
    public boolean isUseRegexAsMatcher();

    public void setUseRegexAsMatcher(boolean useRegexAsMatcher);

    public void clearCommandChannel() throws IOException;
}
