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
 * @(#)FtpFileClient.java 
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

import com.sun.jbi.ftpbc.ftp.TransferNamesAndCommands;
import com.sun.jbi.ftpbc.ftp.exception.FtpFileException;
import java.util.Properties;

import com.sun.jbi.ftpbc.ftp.io.stream.InputStreamAdapter;
import com.sun.jbi.ftpbc.ftp.io.stream.OutputStreamAdapter;
import java.io.IOException;
import java.io.InputStream;

/**
 * This interface represents a ftp client providing ftp access to a FTP host
 * @author jfu (jim.fu@sun.com)
 * @version cvs revision:    Last Modified: 
 */

public interface FtpFileClient {
    public static final String TC_GET = "get";
    public static final String TC_PUT = "put";
    
//    public static final String ENCODING_DEFAULT = "iso-8859-1";
//    public static final String ENCODING_SJIS = "pck";
//    public static final String ENCODING_EUC_JP = "euc-jp";
//    public static final String ENCODING_JIS = "iso-2022-jp";
    
    /**
     * Initializes the ftp client.
     * 
     * @param intf the interface object
     * @exception FtpFileException  If some error occurs.
     */
    public void initialize(FtpInterface intf) throws FtpFileException;
    
    /**
     * Does the FTP log-out and disconnects.
     */
    public void close();
    
    /**
     * Initializes the client configuration properties from user-specified properties.
     * @param  props   The specified properties.
     * @exception   FtpFileException   If some error occurs.
     */
    // used for connector
    public void initialConfigValues(Properties props) throws FtpFileException;
    
    /**
     * Verifies that the client is connected to the external system.
     *
     * @return  <code>true</code> if the connection is still open;
     *          <code>false</code> otherwise.
     */
    // for connector
    public boolean isOpen();
    
    /**
     * Performs the FTP connection, log-in and switch modes, and so on.
     * @exception   FtpFileException  If some error occurs.
     */
    // for connector
    public void open() throws FtpFileException;
    
    /**
     * Performs the FTP connection, log-in and switch modes, and so on.
     * @param encoding The encoding for the server.
     * @exception   FtpFileException  If some error occurs.
     */
    // for connector
    public void open(String encoding) throws FtpFileException;
    
    /**
     * Resets data content and flags (internal states) of a ftp client.
     * @exception   FtpFileException   If some error occurs.
     */
    public void reset() throws FtpFileException;
    
    /**
     * restore the configuration to the values it is initilialized.
     * @exception   FtpFileException   If some error occurs.
     */
    public void restoreConfigValues() throws FtpFileException;
    
    /**
     * getthe payload of a put/get operation.
     * <p>The data payload is a blob (byte array) used to store the raw content of a file.
     * @return    The data payload.
     */
    public byte[] getPayload();
    
    /**
     * Sets the data payload.
     * <p>The data payload is a blob (byte array) used to store the raw content of a file.
     * @param       newPayload  The data payload.
     */
    public void setPayload(byte[] newPayload);
    
    /**
     * Performs the FTP connection, log-in and switch modes, and so on.
     * @exception   FtpFileException  If some error occurs.
     */
    public void connect() throws FtpFileException;
    
    /**
     * Performs the FTP connection, log-in and switch modes, and so on.
     * @param encoding The encoding for the server.
     * @exception   FtpFileException  If some error occurs.
     */
    public void connect(String encoding) throws FtpFileException;
    
    /**
     * Does the FTP log-out and disconnects.
     */
    public void disconnect();
    
    /**
     * Verifies that the e*Way Connection to the external system is still available.
     *
     * @return  <code>true</code>  If the connection is still open and available;
     *          <code>false</code> if otherwise.
     */
    public boolean isConnected();
    
    /**
     * accept an input stream from the caller.
     * @param     isa   InputStreamAdapter object.
     */
    public void setInputStreamAdapter(InputStreamAdapter isa);
    
    /**
     * accept an output stream from the caller.
     * @param     osa   OutputStreamAdapter object.
     */
    public void setOutputStreamAdapter(OutputStreamAdapter osa);
    
    /**
     * Resolves names from patterns for the FTP get operation.
     * @return    An object of the TransferNamesAndCommands class.
     * @exception  FtpFileException    If some error occurs.
     */
    public TransferNamesAndCommands getResolvedNamesForGet() throws FtpFileException;
    
    /**
     * Resolves names from patterns for the FTP put operation.
     * @return    An object of the TransferNamesAndCommands class.
     * @exception  FtpFileException    If some error occurs.
     */
    public TransferNamesAndCommands getResolvedNamesForPut() throws FtpFileException;
    
    /**
     * Stores a remote FTP file, it takes the configuration parameters when doing
     * ftp operations.
     * @exception   FtpFileException  If some error occurs.
     */
    public void put() throws FtpFileException;
    
    /**
     * Cleans up any failures during Post Transfer Command for "get".
     * @param    tncg   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void cleanupPostTransferGet(TransferNamesAndCommands tncg) throws Exception;
    
    /**
     * Cleans up any failures during Post Transfer Command for "put".
     * @param    tncp   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void cleanupPostTransferPut(TransferNamesAndCommands tncp) throws Exception;
    
    /**
     * Cleans up any failures during Pre Transfer Command for "get".
     * @param    tncg   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void cleanupPreTransferGet(TransferNamesAndCommands tncg) throws Exception;
    
    /**
     * Cleans up any failures during Pre Transfer Command for "put".
     * @param    tncp   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void cleanupPreTransferPut(TransferNamesAndCommands tncp) throws Exception;
    
    /**
     * Cleans up any failures during pre/post ftp raw commands.
     * @param    tncr   An instance of TransferNamesAndCommands class.
     * @exception   FtpFileException  If some error occurs.
     */
    public void cleanupRawCommands(TransferNamesAndCommands tncr) throws FtpFileException;
    
    /**
     * Cleans up any failures during ftp Transfer "get".
     * @param    tncg   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void cleanupTransferGet(TransferNamesAndCommands tncg) throws Exception;
    
    /**
     * Cleans up any failures during ftp Transfer "put".
     * @param    tncp   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void cleanupTransferPut(TransferNamesAndCommands tncp) throws Exception;
    
    /**
     * Performs Post Transfer Command for FTP get operation.
     * @param    tncg   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void doPostTransferGet(TransferNamesAndCommands tncg) throws Exception;
    
    /**
     * Performs Post Transfer Command for FTP put operation.
     * @param    tncp   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void doPostTransferPut(TransferNamesAndCommands tncp) throws Exception;
    
    /**
     * Performs Pre Transfer Command for FTP get operation.
     * @param    tncg   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void doPreTransferGet(TransferNamesAndCommands tncg) throws Exception;
    
    /**
     * Performs Pre Transfer Command for FTP put operation.
     * @param    tncp   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void doPreTransferPut(TransferNamesAndCommands tncp) throws Exception;
    
    /**
     * Performs FTP pre and post transfer raw commands, for example:
     * SITE RECFM=FB;SITE LRECL=50;SITE BLOCKSIZE=32750;SITE TRACKS;SITE PRI=5;SITE SEC=5
     * NOTE: The commands are separated by a semicolon (;), and
     * only FTP raw commands are expected.
     * @param       commands  The FTP raw command set.
     * @param       commands  The raw command set.
     *
     * @exception   FtpFileException  If some error occurs.
     */
    public void doRawCommands(String commands) throws FtpFileException;
    
    /**
     * Performs the real FTP get transfer.
     * @param    tncg   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void doTransferGet(TransferNamesAndCommands tncg) throws Exception;
    
    /**
     * Performs the real FTP put transfer.
     * @param    tncp   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void doTransferPut(TransferNamesAndCommands tncp) throws Exception;
    
    /**
     * Undoes Post Transfer for FTP get operation.
     * @param    tncg   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void undoPostTransferGet(TransferNamesAndCommands tncg) throws Exception;
    
    /**
     * Undoes Post Transfer for FTP put operation.
     * @param    tncp   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void undoPostTransferPut(TransferNamesAndCommands tncp) throws Exception;
    
    /**
     * Undoes Pre Transfer for FTP get operation.
     * @param    tncg  An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void undoPreTransferGet(TransferNamesAndCommands tncg) throws Exception;
    
    /**
     * Undoes Pre Transfer for FTP put operation.
     * @param    tncp   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void undoPreTransferPut(TransferNamesAndCommands tncp) throws Exception;
    
    /**
     * Undo pre/post ftp raw commands.
     * @param    tncr   An instance of TransferNamesAndCommands.
     * @exception   FtpFileException  If some error occurs.
     */
    public void undoRawCommands(TransferNamesAndCommands tncr) throws FtpFileException;
    
    /**
     * Undoes ftp get transfer.
     * @param    tncg   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void undoTransferGet(TransferNamesAndCommands tncg) throws Exception;
    
    /**
     * Undoes ftp put transfer.
     * @param    tncp   An instance of TransferNamesAndCommands class.
     * @exception  Exception  If some error occurs.
     */
    public void undoTransferPut(TransferNamesAndCommands tncp) throws Exception;
    
    /**
     * Retrieves a file from the remote FTP server.
     * This method retrieves the first matching entry under your directory and file name.
     * it takes the configuration parameters when doing the ftp operations.
     * Note: If no qualified file is available for retrieving, you will get
     *       the exception containing java.io.FileNotFoundException as a nested exception.
     * @exception   FtpFileException  If some error occurs.
     */
    public void get() throws FtpFileException;
    
    /**
     * Same as get but if there is no file found matching the name pattern
     * given in "Target Directory Name" and "Target File Name", just return
     * siliently;
     * Invoking of the method can result in :
     * a) throw Exception ( SoapFault if it is BPEL ), the caller is notified an error
     * b) getIfExists() (readIfExists() for BPEL) successful, payload is NULL, target file does not exists;
     * c) getIfExists() (readIfExists() for BPEL) successful, payload is NOT NULL, target file detected and content is read, if the content is 0 length, this lead to a byte[] of 0 length as payload;
     *
     * @exception   FtpFileException  If some error occurs.
     */
    public void getIfExists() throws FtpFileException;
    
    /**
     * Checks to determine whether a given file exists on an FTP server.
     * @param         dirName  The directory name.
     * @param        fileName  The file name.
     * @return     true or false.
     * @excetpion  FtpFileException   If some error occurs.
     */
    public boolean isFileExist(String dirName, String fileName) throws FtpFileException;

    public void setWarningOff(boolean b);
    public boolean getWarningOff();
    
    /**
     * prototyping persisted sequencing on FTP - this approach manipulates
     * remote file name as sequence persistence - instead of storing sequence info 
     * as file content which needs more file IO.
     * 
     * file is either <seq-ref>.<curr_seq> or <seq-ref>.<curr_seq>.<timestamp>
     * where <seq-ref> is a sequence name, e.g. s1, s2, etc. and <curr_seq> is the current sequence
     * value and <timestamp> is a timestamp tag added by the client currently fetching
     * the sequence (sequence is leased until the tagged time), competing client will
     * check this tag, if the current system time
     * > the tag, the lease expired, the competing client can re-tag (lease) the sequence.
     * after the current lease holder has done its sequence fetching, and if the lease is not
     * expired yet, it will rename the sequence file name to <seq-ref>.<curr_seq+1>.<current_time>,
     * if the rename is successful, return the sequence number to the caller, otherwise,
     * it could be that lease expired before renaming (ftp return 550 - file does not exist), 
     * in this case, this sequence fetching will be voided and the caller might want to 
     * try again after a retry interval, for other cases, it is an error, and caller will
     * be notified of the error.
     * 
     * @param leaseExpire - lease period in milli-seconds
     * @return the current value of the specified sequence <code>seq</code> or
     * -1 error condition
     * -2 still leased by others
     * -3 found sequence but failed to lease it (assuming leased by others)
     * -4 found sequence but failed to persist sequence (assuming leased by others before sequence update committed)
     * -5 does not find sequence (if ever occurs, assuming this is caused by slight chance of transient state during renaming)
     */
    public int getSequence(long leasePeriodMillis) throws Exception;
    public int getFile(long leasePeriodMillis, String[] content, String[] target) throws Exception;
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
}
