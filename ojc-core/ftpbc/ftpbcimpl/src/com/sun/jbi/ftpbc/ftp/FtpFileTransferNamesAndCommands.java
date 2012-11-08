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
 * @(#)FtpFileTransferNamesAndCommands.java 
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
import com.sun.jbi.ftpbc.ftp.namepattern.NamePattern;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;

/**
 * This class is used to access the resolved transfer names and
 * transfer commands for Batch FTP operations
 * @author Harry Liu
 * @version cvs revision:    Last Modified: 
 */

public abstract class FtpFileTransferNamesAndCommands implements TransferNamesAndCommands {
    private static final Messages mMessages =
            Messages.getMessages(FtpFileTransferNamesAndCommands.class);
    private static final Logger mLogger =
            Messages.getLogger(FtpFileTransferNamesAndCommands.class);
    
    transient protected String sequencePersistDir = null;
    
    transient protected FtpInterface intf = null;
    protected boolean cleanupOnPutFailure = false;
    protected boolean append = false;
    protected String targetDirectoryName = null;
    protected String targetFileName = null;
    protected String preTransferCommand = null;
    protected String preDirectoryName = null;
    protected String preFileName = null;
    protected String postTransferCommand = null;
    protected String postDirectoryName = null;
    protected String postFileName = null;
    


    /**
     * Constructs a new FtpFileTransferNamesAndCommands object.
     * 
     * @param etd  BatchFtp objeFtpInterface
     */
    public FtpFileTransferNamesAndCommands(FtpInterface intf) {
        this.intf = intf;
        this.append = this.intf.getConfiguration().getAppend(); // got only one time
    }
    
    /**
     * copy constructor since we don't have setters for some of the
     * attributes that needs to be changed dynamically;
     * @param
     */
    public FtpFileTransferNamesAndCommands(FtpFileTransferNamesAndCommands original, FtpFileConfiguration config) {
        sequencePersistDir = original.sequencePersistDir;
        intf = original.intf;
        cleanupOnPutFailure = original.cleanupOnPutFailure;
        append = config.getAppend();
        targetDirectoryName = original.targetDirectoryName;
        targetFileName = original.targetFileName;
        preTransferCommand = original.preTransferCommand;
        preDirectoryName = original.preDirectoryName;
        preFileName = original.preFileName;
        postTransferCommand = original.postTransferCommand;
        postDirectoryName = original.postDirectoryName;
        postFileName = original.postFileName;
    }
    
    /**
     * Gets the resolved target directory name.
     * This name is resolved from 'Target Directory Name' pattern.
     * It represents the original working directory name right
     * before the 'Pre Transfer Command'.
     * @return    The resolved Target Directory Name.
     * @exception    FtpFileException    If some error occurs.
     */
    public String getTargetDirectoryName() throws FtpFileException {
        if (this.targetDirectoryName == null ) {
            this.resolveTargetLocation();
        }
        return this.targetDirectoryName;
    }
    /**
     * Gets the resolved target file name.
     * This name is resolved from 'Target File Name' pattern.
     * It represents the original working file name right
     * before the 'Pre Transfer Command'.
     * @return    The resolved Target File Name.
     * @exception    FtpFileException    If some error occurs.
     */
    public String getTargetFileName() throws FtpFileException {
        if ( this.targetFileName == null ) {
            this.resolveTargetLocation();
        }
        return this.targetFileName;
    }
    /**
     * Gets the Pre Transfer Command.
     * @return    The Pre Transfer Command.
     * @exception    FtpFileException    If some error occurs.
     */
    public String getPreTransferCommand() throws FtpFileException {
        if ( this.preTransferCommand == null ) {
            this.resolvePreTransfer();
        }
        return this.preTransferCommand;
    }
    /**
     * Gets the resolved pre directory name.
     * This name is resolved from 'Pre Directory Name' pattern.
     * It represents the working directory name right
     * after the 'Pre Transfer Command'.
     * @return    The resolved Pre Directory Name.
     * @exception    FtpFileException    If some error occurs.
     */
    public String getPreDirectoryName() throws FtpFileException {
        if (this.preDirectoryName == null) {
            this.resolvePreTransfer();
        }
        return this.preDirectoryName;
    }
    /**
     * Gets the resolved pre file name.
     * This name is resolved from 'Pre File Name' pattern.
     * It represents the working file name right
     * after the 'Pre Transfer Command'.
     * @return    The resolved Pre File Name.
     * @exception    FtpFileException    If some error occurs.
     */
    public String getPreFileName() throws FtpFileException {
        if (this.preFileName == null) {
            this.resolvePreTransfer();
        }
        return this.preFileName;
    }
    /**
     * Gets Post Transfer Command.
     * @return    The Post Transfer Command.
     * @exception    FtpFileException    If some error occurs.
     */
    public String getPostTransferCommand() throws FtpFileException {
        if (this.postTransferCommand == null) {
            this.resolvePostTransfer();
        }
        return this.postTransferCommand;
    }
    /**
     * Gets the resolved post directory name.
     * This name is resolved from 'Post Directory Name' pattern.
     * It represents the working directory name right
     * after the 'Post Transfer Command'.
     * @return    The resolved Post Directory Name.
     * @exception    FtpFileException    If some error occurs.
     */
    public String getPostDirectoryName() throws FtpFileException {
        if (this.postDirectoryName == null) {
            this.resolvePostTransfer();
        }
        return this.postDirectoryName;
    }
    /**
     * Gets the resolved post file name.
     * This is resolved from 'Post File Name' pattern.
     * It represents the working file name right
     * after the 'Post Transfer Command'.
     * @return    The resolved Post File Name.
     * @exception    FtpFileException    If some error occurs.
     */
    public String getPostFileName() throws FtpFileException {
        if (this.postFileName == null) {
            this.resolvePostTransfer();
        }
        return this.postFileName;
    }
    /**
     * Checks on whether the outbound transfer is "Append" mode.
     * @return    true or false.
     */
    public boolean getAppend() {
        return this.append;
    }
    
    /**
     * Checks on whether the cleanup is needed on transfer put failure.
     * @return    true or false.
     */
    public boolean getCleanupOnPutFailure() {
        return this.cleanupOnPutFailure;
    }
    
    /**
     * Sets on whether the cleanup is needed on transfer put failure.
     * @param   cleanup    true or false.
     */
    public void setCleanupOnPutFailure(boolean cleanup) {
        this.cleanupOnPutFailure = cleanup;
    }
    
    /**
     * Resolves Post Transfer Names and Command.
     * @exception    FtpFileException    If some error occurs.
     */
    public void resolvePostTransfer() throws FtpFileException {
        if ( this.postDirectoryName != null
                && this.postFileName != null ) {
            return;
        }
        
        this.postTransferCommand = this.intf.getConfiguration().getPostTransferCommand(); //?in case
        
        this.postDirectoryName = "";
        this.postFileName = "";
        
        if (this.intf.getConfiguration().isPostTransferCommandNone() ||
                this.intf.getConfiguration().isPostTransferCommandDelete()) {
            return;
        }
        
        if (this.getTargetDirectoryName().length() == 0 ||
                this.getTargetFileName().length() == 0) {
            return;
        }
        
        if (this.intf.getConfiguration().isPreTransferCommandRename()) {
            if (this.getPreDirectoryName().length() == 0 ||
                    this.getPreFileName().length() == 0) {
                return;
            }
        }
        
        // "Rename"
        NamePattern np = null;
        if (!this.intf.getConfiguration().isPostDirectoryNameIsPattern()) {
            this.postDirectoryName = this.intf.getConfiguration().getPostDirectoryName();
        } else {
            np = new NamePattern(this.intf.getConfiguration().getPostDirectoryName(),
                    this.intf.getConfiguration().getCurrentEndpoint(),
                    this.intf.getConfiguration().getCurrentOperationName());
            np.setFileNameFromEgate(this.getTargetDirectoryName());
            
            //setSequenceNumber(np);
            
            try {
                this.postDirectoryName = np.expand();
            } catch (Exception e) {
                String msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION",
                		new Object[] {
                		"resolvePostTransfer()", e
                });
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg, e);
                }
                throw new FtpFileException(msg, e);
            }
            //updateState(np);
        }
        
        if (!this.intf.getConfiguration().isPostFileNameIsPattern()) {
            this.postFileName = this.intf.getConfiguration().getPostFileName();
        } else {
            np = new NamePattern(this.intf.getConfiguration().getPostFileName(),
                    this.intf.getConfiguration().getCurrentEndpoint(),
                    this.intf.getConfiguration().getCurrentOperationName());
            np.setFileNameFromEgate(this.getTargetFileName());
            //setSequenceNumber(np);
            try {
                this.postFileName = np.expand();
            } catch (Exception e) {
                String msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION",
                		new Object[] {
                		"resolvePostTransfer()", e
                });
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg, e);
                }
                throw new FtpFileException(msg, e);
            }
            //updateState(np);
        }
    }
    
    /**
     * Resolves Pre Transfer Names and Command.
     * @exception    FtpFileException    If some error occurs.
     */
    public void resolvePreTransfer() throws FtpFileException {
        if ( this.preDirectoryName != null &&
                this.preFileName != null ) {
            return;
        }
        
        this.preTransferCommand = this.intf.getConfiguration().getPreTransferCommand(); //?in case
        
        this.preDirectoryName = "";
        this.preFileName = "";
        if (this.intf.getConfiguration().isPreTransferCommandNone()) {
            return;
        }
        
        if (this.getTargetDirectoryName().length() == 0 ||
                this.getTargetFileName().length() == 0) {
            return;
        }
        
        // "Rename", "Copy"
        NamePattern np = null;
        if (!this.intf.getConfiguration().isPreDirectoryNameIsPattern()) {
            this.preDirectoryName = this.intf.getConfiguration().getPreDirectoryName();
        } else {
            np = new NamePattern(this.intf.getConfiguration().getPreDirectoryName(),
                    this.intf.getConfiguration().getCurrentEndpoint(),
                    this.intf.getConfiguration().getCurrentOperationName());
            np.setFileNameFromEgate(this.getTargetDirectoryName());
            try {
                this.preDirectoryName = np.expand();
            } catch (Exception e) {
                String msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION",
                		new Object[] {
                		"resolvePreTransfer()", e
                });
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg, e);
                }
                throw new FtpFileException(msg, e);
            }
        }
        
        if (!this.intf.getConfiguration().isPreFileNameIsPattern()) {
            this.preFileName = this.intf.getConfiguration().getPreFileName();
        } else {
            np = new NamePattern(this.intf.getConfiguration().getPreFileName(),
                    this.intf.getConfiguration().getCurrentEndpoint(),
                    this.intf.getConfiguration().getCurrentOperationName());
            np.setFileNameFromEgate(this.getTargetFileName());
            
            try {
                this.preFileName = np.expand();
            } catch (Exception e) {
                String msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION",
                		new Object[] {
                		"resolvePreTransfer()", e
                });
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg, e);
                }
                throw new FtpFileException(msg, e);
            }
        }
    }
    
    /**
     * setters added for recovery
     * where the dirs and files are known resolved values
     * recorded in previous run
     * @param dirName
     */
    public void setTargetDirectoryName(String dirName) {
        targetDirectoryName = dirName;
    }

    public void setTargetFileName(String fileName) {
        targetFileName = fileName;
    }

    public void setPreDirectoryName(String dirName) {
        preDirectoryName = dirName;
    }

    public void setPreFileName(String fileName) {
        preFileName = fileName;
    }

    public void setPostDirectoryName(String dirName) {
        postDirectoryName = dirName;
    }

    public void setPostFileName(String fileName) {
        postFileName = fileName;
    }
    
    /**
     * Resolves Target Location Names.
     * @exception    FtpFileException    If some error occurs.
     */
    abstract public void resolveTargetLocation() throws FtpFileException;
    
    public String toString() {
        StringBuffer sb = new StringBuffer();
        sb.append("PreTransferCmd=".concat(preTransferCommand != null ? preTransferCommand : ""));
        sb.append("PreDirName=".concat(preDirectoryName != null ? preDirectoryName : ""));
        sb.append("PreFileName=".concat(preFileName != null ? preFileName : ""));
        sb.append("TargetDirName=".concat(targetDirectoryName != null ? targetDirectoryName : ""));
        sb.append("TargetDirName=".concat(targetFileName != null ? targetFileName : ""));
        sb.append("PostTransferCmd=".concat(postTransferCommand != null ? postTransferCommand : ""));
        sb.append("PostDirName=".concat(postDirectoryName != null ? postDirectoryName : ""));
        sb.append("PostFileName=".concat(postFileName != null ? postFileName : ""));
        return sb.toString();
    }
}
