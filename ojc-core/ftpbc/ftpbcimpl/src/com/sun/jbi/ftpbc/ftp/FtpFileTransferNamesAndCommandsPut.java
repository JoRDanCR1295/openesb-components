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
 * @(#)FtpFileTransferNamesAndCommandsPut.java 
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

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.ftpbc.ftp.exception.FtpFileException;
import com.sun.jbi.ftpbc.ftp.namepattern.NamePattern;
import com.sun.jbi.internationalization.Messages;

/**
 * This class is used to access the resolved transfer names and
 * transfer commands for Batch Ftp "get" operation.
 * @author Harry Liu
 * @version cvs revision:    Last Modified: 
 */

public class FtpFileTransferNamesAndCommandsPut extends FtpFileTransferNamesAndCommands {
    private static final Messages mMessages =
            Messages.getMessages(FtpFileTransferNamesAndCommandsPut.class);
    private static final Logger mLogger =
            Messages.getLogger(FtpFileTransferNamesAndCommandsPut.class);
    
    protected String stageDirectoryName = null;
    protected String stageFileName = null;

    /**
     * Constructs a new FtpFileTransferNamesAndCommandsPut object.
     * 
     * @param etd  BatchFtp objeFtpInterface
     */
    public FtpFileTransferNamesAndCommandsPut(FtpInterface etd) {
        super(etd);
    }
    
    public FtpFileTransferNamesAndCommandsPut(FtpFileTransferNamesAndCommands original, FtpFileConfiguration config) {
        super(original, config);
    }
    
    /**
     * Gets the resolved stage directory name.
     * This name is resolved from 'Stage Directory Name' pattern.
     * @return    The resolved Stage Directory Name.
     * @exception    FtpFileException    If some error occurs.
     */
    public String getStageDirectoryName() throws FtpFileException {
        if (this.stageDirectoryName == null ) {
            this.resolveStageLocation();
        }
        return this.stageDirectoryName;
    }
    
    /**
     * Gets the resolved stage file name.
     * This name is resolved from 'Stage File Name' pattern.
     * @return    The resolved Stage File Name.
     * @exception    FtpFileException    If some error occurs.
     */
    public String getStageFileName() throws FtpFileException {
        if ( this.stageFileName == null ) {
            this.resolveStageLocation();
        }
        return this.stageFileName;
    }

    /**
     * Resolves Stage Location Names.
     * @exception    FtpFileException    If some error occurs.
     */
    public void resolveStageLocation() throws FtpFileException {
        if (!this.intf.getConfiguration().isStageEnabled()) {
            return;
        }
        
        if ( this.stageDirectoryName != null
                && this.stageFileName != null ) {
            return;
        }
        
        NamePattern np = null;

        np = new NamePattern(this.intf.getConfiguration().getStageDirectoryName(),
                this.intf.getConfiguration().getCurrentEndpoint(),
                this.intf.getConfiguration().getCurrentOperationName());
        np.setFileNameFromEgate(this.getTargetDirectoryName());

        try {
            this.stageDirectoryName = np.expand();
        } catch (Exception e) {
            String msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION",
                            new Object[] {
                            "resolveStageLocation()", e
            });
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg, e);
            }
            throw new FtpFileException(msg, e);
        }
        
        np = new NamePattern(this.intf.getConfiguration().getStageFileName(),
                this.intf.getConfiguration().getCurrentEndpoint(),
                this.intf.getConfiguration().getCurrentOperationName());
        np.setFileNameFromEgate(this.getTargetFileName());
        try {
            this.stageFileName = np.expand();
        } catch (Exception e) {
            String msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION",
                            new Object[] {
                            "resolveStageLocation()", e
            });
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg, e);
            }
            throw new FtpFileException(msg, e);
        }
        
    }
    
    /**
     * Resolves Target Location Names.
     * @exception    FtpFileException    If some error occurs.
     */
    public void resolveTargetLocation() throws FtpFileException {
        if ( this.targetDirectoryName != null
                && this.targetFileName != null ) {
            return;
        }
        
        NamePattern np = null;
        if (!this.intf.getConfiguration().isTargetDirectoryNameIsPattern()) {
            this.targetDirectoryName = this.intf.getConfiguration().getTargetDirectoryName();
        } else {
            np = new NamePattern(this.intf.getConfiguration().getTargetDirectoryName(),
                    this.intf.getConfiguration().getCurrentEndpoint(),
                    this.intf.getConfiguration().getCurrentOperationName());
            try {
                this.targetDirectoryName = np.expand();
            } catch (Exception e) {
                String msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION",
                		new Object[] {
                		"resolveTargetLocation()", e
                });
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg, e);
                }
                throw new FtpFileException(msg, e);
            }
        }
        
        if (!this.intf.getConfiguration().isTargetFileNameIsPattern()) {
            this.targetFileName = this.intf.getConfiguration().getTargetFileName();
        } else {
            np = new NamePattern(this.intf.getConfiguration().getTargetFileName(),
                    this.intf.getConfiguration().getCurrentEndpoint(),
                    this.intf.getConfiguration().getCurrentOperationName());
            try {
                this.targetFileName = np.expand();
            } catch (Exception e) {
                String msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION",
                		new Object[] {
                		"resolveTargetLocation()", e
                });
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg, e);
                }
                throw new FtpFileException(msg, e);
            }
        }
        
    }
}
