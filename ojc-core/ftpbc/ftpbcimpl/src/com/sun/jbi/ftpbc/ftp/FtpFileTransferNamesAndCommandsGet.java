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
 * @(#)FtpFileTransferNamesAndCommandsGet.java 
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

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;

/**
 * This class is used to access the resolved transfer names and
 * transfer commands for Batch Ftp "get" operation.
 * @author Harry Liu
 * @version cvs revision:    Last Modified: 
 */

public class FtpFileTransferNamesAndCommandsGet extends FtpFileTransferNamesAndCommands {
    private static final Messages mMessages =
            Messages.getMessages(FtpFileTransferNamesAndCommandsGet.class);
    private static final Logger mLogger =
            Messages.getLogger(FtpFileTransferNamesAndCommandsGet.class);
    /**
     * Constructs a new FtpFileTransferNamesAndCommandsGet object.
     * 
     * @param etd  BatchFtp objeFtpInterface
     */
    public FtpFileTransferNamesAndCommandsGet(FtpInterface etd) {
        super(etd);
    }
    
    /**
     * copy constructor since we don't have setters and we need to
     * change some of the attributes dynamically
     *
     */
    public FtpFileTransferNamesAndCommandsGet(FtpFileTransferNamesAndCommandsGet original, FtpFileConfiguration config) {
        super(original, config);
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
        
        String fullFile = null;
        try {
            fullFile = this.intf.getProvider().getFirstFileName(
                    this.intf.getConfiguration().getTargetDirectoryName(),
                    this.intf.getConfiguration().getTargetDirectoryNameIsPattern(),
                    this.intf.getConfiguration().getTargetFileName(),
                    this.intf.getConfiguration().getTargetFileNameIsPattern());
        } catch (Exception e) {
        	
            String msg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", 
            		new Object[] {"resolveTargetLocation()", e});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg, e);
            }
            throw new FtpFileException(msg, e);
        }
        
        if (fullFile == null || fullFile.equals("")) {
            this.targetFileName = "";
            this.targetDirectoryName = "";
            return;
        }
        
        if (!this.intf.getConfiguration().getDirectoryListingStyle().equals("VOSK (Hitachi)")) {
            // there is a boundary condition that can cause problem
            // when:
            // "targetDir" is left blank
            // "targetFile" is left blank
            // in this case, the target name resolution for GET
            // will return the first file in the dir listing
            // and the fullPath will be only that file name, e.g. "my_file.txt"
            // which is not handled by Heuristics.getBaseFileName(fullFilePath) and Heuristics.getDirectoryName(fullFilePath)
            // because both will take "my_file.txt" as their value, which is wrong and can further confuse certain operations
            // such as FtpFileClient.get(), FtpFileClient.getIfExists()...
            // the fix is to do some heuristics checking here:
            // if fullFile == resolved target file name == resolved target dir name
            // then resolved target dir should be blank -assuming that the fullFile only comprise of the base file name
            
            // why change here, instead of fixing the Heuristics().getDirectoryName(path):
            // because there could be other places that use the Heuristics under 
            // different scenario - e.g. - when the fullPath only contains dir components
            // place a fix here to avoid disturb other scenarios
            
            // the fix is based on an assumption that:
            // the fullFile represents a base file - then it must have 
            // a base file name, and the dir parts are optional
            String fn = this.intf.getProvider().getHeuristics().getBaseFileName(fullFile);
            String dn = this.intf.getProvider().getHeuristics().getDirectoryName(fullFile);
            this.targetFileName = fn;
            if ( fullFile.equals(fn) && fullFile.equals(dn) ) {
                this.targetDirectoryName = "";
            }
            else {
                this.targetDirectoryName = dn;
            }
            this.targetFileName = fn;
        } else {
            // for VOSK - leave it as is - until someone complain
            this.targetFileName =
                    this.intf.getProvider().getHeuristics().getBaseFileName(fullFile.substring(this.intf.getConfiguration().getTargetDirectoryName().length()));
            this.targetDirectoryName =
                    this.intf.getProvider().getHeuristics().getDirectoryName(this.intf.getConfiguration().getTargetDirectoryName());
        }
    }

}
