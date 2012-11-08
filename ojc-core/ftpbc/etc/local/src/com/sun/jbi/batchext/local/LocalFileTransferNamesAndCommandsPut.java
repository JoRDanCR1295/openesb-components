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
 * @(#)LocalFileTransferNamesAndCommandsPut.java 
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

import com.sun.jbi.batchext.BatchException;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;

public class LocalFileTransferNamesAndCommandsPut extends LocalFileTransferNamesAndCommands {
    //private static final Messages mMessages =
    //        Messages.getMessages(LocalFileTransferNamesAndCommandsPut.class);
    private static final Logger mLogger =
            Messages.getLogger(LocalFileTransferNamesAndCommandsPut.class);
    transient private BatchLocal etd = null;
    public LocalFileTransferNamesAndCommandsPut(BatchLocal etd) {
        this.etd = etd;
    }
    
    public void setEtd(BatchLocal etd) {
        this.etd = etd;
    }
    
    long originalFileSize = -1;
    public void setOriginalFileSize(long size) {
        originalFileSize = size;
    }
    public long getOriginalFileSize() {
        return originalFileSize;
    }
    
    /**
     * Resolve Target Location Names.
     * @exception    BatchException    If some error occurs.
     */
    public void resolveTargetLocation() throws BatchException {
        if ((null == _TargetDirectoryName) || (null == _TargetFileName)) {
            if (etd.getConfiguration().getTargetDirectoryNameIsPattern()) {
                _TargetDirectoryName = resolveNamePattern("TargetDirectoryName",
                        etd.getConfiguration().getTargetDirectoryName(),
                        null, etd);
            } else {
                _TargetDirectoryName = etd.getConfiguration().getTargetDirectoryName();
            }
            if (etd.getConfiguration().getTargetFileNameIsPattern()) {
                _TargetFileName = resolveNamePattern("TargetFileName",
                        etd.getConfiguration().getTargetFileName(),
                        null, etd);
            } else {
                _TargetFileName = etd.getConfiguration().getTargetFileName();
            }
            _Append = etd.getConfiguration().getAppend();
            // validate
            if (_TargetDirectoryName.length() == 0) {
                String msg = "Target Directory Name is empty.";
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new BatchException(msg);
            }
            if (_TargetFileName.length() == 0) {
                String msg = "Target File Name is empty.";
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new BatchException(msg);
            }
        }
    }
    
    /**
     * Resolve Pre Transfer Names/Command.
     * @exception    BatchException    If some error occurs.
     */
    public void resolvePreTransfer() throws BatchException {
        _PreTransferCommand = etd.getConfiguration().getPreTransferCommand();
        if (LocalFileConfiguration.PRETC_NONE.equalsIgnoreCase(_PreTransferCommand)) {
            // make sure that it will fail if they are used somewhere by mistake
            _PreDirectoryName = null;
            _PreFileName = null;
        }
        if ((null != _PreDirectoryName) && (null != _PreFileName)) {
            // the name resolution has been performed
            return;
        }
        if (LocalFileConfiguration.PRETC_COPY.equalsIgnoreCase(_PreTransferCommand)
        || LocalFileConfiguration.PRETC_RENAME.equalsIgnoreCase(_PreTransferCommand)) {
            if (etd.getConfiguration().getPreDirectoryNameIsPattern()) {
                _PreDirectoryName = resolveNamePattern("PreDirectoryName",
                        etd.getConfiguration().getPreDirectoryName(),
                        getTargetDirectoryName(), etd);
            } else {
                _PreDirectoryName = etd.getConfiguration().getPreDirectoryName();
            }
            if (_PreDirectoryName.length() == 0) {
                String msg = "Pre Directory Name is empty.";
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new BatchException(msg);
            }
            if (etd.getConfiguration().getPreFileNameIsPattern()) {
                _PreFileName = resolveNamePattern("PreFileName",
                        etd.getConfiguration().getPreFileName(),
                        getTargetFileName(), etd);
            } else {
                _PreFileName = etd.getConfiguration().getPreFileName();
            }
            if (_PreFileName.length() == 0) {
                String msg = "Pre File Name is empty.";
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new BatchException(msg);
            }
        }
    }
    
    /**
     * Resolve Post Transfer Names/Command.
     * @exception    BatchException    If some error occurs.
     */
    public void resolvePostTransfer() throws BatchException {
        _PostTransferCommand = etd.getConfiguration().getPostTransferCommand();
        if (LocalFileConfiguration.POSTTC_DELETE.equalsIgnoreCase(_PostTransferCommand)) {
            String msg = "Post Transfer Command can not be 'Delete' for outbound transfers.";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new BatchException(msg);
        }
        if (LocalFileConfiguration.POSTTC_NONE.equalsIgnoreCase(_PostTransferCommand)) {
            // make sure that it will fail if they are used somewhere by mistake
            _PostDirectoryName = null;
            _PostFileName = null;
        }
        if ((null != _PostDirectoryName) && (null != _PostFileName)) {
            // the name resolution has been performed
            return;
        }
        if (LocalFileConfiguration.POSTTC_RENAME.equalsIgnoreCase(_PostTransferCommand)
        || LocalFileConfiguration.POSTTC_COPY.equalsIgnoreCase(_PostTransferCommand)) {
            if (etd.getConfiguration().getPostDirectoryNameIsPattern()) {
                _PostDirectoryName = resolveNamePattern("PostDirectoryName",
                        etd.getConfiguration().getPostDirectoryName(),
                        getTargetDirectoryName(), etd);
            } else {
                _PostDirectoryName = etd.getConfiguration().getPostDirectoryName();
            }
            if (_PostDirectoryName.length() == 0) {
                String msg = "Post Directory Name is empty.";
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new BatchException(msg);
            }
            if (etd.getConfiguration().getPostFileNameIsPattern()) {
                _PostFileName = resolveNamePattern("PostFileName",
                        etd.getConfiguration().getPostFileName(),
                        getTargetFileName(), etd);
            } else {
                _PostFileName = etd.getConfiguration().getPostFileName();
            }
            if (_PostFileName.length() == 0) {
                String msg = "Post File Name is empty.";
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new BatchException(msg);
            }
        }
    }
}
