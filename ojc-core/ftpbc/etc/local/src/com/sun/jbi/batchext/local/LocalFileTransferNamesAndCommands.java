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
 * @(#)LocalFileTransferNamesAndCommands.java 
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
import com.sun.jbi.batchext.NamePattern;
import com.sun.jbi.batchext.TransferNamesAndCommands;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;

public abstract class LocalFileTransferNamesAndCommands implements TransferNamesAndCommands {
    //private static final Messages mMessages =
    //        Messages.getMessages(LocalFileTransferNamesAndCommands.class);
    private static final Logger mLogger =
            Messages.getLogger(LocalFileTransferNamesAndCommands.class);
    
    protected String _TargetDirectoryName = null;
    public String getTargetDirectoryName() throws BatchException {
        resolveTargetLocation();
        return _TargetDirectoryName;
    }
    
    protected String _TargetFileName = null;
    public String getTargetFileName() throws BatchException {
        resolveTargetLocation();
        return _TargetFileName;
    }
    
    protected boolean _Append = false;
    public boolean getAppend() {
        return _Append;
    }
    
    protected String _PreTransferCommand = LocalFileConfiguration.PRETC_UNDEFINED;
    public String getPreTransferCommand() throws BatchException {
        resolvePreTransfer();
        return _PreTransferCommand;
    }
    
    protected String _PreDirectoryName = null;
    public String getPreDirectoryName() throws BatchException {
        resolvePreTransfer();
        return _PreDirectoryName;
    }
    
    protected String _PreFileName = null;
    public String getPreFileName() throws BatchException {
        resolvePreTransfer();
        return _PreFileName;
    }
    
    protected String _PostTransferCommand = LocalFileConfiguration.POSTTC_UNDEFINED;
    public String getPostTransferCommand() throws BatchException {
        resolvePostTransfer();
        return _PostTransferCommand;
    }
    
    protected String _PostDirectoryName = null;
    public String getPostDirectoryName() throws BatchException {
        resolvePostTransfer();
        return _PostDirectoryName;
    }
    
    protected String _PostFileName = null;
    public String getPostFileName() throws BatchException {
        resolvePostTransfer();
        return _PostFileName;
    }
    
    // It will be nice if Harry can include the following two
    // methods in the TransferNamesAndCommands interface
    public String getTransferDirectoryName() throws BatchException {
        if (LocalFileConfiguration.PRETC_RENAME.equalsIgnoreCase(getPreTransferCommand())) {
            return getPreDirectoryName();
        } else {
            return getTargetDirectoryName();
        }
    }
    
    public String getTransferFileName() throws BatchException {
        if (LocalFileConfiguration.PRETC_RENAME.equalsIgnoreCase(getPreTransferCommand())) {
            return getPreFileName();
        } else {
            return getTargetFileName();
        }
    }
    
    protected String resolveNamePattern(String patternTag, String pattern,
            String originalName, BatchLocal local) throws BatchException {
        String name = null;
        NamePattern np = new NamePattern(pattern);
        try {
            np.setFileNameFromEgate(originalName);
            if ( local.getConfiguration().getSynchronized()) {
                np.setSeqNo(local.getPersistentState().getSequenceNumber());
                np.setStartSeqNo(local.getConfiguration().getStartingSequenceNumber());
                np.setMaxSeqNo(local.getConfiguration().getMaxSequenceNumber());
                name = np.expand();
                local.getPersistentState().setSequenceNumber(np.getSeqNo());
            } else {
                name = np.expand();
            }
        } catch (Exception ex) {
            String msg = "LocalFileTransferNamesAndCommands.resolveNamePattern: "
                    + "Exception occurred while resolving pattern [" + patternTag
                    + ": " + pattern + "]. ";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
                mLogger.log(Level.SEVERE, ex.toString());
            }
            throw new BatchException(msg, ex);
        }
        return name;
    }
}
