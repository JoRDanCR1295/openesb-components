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
 * @(#)LocalFileClient.java 
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
import com.sun.jbi.batchext.TransferNamesAndCommands;
import com.sun.jbi.batchext.streaming.InputStreamAdapter;
import com.sun.jbi.batchext.streaming.OutputStreamAdapter;

/**
 * This class represents the local file client functionality
 * available to the user.
 */
public interface LocalFileClient extends InputStreamAdapter, OutputStreamAdapter {
    public void initialize(BatchLocal etd);

    /**
     * Resets the LocalFileClient, so it can be used in subsequent transfers.
     * @return true on success, otherwise false.
     */    
    public boolean reset();

    public void allowTransfer();

    /**
     * Obtains a TransferNamesAndCommands instance, which can be used to get
     * access to the resolved names and commands for the current inbound
     * transfer.
     * @throws LocalFileException If anything goes wrong.
     *      LocalFileException.getNestedException() can be used to retrieve the
     *      original exception that caused the failure, if any.
     * @return TransferNamesAndCommands instance.
     */    
    public TransferNamesAndCommands getResolvedNamesToGet() throws LocalFileException;

    /**
     * Obtains a TransferNamesAndCommands instance, which can be used to get
     * access to the resolved names and commands for the current outbound
     * transfer.
     * @throws LocalFileException If anything goes wrong.
     *      LocalFileException.getNestedException() can be used to retrieve the
     *      original exception that caused the failure, if any.
     * @return TransferNamesAndCommands instance.
     */    
    public TransferNamesAndCommands getResolvedNamesToPut() throws LocalFileException;

    /**
     * Gets the content of the Payload field.
     * @return The Payload content.
     */    
    public byte[] getPayload();

    /**
     * Sets the content of the Payload field.
     * @param newPayload The new Payload content.
     */    
    public void  setPayload(byte[] newPayload);

    // Stream adapters for passive transfer
    // These adapters are used by other ETDs to request streams from this ETD.

    /**
     * Gets an InputStreamAdapter which can be used to perform inbound
     * streaming transfers with this LocalFileClient.
     * @return The InputStreamAdapter.
     */    
    public InputStreamAdapter getInputStreamAdapter();

    /**
     * Gets an OutputStreamAdapter which can be used to perform outbound
     * streaming transfers with this LocalFileClient.
     * @return The OutputStreamAdapter.
     */    
    public OutputStreamAdapter getOutputStreamAdapter();

    // Transfer actions

    /**
     * Performs an inbound transfer to the Payload field.
     * @throws LocalFileException If anything goes wrong.
     *      LocalFileException.getNestedException() can be used to retrieve the
     *      original exception that caused the failure, if any.
     */    
    public void get() throws LocalFileException;

    /**
     * Same as get but if there is no file found matching the name pattern
     * given in "Target Directory Name" and "Target File Name", just return
     * siliently;
     * Invoking of the method can result in :
     * a) throw Exception ( SoapFault if it is BPEL ), the caller is notified an error 
     * b) getIfExists() (readIfExists() for BPEL) successful, payload is NULL, target file does not exists; 
     * c) getIfExists() (readIfExists() for BPEL) successful, payload is NOT NULL, target file detected and content is read, if the content is 0 length, this lead to a byte[] of 0 length as payload;
     * @exception   FtpFileException  If some error occurs.
     */
    public void getIfExists() throws LocalFileException;

    /**
     * Performs an inbound transfer to the Payload field - set payload 
     * to NULL if the target file does not exists.
     * @throws LocalFileException If anything goes wrong.
     *      LocalFileException.getNestedException() can be used to retrieve the
     *      original exception that caused the failure, if any.
     */    
    //public void getIfAvailable() throws LocalFileException;

    /**
     * Performs an outbound transfer from the Payload field.
     * @throws LocalFileException If anything goes wrong.
     *      LocalFileException.getNestedException() can be used to retrieve the
     *      original exception that caused the failure, if any.
     */    
    public void put() throws LocalFileException;
    
    // Resume reading support

    /**
     * Indicates whether there is a 'Resume Reading' operation in progres.
     * @return true if 'Resume Reading' state was recorded in the preceeding
        inbound transfer, otherwise false.
     */    
    public boolean getResumeReadingInProgress();

    // Transfer actions and pre-/post- commands
    // Implementation not exposed in the XSC
    public void executePreCommandGet(LocalFileTransferNamesAndCommandsGet tnc) throws LocalFileException, BatchException;
    public void undoPreCommandGet(LocalFileTransferNamesAndCommandsGet tnc) throws LocalFileException, BatchException;
    public void executePostCommandGet(LocalFileTransferNamesAndCommandsGet tnc) throws LocalFileException, BatchException;
    public void undoTransferGet(LocalFileTransferNamesAndCommandsGet tnc) throws LocalFileException, BatchException;
    public void executePreCommandPut(LocalFileTransferNamesAndCommandsPut tnc) throws LocalFileException, BatchException;
    public void undoPreCommandPut(LocalFileTransferNamesAndCommandsPut tnc) throws LocalFileException, BatchException;
    public void executePostCommandPut(LocalFileTransferNamesAndCommandsPut tnc) throws LocalFileException, BatchException;
    public void undoTransferPut(LocalFileTransferNamesAndCommandsPut tnc) throws LocalFileException, BatchException;
}
