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
 * @(#)LocalFileClientImpl.java 
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
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.RandomAccessFile;

//~import com.stc.connector.batchadapter.alerts.batch.BatchAlertUtil;
//~import com.stc.connector.batchadapter.alerts.batch.BatchLocalFileAlertCodes;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;

//~import com.stc.eventmanagement.NotificationEvent;
import com.sun.jbi.batchext.statemanager.StateManagerException;
import com.sun.jbi.batchext.streaming.FileInputStream;
import com.sun.jbi.batchext.streaming.InputStreamAdapter;
import com.sun.jbi.batchext.streaming.OutputStreamAdapter;
import com.sun.jbi.batchext.streaming.StreamUtil;
import com.sun.jbi.batchext.streaming.StreamingException;

public class LocalFileClientImpl implements LocalFileClient, ConfigChangeListener {
    //private static final Messages mMessages =
    //        Messages.getMessages(LocalFileClientImpl.class);
    private static final Logger mLogger =
            Messages.getLogger(LocalFileClientImpl.class);
    protected BatchLocal etd = null;
    protected boolean transferAllowed = true;
    protected TransferNamesAndCommands tnc = null;
    protected byte[] payload = null;
    protected FileInputStream is = null;
    protected OutputStream os = null;
    
    public LocalFileClientImpl() {
    }
    
    public void initialize(BatchLocal etd) {
        this.etd = etd;
        transferAllowed = true;
        if ( mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "OTD referenced by client:" + etd);
            mLogger.log(Level.FINE, "StateManager referenced by OTD:" + etd.getStateManager());
            mLogger.log(Level.FINE, "State referenced by OTD:" + etd.getPersistentState());
            mLogger.log(Level.FINE, "State referenced by OTD:" + (etd.getStateManager() != null ? etd.getStateManager().getState():"None"));
        }
    }
    
    public boolean reset() {
        if ( mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "OTD referenced by client:" + etd);
            mLogger.log(Level.FINE, "StateManager referenced by OTD:" + etd.getStateManager());
            mLogger.log(Level.FINE, "State referenced by OTD:" + etd.getPersistentState());
            mLogger.log(Level.FINE, "State referenced by OTD:" + (etd.getStateManager() != null ? etd.getStateManager().getState():"None"));
        }
        tnc = null;
        payload = null;
        try {
            if (null != is) {
                // The XA Action should abort the transaction in this case
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, "LocalFileClientImpl.reset: Open input stream encountered.");
                }
                releaseInputStream(false);
            }
            if (null != os) {
                // The XA Action should abort the transaction in this case
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, "LocalFileClientImpl.reset: Open output stream encountered.");
                }
                releaseOutputStream(false);
            }
        } catch (StreamingException ex) {
            ex.printStackTrace();
        } finally {
        }
        return true;
    }
    
    public void allowTransfer() {
        transferAllowed = true;
    }
    
    // File name and configuration resolution
    public TransferNamesAndCommands getResolvedNamesToGet() throws LocalFileException {
        if ( this.etd.getConfiguration().getSynchronized()) {
            if (null == tnc) {
                // Check whether we have a resume reading in progress
                // from a previous execution.
                if (getResumeReadingInProgress()) {
                    // We have one in progress...
                    if (etd.getConfiguration().getResumeReadingEnabled()) {
                        // ...and we can use it.
                        tnc = etd.getPersistentState().getTnc();
                        ((LocalFileTransferNamesAndCommandsGet)tnc).setEtd(etd);
                        if (mLogger.isLoggable(Level.FINE)) {
                            mLogger.log(Level.FINE, "LocalFileClientImpl.getResolvedNamesToGet: 'Resume reading' state recovered with current position "
                                    + etd.getPersistentState().getTnc().getReadPosition());
                        }
                    } else {
                        // ...and the configuration was changed afterwards.
                        String msg = "'Resume reading' state found, however the configuration does not allow to continue.";
                        if (mLogger.isLoggable(Level.SEVERE)) {
                            mLogger.log(Level.SEVERE, msg);
                        }
                        throw new LocalFileException(msg);
                    }
                } else {
                    // This should be the only place where we create a new
                    // LocalFileTransferNamesAndCommandsGet instance.
                    if ( etd.getConfiguration().getSynchronized()) {
                        etd.getPersistentState().setTnc(new LocalFileTransferNamesAndCommandsGet(etd));
                        tnc = etd.getPersistentState().getTnc();
                        if (mLogger.isLoggable(Level.FINE)) {
                            mLogger.log(Level.FINE, "LocalFileClientImpl.getResolvedNamesToGet: New tnc instance created.");
                        }
                    }
                }
            } else if (!(tnc instanceof LocalFileTransferNamesAndCommandsGet)) {
                String msg = "Attempting to resolve inbound transfer while the internal state was set up for outbound.";
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new LocalFileException(msg);
            }
            // to support dynamic configurable properties
            // we need to detect configuration changes here;
        } else {
            // multi-threaded mode, create a new tnc every time
            tnc = new LocalFileTransferNamesAndCommandsGet(this.etd);
        }
        return tnc;
    }
    
    public TransferNamesAndCommands getResolvedNamesToPut() throws LocalFileException {
        if (getResumeReadingInProgress()) {
            String msg = "Attempting to resolve outbound transfer with existing 'Resume reading' state. NOTE: Please complete the inbound transfer first!";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new LocalFileException(msg);
        }
        if (null == tnc) {
            // This should be the only place where we create a new
            // LocalFileTransferNamesAndCommandsPut instance.
            tnc = new LocalFileTransferNamesAndCommandsPut(etd);
        } else if (!(tnc instanceof LocalFileTransferNamesAndCommandsPut)) {
            String msg = "Attempting to resolve outbound transfer while the internal state was set up for inbound.";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new LocalFileException(msg);
        }
        // to support dynamic configurable properties, here we should check if configuration has been changed.
        return tnc;
    }
    
    // Payload for in-memory active transfer
    public byte[] getPayload() {
        return payload;
    }
    
    public void setPayload(byte[] payload) {
        this.payload = payload;
    }
    
    // Stream adapters for passive transfer
    // These adapters are used by other ETDs to request streams from this ETD
    public InputStreamAdapter getInputStreamAdapter() {
        return this;
    }
    
    public OutputStreamAdapter getOutputStreamAdapter() {
        return this;
    }
    
    public InputStream requestInputStream() throws StreamingException {
        if (LocalFileConfiguration.TT_XA_COMPLIANT.equalsIgnoreCase(etd.getConfiguration().getTransactionType())) {
            if (!transferAllowed) {
                String msg = new String("LocalFileClientImpl.requestInputStream: Only one transfer request is allowed in XA mode.");
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new StreamingException(msg);
            }
            transferAllowed = false;
        }
        
        if (null != is) {
            String msg = "LocalFileClientImpl.requestInputStream: The previous input stream has not been released.";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new StreamingException(msg);
        }
        if (null != os) {
            String msg = "LocalFileClientImpl.requestInputStream: Already set up for outbound streaming transfer.";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new StreamingException(msg);
        }
        LocalFileTransferNamesAndCommandsGet tncg = null;
        String fname = null;
        try {
            tncg = (LocalFileTransferNamesAndCommandsGet) getResolvedNamesToGet();
            if ( tncg.getReadPosition() == 0 ) {
                // Execute PreCommands only if new file
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "LocalFileClientImpl.requestInputStream: Executing Pre-transfer command on get().");
                }
                executePreCommandGet(tncg);
            } else {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "LocalFileClientImpl.requestInputStream: Skipping Pre-transfer command on get().");
                }
            }
            checkFileName(tncg.getTransferFileName(), "Local File Transfer - Request InputStream - Transfer File Name:");
            fname = new File(tncg.getTransferDirectoryName(), tncg.getTransferFileName()).getAbsolutePath();
            // Get the FileInputStream...
            is = etd.getProvider().getFileInputStream(fname);
            // ...and skip to the read position.
            is.skip(tncg.getReadPosition());
            // The XA Action should fail if this point has not been reached.
        } catch (LocalFileException lex) {
            String msg = "LocalFileClientImpl.requestInputStream: getResolvedNamesToGet() or executePreCommandGet() failed.";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new StreamingException(msg, lex);
        } catch (BatchException bex) {
            String msg = "LocalFileClientImpl.requestInputStream: Exception occurred.";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new StreamingException(msg, bex);
        } catch (FileNotFoundException fex) {
            String msg = "LocalFileClientImpl.requestInputStream: Can not open file "
                    + fname + " for reading.";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new StreamingException(msg, fex);
        } catch (IOException ioex) {
            String msg = "LocalFileClientImpl.requestInputStream: Can not skip to the proper read position.";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new StreamingException(msg, ioex);
        }
        return is;
    }
    
    public void releaseInputStream(boolean success) throws StreamingException {
        if (null == is) {
            // Assume that requestInputStream() either failed or has not been called.
            return;
        }
        LocalFileTransferNamesAndCommandsGet tncg = null;
        String fname = null;
        long inputStreamSize = is.getSize();
        try {
            tncg = (LocalFileTransferNamesAndCommandsGet) getResolvedNamesToGet();
            checkFileName(tncg.getTransferFileName(), "Local File Transfer - Release InputStream - Transfer File Name:");
            fname = new File(tncg.getTransferDirectoryName(), tncg.getTransferFileName()).getAbsolutePath();
            // Get the current readPosition
            tncg.setReadPosition(is.getPosition());
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "LocalFileClientImpl.releaseInputStream: Read position for future 'resume read' is "
                        + is.getPosition());
            }
            is.close();
        } catch (IOException ioex) {
            String msg = "LocalFileClientImpl.releaseInputStream: Failed to close file "
                    + fname;
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new StreamingException(msg, ioex);
        } catch (LocalFileException lex) {
            String msg = "LocalFileClientImpl.releaseInputStream: getResolvedNamesToGet() failed.";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new StreamingException(msg, lex);
        } catch (BatchException bex) {
            String msg = "LocalFileClientImpl.releaseInputStream: unable to obtain transfer directory and/or file name.";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new StreamingException(msg, bex);
        } finally {
            // Clear the reference to the input stream
            is = null;
        }
        if (success) {
            if ( this.etd.getConfiguration().getSynchronized() ) {
                try {
                    // The XA Action should fail if this point has not been reached.
                    // executePostCommand() is part of the XA COMMIT phase.
                    if (!etd.getConfiguration().getResumeReadingEnabled()
                    || (inputStreamSize == tncg.getReadPosition())) {
                        // Execute PostCommands only if end of data has been reached.
                        if (mLogger.isLoggable(Level.FINE)) {
                            mLogger.log(Level.FINE, "LocalFileClientImpl.releaseInputStream: Executing Post-transfer command on get().");
                        }
                        executePostCommandGet(tncg);
                    } else {
                        if (mLogger.isLoggable(Level.FINE)) {
                            mLogger.log(Level.FINE, "LocalFileClientImpl.releaseInputStream: Skipping Post-transfer command on get().");
                        }
                    }
                } catch (BatchException bex) {
                    String msg = "LocalFileClientImpl.releaseInputStream: Exception occurred while executing the post commands.";
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, msg);
                    }
                    throw new StreamingException(msg, bex);
                }
                // Determine the resume reading state
                if ((inputStreamSize == tncg.getReadPosition())
                || (tncg.getReadPosition() == 0)) {
                    // The stream has been consummed completely or
                    // no data has been read. Ignore the reading.
                    etd.getPersistentState().setTnc(null);
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "LocalFileClientImpl.releaseInputStream: Clearing 'resume reading' information.");
                    }
                } else if (etd.getConfiguration().getResumeReadingEnabled()) {
                    // Some data has been read. Store for future reading.
                    etd.getPersistentState().setTnc(tncg);
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "LocalFileClientImpl.releaseInputStream: Storing 'resume reading' information.");
                    }
                } else {
                    // Some data has been read. Ignore because the configuration does not allow.
                    etd.getPersistentState().setTnc(null);
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "LocalFileClientImpl.releaseInputStream: Ignoring 'resume reading' information - not enabled.");
                    }
                }
                // Save the ETD state.
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "In synchronized mode, save persisted state.");
                }
                try {
                    etd.storePersistentState();
                } catch (StateManagerException smex) {
                    String msg = "LocalFileClientImpl.releaseInputStream: Unable to store ETD persistent state";
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, msg);
                    }
                    throw new StreamingException(msg, smex);
                }
            } else {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Not in synchronized mode, do not save persisted state.");
                }
            }
        }
    }
    
    public OutputStream requestOutputStream() throws StreamingException {
        if (LocalFileConfiguration.TT_XA_COMPLIANT.equalsIgnoreCase(etd.getConfiguration().getTransactionType())) {
            if (!transferAllowed) {
                String msg = new String("LocalFileClientImpl.requestOutputStream: Only one transfer request is allowed in XA mode.");
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new StreamingException(msg);
            }
            transferAllowed = false;
        }
        
        if (null != os) {
            String msg = "LocalFileClientImpl.requestOutputStream: The previous output stream has not been released.";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new StreamingException(msg);
        }
        if (null != is) {
            String msg = "LocalFileClientImpl.requestOutputStream: Already set up for inbound streaming transfer.";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new StreamingException(msg);
        }
        LocalFileTransferNamesAndCommandsPut tncp = null;
        File file = null;
        String fname = null;
        try {
            tncp = (LocalFileTransferNamesAndCommandsPut) getResolvedNamesToPut();
            executePreCommandPut(tncp);
            // Create the destination directory.
            try {
                etd.getProvider().mkdirs(tncp.getTransferDirectoryName());
            } catch (LocalFileException lfex) {
                String msg = "LocalFileClientImpl.requestOutputStream: Unable to create transfer directory "
                        + tncp.getTransferDirectoryName();
                throw new StreamingException(msg, lfex);
            }
            checkFileName(tncp.getTransferFileName(), "Local File Transfer - Release OutputStream - Transfer File Name:");
            file = new File(tncp.getTransferDirectoryName(), tncp.getTransferFileName());
            fname = file.getAbsolutePath();
            if (file.exists() && tncp.getAppend()) {
                tncp.setOriginalFileSize(fname.length());
            } else {
                tncp.setOriginalFileSize(-1);
            }
            os = etd.getProvider().getFileOutputStream(fname, tncp.getAppend());
        } catch (LocalFileException lex) {
            String msg = "LocalFileClientImpl.requestOutputStream: getResolvedNamesToPut() or executePreCommandPut() failed.";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new StreamingException(msg, lex);
        } catch (BatchException bex) {
            String msg = "LocalFileClientImpl.requestOutputStream: executePreCommand() failed.";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new StreamingException(msg, bex);
        } catch (FileNotFoundException fex) {
            String msg = "LocalFileClientImpl.requestOutputStream: Can not open file "
                    + fname + " for writing in " + (tncp.getAppend()?"append":"overwrite") + " mode.";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new StreamingException(msg, fex);
        }
        return os;
    }
    
    public void releaseOutputStream(boolean success) throws StreamingException {
        if (null == os) {
            // Assume that requestOutputStream() either failed or has not been called.
            return;
        }
        LocalFileTransferNamesAndCommandsPut tncp = null;
        String fname = null;
        try {
            tncp = (LocalFileTransferNamesAndCommandsPut) getResolvedNamesToPut();
            checkFileName(tncp.getTransferFileName(), "Local File Transfer - Release OutputStream - Transfer File Name:");
            fname = new File(tncp.getTransferDirectoryName(), tncp.getTransferFileName()).getAbsolutePath();
            if (os instanceof FileOutputStream) {
                ((FileOutputStream) os).getFD().sync();
            }
            os.flush();
            os.close();
        } catch (IOException ioex) {
            String msg = "LocalFileClientImpl.releaseOutputStream: Failed to close file "
                    + fname;
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new StreamingException(msg, ioex);
        } catch (LocalFileException lex) {
            String msg = "LocalFileClientImpl.releaseOutputStream: getResolvedNamesToPut() failed.";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new StreamingException(msg, lex);
        } catch (BatchException bex) {
            String msg = "LocalFileClientImpl.releaseOutputStream: unable to obtain transfer directory and/or file name.";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new StreamingException(msg, bex);
        } finally {
            // Clear the reference to the output stream
            os = null;
        }
        if (success) {
            try {
                executePostCommandPut(tncp);
            } catch (BatchException bex) {
                String msg = "LocalFileClientImpl.releaseOutputStream: executePostCommand() failed.";
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new StreamingException(msg, bex);
            }
            // Save the ETD state.
            if ( this.etd.getConfiguration().getSynchronized() ) {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "In synchronized mode, save persisted state.");
                }
                try {
                    etd.storePersistentState();
                } catch (StateManagerException smex) {
                    String msg = "LocalFileClientImpl.releaseInputStream: Unable to store ETD persistent state";
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, msg);
                    }
                    throw new StreamingException(msg, smex);
                }
            } else {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Not in synchronized mode, do not save persisted state.");
                }
            }
        }
    }
    
    public void get() throws LocalFileException {
        boolean success = false;
        payload = null;
        ByteArrayOutputStream osTemp = new ByteArrayOutputStream();
        try {
            try {
                InputStream isTemp = requestInputStream();
                StreamUtil.copyStream(isTemp, osTemp, 64536);
                payload = osTemp.toByteArray();
                success = true;
            } catch (Exception ex) {
                success = false;
                String msg = "LocalFileClientImpl.get: Failed to get file to the payload.";
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new LocalFileException(msg, ex);
            } finally {
                try {
                    releaseInputStream(success);
                    osTemp.close();
                } catch (Exception exf) {
                    String msgf = "LocalFileClientImpl.get: Failed to clean up.";
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, msgf);
                    }
                    throw new LocalFileException(msgf, exf);
                } finally {
                    osTemp = null;
                }
            }
        } catch (Exception e) {
            //~sendAlertAndThrowException(BatchLocalFileAlertCodes.BATCH_LOCALFILE_EWAY_OPERATION_ERROR, new String[] {"get()", e.getMessage()}, e);
            throw new LocalFileException(e.getMessage(), e);
        }
    }
    
    /**
     * per QAI 94512 - requested by eXchange team
     */
    public void getIfExists() throws LocalFileException {
        File f = null;
        String dir = this.etd.getConfiguration().getTargetDirectoryName();
        String file = this.etd.getConfiguration().getTargetFileName();
        LocalFileTransferNamesAndCommandsGet tncg = null;
        if ( targetDirNameIsPattern() || targetFileNameIsPattern() ) {
            // if there are patterns - need to resolve them
            tncg = (LocalFileTransferNamesAndCommandsGet) getResolvedNamesToGet();
            try {
                dir = tncg.getTargetDirectoryName();
                file = tncg.getTargetFileName();
            } catch (BatchException e) {
                if ( !(e.getNestedException() instanceof FileNotFoundException) ) {
                    throw new LocalFileException("BatchException when resolving target location, e=" + e, e);
                }
            }
            if ( file == null || file.trim().length() == 0 )
                return; // if can not resolve to a real file name, then file does not exists
        } else {
            if ( file == null || file.trim().length() == 0 )
                throw new LocalFileException("Missing target file name, please set parameter 'Target File Name' to a proper value.");
        }
        if ( dir == null || dir.trim().length() == 0 )
            f = new File(file);
        else
            f = new File(dir, file);
        if ( !f.exists() ) // siliently return if target does not exists
            return;
        get();
    }
    
    public void put() throws LocalFileException {
        boolean success = false;
        try {
            try {
                OutputStream osTemp = requestOutputStream();
                StreamUtil.writeToStream(payload, osTemp);
                success = true;
            } catch (Exception ex) {
                success = false;
                String msg = "LocalFileClientImpl.put: Failed to put the payload to file.";
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new LocalFileException(msg, ex);
            } finally {
                try {
                    releaseOutputStream(success);
                } catch (Exception exf) {
                    String msgf = "LocalFileClientImpl.put: Failed to clean up.";
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, msgf);
                    }
                    throw new LocalFileException(msgf, exf);
                }
            }
        } catch (Exception e) {
            //~sendAlertAndThrowException(BatchLocalFileAlertCodes.BATCH_LOCALFILE_EWAY_OPERATION_ERROR, new String[] {"put()", e.getMessage()}, e);
            throw new LocalFileException(e.getMessage(), e);
        }
    }
    
    // Resume reading support
    public boolean getResumeReadingInProgress() {
        if ( this.etd.getConfiguration().getSynchronized() ) {
            return (etd.getPersistentState().getTnc() != null);
        } else {
            // in multi-threaded mode, no resume reading
            return false;
        }
    }
    
    // Transfer actions and pre-/post- commands
    // Implementation not exposed in the XSC
    public void executePreCommandGet(LocalFileTransferNamesAndCommandsGet tncg) throws LocalFileException, BatchException {
        String cmd = tncg.getPreTransferCommand();
        if (LocalFileConfiguration.PRETC_NONE.equalsIgnoreCase(cmd)) {
            return;
        }
        checkFileName(tncg.getTargetFileName(), "Local File Transfer - Pre Command Get - Target File Name:");
        File srcFile = new File(tncg.getTargetDirectoryName(), tncg.getTargetFileName());
        if (!srcFile.exists()) {
            throw new LocalFileException("LocalFileClientImpl.executePreCommandGet: The file "
                    + srcFile.getAbsolutePath() + " does not exist.",
                    new FileNotFoundException(srcFile.getAbsolutePath()));
        }
        checkFileName(tncg.getPreFileName(), "Local File Transfer - Pre Command Get - Pre File Name:");
        File destFile = new File(tncg.getPreDirectoryName(), tncg.getPreFileName());
        if(destFile.exists()) {
            // attempt to delete it first
            try {
                etd.getProvider().delete(destFile.getAbsolutePath());
            } catch(LocalFileException lex) {
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, "LocalFileClientImpl.executePreCommandGet: Unable to delete existing file "
                            + destFile.getAbsolutePath() + " Exception caught " + lex.toString());
                }
                throw lex;
            }
        }
        etd.getProvider().mkdirs(tncg.getPreDirectoryName());
        if (LocalFileConfiguration.PRETC_COPY.equalsIgnoreCase(cmd)) {
            etd.getProvider().copy(srcFile.getAbsolutePath(), destFile.getAbsolutePath());
        } else if (LocalFileConfiguration.PRETC_RENAME.equalsIgnoreCase(cmd)) {
            etd.getProvider().rename(srcFile.getAbsolutePath(), destFile.getAbsolutePath());
        } else {
            String msg = "LocalFileClientImpl.executePreCommandGet: Unknown command " + cmd;
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new LocalFileException(msg);
        }
    }
    
    public void undoPreCommandGet(LocalFileTransferNamesAndCommandsGet tncg) throws LocalFileException, BatchException {
        String cmd = tncg.getPreTransferCommand();
        if (LocalFileConfiguration.PRETC_NONE.equalsIgnoreCase(cmd)) {
            return;
        }
        checkFileName(tncg.getPreFileName(), "Local File Transfer - undo Pre Command Get - Pre File Name:");
        File destFile = new File(tncg.getPreDirectoryName(), tncg.getPreFileName());
        if (!destFile.exists()) {
            return;
        }
        if (LocalFileConfiguration.PRETC_COPY.equalsIgnoreCase(cmd)) {
            etd.getProvider().delete(destFile.getAbsolutePath());
        } else if (LocalFileConfiguration.PRETC_RENAME.equalsIgnoreCase(cmd)) {
            checkFileName(tncg.getTargetFileName(), "Local File Transfer - undo Pre Command Get - Target File Name:");
            File srcFile = new File(tncg.getTargetDirectoryName(), tncg.getTargetFileName());
            if(!srcFile.exists()) {
                etd.getProvider().rename(destFile.getAbsolutePath(), srcFile.getAbsolutePath());
            }
        } else {
            String msg = "LocalFileClientImpl.undoPreCommandGet: Unknown command " + cmd;
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new LocalFileException(msg);
        }
    }
    
    public void executePostCommandGet(LocalFileTransferNamesAndCommandsGet tncg) throws LocalFileException, BatchException {
        String cmd = tncg.getPostTransferCommand();
        if (LocalFileConfiguration.POSTTC_NONE.equalsIgnoreCase(cmd)) {
            return;
        }
        checkFileName(tncg.getTransferFileName(), "Local File Transfer - execute Post Command Get - Transfer File Name:");
        File srcFile = new File(tncg.getTransferDirectoryName(), tncg.getTransferFileName());
        if (LocalFileConfiguration.POSTTC_DELETE.equalsIgnoreCase(cmd)) {
            if (srcFile.exists()) {
                etd.getProvider().delete(srcFile.getAbsolutePath());
            }
        } else if (LocalFileConfiguration.POSTTC_RENAME.equalsIgnoreCase(cmd)) {
            checkFileName(tncg.getPostFileName(), "Local File Transfer - execute Post Command Get - Post File Name:");
            File destFile = new File(tncg.getPostDirectoryName(), tncg.getPostFileName());
            if (!srcFile.exists() && !destFile.exists()) {
                String msg = "LocalFileClientImpl.executePostCommandGet: Expected to find either transfer file "
                        + srcFile.getAbsolutePath() + " or post transfer file " + destFile.getAbsolutePath()
                        + "However, none of them was found. The operation can not be completed successfully.";
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new LocalFileException(msg);
            }
            if (srcFile.exists()) {
                if (destFile.exists()) {
                    // attempt to delete it first
                    try {
                        etd.getProvider().delete(destFile.getAbsolutePath());
                    } catch(LocalFileException lex) {
                        if (mLogger.isLoggable(Level.SEVERE)) {
                            mLogger.log(Level.SEVERE, "LocalFileClientImpl.executePostCommandGet: Unable to delete existing file "
                                    + destFile.getAbsolutePath() + " Exception caught " + lex.toString());
                        }
                        throw lex;
                    }
                }
                etd.getProvider().mkdirs(tncg.getPostDirectoryName());
                etd.getProvider().rename(srcFile.getAbsolutePath(), destFile.getAbsolutePath());
            }
        } else if (LocalFileConfiguration.POSTTC_COPY.equalsIgnoreCase(cmd)) {
            checkFileName(tncg.getPostFileName(), "Local File Transfer - execute Post Command Get - Post File Name:");
            File destFile = new File(tncg.getPostDirectoryName(), tncg.getPostFileName());
            if (!srcFile.exists() && !destFile.exists()) {
                String msg = "LocalFileClientImpl.executePostCommandGet: Expected to find either transfer file "
                        + srcFile.getAbsolutePath() + " or post transfer file " + destFile.getAbsolutePath()
                        + "However, none of them was found. The operation can not be completed successfully.";
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new LocalFileException(msg);
            }
            if (srcFile.exists()) {
                if (destFile.exists()) {
                    // attempt to delete it first
                    try {
                        etd.getProvider().delete(destFile.getAbsolutePath());
                    } catch(LocalFileException lex) {
                        if (mLogger.isLoggable(Level.SEVERE)) {
                            mLogger.log(Level.SEVERE, "LocalFileClientImpl.executePostCommandGet: Unable to delete existing file "
                                    + destFile.getAbsolutePath() + " Exception caught " + lex.toString());
                        }
                        throw lex;
                    }
                }
                etd.getProvider().mkdirs(tncg.getPostDirectoryName());
                etd.getProvider().copy(srcFile.getAbsolutePath(), destFile.getAbsolutePath());
            }
        } else {
            String msg = "LocalFileClientImpl.executePostCommandGet: Unknown command " + cmd;
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new LocalFileException(msg);
        }
    }
    
    public void undoTransferGet(LocalFileTransferNamesAndCommandsGet tncg) throws LocalFileException, BatchException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "LocalFileClientImpl.undoTransferGet called.");
        }
    }
    
    public void executePreCommandPut(LocalFileTransferNamesAndCommandsPut tncp) throws LocalFileException, BatchException {
        String cmd = tncp.getPreTransferCommand();
        if (LocalFileConfiguration.PRETC_NONE.equalsIgnoreCase(cmd)) {
            return;
        }
        checkFileName(tncp.getPreFileName(), "Local File Transfer - execute Pre Command Put - Pre File Name:");
        File destFile = new File(tncp.getPreDirectoryName(), tncp.getPreFileName());
        if (destFile.exists()) {
            // attempt to delete it first
            try {
                etd.getProvider().delete(destFile.getAbsolutePath());
            } catch(LocalFileException lex) {
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, "LocalFileClientImpl.executePreCommandPut: Unable to delete existing file "
                            + destFile.getAbsolutePath() + " Exception caught " + lex.toString());
                }
                throw lex;
            }
        }
        checkFileName(tncp.getTargetFileName(), "Local File Transfer - execute Pre Command Put - Target File Name:");
        File srcFile = new File(tncp.getTargetDirectoryName(), tncp.getTargetFileName());
        if (!srcFile.exists()) {
            return;
        }
        etd.getProvider().mkdirs(tncp.getPreDirectoryName());
        if (LocalFileConfiguration.PRETC_COPY.equalsIgnoreCase(cmd)) {
            etd.getProvider().copy(srcFile.getAbsolutePath(), destFile.getAbsolutePath());
        } else if (LocalFileConfiguration.PRETC_RENAME.equalsIgnoreCase(cmd)) {
            etd.getProvider().rename(srcFile.getAbsolutePath(), destFile.getAbsolutePath());
        } else {
            String msg = "LocalFileClientImpl.executePreCommandPut: unknown command " + cmd;
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new LocalFileException(msg);
        }
    }
    
    public void undoPreCommandPut(LocalFileTransferNamesAndCommandsPut tncp) throws LocalFileException, BatchException {
        String cmd = tncp.getPreTransferCommand();
        if (LocalFileConfiguration.PRETC_NONE.equalsIgnoreCase(cmd)) {
            return;
        }
        checkFileName(tncp.getPreFileName(), "Local File Transfer - undo Pre Command Put - Pre File Name:");
        File destFile = new File(tncp.getPreDirectoryName(), tncp.getPreFileName());
        if (!destFile.exists()) {
            return;
        }
        if (LocalFileConfiguration.PRETC_COPY.equalsIgnoreCase(cmd)) {
            etd.getProvider().delete(destFile.getAbsolutePath());
        } else if (LocalFileConfiguration.PRETC_RENAME.equalsIgnoreCase(cmd)) {
            checkFileName(tncp.getTargetFileName(), "Local File Transfer - undo Pre Command Put - Target File Name:");
            File srcFile = new File(tncp.getTargetDirectoryName(), tncp.getTargetFileName());
            if (!srcFile.exists()) {
                etd.getProvider().rename(destFile.getAbsolutePath(), srcFile.getAbsolutePath());
            }
        } else {
            String msg = "LocalFileClientImpl.undoPreCommandPut: unknown command " + cmd;
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new LocalFileException(msg);
        }
    }
    
    public void executePostCommandPut(LocalFileTransferNamesAndCommandsPut tncp) throws LocalFileException, BatchException {
        String cmd = tncp.getPostTransferCommand();
        if (LocalFileConfiguration.POSTTC_NONE.equalsIgnoreCase(cmd)) {
            return;
        } else if (LocalFileConfiguration.POSTTC_RENAME.equalsIgnoreCase(cmd)) {
            checkFileName(tncp.getTransferFileName(), "Local File Transfer - execute Post Command Put - Transfer File Name:");
            File srcFile = new File(tncp.getTransferDirectoryName(), tncp.getTransferFileName());
            checkFileName(tncp.getPostFileName(), "Local File Transfer - execute Post Command Put - Post File Name:");
            File destFile = new File(tncp.getPostDirectoryName(), tncp.getPostFileName());
            if (!srcFile.exists() && !destFile.exists()) {
                String msg = "LocalFileClientImpl.executePostCommandPut: Expected to find either transfer file "
                        + srcFile.getAbsolutePath() + " or post transfer file " + destFile.getAbsolutePath()
                        + "However, none of them was found. The operation can not be completed successfully.";
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new LocalFileException(msg);
            }
            if (srcFile.exists()) {
                if (destFile.exists()) {
                    // attempt to delete it first
                    try {
                        etd.getProvider().delete(destFile.getAbsolutePath());
                    } catch(LocalFileException lex) {
                        if (mLogger.isLoggable(Level.SEVERE)) {
                            mLogger.log(Level.SEVERE, "LocalFileClientImpl.executePostCommandPut: Unable to delete existing file "
                                    + destFile.getAbsolutePath() + " Exception caught " + lex.toString());
                        }
                        throw lex;
                    }
                }
                etd.getProvider().mkdirs(tncp.getPostDirectoryName());
                etd.getProvider().rename(srcFile.getAbsolutePath(), destFile.getAbsolutePath());
            }
        } else if (LocalFileConfiguration.POSTTC_COPY.equalsIgnoreCase(cmd)) {
            checkFileName(tncp.getTransferFileName(), "Local File Transfer - execute Post Command Put - Transfer File Name:");
            File srcFile = new File(tncp.getTransferDirectoryName(), tncp.getTransferFileName());
            checkFileName(tncp.getPostFileName(), "Local File Transfer - execute Post Command Put - Post File Name:");
            File destFile = new File(tncp.getPostDirectoryName(), tncp.getPostFileName());
            if (!srcFile.exists() && !destFile.exists()) {
                String msg = "LocalFileClientImpl.executePostCommandPut: Expected to find either transfer file "
                        + srcFile.getAbsolutePath() + " or post transfer file " + destFile.getAbsolutePath()
                        + "However, none of them was found. The operation can not be completed successfully.";
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new LocalFileException(msg);
            }
            if (srcFile.exists()) {
                if (destFile.exists()) {
                    // attempt to delete it first
                    try {
                        etd.getProvider().delete(destFile.getAbsolutePath());
                    } catch(LocalFileException lex) {
                        if (mLogger.isLoggable(Level.SEVERE)) {
                            mLogger.log(Level.SEVERE, "LocalFileClientImpl.executePostCommandPut: Unable to delete existing file "
                                    + destFile.getAbsolutePath() + " Exception caught " + lex.toString());
                        }
                        throw lex;
                    }
                }
                etd.getProvider().mkdirs(tncp.getPostDirectoryName());
                etd.getProvider().copy(srcFile.getAbsolutePath(), destFile.getAbsolutePath());
            }
        } else {
            String msg = "LocalFileClientImpl.executePostCommandPut: unknown command " + cmd;
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new LocalFileException(msg);
        }
    }
    
    public void undoTransferPut(LocalFileTransferNamesAndCommandsPut tncp) throws LocalFileException, BatchException {
        checkFileName(tncp.getTransferFileName(), "Local File Transfer - undo Transfer Put - Transfer File Name:");
        File file = new File(tncp.getTransferDirectoryName(), tncp.getTransferFileName());
        String fname = file.getAbsolutePath();
        if (!file.exists()) {
            if (tncp.getAppend() && (tncp.getOriginalFileSize() > 0 )) {
                if (mLogger.isLoggable(Level.WARNING)) {
                    mLogger.log(Level.WARNING, "LocalFileClientImpl.undoTransferPut: Expected to find file "
                            + fname + " with length of at least" + tncp.getOriginalFileSize()
                            + " bytes. However, it is missing and it may have caused data loss.");
                }
            }
            // nothing else to do
            return;
        }
        if (!tncp.getAppend() || (tncp.getOriginalFileSize() == -1)) {
            // delete the existing file
            etd.getProvider().delete(fname);
            return;
        }
        
        if (tncp.getOriginalFileSize() > file.length()) {
            // some data was appended, truncate to original size
            RandomAccessFile raf = null;
            try {
                raf = new RandomAccessFile(file, "rw");
                raf.setLength(tncp.getOriginalFileSize());
                raf.getFD().sync();
            } catch (Exception ex) {
                String msg = "LocalFileClientImpl.undoTransferPut: Exception occurred while truncating file " + fname;
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new BatchException(msg, ex);
            } finally {
                if (raf != null) {
                    try {
                        raf.close();
                    } catch (Exception ex) {
                        // do nothing
                    }
                }
            }
        } else if (tncp.getOriginalFileSize() == file.length()) {
            // do nothing - same size (should be same file)
        } else {
            // the file got truncated somehow (not the same)
            String msg = "LocalFileClientImpl.undoTransferPut: Expected to find file "
                    + fname + " with length of at least " + tncp.getOriginalFileSize()
                    + " bytes. However, the length is " + file.length()
                    + " bytes and it may have caused data loss."
                    + " NOTE: Please delete the file in order to proceed! Make a back up copy if you need";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new BatchException(msg);
        }
    }
    
    public void notifyChanges() {
        // this will trigger re-evaluation of the tnc
        this.tnc = null;
    }
    
    private void checkFileName(String file, String msg) throws BatchException {
        if ( file == null )
            throw new BatchException("Error: " + msg + "\nCause: File name is null (not available)");
    }
    
    //~private void sendAlertAndThrowException(String aKey, String[] aParams, Exception e) throws LocalFileException {
    //~    String msg = BatchAlertUtil.getMessage(aKey, aParams);
    //~    BatchAlertUtil.sendAlert(
    //~            this.etd.getMonitor(),
    //~            aKey,
    //~            aKey,
    //~            aParams,
    //~            NotificationEvent.SEVERITY_TYPE_MAJOR);
    //~    if ( e != null )
    //~        throw new LocalFileException(msg, e);
    //~    else
    //~        throw new LocalFileException(msg);
    //~}
    
    private boolean causedByFileNotFound(Exception e) {
        Throwable te = e;
        while ( te != null && !(te instanceof FileNotFoundException) )
            te = te.getCause();
        return ( te != null ? true : false );
    }
    
    private boolean targetDirNameIsPattern() {
        return this.etd.getConfiguration().getTargetDirectoryNameIsPattern();
    }
    
    private boolean targetFileNameIsPattern() {
        return this.etd.getConfiguration().getTargetFileNameIsPattern();
    }
}
