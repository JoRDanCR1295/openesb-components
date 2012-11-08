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
 * @(#)FtpInterface.java 
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

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.ftpbc.ftp.exception.ConfigurationException;
import com.sun.jbi.ftpbc.ftp.exception.FtpFileException;
import com.sun.jbi.ftpbc.ftp.exception.FtpInterfaceException;
import com.sun.jbi.ftpbc.ftp.statemanager.FileStatePersistenceAdapter;
import com.sun.jbi.ftpbc.ftp.statemanager.StateManager;
import com.sun.jbi.ftpbc.ftp.statemanager.StatePersistenceAdapter;
import com.sun.jbi.internationalization.Messages;

/**
 * This class represents the implementation of Batch FTP.
 * @author Harry Liu (harry.liu@sun.com)
 * @author jfu (jim.fu@sun.com)
 * @version cvs revision:    Last Modified: 
 */
public class FtpInterface {
    private static final Messages mMessages =
            Messages.getMessages(FtpInterface.class);
    private static final Logger mLogger =
            Messages.getLogger(FtpInterface.class);

    private static final String LOGICAL_HOST_ROOT_DIR="com.sun.aas.instanceRoot"; 
    private static final String APPSERVER_APPLICATIONDATA_SUB_DIR="applicationdata"; 

//    private static final String LOGICAL_HOST_ROOT_DIR="com.stc.application.dataroot";
    private static final String STATE_KEY_EXTENSION = ".state";
    private static final String COMPONENT_TYPE = "BatchFtp";
    
    private String logMsg = null;
    private FtpFileConfiguration configuration = null;
    private FtpFileProvider provider = null;
    private FtpFileClient client = null;
    private StateManager stateManager = null;
    private boolean stateChanged = false;
    
    /**
     * Constructs a new FTP Otd object.
     */
    public FtpInterface() {
    }
    
    public void initialize(Properties p)  throws FtpInterfaceException, ConfigurationException 
    {
        initialize(p, null);
    }
    
    public void initialize(Properties p, HashMap dispNameMapping)
    throws FtpInterfaceException, ConfigurationException {
        //~this.mMonitor = (ObjectReference)p.get("monitor");
        try {
            this.configuration = new FtpFileConfiguration(this);
            // in this config init phase - client instance is created
            this.configuration.initialConfigValues(p);
        } catch (Exception e) {
            logMsg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {e, "initialize(Properties p, HashMap dispNameMapping)"});
            throw new ConfigurationException(logMsg, e);
        }
        // here depend on conn mode, logon to remote or not
        if ( this.getClient() != null ) {
            if ( this.getConfiguration().getConnectionEstablishmentMode().equals("Automatic")) {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006042.DBG_EXT_FTP_AUTO_CONN"));
                }
                // enforce retry here
                long retryCount = this.configuration.getMaxRetry();
                long intervalRetry = this.configuration.getRetryInterval();
                
                if (!(retryCount > 0 && intervalRetry > 0)) {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006043.DBG_EXT_FTP_ATTEMPT_CONN"));
                    }
                    if (this.getConfiguration().getDirectoryListingStyle().equals(FtpHeuristics.EUC_JP_UNIX_DIR_LISTING_STYLE)) {
                        this.getClient().connect(FtpFileConfigConstants.ENCODING_EUC_JP);
                    } else if (this.getConfiguration().getDirectoryListingStyle().equals(FtpHeuristics.SJIS_UNIX_DIR_LISTING_STYLE)){
                        this.getClient().connect(FtpFileConfigConstants.ENCODING_SJIS);
                    } else {
                        this.getClient().connect(FtpFileConfigConstants.DEFAULT_CNTRL_ENCODING);
                    }
                } else {
                    String retryErrorMessages = "";
                    while ( retryCount >= 0 ) {
                        if (mLogger.isLoggable(Level.FINE)) {
                            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006044.DBG_EXT_FTP_ATTEMPT_RETRY", new Object[] {new Long(retryCount)}));
                        }
                        try {
                            this.getClient().connect();
                            if ( this.getClient().isConnected() )
                                break;
                            else
                                retryErrorMessages += mMessages.getString("FTPBC-E006052.ERR_EXT_FTP_NOT_CONN") + "\n";
                        } catch (Exception e) {
                            // whatever the reason retry - though this is not optimized
                            retryErrorMessages += e.getMessage() + "\n";
                        }
                        retryCount--;
                        try {
                            Thread.sleep(intervalRetry);
                        } catch (Exception e) {
                            throw new FtpInterfaceException(mMessages.getString("FTPBC-E006053.ERR_EXT_FTP_EXCEPTION_RETRY_SLEEP", new Object[] {e}));
                        }
                    }
                    if ( this.getClient().isConnected() ) {
                        if (mLogger.isLoggable(Level.FINE)) {
                            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006045.DBG_EXT_FTP_CONN_OK"));
                        }
                    } else {
                        throw new FtpInterfaceException(mMessages.getString("FTPBC-E006054.ERR_EXT_FTP_RETRY_EXCEED_MAX", new Object[] {new Long(retryCount), retryErrorMessages}));
                    }
                }
            } else {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006046.DBG_EXT_FTP_MANUAL_CONN"));
                }
            }
        } else {
            throw new FtpInterfaceException(mMessages.getString("FTPBC-E006055.ERR_EXT_FTP_NO_CLIENT"));
        }
    }
    
    public void reset() throws FtpInterfaceException {
        if (mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"reset()"}));
        if ( client != null ) {
            try {
                client.reset();
            } catch (Exception e) {
                throw new FtpInterfaceException(mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {e, "reset()"}), e);
            }
        }
    }
    
    public FtpFileConfiguration getConfiguration() {
        return this.configuration;
    }
    
    public void setConfiguration(FtpFileConfiguration cfg) {
        this.configuration=cfg;
    }
    
    public FtpFileClient getClient() {
        return this.client;
    }
    
    public FtpFileProvider getProvider() {
        return this.provider;
    }
    
    public void setClient(FtpFileClient client) {
        this.client = client;
    }
    
    public void setProvider(FtpFileProvider provider) {
        this.provider = provider;
    }
    
    /**
     * Gets a state manager instance.
     * @return    The state manager.
     */
    public StateManager getStateManager() {
        if ( this.stateManager == null ) {
            try {
                this.createManagers();
            } catch (Exception e) {
                throw new RuntimeException(mMessages.getString("FTPBC-E006056.ERR_EXT_FTP_EXCEPTION_CREATE_STATE_MGR", new Object[] {e}), e);
            }
        }
        return this.stateManager;
    }
    
    /**
     * Gets a FTP state instance.
     * @return    The FTP state.
     */
    public FtpFileState getState() throws FtpFileException {
        if (this.getStateManager().getState() == null) {
            try {
                this.getStateManager().load();
            } catch (Exception e) {
                throw new FtpFileException(mMessages.getString("FTPBC-E006057.ERR_EXT_FTP_LOAD_STATE", new Object[] {e}), e);
            }
            
            if (this.getStateManager().getState() == null) {
                // set initial state
                FtpFileState state = new FtpFileState();
                Arrays.fill(state.getSequenceNo(), this.configuration.getStartingSequenceNumber());
                this.getStateManager().setState(state);
            }
        }
        return (FtpFileState)this.getStateManager().getState();
    }
    
    /**
     * Creates a StateManager instance and ResourceManger instance if necessary.
     * @exception FtpInterfaceException    If some error occurs.
     */
    private void createManagers() throws FtpInterfaceException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D006003.DBG_EXT_FTP_METHOD_CALLED", new Object[] {"createManagers()"}));
        }
        
        String baseLoc = getConfiguration().getStatePersistenceBaseLocation();
        if ( baseLoc == null || baseLoc.trim().length() == 0 ) {
            baseLoc = System.getProperty(LOGICAL_HOST_ROOT_DIR);
            if ( baseLoc == null || baseLoc.trim().length() == 0 ) {
                throw new FtpInterfaceException(mMessages.getString("FTPBC-E006058.ERR_EXT_FTP_MISSING_PERSIST_BASE"));
            }
            baseLoc += File.separator + APPSERVER_APPLICATIONDATA_SUB_DIR;
        }
        
        String oid = this.getConfiguration().getOID();
        String componentDirName = baseLoc
                + File.separator
                + COMPONENT_TYPE
                + ((null == oid || 0 == oid.length()) ? "" :
                    File.separator + oid);

        String componentName = this.getConfiguration().getExternalName();
        String stateFileName = ((null == componentName || 0 == componentName.length()) ? "" : oid) + STATE_KEY_EXTENSION;
        
        try {
            StatePersistenceAdapter storeAdapter = null;
            if (getConfiguration().isSequenceNumberPersistenceDB()) {
                storeAdapter = new FtpFileStateDBPersistenceAdapter(
                        componentDirName + "/" + stateFileName);
            } else if (getConfiguration().isSequenceNumberPersistenceFlatFile()) {
                storeAdapter = new FtpFileStatePersistenceAdapter(
                        componentDirName, stateFileName);
            } else {
                storeAdapter = new FileStatePersistenceAdapter(
                        componentDirName, stateFileName);
            }
            this.stateManager = new StateManager(storeAdapter);
        } catch (Exception e) {
            throw new FtpInterfaceException(mMessages.getString("FTPBC-E006059.ERR_EXT_FTP_EXCEPTION_CREATE_PERSIST_ADAPTOR", new Object[] {e}), e);
        }
        
        if (this.stateManager == null) {
            throw new FtpInterfaceException(mMessages.getString("FTPBC-E006060.ERR_EXT_FTP_FAILED_CREATE_STATE_MGR"));
        }
        
    }
    
    public boolean isStateChanged() {
        return this.stateChanged;
    }
    
    public void setStateChanged(boolean newStateChanged) {
        this.stateChanged = newStateChanged;
    }
}
