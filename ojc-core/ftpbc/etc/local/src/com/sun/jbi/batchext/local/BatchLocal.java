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
 * @(#)BatchLocal.java 
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

import com.sun.jbi.batchext.BatchCFGException;
import com.sun.jbi.batchext.BatchException;
import java.io.File;
import java.util.Properties;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.batchext.statemanager.StateManager;
import com.sun.jbi.batchext.statemanager.StateManagerException;
import com.sun.jbi.batchext.statemanager.StatePersistenceAdapter;
import com.sun.jbi.batchext.statemanager.StatePersistenceAdapterFactory;
import com.sun.jbi.batchext.statemanager.StatePersistenceException;

//~import com.stc.connector.management.util.ObjectReference;

/**
 * This class is the implementation of Local file.
 */
// This class is renamed from class LocalFileETD.
public class BatchLocal {
    //private static final Messages mMessages =
    //        Messages.getMessages(BatchLocal.class);
    private static final Logger mLogger =
            Messages.getLogger(BatchLocal.class);
    private static final String LOGICAL_HOST_ROOT_DIR="com.stc.application.dataroot";
    private static final String STATE_KEY_EXTENSION = ".state";
    private static final String XA_LOG_KEY_EXTENSION = ".xalog";
    private static final String COMPONENT_TYPE = "BatchLocal";
    //~ObjectReference mMonitor;
    private boolean isInSubCollab = false;
    private LocalFileConfiguration mConfig;
    
    /**
     * Constructor
     */
    public BatchLocal() {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "BatchLocal.BatchLocal() ...");
        }
    }
    
    /**
     * Called by external (collab service) to iniatialize object.
     * @param     cntrCollab  The Java Collaboration Controller object.
     * @param            key  The instance name.
     * @param           mode  The input/output mode.
     * @exception BatchException For connection exception.
     * @exception BatchException For data exception.
     * @see       com.stc.jcsre.ETD
     */
    public void initialize(Properties p)
    throws BatchException, BatchCFGException {
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "BatchLocal.initialize() ...");
        }
        
        this.mConfig = new LocalFileConfiguration();
        
        try {
            this.mConfig.initialize(p);
        } catch (Exception e) {
            throw new BatchCFGException("Exception when initialize configuration for Batch Local File OTD, e=" + e, e);
        }
        
        //~this.mMonitor = (ObjectReference)p.get("monitor");
        
        String className = null;
        Class clas = null;
        
        try {
            // Initialize the client
            className = getConfiguration().getClientClassName();
            clas = Class.forName(className);
            if (com.sun.jbi.batchext.local.LocalFileClient.class.isAssignableFrom(clas)) {
                _Client = (LocalFileClient) clas.newInstance();
                _Client.initialize(this);
                this.mConfig.registerConfigChangeListener((ConfigChangeListener)_Client);
            } else {
                String msg = "BatchLocal.initialize: The specified class name for LocalFileClient does not implement the required interface."
                        + "The class name is " + className;
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new BatchException(msg);
            }
            // Initialize the provider
            className = getConfiguration().getProviderClassName();
            clas = Class.forName(className);
            if (com.sun.jbi.batchext.local.LocalFileProvider.class.isAssignableFrom(clas)) {
                _Provider = (LocalFileProvider) clas.newInstance();
                _Provider.initialize(this);
            } else {
                String msg = "BatchLocal.initialize: The specified class name for LocalFileProvider does not implement the required interface."
                        + "The class name is " + className;
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new BatchException(msg);
            }
        } catch (Exception ex) {
            String msg = "BatchLocal.initialize: Unable to create or initialize the local file client or provider instance."
                    + "The class name is " + className;
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new BatchException(msg, ex);
        }
        
        if ( this.getConfiguration().getSynchronized()
        && (LocalFileConfiguration.TT_XA_COMPLIANT.equalsIgnoreCase(getConfiguration().getTransactionType()))) {
            throw new BatchException("Legacy feature no longer supported.");
        }
    }
    
    /**
     * Resets the data content of an ETD.
     *
     * @return   <code>false</code> if the ETD doesn't have a meaningful implementation of
     *           <code>reset()</code>; so do a <code>new</code> of the ETD instead.
     *           Otherwise, return <code>true</code> if the reset will clear the data content
     *           of the ETD.
     * @exception   BatchException  For connection exception.
     * @exception   BatchException  For data exception.
     */
    public boolean reset() throws BatchException {
        boolean bRet = true;
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "BatchLocal.reset() ...");
        }
        if ( this.getConfiguration() != null && this.getConfiguration().getSynchronized()) {
            if ( stateManager != null ) {
                try {
                    stateManager.load();
                } catch (StateManagerException smex) {
                    String msg = "BatchLocal.initialize: Unable to load persistent state.";
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, msg);
                    }
                    throw new BatchException(msg, smex);
                }
            }
        }
        if ( this.getConfiguration() != null )
            this.getConfiguration().reset();
        if ( _Client != null )
            bRet &= _Client.reset();
        if ( _Provider != null )
            bRet &= _Provider.reset();
        if(bRet) {
            if ( _Client != null )
                _Client.allowTransfer();
        }
        return bRet;
    }
    
    /**
     * Terminates the ETD.
     *
     * @exception     com.stc.common.collabService.BatchException
     *                  thrown when there's an external connection problem
     */
    public void terminate() throws BatchException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "BatchLocal.terminate() ...");
        }
        if (null != stateManager) {
            try {
                stateManager.close();
            } catch (StatePersistenceException spex) {
                String msg = "BatchLocal.initialize: Unable to close the state manager.";
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, msg);
                }
                throw new BatchException(msg, spex);
            }
        }
    }
    
    /**
     * This is a helper method.
     * There is a node in the BatchLocal named Configuration, it is used
     * to group the set of configurable parameters.
     * 
     * @return An instance of LocalFileConfiguration.
     */
    public LocalFileConfiguration getConfiguration() {
        return this.mConfig;
    }
    
    public void setConfiguration(LocalFileConfiguration cfg) {
        this.mConfig=cfg;
    }
    
    /**
     * This is a helper method.
     * There is a node in the BatchLocal named Client, it is used
     * to group the user customizable ETD client functionality.
     * 
     * @return An instance of LocalFileClient.
     */
    private LocalFileClient _Client = null;
    public LocalFileClient getClient() {
        return this._Client;
    }
    
    /**
     * This is a helper method.
     * There is a node in the BatchLocal named Provider, it is used
     * to group the user customizable ETD provider functionality.
     * 
     * @return An instance of LocalFileProvider.
     */
    private LocalFileProvider _Provider = null;
    public LocalFileProvider getProvider() {
        return this._Provider;
    }
    
    private StateManager stateManager = null;
    public StateManager getStateManager() {
        if ( stateManager == null ) {
            try {
                this.createStateManager();
            } catch (Exception e) {
                throw new RuntimeException("Exception when creating state manager, e=" + e, e);
            }
        }
        return stateManager;
    }
    
    public LocalFileState getPersistentState() {
        if ( this.getConfiguration().getSynchronized()
        && stateManager != null ) {
            if ( stateManager.getState() == null ) {
                // Set default state
                LocalFileState state = new LocalFileState();
                state.setTnc(null);
                state.setSequenceNumber(getConfiguration().getStartingSequenceNumber());
                stateManager.setState(state);
            }
            return (LocalFileState) (stateManager.getState());
        }
        return null;
    }
    
    public void storePersistentState() throws StateManagerException {
        stateManager.store();
    }
    
    //~public ObjectReference getMonitor() {
    //~    return this.mMonitor;
    //~}
    
    protected void createStateManager() throws BatchException, StatePersistenceException {
        String baseLoc = getConfiguration().getStatePersistenceBaseLocation();
        if ( baseLoc == null || baseLoc.trim().length() == 0 ) {
            baseLoc = System.getProperty(LOGICAL_HOST_ROOT_DIR);
            if ( baseLoc == null || baseLoc.trim().length() == 0 ) {
                throw new BatchException("StatePersistenceBaseLocation not specified for Synchronized external Batch FTP OTD.");
            }
            baseLoc += File.separator + "hostadapter";
        }
        String componentDirName = baseLoc
                + File.separator
                + COMPONENT_TYPE
                + File.separator
                + this.getConfiguration().getOID();
        String componentName = this.getConfiguration().getExternalName();
        StatePersistenceAdapter storeAdapter =
                StatePersistenceAdapterFactory.createFileStatePersistenceAdapter(
                componentDirName, componentName + STATE_KEY_EXTENSION);
        stateManager = new StateManager(storeAdapter);
    }
}
