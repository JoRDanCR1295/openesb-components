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
 * @(#)SAPBCConnectorClient.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**************************************************************************
 *
 *          Copyright (c) 2002, SeeBeyond Technology Corporation,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ***************************************************************************/
package com.sun.jbi.sapbc.extservice;

import com.sap.mw.jco.JCO;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.sapbc.Utils;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.JBIException;

/**
 * An interface to define the methods required by a SAP BAPI and IDOC connectors
 * for outbound transactions from SAP
 * @author Rajesh Dhingra
 * @version 
 */
//Formally ConnectorClient
public abstract class SAPBCConnectorClient
    implements TIDConnector {
  
    private static final Messages mMessages = Messages.getMessages(SAPBCConnectorClient.class);
    private static final Logger mLogger = Messages.getLogger(SAPBCConnectorClient.class);

    /** DOCUMENT ME */
    private Properties props = null;
    /** DOCUMENT ME */
    private long lastActivityTime;
    /** DOCUMENT ME */
    private java.lang.Throwable lastError;

    /** RFC Client. */
    //private com.sap.mw.jco.JCO.Client client = null;
    private SAPJCoClient client = null;

    /** RFC Client connection is needed.
     */
    private boolean clientNeeded = false;
    private String systemID = null;

    /** RFC Client Transactional work done; not yet committed.
     */
    private boolean clientDirty = false;

    /** tRFC Client TID Manager.
     */
    private TIDManager clientTidMgr = null;
    
    private boolean isClientTrfcEnabled = false;
    
    // these will need to be set from the RfcRepository using SAPBinding
    private String clientTidDB = "C://Temp//SAP//tid";
    private int maxDBRows = 200; //default value
    private String transMode = null;
    
       
    /** RFC Client Transaction modes. */    
    /**
     * Non-Transactional mode string.
     */
    public static final String XACT_NONE_STR = "Non-Transactional";    
    /**
     * Transactional RFC (tRFC) mode string.
     */
    public static final String XACT_TRFC_STR = "Transactional RFC(tRFC)";  
    /**
     * Via COMMIT/ROLLBACK BAPI string.
     */
    public static final String XACT_COMMITROLLBACK_BAPI_STR = "Via COMMIT/ROLLBACK BAPI";
    /**
     * XA-compliant mode string.
     */
    public static final String XACT_XA_STR = "XA-compliant";

    // QAI 64084, version check to maintain backward compatibilty with 50x OTD
    private String bapiOTDVer = null;


    /**
     * Initalizes the TID Manager Factory and misc
     *
     * @throws ResourceException on error
     */
    protected void init() throws JBIException {
        try {
            if(isClientTrfcEnabled()) {
              clientTidMgr = TIDManagerFactory.newInstance(this, false);
              //clientTidMgr.setMonitor(getSAPConnector().getMonitor());
              Utils.checkLog(mLogger, Level.FINE, "SAPBCConnectorClient.Activate_TID");
            }                                  
        } catch (Exception ex) {
            String errMsg = mMessages.getString("SAPBCConnectorClient.Failed_to_get_SAP_CRI", ex.getMessage());
            mLogger.log(Level.SEVERE,errMsg);
            throw new JBIException(errMsg,ex);
        }
    }

    /**
     * Returns the SAPBCConnectorClient object.
     * 
     * 
     * @return SAPBCConnectorClient
     */
    protected SAPBCConnectorClient getConnector() {
        return this;
    }
    
    /**
     * Set the system ID
     *@param sysID the system ID string
     */
    public void setSystemID(String sysID) {
        systemID = sysID;
    }

    /**
     * DOCUMENT ME!
     *
     * @return The System ID string 
     */
    public String getSystemID() {
        return ((null == systemID) ? "SBYN" : systemID);
    }

    /**
     * Retrieves the SAPJCoClient client object.
     *
     * @return    The jCO client handle.
    public SAPJCoClient getClient() {
        return client;
    }
     */

    /**
     * DOCUMENT ME!
     *
     * @param sapJCOclient DOCUMENT ME!
    public void setClient(SAPJCoClient sapJCOclient) {
        client = sapJCOclient;
    }
     */

    /**
     * Retrieves the TID Manager for tRFC client mode.
     *
     * @return    The TID Manager (can be <code>null</code> if tRFC not enabled)
     */

    protected TIDManager getClientTiDManager() { 
      return this.clientTidMgr;
    }

    public void setBapiOTDVer(String otdVer){
        bapiOTDVer = otdVer;
    }

    public String getBapiOTDVer(){
        if(bapiOTDVer != null){
            return bapiOTDVer;
        }else{
            return new String("");
        }
    }

    /**
     * Commit all the work done on SAP Server thus far.
     * @throws      Exception When SAP problems occur.
     */
    public void commit() throws SAPException {
        try {                    
            if ((getClient() != null) && isClientDirty()) {
                if (getClientTransactionalMode() == XACT_NONE_STR) {
                    // BAPI/RFC will be committed by the App Server
                }else if (getClientTransactionalMode() == XACT_TRFC_STR) {
                    getClientTiDManager().confirmTID();
                // QAI 64084
                }else if (getClientTransactionalMode() == XACT_COMMITROLLBACK_BAPI_STR) {
                    // Transaction Commit Bapi already executed in the OTD commit()
                }
                clientDirty = false;
                setLastActivityTime(System.currentTimeMillis());
            }        
        } catch (Exception t) {
            throw new SAPException("commit(): failed!" + t.getMessage(), t);
        }
    }
    
    // QAI 64084
    /**
     * Rollback all the work done on SAP Server thus far.
     * @throws      Exception When SAP problems occur.
     */
    public void rollback() throws SAPException {
        try {
            if ((getClient() != null) && isClientDirty()){
                if (getClientTransactionalMode() == XACT_NONE_STR) {
                    // BAPI/RFC will be rolledback by the App Server
                }else if (getClientTransactionalMode() == XACT_TRFC_STR) {

                }else if (getClientTransactionalMode() == XACT_COMMITROLLBACK_BAPI_STR) {
                    // Transaction Rollback Bapi already executed in the OTD rollback ()
                }
            }
            clientDirty = false;
            setLastActivityTime(System.currentTimeMillis());
        } catch (Exception t) {
            throw new SAPException("rollback(): failed!" + t.getMessage(), t);
        }
    }
    /**
     * Retrieves the client Transactional Mode.
     * @return      Client Transactional Mode.
     */
    public String getClientTransactionalMode() {
        String clientXactMode = null;
            if (transMode.equals(XACT_NONE_STR)) {
                clientXactMode = XACT_NONE_STR;
            } else if (transMode.equals(XACT_TRFC_STR)) {
                clientXactMode = XACT_TRFC_STR;
            } else if (transMode.equals(XACT_COMMITROLLBACK_BAPI_STR)) {
                clientXactMode = XACT_COMMITROLLBACK_BAPI_STR;
            } else if (transMode.equals(XACT_XA_STR)) {
                clientXactMode = XACT_XA_STR;
            }
        return clientXactMode;
    }
    
    
    public void setClientTransactionMode(String mode){
       transMode = mode;	
    }
    
    /**
     * Retrieves the TID Database to use for client mode.  Interpretation subject
     * to TID Manager Class.
     * 
     * @return    The TID Database name.
     */
    public String getTidDatabase() { 
        return clientTidDB;
    }
    
    
    public void setTidDatabase(String tidDB){
    	clientTidDB = tidDB;
    }
   	
   	
    public void setMaxDBRows(String maxRows){
        maxDBRows = Integer.parseInt(maxRows);
    }
    
    /**
     * Retrieves the Maximum number of Rows the TID Database for client mode
     * should have.
     * 
     * @return      Maximum Rows.
     */
    public int getClientMaxDBRows() {
        return maxDBRows;
    }
    
    /**
     * Checks whether tRFC (transactional RFC) is enabled for server mode.
     * 
     * @return    <code>true</code> if tRFC is enabled; else <code>false</code>.
     */
    public boolean isClientTrfcEnabled(){
      return (getClientTransactionalMode() == XACT_TRFC_STR);	
    }
    
    /**
     * Checks whether tRFC (transactional RFC) is enabled for server mode.
     * 
     * @return    <code>true</code> if tRFC is enabled; else <code>false</code>.
     */
    public boolean isTrfcEnabled(){
    	return isClientTrfcEnabled();
    }  
    
    /**
     * Indicates to the connector that the RFC Client connection is needed.
     */
    public void setClientNeeded() {
        clientNeeded = true;
    }

    /**
     * Sets the last time a connection activity was performed.
     *
     *@param lastActivityTime the last time 
     */
    public void setLastActivityTime(long lastActivityTime) {
        this.lastActivityTime = lastActivityTime; // System.currentTimeMillis();
    }

    /**
     * Gets the last time a connection activity was performed.
     *
     *@return the lastActivityTime
     */
    public long getLastActivityTime() {
        return this.lastActivityTime;
    }

    /**
     * Sets the last error due to a connection attempt.
     *
     *@param lastError the error 
     */
    public void setLastError(java.lang.Throwable lastError) {
        this.lastError = lastError;
    }

    /**
     * Gets the last error due to a connection attempt.
     *
     *@return Throwable  the error 
     */
    public java.lang.Throwable getLastError() {
        return this.lastError;
    }

    /**
     * Tests if the Client portion is available.
     * @return  <tt>true</tt> if client is available.
     *@throws  Exception on error 
     */
    public boolean isClientAvailable() throws Exception {
        boolean ansC = false;

        try {
            if (client != null) {
                ansC = client.isAlive();
            }
        } catch (com.sap.mw.jco.JCO.Exception e) {
            throw new Exception("ConnectorClient.isClientAvailable() Caught JCO.Exception: " +
                e.getMessage());
        }

        return ansC;
    }

    /**
     * Tests whether Client Transactional work has been done.
     * @return      <tt>true</tt> if work done.
     */
    public boolean isClientDirty() {
        return clientDirty;
    }

    /**
     * Sets Client Transactional work has been done.
     */
    public void setClientDirty() {
        clientDirty = true;
    }
        
    
}
