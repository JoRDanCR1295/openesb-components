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
 * @(#)SAPBCClient.java 
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

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.sapbc.Endpoint;
import com.sun.jbi.sapbc.Utils;
import com.sun.jbi.sapbc.extensions.SAPAddress;
import com.sun.jbi.sapbc.extensions.SAPBinding;
import javax.jbi.JBIException;
import java.util.logging.Logger;
import java.util.logging.Level;

/**
 * A class that encapsulates all the necessary objects essential to executing
 * (t)RFC-enabled ABAP/4 Functions on SAP (client mode)
 *
 * @author  
 */
//Formally BapiClient
public class SAPBCClient
        extends SAPBCConnectorClient {
    
    private static final Messages mMessages = Messages.getMessages(SAPBCClient.class);
    private static final Logger mLogger = Messages.getLogger(SAPBCClient.class);
    
    private SAPJCoClient mSAPJcoClient = null;
    
    /**
     * Constructs a <code>SAPBCClient</code> object with the given properties.
     */
    public SAPBCClient(Endpoint endpoint)
    throws JBIException, SAPException {
        SAPAddress addr = endpoint.getSAPAddress();
        SAPBinding binding = endpoint.getSAPBinding();
        
        setClient(new SAPJCoClient(endpoint));
               
        setSystemID(addr.systemId());
        setClientTransactionMode(binding.transactionalMode().toString());
        setTidDatabase(binding.transactionIdDatabase());
        setMaxDBRows(binding.maxTransactionIdRows().toString());
        
        init();
    }
    
    /**
     * Executes (calls) synchronously the BAPI/RFC function on the Application Server.
     *
     * @param       rfm         Remote Function Module to execute.
     * @throws      SAPException  com.stc.connector.sapbapiadapter.sapbapi.SAPException
     *              Thrown if something went wrong during communication, marshalling, etc.
     */
    public boolean execute(com.sap.mw.jco.JCO.Function rfm)
    throws SAPException {
        return execute(rfm, null, null);
    }
    
    /**
     * Executes (calls) asynchronously the BAPI/RFC function on the R/3 Application Server.
     * When appropriate, <code>confirmTID()</code> is automatically called (when the JCS
     * Transaction Manager commits) to allow R/3 to delete processed TIDs (Transaction ID)
     * from its tracking database.
     *
     * @param         rfm         Remote Function Module to execute.
     * @param         eid         The Event ID of the triggering event (via
     *                            <code>getHashKey()</code>) to be used for this call.
     * @exception SAPException  Thrown if something went awry in the called function.
     *
     */
    public boolean executeAsynchronous(com.sap.mw.jco.JCO.Function rfm, String eid)
    throws SAPException {
        boolean result = true;
        
        if(isClientTrfcEnabled()){
            result = execute(rfm, eid, null);
        }else{
            result = false;
            throw new SAPException("Please enable Transactional RFC mode in Connectivity Map BAPI Outbound Configuration for using executeAsynchronous !");
        }
        return result;
    }
    
    /**
     * Executes (calls) asynchronously the BAPI/RFC function on the R/3 Application Server
     * but in an ordered (queued) fashion.  When appropriate, <code>confirmTID()</code> is
     * automatically called (when the JCS Transaction Manager commits) to allow R/3 to
     * delete processed TIDs (Transaction ID) from its tracking database.
     *
     * @param         rfm         Remote Function Module to execute.
     * @param         eid         The Event ID of the triggering event (via
     *                            <code>getHashKey()</code>) to be used for this call.
     *                            If <tt>null</tt>, a synchronous RFC execute is done.
     * @param         queueName   Queue to determine ordering.  If <tt>null</tt>, a
     *                            tRFC (asynchronous) execute is done.
     *
     * @exception SAPException  Thrown if something went awry in the called function.
     *
     */
    public boolean executeAsynchronous(com.sap.mw.jco.JCO.Function rfm, String eid, String queueName)
    throws SAPException {
        
        return execute(rfm, eid, queueName);
        
    }
    
    /**
     * Executes (calls) asynchronously the BAPI/RFC function on the R/3 Application Server
     * but in an ordered (queued) fashion.  When appropriate, <code>confirmTID()</code> is
     * automatically called (when the JCS Transaction Manager commits) to allow R/3 to
     * delete processed TIDs (Transaction ID) from its tracking database.
     *
     * @param         rfm         Remote Function Module to execute.
     * @param         eid         The Event ID of the triggering event (via
     *                            <code>getHashKey()</code>) to be used for this call.
     *                            If <tt>null</tt>, a synchronous RFC execute is done.
     * @param         queueName   Queue to determine ordering.  If <tt>null</tt>, a
     *                            tRFC (asynchronous) execute is done.
     *
     * @throws        SAPException com.sap.mw.jco.JCO.AbapException
     *              Thrown if something went awry in the called function.
     */
    public boolean execute(com.sap.mw.jco.JCO.Function rfm, String eid, String queueName)
    throws SAPException {
        boolean result = true; // variable to say if execute was sucessful
        try {
            
            String tid = null;
            setClientNeeded();
            if(getClient() == null){
                result = false;
                throw new Exception("*** Client Connection is NULL !!. Cannot proceed with Execute."
                        + " Please verify the Connection Mode configuration.");
            }
            
            if (null == eid) {
                synchronized ( getClient() ){
                    getClient().getJCOClient().execute(rfm);
                }
                Utils.checkLog(mLogger, Level.INFO, "SAPBCClient.Sucessfully_Executed_Function", rfm.getName());
                
            } else {
                
                tid = getClientTiDManager().createTID(eid);
                boolean isLockErr = ((FileTIDManagerImpl)getClientTiDManager()).getLockErr();
                Utils.checkLog(mLogger, Level.INFO, "SAPBCClient.Execute_TID_Value", new Object[]{eid, tid});
                if(!isLockErr){
                    if (null == queueName) {
                        Utils.checkLog(mLogger, Level.FINE, "SAPBCClient.Calling_Execute");
                        synchronized ( getClient() ){
                            getClient().getJCOClient().execute(rfm, tid);
                        }
                        Utils.checkLog(mLogger, Level.INFO, "SAPBCClient.Sucessfully_Executed_tRFCFunction", new Object[]{rfm.getName(), tid});
                    } else {
                        Utils.checkLog(mLogger, Level.FINE, "SAPBCClient.Calling_Execute_Queue");
                        synchronized ( getClient() ){
                            getClient().getJCOClient().execute(rfm, tid, queueName);
                        }
                        Utils.checkLog(mLogger, Level.INFO, "SAPBCClient.Sucessfully_Executed_qRFCFunction", new Object[]{rfm.getName(), tid, queueName});
                    }
                }else{
                    result = false;
                    Utils.checkLog(mLogger, Level.INFO, "SAPBCClient.Lock_File_Error");
                }
                
            }
            // client must be set dirty before calling commit()
            setClientDirty();
            setLastActivityTime(System.currentTimeMillis());
            
        } catch (Exception e) {
            result = false;
            setLastError(e);
            mLogger.log(Level.SEVERE, "SAPBCClient.Failed_To_Execute_JCO");

            throw new SAPException("execute(): failed! Error is: " + e.getMessage(), e);
        }
        return result;
    }
    
    /**
     * Retrieves the SAPJCoClient client object.
     *
     * @return    The jCO client handle.
     */
    public SAPJCoClient getClient() {
        return mSAPJcoClient;
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param sapJCOclient DOCUMENT ME!
     */
    public void setClient(SAPJCoClient sapJCOclient) {
        mSAPJcoClient = sapJCOclient;
    }

    
}
