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
 * @(#)SAPJCoClient.java 
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
import com.sun.jbi.sapbc.Endpoint;
import com.sun.jbi.sapbc.Utils;
import com.sun.jbi.sapbc.extensions.SAPAddress;
import com.sun.jbi.sapbc.extensions.SAPAddressClient;
import com.sun.jbi.sapbc.extensions.SAPBinding;
import javax.jbi.JBIException;
import java.util.logging.Logger;
import java.util.logging.Level;

/**
 * An interface to define the methods required by a Bapi and IDOC connectors for
 * inbound and outbound transaction from and to SAP, respectively.
 *
 * @author  
 */
// Formally SAPConnector
public class SAPJCoClient {
	
    private static final Messages mMessages = Messages.getMessages(SAPJCoClient.class);
    private static final Logger mLogger = Messages.getLogger(SAPJCoClient.class);
  
    private static final String newline = System.getProperty("line.separator");
      
    /**
     * Property: Client.Application_Server_Hostname
     */
    private static final String APP_SERVER_HOST = "ApplicationServerHostname";
    private static final String ROUTER_STRING   = "Router";
    private static final String SYSTEM_NUMBER   = "SystemNumber";
    private static final String CLIENT          = "Client";
    private static final String USER            = "User";
    private static final String PASSWORD        = "Password";
    private static final String LANGUAGE        = "Language";
    private static final String TRACE           = "EnableRFCTrace";
    private static final String ABAP4_DEBUG     = "EnableABAP4Debug";
    private static final String GATEWAY_HOST    = "GatewayHostname";
    private static final String GATEWAY_SERVICE = "GatewayService";
    private static final String SYSTEM_ID       = "SystemID";
    private static final String MESSAGE_SERVER  = "MessageServer";
    private static final String APPSERVER_GROUP = "ApplicationServerGroup";
    private static final String TID_DATABASE    = "TIDVerificationDatabase";
    private static final String MAX_DB_ROWS     = "MaxTIDDatabaseRows";
    //QAI 78784, Adding manual and automatic modes of client connection.
    private static final String AUTOMATIC       = "Automatic";
    private static final String MANUAL          = "Manual";
    
    /** JCO client handle
     */
    private JCO.Client mJcoClient = null;

    private String mClient;
    private String mUser;
    private String mPassword;
    private String mLanguage;
    private String mAppServHost;
    private String mSystemNumber;
    private String mSystemID;
    private boolean mRfcTrace;
    private int mRfcTraceLevel;
    private String mGatewayHostname = null;
    private String mGatewayService = null;         
    private String mMessageServer = null;          
    private String mAppServerGroup = null;         
    private String mRouter = null;                 
    private String mTransactionMode = null;        
    private String mTidVerificationDB = null;      
    private String mMaxTIDRows = null;             
    private boolean mEnableAbapDebug ; 
    private boolean mUseLoadBalancing ;       
    private String mConnectMode = null;
    
    private int mConnMaxRetries;
    private int mConnRetryInterval;
    
    //private ObjectReference mMonitor;
    
    private String mJcoAppServStr = "UNKNOWN";
    private String mJcoUserStr = "UNKNOWN";
    
    
    /**
     * Constructor
     *
     * @param endpoint - endpoint values
     */
    public SAPJCoClient(Endpoint endpoint) 
    throws JBIException, SAPException {
        SAPBinding bind = endpoint.getSAPBinding();
        SAPAddress addr = endpoint.getSAPAddress();
        SAPAddressClient clientAddr = addr.getSAPAddressClient();
        
        setAppServerGroup(clientAddr.applicationServerGroup());
        setAppServerHost(addr.applicationServer());
        setClient(addr.clientNumber());
        //setConnectionMode();
        //setConnectionRetries();
        //setConnectionRetryInterval();
        setEnableAbapDebug(addr.enableAbapDebugWindow());
        setGatewayHostname(addr.gateway());
        setGatewayService(addr.gatewayService());
        setLanguage(addr.language());
        setMaxTIDRows(new Long(bind.maxTransactionIdRows()).toString());
        setMessageServer(clientAddr.messageServerHostname());
        setPassword(addr.password());
        //setRfcTrace();
        //setRfcTraceLevel();
        setRouter(addr.routerString());
        setSystemID(addr.systemId());
        setSystemNumber(addr.systemNumber());
        setTidVerificationDB(bind.transactionIdDatabase());
        setTransactionMode(bind.transactionalMode().toString());
        setUseLoadBalancing(clientAddr.useLoadBalancing());
        setUser(addr.username());       
        
        displayVals();
        
        initConnection();
    }
    
    public void displayVals() 
    throws JBIException, SAPException {
        Utils.checkLog(mLogger, Level.INFO, "ValueIs", new Object[]{"getAppserverGroup", getAppServerGroup()});
        Utils.checkLog(mLogger, Level.INFO, "ValueIs", new Object[]{"getAppServerHost", getAppServerHost()});
        Utils.checkLog(mLogger, Level.INFO, "ValueIs", new Object[]{"getClient", getClient()});
        Utils.checkLog(mLogger, Level.INFO, "ValueIs", new Object[]{"getConnectionMode", getConnectionMode()});
        Utils.checkLog(mLogger, Level.INFO, "ValueIs", new Object[]{"getConnectionRetries", getConnectionRetries()});
        Utils.checkLog(mLogger, Level.INFO, "ValueIs", new Object[]{"getConnectionRetryInterval", getConnectionRetryInterval()});
        Utils.checkLog(mLogger, Level.INFO, "ValueIs", new Object[]{"getEnableAbapDebug", getEnableAbapDebug()});
        Utils.checkLog(mLogger, Level.INFO, "ValueIs", new Object[]{"getGatewayHostname", getGatewayHostname()});
        Utils.checkLog(mLogger, Level.INFO, "ValueIs", new Object[]{"getGatewayService", getGatewayService()});
        Utils.checkLog(mLogger, Level.INFO, "ValueIs", new Object[]{"getLanguage", getLanguage()});
        Utils.checkLog(mLogger, Level.INFO, "ValueIs", new Object[]{"getMaxTIDRows", getMaxTIDRows()});
        Utils.checkLog(mLogger, Level.INFO, "ValueIs", new Object[]{"getMessageServer", getMessageServer()});
        //Utils.checkLog(mLogger, Level.INFO, "ValueIs", new Object[]{"getPassword", getPassword()});
        Utils.checkLog(mLogger, Level.INFO, "ValueIs", new Object[]{"getRfcTrace", getRfcTrace()});
        Utils.checkLog(mLogger, Level.INFO, "ValueIs", new Object[]{"getRfcTraceLevel", getRfcTraceLevel()});
        Utils.checkLog(mLogger, Level.INFO, "ValueIs", new Object[]{"getRouter", getRouter()});
        Utils.checkLog(mLogger, Level.INFO, "ValueIs", new Object[]{"getSystemID", getSystemID()});
        Utils.checkLog(mLogger, Level.INFO, "ValueIs", new Object[]{"getSystemNumber", getSystemNumber()});
        Utils.checkLog(mLogger, Level.INFO, "ValueIs", new Object[]{"getTidVerificationDB", getTidVerificationDB()});
        Utils.checkLog(mLogger, Level.INFO, "ValueIs", new Object[]{"getTransactionMode", getTransactionMode()});
        Utils.checkLog(mLogger, Level.INFO, "ValueIs", new Object[]{"getUseLoadBalancing", getUseLoadBalancing()});
        Utils.checkLog(mLogger, Level.INFO, "ValueIs", new Object[]{"getUser", getUser()});
        
    }
    
    /**
     * Set client
     *
     * @param client - client string
     */
    public void setClient(String client) {
        mClient = client;
    }

    /**
     * Get client
     *
     * @return client string
     */
    public String getClient() {
        return mClient;
    }

    /**
     * Set user
     *
     * @param user - user name
     */
    public void setUser(String user) {
        mUser = user;
        mJcoUserStr = user;
    }

    /**
     * Get user
     *
     * @return user name
     */
    public String getUser() {
        return mUser;
    }

    /**
     * Set password
     *
     * @param passwd - password string
     */
    public void setPassword(String passwd) {
        mPassword = passwd;
    }

    /**CONNRETRY_MAXRETRIES
     * Get password
     *
     * @return password string
     */
    public String getPassword() {
        return mPassword;
    }

    /**
     * Set Language
     *
     * @param lang - language string
     */
    public void setLanguage(String lang) {
        mLanguage = lang;
    }

    /**
     * Get Language
     *
     * @return language string
     */
    public String getLanguage() {
        return mLanguage;
    }

    /**
     * Set app server host name
     *
     * @param appServer - app server host name
     */
    public void setAppServerHost(String appServer) {
        mAppServHost = appServer;
        mJcoAppServStr = appServer;
    }

    /**
     * Get app server host name
     *
     * @return app server host name
     */
    public String getAppServerHost() {
        return mAppServHost;
    }

    /**
     * Set system number
     *
     * @param sysNum - system number
     */
    public void setSystemNumber(String sysNum) {
        mSystemNumber = sysNum;
    }

    /**
     * Get system number
     *
     * @return system number
     */
    public String getSystemNumber() {
        return mSystemNumber;
    }

    /**
     * Set system id
     *
     * @param sysID - system id
     */
    public void setSystemID(String sysID) {
        mSystemID = sysID;
    }

    /**
     * Get system id
     *
     * @return system id
     */
    public String getSystemID() {
        return mSystemID;
    }

    /**
     * Set RFC trace
     *
     * @param rfcTrace - RFC trace
     */
    public void setRfcTrace(String rfcTrace) {
       if(rfcTrace.equalsIgnoreCase("YES")) {
         mRfcTrace = true;	
       } else {
       	 mRfcTrace = false;	
       }
    }
    
    /**
     * Get RFC trace
     *
     * @param rfcTrace - RFC trace
     */
    public boolean getRfcTrace(){
      return mRfcTrace;	
    }

    /**
     * Set RFC trace level
     *
     * @param rfcTrace - RFC trace level
     */
    public void setRfcTraceLevel(int traceLvl) {
        mRfcTraceLevel = traceLvl;
    }

    /**
     * Get RFC trace level
     *
     * @return RFC trace level
     */
    public int getRfcTraceLevel() {
        return mRfcTraceLevel;
    }

    /**
     * Set gateway host name
     *
     * @param gatewayHostname - gateway host name
     */
    public void setGatewayHostname(String gatewayHost) {
        mGatewayHostname = gatewayHost;
    }

    /**
     * Get gateway host name
     *
     * @return gateway host name
     */
    public String getGatewayHostname() {
        return mGatewayHostname;
    }        
    
    /**
     * Set gateway service name
     *
     * @param gatewayService - gateway service name
     */
    public void setGatewayService(String gatewayService) {
        mGatewayService = gatewayService;
    }

    /**
     * Get gateway service name
     *
     * @return gateway service name
     */
    public String getGatewayService() {
        return mGatewayService;
    }

    /**
     * Set message server name
     *
     * @param messageServer - message server name
     */
    public void setMessageServer(String messageServer) {
        mMessageServer = messageServer;
    }

    /**
     * Get message server name
     *
     * @return message server name
     */
    public String getMessageServer() {
        return mMessageServer;
    }
            
    /**
     * Set app server group
     *
     * @param appServerGrp - app server group
     */
    public void setAppServerGroup(String appServerGrp) {
        mAppServerGroup = appServerGrp;
    }

    /**
     * Get app server group
     *
     * @return app server group
     */
    public String getAppServerGroup() {
        return mAppServerGroup;
    }
            
    /**
     * Set router
     *
     * @param router - router name
     */
    public void setRouter(String router) {
        mRouter = router;
    }

    /**
     * Get router
     *
     * @return router name
     */
    public String getRouter() {
        return mRouter;
    }    
        
    /**
     * Set transaction mode
     *
     * @param transMode - transaction mode
     */
    public void setTransactionMode(String transMode) {
        mTransactionMode = transMode;
    }

    /**
     * Get transaction mode
     *CONNRETRY_MAXRETRIES
     * @return transaction mode
     */
    public String getTransactionMode() {
        return mTransactionMode;
    }
    
    //QAI 78784
    public void setConnectionMode(String connectMode) {
        mConnectMode = connectMode;
    }

    //QAI 78784
    public String getConnectionMode() {
        return mConnectMode;
    }
    
    /**
     * Set TID verification DB name
     *
     * @param tidDB - TID verification DB name
     */
    public void setTidVerificationDB(String tidDB) {
        mTidVerificationDB = tidDB;
    }

    /**
     * Get TID verification DB name
     *
     * @return TID verification DB name
     */
    public String getTidVerificationDB() {
        return mTidVerificationDB;
    }
        
    /**
     * Set max TID rows
     *
     * @param maxTidRows - max TID rows
     */
    public void setMaxTIDRows(String maxTidRows) {
        mMaxTIDRows = maxTidRows;
    }

    /**
     * Get max TID rows
     *
     * @return max TID rows
     */
    public String getMaxTIDRows() {
        return mMaxTIDRows;
    }
        
    /**
     * Set use load balancing flag
     *
     * @param useLoadBal - use load balancing flag
     */
    public void setUseLoadBalancing(boolean useLoadBal) {
        mUseLoadBalancing = useLoadBal;
    }
    
    /**
     * Get use load balancing flag
     *
     * @return use load balancing flag
     */
    public boolean getUseLoadBalancing(){
    	return mUseLoadBalancing;
    }
    
    /**
     * Set enable ABAP debug flag
     *
     * @param enableDebugWin - enable ABAP debugging flag
     */
    public void setEnableAbapDebug(boolean enableDebug) { 
         mEnableAbapDebug = enableDebug; 
    }
    
    /**
     * Get enable ABAP debugging flag
     *
     * @return enable ABAP debugging flag
     */
    public boolean getEnableAbapDebug(){
      return 	mEnableAbapDebug;
    }    

    public void setConnectionRetries(int connMaxRetries) {
        mConnMaxRetries = connMaxRetries;
    }
    public int getConnectionRetries() {
        return mConnMaxRetries;
    }
    
    public void setConnectionRetryInterval(int connRetryInterval) {
        mConnRetryInterval = connRetryInterval;
    }
    public int getConnectionRetryInterval() {
        return mConnRetryInterval;
    }
    
    /**
     * get SAP Client Connection.
     *
     * @throws JBIException exception while retrieving connection
     */
    public void connect() throws JBIException {
        
        if ( (getClient() == null) || (getUser() == null) ||
                (getPassword() == null) || (getAppServerHost() == null) ||
                (getSystemNumber() == null) ) {
            
            String errMsg = mMessages.getString("SAPJCoClient.Bad_SAP_Address");
            mLogger.log(Level.SEVERE, errMsg);
            throw new JBIException(errMsg);
        }
        Utils.checkLog(mLogger, Level.FINE, "SAPJCoClient.Call_initConnection");
        try {
            initConnection();
        } catch (SAPException ex) {
            throw new JBIException(ex.getMessage(),ex);
        }
    }
    
    //QAI 78784
    /**
     * Closes the SAP Client Connection.
     *
     * @throws JBIException exception while closing connection
     */
    public void disconnect() throws JBIException, SAPException {
        StringBuffer sb = new StringBuffer();
        try{
            if (mJcoClient != null) {
                String clientNo     = verifyConnectionValue(getClient(),   CLIENT);
                String user         = verifyConnectionValue(getUser(),     USER);
                String appServer    = verifyConnectionValue(getAppServerHost(), APP_SERVER_HOST);
                String systemNumber = verifyConnectionValue(getSystemNumber(), SYSTEM_NUMBER);

                sb.append("SAPConnector.disconnect(): disconnecting RFC Client Connection for:").append(newline);
                sb.append("\tR/3 Application Server = ").append(appServer).append(newline);
                sb.append("\tSystem Number          = ").append(systemNumber).append(newline);
                sb.append("\tUser                   = ").append(user).append(newline);
                sb.append("\tClient                 = ").append(clientNo).append(newline);
                mLogger.info(sb.toString());

                mJcoClient.disconnect();
                //SAPBAPIAlertCodes.sendAlert(SAPBAPIAlertCodes.SAPBAPI_CONNECT_CLIENT_DOWN,
                //        new String[] {mJcoAppServStr, mJcoUserStr},
                //        "Client disconnected for SAP R/3 for user " + mJcoUserStr + " on host "+ mJcoAppServStr+ ".",
                //         NotificationEvent.SEVERITY_TYPE_INFO, getMonitor());
                Utils.checkLog(mLogger, Level.FINE, "SAPJCoClient.Connection_Disabled", mJcoClient.getAttributes().toString());
                
                mJcoClient = null;
            }
        } catch (com.sap.mw.jco.JCO.Exception je) {
            String errMsg = mMessages.getString("SAPJCoClient.Exception", new Object[]{"disconnect",je.getMessage()});
            mLogger.log(Level.SEVERE, errMsg);
            throw new SAPException(je);
        }

    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public JCO.Client getJCOClient() {
        return mJcoClient;
    }

    /**
     * DOCUMENT ME!
     *
     * @throws JBIException DOCUMENT ME!
     * @throws SAPException DOCUMENT ME!
     */
    public void initConnection() throws JBIException, SAPException {

        try {
            if ((getClient() == null) || (getUser() == null) ||
    	       (getPassword() == null) || (getAppServerHost() == null) ||
    	       (getSystemNumber() == null) ) {
                   
                String errMsg = mMessages.getString("SAPJCoClient.Bad_SAP_Address");
                mLogger.log(Level.SEVERE, errMsg);
                throw new JBIException(errMsg);
    	    }
            
            StringBuffer sb = new StringBuffer();
            String clientNo = verifyConnectionValue(getClient(),   CLIENT);
            String user     = verifyConnectionValue(getUser(),     USER);
            String password = verifyConnectionValue(getPassword(), PASSWORD);
            String language = verifyConnectionValue(getLanguage(), LANGUAGE);
                  
            if (getTransactionMode().equalsIgnoreCase("Transactional RFC")) {
                if( getTidVerificationDB() == null || getMaxTIDRows() == null) {
                    String errMsg = mMessages.getString("SAPJCoClient.Use_Transactional_RFC_Mode");
                    mLogger.log(Level.SEVERE, errMsg);
                    throw new JBIException(errMsg);
                }
            }
                    
    	    // Load Balancing
            mLogger.log(Level.INFO, "Use Load Balancing? ["+mUseLoadBalancing+"]");
            if (mUseLoadBalancing) { 
                String messageServer = verifyConnectionValue(getMessageServer(), MESSAGE_SERVER);
                String systemID      = verifyConnectionValue(getSystemID(),      SYSTEM_ID);
                String group         = verifyConnectionValue(getAppServerGroup(),APPSERVER_GROUP);
        
                mJcoClient = com.sap.mw.jco.JCO.createClient(clientNo, user,
                                                             password, language,
                                                             messageServer, systemID,
                                                             group);	
                sb.append("SAPConnector.initConnection(): Successfully became a Load-Balanced RFC client of:").append(newline);
                sb.append("\tR/3 Message Server = ").append(messageServer).append(newline);
                sb.append("\tGroup              = ").append(group).append(newline);
                sb.append("\tSystem ID          = ").append(systemID).append(newline);
                sb.append("\tUser               = ").append(user).append(newline);
                sb.append("\tClient             = ").append(clientNo).append(newline);
                sb.append("\tLanguage           = ").append(language);
      	
            } else { 
                // No Load Balancing
                String appServer    = verifyConnectionValue(getAppServerHost(), APP_SERVER_HOST);
                String systemNumber = verifyConnectionValue(getSystemNumber(), SYSTEM_NUMBER);
         
                // No optional Gateway
                if (getGatewayHostname() == null) { 
                    mJcoClient = com.sap.mw.jco.JCO.createClient(clientNo, user,
                                                                 password, language,
                                                                 appServer, systemNumber);
                    sb.append("SAPConnector.initConnection(): Successfully became a RFC client of:").append(newline);
                    sb.append("\tR/3 Application Server = ").append(appServer).append(newline);
                    sb.append("\tSystem Number          = ").append(systemNumber).append(newline);
                    sb.append("\tUser                   = ").append(user).append(newline);
                    sb.append("\tClient                 = ").append(clientNo).append(newline);
                    sb.append("\tLanguage               = ").append(language);
                } else {
                    // With Gateway
                    String gateway = verifyConnectionValue(getGatewayHostname(), GATEWAY_HOST);
                    String service = verifyConnectionValue(getGatewayService(), GATEWAY_SERVICE);
                    mJcoClient = com.sap.mw.jco.JCO.createClient(clientNo, user,
                                                                 password, language,
                                                                 appServer, systemNumber,
                                                                 gateway, service);
                    sb.append("SAPConnector.initConnection(): Successfully became a RFC client of:").append(newline);
                    sb.append("\tR/3 Application Server = ").append(appServer).append(newline);
                    sb.append("\tSystem Number          = ").append(systemNumber).append(newline);
                    sb.append("\tGateway                = ").append(newline);
                    sb.append("\tService                = ").append(service).append(newline);
                    sb.append("\tUser                   = ").append(user).append(newline);
                    sb.append("\tClient                 = ").append(clientNo).append(newline);
                    sb.append("\tLanguage               = ").append(language);
                }
            }
            
            if (mRfcTrace) {
                mJcoClient.setTrace(true);
                com.sap.mw.jco.JCO.setTracePath(System.getProperty("user.dir"));
                com.sap.mw.jco.JCO.setTraceLevel(getRfcTraceLevel());
            }
            else {
                com.sap.mw.jco.JCO.setTraceLevel(0);
            }


            if (mEnableAbapDebug) {
                mJcoClient.setAbapDebug(true);
                mJcoClient.setSapGui(2);
            } else {
                mJcoClient.setAbapDebug(false);	
            }       
       
            mJcoClient.connect();
            
            mJcoAppServStr = mJcoClient.getASHost();
            mJcoUserStr = mJcoClient.getUser();

            //Print strings after connection has been achieved
                Utils.checkLog(mLogger, Level.FINE, sb.toString());
                
            //SAPBAPIAlertCodes.sendAlert(SAPBAPIAlertCodes.SAPBAPI_CONNECT_CLIENT_SUCCEEDED,
            //        new String[] {mJcoAppServStr, mJcoUserStr},
            //        "Client connection established to SAP R/3 for user " + mJcoUserStr + " on host "+ mJcoAppServStr+ ".",
            //        NotificationEvent.SEVERITY_TYPE_INFO, getMonitor());
            Utils.checkLog(mLogger, Level.INFO,"SAPJCoClient.Connect_Established", mJcoClient.getAttributes().toString());
            
        } catch (com.sap.mw.jco.JCO.Exception je) {
            //SAPBAPIAlertCodes.sendAlert(SAPBAPIAlertCodes.SAPBAPI_CONNECT_CLIENT_FAILED,
            //        new String[] {mJcoAppServStr, mJcoUserStr},
            //        "Connection to SAP R/3 failed for user " + mJcoUserStr + " on host "+ mJcoAppServStr+ ".",
            //        NotificationEvent.SEVERITY_TYPE_FATAL, getMonitor());
            
            String errMsg = mMessages.getString("SAPJCoClient.Exception", new Object[]{"initConnection",je.getMessage()});
            mLogger.log(Level.SEVERE, errMsg);
            errMsg = mMessages.getString("SAPJCoClient.Unable_To_Connect_To_SAP");
            mLogger.log(Level.SEVERE, errMsg);
            throw new SAPException(je);
            
        }
        
        if (!getSystemID().equals(mJcoClient.getAttributes().getSystemID())) { 
            mJcoClient.disconnect();
            
            String errMsg = mMessages.getString("SAPJCoClient.ID_Not_Match", new Object[]{getSystemID(),mJcoClient.getAttributes().getSystemID(),getAppServerHost()});
            mLogger.log(Level.SEVERE, errMsg);
            throw new JBIException(errMsg);
        }   	
 
    } // end initConnection()
    
    public boolean isConnected() {
        if (mJcoClient != null) {
          mJcoAppServStr = mJcoClient.getASHost();
          mJcoUserStr = mJcoClient.getUser();
            if (mJcoClient.isAlive() &&
                    mJcoClient.isValid()) {
                //SAPBAPIAlertCodes.sendAlert(SAPBAPIAlertCodes.SAPBAPI_CONNECT_CONNECT_UP,
                //        new String[] {mJcoAppServStr, mJcoUserStr},
                //        "A connection is established to SAP R/3 for user " + mJcoUserStr + " on host "+ mJcoAppServStr+ ".",
                //         NotificationEvent.SEVERITY_TYPE_INFO, getMonitor());
                Utils.checkLog(mLogger,Level.FINE,"SAPJCoClient.Connection_Established", mJcoClient.getAttributes().toString());
                return true;
            } else {
              mLogger.log(Level.WARNING,"SAPJCoClient.JCo_Client_Alive",new Object[]{mJcoClient.isAlive(), mJcoClient.isValid()});
            }
        } else {
              mLogger.log(Level.WARNING,"SAPJCoClient.JCo_Client_Null");
        }
        //SAPBAPIAlertCodes.sendAlert(SAPBAPIAlertCodes.SAPBAPI_CONNECT_CLIENT_DOWN,
        //        new String[] {mJcoAppServStr, mJcoUserStr},
        //        "No connection established to SAP R/3 for user " + mJcoUserStr + " on host "+ mJcoAppServStr+ ".",
        //        NotificationEvent.SEVERITY_TYPE_WARNING, getMonitor());
              mLogger.log(Level.WARNING,"SAPJCoClient.JCo_Client_Not_Connected");
        return false;
    }
    
    public boolean isValid() {
        if (mJcoClient != null) {
            if (mJcoClient.isValid()) {
                return true;
            }
        }
        return false;
    }

    public boolean isAlive() {
        if (mJcoClient != null) {
            if (mJcoClient.isAlive()) {
                return true;
            }
        }
        return false;
    }
    
    protected String verifyConnectionValue(String value, String propKey)
        throws JBIException { 
        if (value == null) {
            String errMsg = mMessages.getString("SAPJCoClient.Connection_Value_Null", propKey);
            mLogger.log(Level.SEVERE, errMsg);
            throw new JBIException(errMsg);
        }
        return value;
    }

    /**
     * Reset connection
     */
    public void reset() {
        if (mJcoClient != null) {
            Utils.checkLog(mLogger, Level.FINE, "SAPJCoClient.Disconnecting_JCo_Client");
            mJcoClient.disconnect();
            mJcoClient = null;
        }
    }
  
    /*
    public void setMonitor(ObjectReference mbean) {
      mMonitor = mbean;     
    }

    public ObjectReference getMonitor() {
      return mMonitor;
    }
     */
}
