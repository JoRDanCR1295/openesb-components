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
 * @(#)InboundMessageProcessor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ParameterMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.naming.Context;
import javax.naming.NamingException;
import javax.sql.DataSource;
import javax.sql.XAConnection;
import javax.transaction.Status;
import javax.transaction.Transaction;
import javax.transaction.TransactionManager;
import javax.transaction.xa.XAResource;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import com.sun.jbi.internationalization.Messages;
import org.glassfish.openesb.databasebc.model.metadata.DBMetaData;
import org.glassfish.openesb.databasebc.model.runtime.DBConnectionInfo;
import org.glassfish.openesb.databasebc.model.runtime.DatabaseModel;
import org.glassfish.openesb.databasebc.model.runtime.DatabaseModelImpl;
import org.glassfish.openesb.databasebc.model.runtime.Db2DataAccess;
import org.glassfish.openesb.databasebc.model.runtime.DerbyDataAccess;
import org.glassfish.openesb.databasebc.model.runtime.OracleDataAccess;
import org.glassfish.openesb.databasebc.model.runtime.SqlServerDataAccess;
import org.glassfish.openesb.databasebc.transaction.TransactionHelper;
import org.glassfish.openesb.databasebc.transaction.XidImpl;
import com.sun.jbi.nms.exchange.ExchangePattern;

import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.qos.redelivery.Redelivery;
import com.sun.jbi.common.qos.redelivery.RedeliveryStatus;

import net.java.hulp.measure.Probe;

/**
 * author : Venkat P Process requests received from the External Database
 */
public class InboundMessageProcessor implements Runnable, MessageExchangeReplyListener, RedeliveryListener {
    private static final String DERBY_PROD_NAME = "DERBY";

    private static final String ORACLE_PROD_NAME = "ORACLE";

    private static final String SQLSERVER_PROD_NAME = "SQLSERVER";

    private static final String DB2_PROD_NAME = "DB2";

    private static final String JDBC_PROD_NAME = "JDBC";

    private XidImpl xid = null;

    private TransactionHelper mTxHelper = null;
    private static final String SENT_TO_NMR = "SENT";

    // private boolean mtxFlag;

    XAResource xaResource = null;

    //private TransactionManager mTxManager = null;

    private static final Messages mMessages = Messages.getMessages(InboundMessageProcessor.class);

    private static final Logger mLogger = Messages.getLogger(InboundMessageProcessor.class);

    @SuppressWarnings("unchecked")
    private static final Map mInboundExchanges = Collections.synchronizedMap(new HashMap());

    private static Map exchangeIDToMeta = Collections.synchronizedMap(new HashMap());   
    private Map mMapInboundExchangesProcessRecords = new HashMap();
    private ArrayList mProcessedList = new ArrayList();

    
    Map mEndpoint;

    EndpointBean epb;

    DocumentBuilder mDocBuilder;

    //private DeliveryChannel mChannel;
    private MessagingChannel mChannel;

    private MessageExchange mExchange;

    private ComponentContext mContext;
    private RuntimeConfiguration mRuntimeConfig;

    private MessageExchangeFactory mMsgExchangeFactory;

    private ServiceEndpoint mServiceEndpoint;

    private final QName mOperation;

    private AtomicBoolean mMonitor;

    private String mPKName = null;

    private String mSelectSQL = null;

    private String mMarkColumnName = null;

    private String mMarkColumnValue = null;

    private String mPKType = null;

    private String mFlagColumnType = null;

    private String mSchemaName = null;

    DBConnectionInfo dbConnectionInfo;

    private String mXAEnabled = null;

    // private DBMetaData mdbMetaData = null;
    private DatabaseModel dbDataAccessObject = null;

    PreparedStatement ps = null;

	ResultSet rs = null;
	private int mRowCount = 0;

    Connection connection = null;

    Connection con = null;

    XAConnection xaConnection = null;
    
    Connection mClusterConnection = null;

    private String mTableName = null;
    private String mDbName=null;

    private String mPollingPostProcessing = null;

    private String mMoveRowToTableName = null;

    private int mPollMilliSeconds = 10000;

    private int mThrottleNumber = -1;
    private JDBCClusterManager mJDBCClusterManager = null;

    // Settings for custom reliability header extensions
    public static final String CUSTOM_RELIABILITY_MESSAGE_ID_PROPERTY = "com.stc.jbi.messaging.messageid"; // NOI18N
    public static final String CUSTOM_RELIABILITY_HEADER_NAMESPACE_URI = "http://schemas.stc.com/ws/2005/07/custrm"; // NOI18N
    public static final String CUSTOM_RELIABILITY_HEADER_LOCAL_NAME = "MessageID"; // NOI18N
    // this JNDI is used if the component is installed in cluster environment/
    // TODO, need to change it later
    public static final String DEFAULT_CLUSTER_JNDI_NAME = "jdbc/__defaultDS";

    /**
     * JBI message exchange properties for message grouping and sequencing (new CRMP)
     */
    public static final String CRMP_GROUP_ID = "com.sun.jbi.messaging.groupid";
    public static final String CRMP_MESSAGE_ID = "com.sun.jbi.messaging.messageid";

    ReplyListener replyListener;

    public InboundMessageProcessor(final MessagingChannel chnl, final EndpointBean endpoint,
            final ComponentContext context, final QName opname) throws ParserConfigurationException {
        mChannel = chnl;
        epb = endpoint;
        mContext = context;
        replyListener = new ReplyListenerImpl(endpoint);
        mOperation = opname;
        mMonitor = new AtomicBoolean(false);
        mTxHelper = new TransactionHelper();
        dbConnectionInfo = new DBConnectionInfo();
        final DocumentBuilderFactory docBuilderFact = DocumentBuilderFactory.newInstance();
        mDocBuilder = docBuilderFact.newDocumentBuilder();
        if(endpoint.isClustered()){
            try{
                mJDBCClusterManager = new JDBCClusterManager(context);
            }catch(Exception e){
                //TODO
            }
        }

    }

    /**
     * @return
     */
    public static Map getInboundExchanges() {
        return InboundMessageProcessor.mInboundExchanges;
    }

    /**
     *
     */
    //@Override
    public void run() {
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "DBBC_R00629.IMP_EP_status");
        }

        do {
            mRowCount = 0;
            try {
                execute();
            } catch (final Exception ex) {
                mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E00659.IMP_ERROR_WHILE_EXECUTING_SQL"), ex);
            }

            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.INFO,mMessages.getString("DBBC_R00660.IMP_FINISHED_EXECUTING_SQL"));
            }

            if (mRowCount <= 0) {
                try {
                    Thread.sleep(mPollMilliSeconds);
                } catch (final Exception e) {
                    mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E00661.IMP_THREAD_SLEEP_ABRUPTED"), e);
                }
            }
        } while (mMonitor.get() != Boolean.TRUE);
    }

    /**
     * @throws MessagingException
     * @throws Exception
     */
    public void execute() throws MessagingException, Exception {
        String exchangeId = null;

        try {
            if (mMsgExchangeFactory == null) {
                mMsgExchangeFactory = mChannel.createExchangeFactory();
            }

            mExchange = mMsgExchangeFactory.createInOnlyExchange();

            if (mServiceEndpoint == null) {
                mServiceEndpoint = locateServiceEndpoint();
                epb.setValueObj(EndpointBean.ENDPOINT_REFERENCE, mServiceEndpoint);
            }

            if (mServiceEndpoint == null) {
                throw new MessagingException(mMessages.getString("DBBC_E00643.IMP_Failed_locate_EP"));
            }

            exchangeId = mExchange.getExchangeId();

            final QName serviceName = (QName) epb.getValueObj(EndpointBean.FULL_SERVICE_NAME);
            final String epntName = epb.getValue(EndpointBean.ENDPOINT_NAME);

            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.fine("Getting bean for" + serviceName + epntName);
            }
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.fine("Getting bean for" + serviceName + epntName);
            }

            mExchange.setEndpoint(mServiceEndpoint);
            mExchange.setOperation(mOperation);

            // Adding re-delivery/re-try support
            // we are sending instead of the client context to the ReplyListener 
            // the EndpointBean
            MessageExchangeSupport.addReplyListener(mExchange.getExchangeId(), replyListener, epb);
            MessageExchangeSupport.addRedeliveryListener(mExchange.getExchangeId(), this, epb);
            Redelivery.setUniqueId(mExchange, exchangeId);
            final String status = epb.getValue(EndpointBean.STATUS);

            if (! status.equalsIgnoreCase(EndpointBean.STATUS_RUNNING)) {
                final String endName = epb.getValue(EndpointBean.ENDPOINT_NAME);

                if (mLogger.isLoggable(Level.FINEST)) {
                    mLogger.log(Level.FINEST, "DBBC_W00630.IMP_EP_NOT_RUNNING", new Object[] { endName,
                            mExchange.getExchangeId() });
                } else if (mLogger.isLoggable(Level.INFO)) {
                    mLogger.info("DBBC_W00667.IMP_EP_NOT_RUNNING");
                }
            } else {

                switch (ExchangePattern.valueOf(mExchange)) {
                case IN_OUT:
                    mLogger.log(Level.INFO, "DBBC_R00631.IMP_Received_INOUT", mExchange.getExchangeId());
                    processInOut(mExchange, epb);
                    break;
                case IN_ONLY:
                    mLogger.log(Level.INFO, "DBBC_R00632.IMP_Received_INONLY", mExchange.getExchangeId());
                    processInOnly(mExchange, epb);
                    break;
                default:
                    mLogger.log(Level.INFO, "DBBC_E00633.IMP_Invalid_pattern", mExchange.getExchangeId());
                    return;
                }
            }
        } catch (final MessagingException ex) {
            InboundMessageProcessor.mInboundExchanges.remove(exchangeId);
            mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E00662.IMP_ERROR_WHILE_EXECUTING_MEP"), exchangeId);
            throw ex;
        } catch (final Exception e) {
            InboundMessageProcessor.mInboundExchanges.remove(exchangeId);
            mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E00662.IMP_ERROR_WHILE_EXECUTING_MEP"), exchangeId);
            throw e;
        }
    }

    /**
     * @param exchange
     * @param epb
     */
    public void processInOut(final MessageExchange exchange, final EndpointBean epb) {
    }

    /**
     * @param exchange
     * @param epb
     */
    public void processInOnly(final MessageExchange exchange, final EndpointBean epb) throws Exception {
        String exchangeId = null;
        String jndiName = null;

        try {
            epb.getEndpointStatus().incrementReceivedRequests();

            NormalizedMessage inMsg = mExchange.createMessage();
            exchangeId = exchange.getExchangeId();

            final Map operationNameToMetaData = (Map) epb.getValueObj(EndpointBean.OPERATION_NAME_TO_META_DATA);
            final OperationMetaData meta = (OperationMetaData) operationNameToMetaData.get(exchange.getOperation().getLocalPart());

            if (meta == null) {
                throw new MessagingException(InboundMessageProcessor.mMessages.getString("DBBC_E00634.IMP_Invalid_Operation",
                    new Object[] { exchange.getOperation() }));
            }

            mPollMilliSeconds = meta.getJDBCSql().getPollMilliSeconds();
            mSelectSQL = meta.getJDBCSql().getSql();
            mPKName = meta.getJDBCSql().getPKName();
            mMarkColumnName = meta.getJDBCSql().getMarkColumnName();
            mMarkColumnValue = meta.getJDBCSql().getMarkColumnValue();
            mTableName = meta.getJDBCSql().getTableName();
            mPollingPostProcessing = meta.getJDBCSql().getPollingPostProcessing();
            mMoveRowToTableName = meta.getJDBCSql().getMoveRowToTableName();
            mXAEnabled = meta.getJDBCSql().getTransaction();
            jndiName = epb.getValue(EndpointBean.JDBC_DATABASE_JNDI_NAME);

            // Throttle Check
            if(throttleConfigurationCheck()) {
                if (mXAEnabled.equalsIgnoreCase("XATransaction")) {
                    getTransactionManager().begin();
                }

                dbDataAccessObject = getDataAccessObject(meta);
                mSelectSQL = dbDataAccessObject.generateSelectQuery(mSelectSQL, mTableName);
                epb.setTableName(mTableName);

                connection = getDatabaseConnection(jndiName);
                if (mDbName==null){
                    mDbName = connection.getMetaData().getDatabaseProductName().toLowerCase();
                }
                String clusterJNDIName = mRuntimeConfig.getProperties().getProperty(RuntimeConfiguration.CONFIG_CLUSTER_DATABASE_JNDINAME);
                if(epb.isClustered()){
                    try{
                        if(jndiName.equalsIgnoreCase(clusterJNDIName)){
                            mClusterConnection = connection;
                            mJDBCClusterManager.setJNDIName(clusterJNDIName);
                            String prdtName = DBMetaData.getDBType(mClusterConnection);
                            mJDBCClusterManager.setProductName(prdtName);
                        }else{
                            mClusterConnection = getDatabaseConnection(mRuntimeConfig.getProperties().getProperty(RuntimeConfiguration.CONFIG_CLUSTER_DATABASE_JNDINAME));
                            mJDBCClusterManager.setJNDIName(mRuntimeConfig.getProperties().getProperty(RuntimeConfiguration.CONFIG_CLUSTER_DATABASE_JNDINAME));
                            String prdtName = DBMetaData.getDBType(mClusterConnection);
                            mJDBCClusterManager.setProductName(prdtName);
                        }
                    }catch(Exception e){
                        if(mClusterConnection == null){
                            //TODO retry;
                            throw new Exception(mMessages.getString("DBBC_E11101.JCM_CONNECTON_EXCEPTION",
                                new Object[] {mRuntimeConfig.getProperties().getProperty(RuntimeConfiguration.CONFIG_CLUSTER_DATABASE_JNDINAME)} ));
                        }
                    }
                }
                Transaction tx = getTransactionManager().getTransaction();
                if (isSelectStatement(mSelectSQL)) {
                    if(epb.isClustered()){
                        mJDBCClusterManager.setDataBaseConnection(mClusterConnection);
                        mJDBCClusterManager.setTableName(mTableName);
                        mJDBCClusterManager.setInstanceName(epb.getInstanceName());
                        mJDBCClusterManager.setHeartbeatConfigInterval(mPollMilliSeconds);
                        mJDBCClusterManager.setPKName(mPKName);
                        mJDBCClusterManager.doClusterTasks();
                        mClusterConnection.setAutoCommit(false);
                    }

                    rs = executeInboundSQLSelect(epb, meta, connection, mTableName, mSelectSQL);

                    if (rs != null) {
                        final JDBCNormalizer normalizer = new JDBCNormalizer();
                            Probe normalizationMeasurement = Probe.info(getClass(),
                                epb.getUniqueName(), JDBCBindingLifeCycle.PERF_CAT_NORMALIZATION);

                        normalizer.setInboundExchangeProcessRecordsMap(mMapInboundExchangesProcessRecords);
                        normalizer.setRecordsProcessedList(mProcessedList);
                        inMsg = normalizer.normalizeSelectInbound(rs, exchange, meta, epb, mPKName,mDbName);
                        mRowCount = normalizer.mRowCount;

                        if(normalizationMeasurement != null){
                            normalizationMeasurement.end();
                        }

                        final List tempList = epb.getProcessList();
                        if (!(tempList.isEmpty()))
                        {
                            if (epb.isClustered())
                            {
                                mJDBCClusterManager.addInstances(tempList);
                                mClusterConnection.setAutoCommit(true);
                            }
                            //set JNDI name on NormalizedMessage for dynamic addressing
                            inMsg.setProperty(JDBCComponentContext.NM_PROP_DATABASEBC_CONNECTION_JNDI_NAME, jndiName);
                            exchange.setMessage(inMsg, "in");

                            if (tx != null) {
                                mExchange.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME, tx);
                                getTransactionManager().suspend();
                            }

                            mInboundExchanges.put(exchangeId, new ListenerMeta(
                                System.currentTimeMillis(), this));

                            mChannel.send(exchange);
                            epb.getEndpointStatus().incrementSentRequests();
                            if(epb.isClustered()){
                                //Records already sent to NMR so update the status to "SENT" for owner table
                                try{
                                    int i[] = mJDBCClusterManager.updateStatus(tempList, "SENT");
                                    mLogger.log(Level.INFO,
                                        "DBBC_R10906.IMP_UPDATED_STATUS_TO_SENT",
                                        new Object[] { tempList });
                                }catch(Exception e){
                                    // TODO need to handled the exception
                                    mLogger.log(Level.SEVERE,
                                        "DBBC_E11108.IMP_ERROR_UPDATING_STATUS_TO_SENT",
                                        new Object[] { tempList, e.getLocalizedMessage() });
                                }
                            }
                        } else {
                            if (epb.isClustered())
                                mClusterConnection.setAutoCommit(true);
                            if (tx != null) {
                                try {
                                    tx.commit();
                                } catch (Exception ex) {
                                    mLogger.log(Level.SEVERE,
                                        "DBBC_E00656.IMP_XA_TX_COMMIT_FAILED",
                                        new Object[] { "commit", ex });
                                    throw ex;
                                }
                            } else {
                                if (mXAEnabled.equalsIgnoreCase("XATransaction")) {
                                    mLogger.log(Level.WARNING,
                                       "DBBC_W00654.IMP_XA_TX_NOT_FOUND_IN_MSG_XCHANGE",
                                       new Object[] { exchange.getExchangeId() });
                                }
                            }
                        }
                    }
                    else if (epb.isClustered())
                        mClusterConnection.setAutoCommit(true);
                }
            }
        } catch (final Exception ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E00663.IMP_ERROR_WHILE_PROCESSING_MEP"), ex);
            Transaction tx = getTransactionManager().getTransaction();
            if (tx != null) {
                tx.rollback();
            }
        } finally {
            try {
                if (rs != null) {
                    rs.close();
                }
            } catch(SQLException se){
                mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E11109.IMP_EXCEPTION_WHILE_CLOSING_THE_RS"), se);
            }
            try{
                if (ps != null) {
                    ps.close();
                }
            }catch(SQLException se){
                mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E11110.IMP_EXCEPTION_WHILE_CLOSING_THE_PS"), se);
            }
            try{
                if (connection != null) {
                    connection.close();
                }
            }catch(SQLException se){
                mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E11111.IMP_EXCEPTION_WHILE_CLOSING_THE_CONNECTION"), se);
            }
            try{
                if (mClusterConnection != null && mClusterConnection != connection){
                    mClusterConnection.close();
                    mClusterConnection = null;
                }
            }catch(SQLException se){
                mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E11111.IMP_EXCEPTION_WHILE_CLOSING_THE_CONNECTION"), se);
            }
        }
    }

    /** Checks if the Throttling configuration is defined on the endpoint,
     * if yes then checks if the messages in the system are within the throttle limit
     * @param 
     * @return boolean
     */
    public boolean throttleConfigurationCheck() {
        
        synchronized(mInboundExchanges) {
            int pendingMsgs = mInboundExchanges.size();
            mThrottleNumber = epb.getMaxConcurrencyLimit();
            if (mThrottleNumber > 0 ) {
                if(pendingMsgs > mThrottleNumber){
                    if (mLogger.isLoggable(Level.FINEST)) {
                        mLogger.log(Level.FINEST, mMessages.getString("DBBC_R00664.IMP_THROTTLE_LIMIT_REACHED", 
                                            new Object[] { Integer.toString(pendingMsgs), Integer.toString(mThrottleNumber) }));
                    } else if (mLogger.isLoggable(Level.INFO)) {
                        mLogger.info(mMessages.getString("DBBC_R00668.IMP_THROTTLE_LIMIT_REACHED",
                            new Object[] { Integer.toString(mThrottleNumber) }));
                    }
                    return false;
                } else {
                    if (mLogger.isLoggable(Level.FINEST)) {
                        mLogger.log(Level.FINEST, mMessages.getString("DBBC_R00665.IMP_THROTTLE_LIMIT_NOT_REACHED", 
                                            new Object[] { Integer.toString(pendingMsgs), Integer.toString(mThrottleNumber) }));
                    } else if (mLogger.isLoggable(Level.INFO)) {
                        mLogger.log(Level.INFO, mMessages.getString("DBBC_R00669.IMP_THROTTLE_LIMIT_NOT_REACHED"), 
                            new Object[] { Integer.toString(mThrottleNumber) });
                    }
                    return true;
                }
            }
            mLogger.log(Level.INFO, mMessages.getString("DBBC_R00666.IMP_THROTTLE_NOT_DEFINED"));                                    
            return true;
        }
    }

    public ResultSet executeInboundSQLSelect(final EndpointBean epb,
                                      final OperationMetaData opMetaData,
                                      Connection connection,
                                      final String mTableName,
                                      String lSelectSQL) throws MessagingException {
        try {
            String jndiName = epb.getValue(EndpointBean.JDBC_DATABASE_JNDI_NAME);
            mLogger.log(Level.INFO, InboundMessageProcessor.mMessages.getString("DBBC_R00629.OMP_UsedJNDI") + jndiName);
            String where = "";
            List<String> bind = new ArrayList<String>();
            if (mMarkColumnName != null && !mMarkColumnName.equals("")) {
                if (mFlagColumnType != null) {
                    where = "("+mMarkColumnName+" != ? OR "+mMarkColumnName+" IS NULL)";
                    bind.add(mMarkColumnValue);
                } else {
                     final String msg = InboundMessageProcessor.mMessages.getString("DBBC_E00638.IMP_Error_IVALID_ColumnName") + mMarkColumnName;
                     throw new MessagingException(msg, new NamingException());
                }
            }
            if (epb.isClustered()) {
                List<String> pkList = mJDBCClusterManager.selectAllProcessed();
                if (pkList.size() > 0) {
                    StringBuilder sb = new StringBuilder();
                    for (int i = 0, l = pkList.size(); i < l; i++)
                        sb.append(i < l-1 ? "?," : "?");
                    where = (where.equals("") ? "" : where+" AND ")+mPKName+" NOT IN ("+sb.toString()+")";
                    bind.addAll(pkList);
                }
            }
            lSelectSQL = lSelectSQL.replace("$WHERE", where.equals("") ? "1=1" : where);
            mLogger.log(Level.INFO, "Executing sql 1. " + lSelectSQL);
            ps = connection.prepareStatement(lSelectSQL);
            ParameterMetaData paramMetaData = ps.getParameterMetaData();
            for (int i = 0, l = bind.size(); i < l; i++)
            {
                int columnType = java.sql.Types.VARCHAR;
                try { columnType = paramMetaData.getParameterType(i+1); } catch(Exception e) {}
                ps.setObject(i+1, JDBCUtil.convert(bind.get(i), columnType), columnType);
            }
            rs = ps.executeQuery();
        }
        catch (final SQLException ex) {
            if (mLogger.isLoggable(Level.FINEST)) {
                mLogger.log(Level.FINEST, mMessages.getString("DBBC_E00639.IMP_Failed_Executing_SQL") + lSelectSQL, ex);
            } else if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("DBBC_E00639.IMP_Failed_Executing_SQL") + lSelectSQL);
            } else if (mLogger.isLoggable(Level.INFO)) {
                mLogger.info(mMessages.getString("DBBC_E00639.IMP_Failed_Executing_SQL"));
            }
            final String msg = InboundMessageProcessor.mMessages.getString("DBBC_E00639.IMP_Failed_Executing_SQL") + lSelectSQL +
                    "Reason: " + ex.getLocalizedMessage() + " SQLState: " + ex.getSQLState() + " ErrorCode: " + ex.getErrorCode();
            throw new MessagingException(msg, ex);
        } catch (final Exception ex) {
            final String msg = InboundMessageProcessor.mMessages.getString("DBBC_E00639.IMP_Failed_Executing_SQL") + lSelectSQL
                    + ex.getLocalizedMessage();
            throw new MessagingException(msg, ex);
        }
        return rs;
    }

    /**
     * @return
     * @throws MessagingException
     */
    public DatabaseModel getDataAccessObject(OperationMetaData meta) throws MessagingException {
        DatabaseModel objDataAccess = null;
        String jndiName = null;
        String prdtName = null;
        String catalog = null;
        jndiName = epb.getValue(EndpointBean.JDBC_DATABASE_JNDI_NAME);
        Connection connection = null;
        ResultSet rs = null;

        try {
            connection = getDatabaseConnection(jndiName);
            prdtName = DBMetaData.getDBType(connection);

            rs = connection.getMetaData().getColumns(catalog, mSchemaName, mTableName, "%");

            int noofColCounter = -1;
            while (rs.next()) {
                noofColCounter++;
                final String colName = rs.getString("COLUMN_NAME");
                if (colName.equalsIgnoreCase(meta.getJDBCSql().getPKName())) {
                    final String defaultValue = rs.getString("COLUMN_DEF");
                    final int sqlTypeCode = rs.getInt("DATA_TYPE");
                    final String sqlType = DBMetaData.getSQLTypeDescription(sqlTypeCode);
                    mPKType = sqlType;
                }
                if (colName.equalsIgnoreCase(meta.getJDBCSql().getMarkColumnName())) {
                    final String defaultValue = rs.getString("COLUMN_DEF");
                    final int sqlTypeCode = rs.getInt("DATA_TYPE");
                    final String sqlType = DBMetaData.getSQLTypeDescription(sqlTypeCode);
                    mFlagColumnType = sqlType;
                }
            }
            if(noofColCounter < 0 ){
                final String msg = InboundMessageProcessor.mMessages.getString("DBBC_E00636.IMP_Table_NotExist");
                throw new MessagingException(msg, new NamingException());
            }
            if (mPKType == null) {
                final String msg = InboundMessageProcessor.mMessages.getString("DBBC_E00637.IMP_PrimaryKey_Error");
                throw new MessagingException(msg, new NamingException());
            }
            if (prdtName.equalsIgnoreCase(InboundMessageProcessor.DERBY_PROD_NAME)) {
                return objDataAccess = DerbyDataAccess.getInstance();
            } else if (prdtName.equalsIgnoreCase(InboundMessageProcessor.ORACLE_PROD_NAME)) {
                return objDataAccess = OracleDataAccess.getInstance();
            } else if (prdtName.equalsIgnoreCase(InboundMessageProcessor.DB2_PROD_NAME)) {
                return objDataAccess = Db2DataAccess.getInstance();
            } else if (prdtName.equalsIgnoreCase(InboundMessageProcessor.SQLSERVER_PROD_NAME)) {
                return objDataAccess = SqlServerDataAccess.getInstance();
            } else if (prdtName.equalsIgnoreCase(InboundMessageProcessor.JDBC_PROD_NAME)) {
                return objDataAccess = DatabaseModelImpl.getInstance();
            } else {
                return objDataAccess = new DatabaseModelImpl();
            }
        } catch (final NamingException ex) {
            final String msg = InboundMessageProcessor.mMessages.getString("DBBC_E00635.IMP_Error_Lookup") + jndiName;
            mLogger.log(Level.SEVERE, msg, ex);
            throw new MessagingException(msg, ex);
        } catch (final SQLException ex) {
            final String msg = InboundMessageProcessor.mMessages.getString("DBBC_E00639.IMP_Failed_Executing_SQL") +
                    "Reason: " + ex.getLocalizedMessage() + " SQLState: " + ex.getSQLState() + " ErrorCode: " + ex.getErrorCode();
            mLogger.log(Level.SEVERE, msg, ex);
            throw new MessagingException(msg, ex);
        } catch (final Exception ex) {
            final String msg = InboundMessageProcessor.mMessages.getString("DBBC_E00639.IMP_Failed_Executing_SQL");
            throw new MessagingException(msg, ex);
        } finally {
            try {
                if (rs != null) {
                    rs.close();
                }
            }catch(SQLException se){
                mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E11109.IMP_EXCEPTION_WHILE_CLOSING_THE_RS"), se);                    
            }
            try{
                if (connection != null) {
                    connection.close();
                }
            }catch(SQLException se){
                mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E11111.IMP_EXCEPTION_WHILE_CLOSING_THE_CONNECTION"), se);   
            }
        }
    }

    /**
     * @return
     */
    public ServiceEndpoint locateServiceEndpoint() {
        ServiceEndpoint activatedEndpoint = null;
        final QName serviceName = (QName) epb.getValueObj(EndpointBean.FULL_SERVICE_NAME);
        final String endpointName = epb.getValue(EndpointBean.ENDPOINT_NAME);
        activatedEndpoint = mContext.getEndpoint(serviceName, endpointName);

        if (activatedEndpoint != null) {
            if (mLogger.isLoggable(Level.FINEST)) {
                mLogger.log(Level.FINEST, "DBBC_E00645.IMP_locate_EP", new Object[] { serviceName, endpointName });
            } else if (mLogger.isLoggable(Level.INFO)) {
                mLogger.info(mMessages.getString("DBBC_E00645.IMP_locate_EP"));
            }
        }

        return (activatedEndpoint);
    }

    public String Qualified(String str) {
        int len = 0;

        if ((str != null) && (!str.equals(""))) {
            final int i = str.indexOf(".");

            if (i > 0) {
                len = str.length();
                mTableName = str.substring(i + 1, len);
                mSchemaName = str.substring(0, i);
                str = "\"" + mSchemaName + "\"" + "." + "\"" + mTableName + "\"";

                return str;
            }

            return "\"" + str + "\"";
        }

        return str;
    }

    /**
     * @param exchange
     * @throws Exception
     */
    //@Override
    public synchronized void processReplyMessage(final MessageExchange exchange) throws Exception {
        String sql = null;
        String jndiName = null;
        Transaction tx = null;
        boolean isTransacted = exchange.isTransacted();
        Connection connection = null;
        PreparedStatement ps = null;

        if (!(exchange instanceof InOnly) && !(exchange instanceof InOut)) {
            mLogger.log(Level.SEVERE, "DBBC_E00647.IMP_Unsupported_exchange_pattern",
                    exchange.getPattern().toString());
            throw new Exception("DBBC_E00647.IMP_Unsupported_exchange_pattern");
        }

        final String messageId = exchange.getExchangeId();
        try {
            if (InboundMessageProcessor.mInboundExchanges.containsKey(messageId)) {

                if (exchange.getStatus() == ExchangeStatus.DONE) {
                    try {
                        jndiName = epb.getValue(EndpointBean.JDBC_DATABASE_JNDI_NAME);
                        mLogger.log(Level.INFO, InboundMessageProcessor.mMessages.getString("DBBC_R00629.OMP_UsedJNDI") + jndiName);
                        if (isTransacted && exchange instanceof InOnly) {
                            tx = (Transaction) exchange.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
                            try {
                                // we have to resume the suspended transaction
                                resumeThreadTx(tx);
                            } catch (Exception ex) {
                                // for in-only there's no sending of status back to nmr
                                // failure will be logged
                                mLogger.log(Level.WARNING, "DBBC_E00651.IMP_RESUME_FAILED",
                                        new Object[] { ex.getLocalizedMessage() });
                            }
                            try {
                                if (tx.getStatus() == Status.STATUS_MARKED_ROLLBACK) {
                                    try {
                                        // As we are the initiator for tx we have to rollback
                                        rollbackThreadTx(exchange);
                                    } catch (Exception ex) {
                                        // for in-only there's no sending of status back to nmr
                                        // failure will be logged
                                        mLogger.log(Level.WARNING, "DBBC_E00652.IMP_ROLLBACK_FAILED",
                                                new Object[] { ex.getLocalizedMessage() });
                                    }
                                }
                            } catch (Exception ex) {
                                if (mLogger.isLoggable(Level.FINEST)) {
                                    mLogger.log(Level.FINEST, mMessages.getString("IMP_POST_PROCESS_FAILED"), ex);
                                } else if (mLogger.isLoggable(Level.INFO)) {
                                    mLogger.info(mMessages.getString("IMP_POST_PROCESS_FAILED"));
                                }
                                mLogger.log(Level.SEVERE, "IMP_POST_PROCESS_FAILED",
                                    new Object[] { ex.getLocalizedMessage() });
                            }
                        }
                        connection = getDatabaseConnection(jndiName);
//                        if (isTransacted && exchange instanceof InOnly) {
                            connection.setAutoCommit(true);
//                        }
                        //final List records = epb.getProcessList();
                        final List records = (List)mMapInboundExchangesProcessRecords.get(messageId);
                        for (final Iterator it = records.iterator(); it.hasNext();) {
                            String pkNameRet = (String) it.next();
                            String pkNameValue = pkNameRet;
                            if (mPKType.equalsIgnoreCase("LONGVARCHAR") || mPKType.equalsIgnoreCase("CHAR")
                                    || mPKType.equalsIgnoreCase("VARCHAR")) {
                                pkNameRet = "'" + pkNameRet + "'";
                            }
                            if (mPollingPostProcessing.equalsIgnoreCase("Delete")) {
                                sql = dbDataAccessObject.createQualifiedQuery(mTableName, mMoveRowToTableName,
                                        mMarkColumnName, mMarkColumnValue, mPKName, "DELETE", mFlagColumnType);
                                sql = sql + "=" + pkNameRet;
                                mLogger.log(Level.INFO, "Executing sql 2. " + sql);
                                ps = connection.prepareStatement(sql);

                                final int delcount = ps.executeUpdate();
                                mLogger.log(Level.FINE, "Records deleted are:" + delcount);
                                try {
                                    if (ps != null) {
                                        ps.close();
                                    }
                                } catch (final SQLException se) {
                                    if (mLogger.isLoggable(Level.FINEST)) {
                                        mLogger.log(Level.FINEST, mMessages.getString("DBBC_E00628.OMP_Cleanup_Failure"), se);
                                    } else if (mLogger.isLoggable(Level.INFO)) {
                                        mLogger.info(mMessages.getString("DBBC_E00628.OMP_Cleanup_Failure"));
                                    }
                                    mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E00628.OMP_Cleanup_Failure"), se);
                                }
                            } else if (mPollingPostProcessing.equalsIgnoreCase("MarkColumn")) {
                                sql = dbDataAccessObject.createQualifiedQuery(mTableName, mMoveRowToTableName,
                                        mMarkColumnName, mMarkColumnValue, mPKName, "UPDATE", mFlagColumnType);
                                sql = sql + "=" + pkNameRet;
                                mLogger.log(Level.INFO, "Executing sql 3. " + sql);
                                ps = connection.prepareStatement(sql);

                                final int count = ps.executeUpdate();
                                mLogger.log(Level.FINE, "Records updated are " + count);
                                try {
                                    if (ps != null) {
                                        ps.close();
                                    }
                                } catch (final SQLException se) {
                                    mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E00628.OMP_Cleanup_Failure"), se);
                                }
                            } else if (mPollingPostProcessing.equalsIgnoreCase("CopyRow")) {

                                sql = dbDataAccessObject.createQualifiedQuery(mTableName, mMoveRowToTableName,
                                        mMarkColumnName, mMarkColumnValue, mPKName, "INSERT", mFlagColumnType);
                                sql = sql + "=" + pkNameRet;
                                mLogger.log(Level.INFO, "Executing sql 4. " + sql);
                                ps = connection.prepareStatement(sql);

                                final int count = ps.executeUpdate();
                                mLogger.log(Level.FINE, "Records updated are " + count);
                                try {
                                    if (ps != null) {
                                        ps.close();
                                    }
                                } catch (final SQLException se) {
                                    mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E00628.OMP_Cleanup_Failure"), se);
                                }
                                sql = dbDataAccessObject.createQualifiedQuery(mTableName, mMoveRowToTableName,
                                        mMarkColumnName, mMarkColumnValue, mPKName, "UPDATE", mFlagColumnType);
                                sql = sql + "=" + pkNameRet;
                                mLogger.log(Level.INFO, "Executing sql 5. " + sql);
                                ps = connection.prepareStatement(sql);

                                final int updatecount = ps.executeUpdate();
                                mLogger.log(Level.FINE, "Records updated are " + updatecount);
                                try {
                                    if (ps != null) {
                                        ps.close();
                                    }
                                } catch (final SQLException se) {
                                    mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E00628.OMP_Cleanup_Failure"), se);
                                }
                            } else if (mPollingPostProcessing.equalsIgnoreCase("MoveRow")) {
                                sql = dbDataAccessObject.createQualifiedQuery(mTableName, mMoveRowToTableName,
                                        mMarkColumnName, mMarkColumnValue, mPKName, "INSERT", mFlagColumnType);
                                sql = sql + "=" + pkNameRet;
                                mLogger.log(Level.INFO, "Executing sql 6. " + sql);
                                ps = connection.prepareStatement(sql);

                                final int count = ps.executeUpdate();
                                mLogger.log(Level.FINE, "Records updated are " + count);
                                try {
                                    if (ps != null) {
                                        ps.close();
                                    }
                                } catch (final SQLException se) {
                                    mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E00628.OMP_Cleanup_Failure"), se);
                                }
                                sql = dbDataAccessObject.createQualifiedQuery(mTableName, mMoveRowToTableName,
                                        mMarkColumnName, mMarkColumnValue, mPKName, "DELETE", mFlagColumnType);
                                sql = sql + "=" + pkNameRet;
                                mLogger.log(Level.INFO, "Executing sql 7. " + sql);
                                ps = connection.prepareStatement(sql);

                                final int delcount = ps.executeUpdate();
                                mLogger.log(Level.FINE, "Records deleted are:" + delcount);
                                try {
                                    if (ps != null) {
                                        ps.close();
                                    }
                                } catch (final SQLException se) {
                                    mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E00628.OMP_Cleanup_Failure"), se);
                                }
                            } // else if
                            mProcessedList.remove(pkNameValue);
                        }
                        if (isTransacted && exchange instanceof InOnly) {
                            try {
                                // As we are the initiator for tx we have to commit
                                commitThreadTx(exchange);
                            } catch (Exception ex) {
                                // for in-only there's no sending of status back to nmr
                                // failure will be logged
                                mLogger.log(Level.SEVERE, "DBBC_E00657.IMP_COMMIT_FAILED",
                                        new Object[] { ex.getLocalizedMessage() });
                            }
                        }

                    } catch (final SQLException ex) {
                        final String msg = InboundMessageProcessor.mMessages.getString("DBBC_E00639.IMP_Failed_Executing_SQL")
                                + sql;
                        mLogger.log(Level.SEVERE, msg, new Object[] {ex.getLocalizedMessage()});
                        if (isTransacted && exchange instanceof InOnly) {
                            try {
                                // As we are the initiator for tx we have to rollback
                                rollbackThreadTx(exchange);
                            } catch (Exception exception) {
                               mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E00653.IMP_XA_TX_ROLLBACK_FAILED"), exception);
                            }
                        }
                    } catch (final Exception ex) {
                        final String msg = InboundMessageProcessor.mMessages.getString("DBBC_E00639.IMP_Failed_Executing_SQL");
                        mLogger.log(Level.SEVERE, msg, new Object[] {ex.getLocalizedMessage()});
                        if (isTransacted && exchange instanceof InOnly) {
                            try {
                                // As we are the initiator for tx we have to rollback
                                rollbackThreadTx(exchange);
                            } catch (Exception exception) {
                                mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E00653.IMP_XA_TX_ROLLBACK_FAILED"), exception);
                            }
                        }
                    }
                    // for cluster environment
                    if(epb.isClustered()){
                        Connection con = null;
                        try{
                            List records = (List)mMapInboundExchangesProcessRecords.get(messageId);
                            if(jndiName.equalsIgnoreCase(mRuntimeConfig.getProperties().getProperty(RuntimeConfiguration.CONFIG_CLUSTER_DATABASE_JNDINAME))){
                                con = connection;
                            }else{
                                con = getDatabaseConnection(mRuntimeConfig.getProperties().getProperty(RuntimeConfiguration.CONFIG_CLUSTER_DATABASE_JNDINAME));
                            }
                            mJDBCClusterManager.setDataBaseConnection(con);
                            mJDBCClusterManager.deleteInstances(records);
                            mLogger.log(Level.INFO,
                                "DBBC_R10907.IMP_UPDATED_STATUS_TO_DONE",
                                new Object[] { records });
                        }catch(Exception e){
                            mLogger.log(Level.SEVERE, "Unable to delete processed records", e.getLocalizedMessage());
                        }finally {
                            try{
                                if(con != null && con != connection){
                                    con.close();
                                }
                            }catch(SQLException se){
                                mLogger.log(Level.SEVERE, "Unable to close the connection", se.getLocalizedMessage());
                            }
                        }
                    }
                } else {
                    final List records = (List)mMapInboundExchangesProcessRecords.get(messageId);
                    for (final Iterator it = records.iterator(); it.hasNext();) {
                        String pkNameRet = (String) it.next();
                        mProcessedList.remove(pkNameRet);
                    }
                    mLogger.log(Level.SEVERE, "IMP_MXCH_BAD_STATUS", new Object[] { exchange.getStatus().toString(), messageId });
                    if (isTransacted && exchange instanceof InOnly) {
                        try {
                            // As we are the initiator for tx we have to rollback
                            rollbackThreadTx(exchange);
                        } catch (Exception ex) {
                            // for in-only there's no sending of status back to nmr
                            mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E00653.IMP_XA_TX_ROLLBACK_FAILED"), ex);
                        }
                    } else {
                        // Any status other than 'DONE' is considered an error
                        final String msgError = "Error occured while getting DONE Response ";
                        throw new Exception(msgError);
                    }
                }

                if (mLogger.isLoggable(Level.INFO)) {
                    mLogger.log(Level.INFO, "DBBC_E00648.IMP_Remove_exchange_msg_id", messageId);
                }
            } else {
                mLogger.log(Level.SEVERE, "DBBC_E00646.IMP_Invalid_reply_msgId", messageId);
            }
        } finally {
            InboundMessageProcessor.mInboundExchanges.remove(messageId);
            mMapInboundExchangesProcessRecords.remove(messageId);
            try {
                if(ps != null) {
                    ps.close();
                }
            }catch(SQLException se){
                mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E11110.IMP_EXCEPTION_WHILE_CLOSING_THE_PS"), se);   
            }
            try{
                if(connection != null) {
                    connection.close();
                }
            }catch(SQLException se){
                mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E11111.IMP_EXCEPTION_WHILE_CLOSING_THE_CONNECTION"), se);   
            }
        }
    }

    // suspend thread transactional context
    private void resumeThreadTx(Transaction tx) throws Exception {
        if (tx != null) {
            ((TransactionManager) mContext.getTransactionManager()).resume(tx);
            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.log(Level.INFO, " resuing txn  ");
            }
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.log(Level.FINER, " resuing txn  ", new Object[] { tx.toString() });
            }
        }
    }

    private void rollbackThreadTx(MessageExchange msgXChange) throws Exception {
        if (msgXChange.isTransacted()) {
            Transaction tx = (Transaction) msgXChange.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
            if (tx != null) {
                try {
                    tx.rollback();
                } catch (Exception ex) {
                    mLogger.log(Level.SEVERE, "DBBC_E00653.IMP_XA_TX_ROLLBACK_FAILED", new Object[]{ex.getLocalizedMessage()});
                    throw ex;
                }
            } else {
                if (mXAEnabled.equalsIgnoreCase("XATransaction")) {
                    mLogger.log(Level.WARNING, "DBBC_W00654.IMP_XA_TX_NOT_FOUND_IN_MSG_XCHANGE",
                            new Object[]{msgXChange.getExchangeId()});
                }
            }
        }
    }

    private void commitThreadTx(MessageExchange msgXChange) throws Exception {
        if (msgXChange.isTransacted()) {
            Transaction tx = (Transaction) msgXChange.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
            if (tx != null) {
                try {
                    tx.commit();
                } catch (Exception ex) {
                    mLogger.log(Level.SEVERE, "DBBC_E00656.IMP_XA_TX_COMMIT_FAILED", new Object[]{"commit", ex});
                    throw ex;
                }
            }
        } else {
            if (mXAEnabled.equalsIgnoreCase("XATransaction")) {
                mLogger.log(Level.WARNING, "DBBC_W00654.IMP_XA_TX_NOT_FOUND_IN_MSG_XCHANGE",
                        new Object[]{msgXChange.getExchangeId()});
            }
        }
    }

    /**
     * @param jndiName
     * @return
     * @throws javax.naming.NamingException
     */
    private Object getDataSourceFromContext(final String jndiName) throws javax.naming.NamingException {
        final Context c = mContext.getNamingContext();

        return c.lookup(jndiName);
    }

    /**
     * @param jndiName
     * @return
     * @throws Exception
     */
    private Connection getDatabaseConnection(final String jndiName) throws SQLException, NamingException {
        mLogger.log(Level.INFO, InboundMessageProcessor.mMessages.getString("DBBC_R00629.OMP_UsedJNDI") + jndiName);
        return ((DataSource) getDataSourceFromContext(jndiName)).getConnection();
    }
    /**
     * @param prepStmtSQLText
     * @return
     */
    private boolean isSelectStatement(final String prepStmtSQLText) {
        prepStmtSQLText.trim();

        final StringTokenizer tok = new StringTokenizer(prepStmtSQLText);

        if (tok.hasMoreTokens()) {
            final String firstTok = (String) tok.nextToken();

            if (firstTok.equalsIgnoreCase("select")) {
                return true;
            }
        }

        return false;
    }

    protected void stopReceiving() {
        mLogger.log(Level.INFO, "DBBC_R00644.IMP_Inbound_stopped");
        mMonitor.set(Boolean.TRUE);
    }
    
    private TransactionManager getTransactionManager() {
        return (TransactionManager)mContext.getTransactionManager();
    }

    public void setMessageExchangeId(String messageExchangeId, Object retryMetaData) {
        exchangeIDToMeta.put(messageExchangeId, retryMetaData);
    }
    
    public void onRedelivery(MessageExchange exchange) throws MessagingException {
        NormalizedMessage inMsg;
        EndpointBean operationMetaData = (EndpointBean) exchangeIDToMeta.remove(exchange.getExchangeId());
        String groupId = (String)exchange.getProperty(CRMP_GROUP_ID);
        String messageId =  (String)exchange.getProperty(CRMP_MESSAGE_ID);
        
        // remove the listener associated with the exchange ID
        MessageExchangeSupport.removeRedeliveryListener(exchange.getExchangeId());
        mInboundExchanges.remove(exchange.getExchangeId());
        try{
        switch (ExchangePattern.valueOf(exchange)) {
            case IN_OUT:
                if (mLogger.isLoggable(Level.FINEST)) {
                    mLogger.log(Level.FINEST, "Resending the InOut exchange with group ID '" 
                            + groupId + "' and message ID '" + messageId + "'...");
                } else if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Resending the InOut exchange with message ID '" + messageId + "'...");
                } else if (mLogger.isLoggable(Level.INFO)) {
                    mLogger.log(Level.INFO, "Resending the InOut exchange");
                }
                inMsg = ((InOut)exchange).getInMessage();
                InOut inout = mMsgExchangeFactory.createInOutExchange();
                // make sure that the message id has is the same 
                 inout.setProperty(CRMP_GROUP_ID, groupId);
                 inout.setProperty(CRMP_MESSAGE_ID, messageId);
                //processInOut(inout, inMsg, operationMetaData);
                 processInOut(inout,operationMetaData);
                break;
            case IN_ONLY:
                if (mLogger.isLoggable(Level.FINEST)) {
                    mLogger.log(Level.FINEST, "Resending the InOnly exchange with group ID '" 
                            + groupId + "' and message ID '" + messageId + "'...");
                } else if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Resending the InOnly exchange with message ID '" + messageId + "'...");
                } else if (mLogger.isLoggable(Level.INFO)) {
                    mLogger.log(Level.INFO, "Resending the InOnly exchange");
                }

                inMsg = ((InOnly)exchange).getInMessage();
                InOnly inonly = mMsgExchangeFactory.createInOnlyExchange();
                // make sure that the message id has is the same 
                inonly.setProperty(CRMP_GROUP_ID, groupId);
                inonly.setProperty(CRMP_MESSAGE_ID, messageId);
                //processInOnly(inonly, inMsg, operationMetaData);

                if (mServiceEndpoint == null) {
                    mServiceEndpoint = locateServiceEndpoint();
                    epb.setValueObj(EndpointBean.ENDPOINT_REFERENCE, mServiceEndpoint);
                }

                if (mServiceEndpoint == null) {
                    throw new MessagingException(mMessages.getString("DBBC_E00643.IMP_Failed_locate_EP"));
                }

                inonly.setEndpoint(mServiceEndpoint);
                inonly.setOperation(mOperation);
                MessageExchangeSupport.addReplyListener(inonly.getExchangeId(), replyListener, epb);
                MessageExchangeSupport.addRedeliveryListener(inonly.getExchangeId(), this, epb);
                List records = (List)mMapInboundExchangesProcessRecords.get(exchange.getExchangeId());
                for (final Iterator it = records.iterator(); it.hasNext();) {
                    String pkNameRet = (String) it.next();
                    mProcessedList.remove(pkNameRet);
                }

                // Removing the records from the Map
                mMapInboundExchangesProcessRecords.remove(exchange.getExchangeId());
                processInOnly(inonly,operationMetaData);
                break;
            default:
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Retry handler receives an unsupported exchange pattern: " 
                            +ExchangePattern.valueOf(exchange) + ". Ignoring the retry attempt...");
                }
                break;
        }
        }catch(Exception e){
            mLogger.log(Level.SEVERE, "Failed in retry handler",
                        e.getMessage());
            throw new MessagingException(e);
        }
    }

    /* Runtime Config object to cluster JNDI name
     */
    public void setRuntimeConfig(RuntimeConfiguration runtimeConfg){
        mRuntimeConfig = runtimeConfg;
    }

}
