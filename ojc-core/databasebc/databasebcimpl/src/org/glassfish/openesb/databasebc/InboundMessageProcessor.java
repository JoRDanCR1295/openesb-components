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
import java.sql.Statement;
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
import org.glassfish.openesb.databasebc.model.runtime.DatabaseModel;
import org.glassfish.openesb.databasebc.model.runtime.DatabaseModelImpl;
import org.glassfish.openesb.databasebc.model.runtime.Db2DataAccess;
import org.glassfish.openesb.databasebc.model.runtime.DerbyDataAccess;
import org.glassfish.openesb.databasebc.model.runtime.OracleDataAccess;
import org.glassfish.openesb.databasebc.model.runtime.SqlServerDataAccess;
import org.glassfish.openesb.databasebc.model.runtime.MysqlDataAccess;
import com.sun.jbi.nms.exchange.ExchangePattern;

import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.qos.redelivery.Redelivery;
import com.sun.jbi.common.qos.redelivery.RedeliveryStatus;

import net.java.hulp.measure.Probe;

/**
 * author : Venkat P Process requests received from the External Database
 */
public class InboundMessageProcessor implements Runnable, MessageExchangeReplyListener, RedeliveryListener {
    private static final Messages mMessages = Messages.getMessages(InboundMessageProcessor.class);

    private static final Logger mLogger = Messages.getLogger(InboundMessageProcessor.class);

    @SuppressWarnings("unchecked")
    private static final Map mInboundExchanges = Collections.synchronizedMap(new HashMap());

    private static Map exchangeIDToMeta = Collections.synchronizedMap(new HashMap());   
    private Map mMapInboundExchangesProcessRecords = new HashMap();
    private ArrayList mProcessedList = new ArrayList();

    EndpointBean epb;

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

    private String mSchemaName = null;

    private String mXAEnabled = null;

    private String mTableName = null;

    private String mPollingPostProcessing = null;

    private String mMoveRowToTableName = null;

    private int mPollMilliSeconds = 10000;

    private int mThrottleNumber = -1;

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
        final DocumentBuilderFactory docBuilderFact = DocumentBuilderFactory.newInstance();
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

        int rowCount;
        do {
            rowCount = 0;
            try {
                rowCount = execute();
            } catch (final Exception ex) {
                mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E00659.IMP_ERROR_WHILE_EXECUTING_SQL"), ex);
            }

            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.INFO,mMessages.getString("DBBC_R00660.IMP_FINISHED_EXECUTING_SQL"));
            }

            if (rowCount <= 0) {
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
    public int execute() throws MessagingException, Exception {
        String exchangeId = null;
        int rowCount = 0;

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
                    rowCount = processInOnly(mExchange, epb);
                    break;
                default:
                    mLogger.log(Level.INFO, "DBBC_E00633.IMP_Invalid_pattern", mExchange.getExchangeId());
                    return 0;
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
        return rowCount;
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
    public int processInOnly(final MessageExchange exchange, final EndpointBean epb) throws Exception {
        String exchangeId = null;
        String jndiName = null;
        Connection connection = null;
        PreparedStatement ps = null;
        ResultSet rs = null;
        int rowCount = 0;

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

                epb.setTableName(mTableName);

                connection = getDatabaseConnection(jndiName);
                if (mSelectSQL == null || mSelectSQL.trim().equals("")) {
                    DatabaseModel dbDataAccessObject = getDataAccessObject(connection);
                    int numberOfRecords = meta.getJDBCOperationInput().getNumberOfRecords();
                    mSelectSQL = dbDataAccessObject.generateSelectQuery(mTableName, numberOfRecords);
                }
                String dbName = connection.getMetaData().getDatabaseProductName().toLowerCase();
                if (epb.isClustered()) {
                    connection.setAutoCommit(false);
                    // In cluster environment, adding a simple "FOR UPDATE" to the poll query is enough
                    // to make different OpenESB instances always process different rows.
                    //
                    // Although, in VERY RARE cases - i.e. if your DBMS supports transactions and
                    // locking, but DOES NOT support FOR UPDATE and you can't use any hacks to
                    // emulate it as the part of poll query - you can use a separate SQL statement
                    // to obtain lock before selecting rows from the polled table.
                    // However, note that in this case your BPEL process MUST NOT update the
                    // "MarkColumn" of the polled table, because there will be no guarantee that
                    // Database Binding updates the MarkColumn BEFORE BPEL thread also updates it.
                    //
                    // TODO: Add a separate "Lock statement" property
                    String lockStatement = meta.getJDBCSql().getGeneratedKey();
                    if (lockStatement != null && !lockStatement.equals("")) {
                        Statement st = connection.createStatement();
                        st.execute(lockStatement);
                        st.close();
                    }
                } else {
                    connection.setAutoCommit(true);
                }

                List tempList = null;
                rs = executeInboundSQLSelect(epb, meta, connection, mTableName, mSelectSQL);

                if (rs != null) {
                    final JDBCNormalizer normalizer = new JDBCNormalizer();
                        Probe normalizationMeasurement = Probe.info(getClass(),
                            epb.getUniqueName(), JDBCBindingLifeCycle.PERF_CAT_NORMALIZATION);

                    normalizer.setInboundExchangeProcessRecordsMap(mMapInboundExchangesProcessRecords);
                    normalizer.setRecordsProcessedList(mProcessedList);
                    inMsg = normalizer.normalizeSelectInbound(rs, exchange, meta, epb, mPKName, dbName);
                    rowCount = normalizer.mRowCount;

                    if(normalizationMeasurement != null){
                        normalizationMeasurement.end();
                    }

                    tempList = epb.getProcessList();
                    if (!tempList.isEmpty())
                    {
                        // set JNDI name on NormalizedMessage for dynamic addressing
                        inMsg.setProperty(JDBCComponentContext.NM_PROP_DATABASEBC_CONNECTION_JNDI_NAME, jndiName);
                        exchange.setMessage(inMsg, "in");

                        mInboundExchanges.put(exchangeId, new ListenerMeta(
                            System.currentTimeMillis(), this));

                        mChannel.sendSync(exchange);
                        epb.getEndpointStatus().incrementSentRequests();
                    }
                }

                if (tempList != null && !tempList.isEmpty()) {
                    doPostProcessing(connection, tempList);
                }
            }
        } catch (final Exception ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E00663.IMP_ERROR_WHILE_PROCESSING_MEP"), ex);
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
                    if (epb.isClustered()) {
                        connection.commit();
                        connection.setAutoCommit(true);
                    }
                    connection.close();
                }
            }catch(SQLException se){
                mLogger.log(Level.SEVERE, mMessages.getString("DBBC_E11111.IMP_EXCEPTION_WHILE_CLOSING_THE_CONNECTION"), se);
            }
        }
        return rowCount;
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
        ResultSet rs = null;
        try {
            String where = "";
            List<String> bind = new ArrayList<String>();
            if (lSelectSQL.indexOf("$WHERE") >= 0) {
                if (mMarkColumnName != null && !mMarkColumnName.equals("")) {
                    where = "("+mMarkColumnName+" != ? OR "+mMarkColumnName+" IS NULL)";
                    bind.add(mMarkColumnValue);
                }
                lSelectSQL = lSelectSQL.replace("$WHERE", where.equals("") ? "1=1" : where);
            }
            mLogger.log(Level.INFO, "Executing sql 1. " + lSelectSQL);
            PreparedStatement ps = connection.prepareStatement(lSelectSQL);
            JDBCUtil.bindParamList(ps, bind);
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
            if (mLogger.isLoggable(Level.FINEST)) {
                mLogger.log(Level.FINEST, mMessages.getString("DBBC_E00639.IMP_Failed_Executing_SQL") + lSelectSQL, ex);
            } else if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("DBBC_E00639.IMP_Failed_Executing_SQL") + lSelectSQL);
            } else if (mLogger.isLoggable(Level.INFO)) {
                mLogger.info(mMessages.getString("DBBC_E00639.IMP_Failed_Executing_SQL"));
            }
            final String msg = InboundMessageProcessor.mMessages.getString("DBBC_E00639.IMP_Failed_Executing_SQL") + lSelectSQL
                    + ex.getLocalizedMessage();
            throw new MessagingException(msg, ex);
        }
        return rs;
    }

    public DatabaseModel getDataAccessObject(Connection connection) throws SQLException {
        String prdtName = DBMetaData.getDBType(connection);
        if (prdtName == DBMetaData.DERBY) {
            return DerbyDataAccess.getInstance();
        } else if (prdtName == DBMetaData.ORACLE) {
            return OracleDataAccess.getInstance();
        } else if (prdtName == DBMetaData.DB2) {
            return Db2DataAccess.getInstance();
        } else if (prdtName == DBMetaData.SQLSERVER) {
            return SqlServerDataAccess.getInstance();
        } else if (prdtName == DBMetaData.MYSQL) {
            return MysqlDataAccess.getInstance();
        } else {
            return DatabaseModelImpl.getInstance();
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

    public void doPostProcessing(Connection connection, List pkList) throws Exception
    {
        PreparedStatement ps;
        String sql = "";
        try
        {
            for (final Iterator it = pkList.iterator(); it.hasNext();)
            {
                String pkValue = (String) it.next();
                if (mPollingPostProcessing.equalsIgnoreCase("CopyRow") ||
                    mPollingPostProcessing.equalsIgnoreCase("MoveRow"))
                {
                    sql = "INSERT INTO "+mMoveRowToTableName+" SELECT * FROM "+mTableName+" WHERE "+mPKName+"=?";
                    mLogger.log(Level.INFO, "Executing sql 4. " + sql);
                    ps = connection.prepareStatement(sql);
                    JDBCUtil.bindParams(ps, pkValue);
                    final int count = ps.executeUpdate();
                    mLogger.log(Level.FINE, "Inserted records: " + count);
                    ps.close();
                }
                if (mPollingPostProcessing.equalsIgnoreCase("Delete") ||
                    mPollingPostProcessing.equalsIgnoreCase("MoveRow"))
                {
                    sql = "DELETE FROM "+mTableName+" WHERE "+mPKName+"=?";
                    mLogger.log(Level.INFO, "Executing sql 2. " + sql);
                    ps = connection.prepareStatement(sql);
                    JDBCUtil.bindParams(ps, pkValue);
                    final int delcount = ps.executeUpdate();
                    mLogger.log(Level.FINE, "Deleted records: " + delcount);
                    ps.close();
                }
                if (mPollingPostProcessing.equalsIgnoreCase("MarkColumn") ||
                    mPollingPostProcessing.equalsIgnoreCase("CopyRow"))
                {
                    sql = "UPDATE "+mTableName+" SET "+mMarkColumnName+"=? WHERE "+mPKName+"=?";
                    mLogger.log(Level.INFO, "Executing sql 3. " + sql);
                    ps = connection.prepareStatement(sql);
                    JDBCUtil.bindParams(ps, mMarkColumnValue, pkValue);
                    final int count = ps.executeUpdate();
                    mLogger.log(Level.FINE, "Updated records: " + count);
                    ps.close();
                }
            }
        }
        catch (final Exception se)
        {
            final String msg = InboundMessageProcessor.mMessages.getString("DBBC_E00639.IMP_Failed_Executing_SQL") + sql;
            mLogger.log(Level.SEVERE, msg, new Object[] { se.getLocalizedMessage() });
            throw se;
        }
    }

    /**
     * @param exchange
     * @throws Exception
     */
    //@Override
    public synchronized void processReplyMessage(final MessageExchange exchange) throws Exception {
        if (!(exchange instanceof InOnly) && !(exchange instanceof InOut)) {
            mLogger.log(Level.SEVERE, "DBBC_E00647.IMP_Unsupported_exchange_pattern",
                    exchange.getPattern().toString());
            throw new Exception("DBBC_E00647.IMP_Unsupported_exchange_pattern");
        }

        final String messageId = exchange.getExchangeId();
        try {
            if (InboundMessageProcessor.mInboundExchanges.containsKey(messageId)) {
                if (exchange.getStatus() != ExchangeStatus.DONE) {
                    mLogger.log(Level.SEVERE, "IMP_MXCH_BAD_STATUS", new Object[] { exchange.getStatus().toString(), messageId });
                    // Any status other than 'DONE' is considered an error
                    final String msgError = "Error occured while getting DONE Response ";
                    throw new Exception(msgError);
                }
                final List records = (List)mMapInboundExchangesProcessRecords.get(messageId);
                for (final Iterator it = records.iterator(); it.hasNext();) {
                    String pkNameRet = (String)it.next();
                    mProcessedList.remove(pkNameRet);
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
