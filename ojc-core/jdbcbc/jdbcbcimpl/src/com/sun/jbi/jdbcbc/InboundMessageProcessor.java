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

package com.sun.jbi.jdbcbc;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
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
import com.sun.jbi.jdbcbc.model.metadata.DBMetaData;
import com.sun.jbi.jdbcbc.model.runtime.DBConnectionInfo;
import com.sun.jbi.jdbcbc.model.runtime.DatabaseModel;
import com.sun.jbi.jdbcbc.model.runtime.DatabaseModelImpl;
import com.sun.jbi.jdbcbc.model.runtime.Db2DataAccess;
import com.sun.jbi.jdbcbc.model.runtime.DerbyDataAccess;
import com.sun.jbi.jdbcbc.model.runtime.OracleDataAccess;
import com.sun.jbi.jdbcbc.model.runtime.SqlServerDataAccess;
import com.sun.jbi.jdbcbc.transaction.TransactionHelper;
import com.sun.jbi.jdbcbc.transaction.XidImpl;
import com.sun.jbi.nms.exchange.ExchangePattern;

import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.qos.redelivery.Redelivery;
import com.sun.jbi.common.qos.redelivery.RedeliveryStatus;


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

    // private boolean mtxFlag;

    XAResource xaResource = null;

    //private TransactionManager mTxManager = null;

    private static final Messages mMessages = Messages.getMessages(InboundMessageProcessor.class);

    private static final Logger mLogger = Messages.getLogger(InboundMessageProcessor.class);

    @SuppressWarnings("unchecked")
    private static final Map mInboundExchanges = Collections.synchronizedMap(new HashMap());

    private static Map exchangeIDToMeta = Collections.synchronizedMap(new HashMap());   

    
    Map mEndpoint;

    EndpointBean epb;

    DocumentBuilder mDocBuilder;

    //private DeliveryChannel mChannel;
    private MessagingChannel mChannel;

    private MessageExchange mExchange;

    private ComponentContext mContext;

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

    private String mTable = null;

    private String mSchemaName = null;

    DBConnectionInfo dbConnectionInfo;

    private String mXAEnabled = null;

    // private DBMetaData mdbMetaData = null;
    private DatabaseModel dbDataAccessObject = null;

    PreparedStatement ps = null;

    Connection connection = null;

    Connection con = null;

    XAConnection xaConnection = null;


    private String mTableName = null;

    private String mPollingPostProcessing = null;

    private String mMoveRowToTableName = null;

    private int mPollMilliSeconds = 10000;


    // Settings for custom reliability header extensions
    public static final String CUSTOM_RELIABILITY_MESSAGE_ID_PROPERTY = "com.stc.jbi.messaging.messageid"; // NOI18N
    public static final String CUSTOM_RELIABILITY_HEADER_NAMESPACE_URI = "http://schemas.stc.com/ws/2005/07/custrm"; // NOI18N
    public static final String CUSTOM_RELIABILITY_HEADER_LOCAL_NAME = "MessageID"; // NOI18N

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
        //mTxManager = (TransactionManager) context.getTransactionManager();

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
            mLogger.log(Level.INFO, "SQLSE_R00629.IMP_EP_status");
        }

        do {
            try {
                execute();
            } catch (final Exception ex) {
                mLogger.log(Level.SEVERE, mMessages.getString("SQLSE_E00659.IMP_ERROR_WHILE_EXECUTING_SQL"), ex);
            }

            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.INFO,mMessages.getString("SQLSE_R00660.IMP_FINISHED_EXECUTING_SQL"));
            }

            try {
                Thread.sleep(mPollMilliSeconds);
            } catch (final Exception e) {
                mLogger.log(Level.SEVERE, mMessages.getString("SQLSE_E00661.IMP_THREAD_SLEEP_ABRUPTED"), e);
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
                throw new MessagingException(mMessages.getString("SQLSE_E00643.IMP_Failed_locate_EP"));
            }

            exchangeId = mExchange.getExchangeId();

            final QName serviceName = (QName) epb.getValueObj(EndpointBean.FULL_SERVICE_NAME);
            final String epntName = epb.getValue(EndpointBean.ENDPOINT_NAME);

            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.fine("Getting bean for" + serviceName + epntName);
            }

            mExchange.setEndpoint(mServiceEndpoint);
            mExchange.setOperation(mOperation);

            // Adding re-delivery/re-try support
            // we are sending instead of the client context to the ReplyListener 
            // the EndpointBean
           MessageExchangeSupport.addReplyListener(mExchange.getExchangeId(), replyListener, epb);
           MessageExchangeSupport.addRedeliveryListener(mExchange.getExchangeId(), this, epb);

            
            final String status = epb.getValue(EndpointBean.STATUS);

            if (! status.equalsIgnoreCase(EndpointBean.STATUS_RUNNING)) {
                final String endName = epb.getValue(EndpointBean.ENDPOINT_NAME);

                if (mLogger.isLoggable(Level.INFO)) {
                    mLogger.log(Level.INFO, "SQLSE_W00630.IMP_EP_NOT_RUNNING", new Object[] { endName,
                            mExchange.getExchangeId() });
                }
            } else {

                switch (ExchangePattern.valueOf(mExchange)) {
                case IN_OUT:
                    mLogger.log(Level.INFO, "SQLSE_R00631.IMP_Received_INOUT", mExchange.getExchangeId());
                    processInOut(mExchange, epb);
                    break;
                case IN_ONLY:
                    mLogger.log(Level.INFO, "SQLSE_R00632.IMP_Received_INONLY", mExchange.getExchangeId());
                    processInOnly(mExchange, epb);
                    break;
                default:
                    mLogger.log(Level.INFO, "SQLSE_E00633.IMP_Invalid_pattern", mExchange.getExchangeId());
                    return;
                }
            }
        } catch (final MessagingException ex) {
            InboundMessageProcessor.mInboundExchanges.remove(exchangeId);
            mLogger.log(Level.SEVERE, mMessages.getString("SQLSE_E00662.IMP_ERROR_WHILE_EXECUTING_MEP"), exchangeId);
            throw ex;
        } catch (final Exception e) {
            InboundMessageProcessor.mInboundExchanges.remove(exchangeId);
            mLogger.log(Level.SEVERE, mMessages.getString("SQLSE_E00662.IMP_ERROR_WHILE_EXECUTING_MEP"), exchangeId);
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
        ResultSet rs = null;
        String jndiName = null;

        try {
            epb.getEndpointStatus().incrementReceivedRequests();

            NormalizedMessage inMsg = mExchange.createMessage();
            exchangeId = exchange.getExchangeId();

            final Map operationNameToMetaData = (Map) epb.getValueObj(EndpointBean.OPERATION_NAME_TO_META_DATA);
            final OperationMetaData meta = (OperationMetaData) operationNameToMetaData.get(exchange.getOperation().getLocalPart());

            if (meta == null) {
                throw new MessagingException(InboundMessageProcessor.mMessages.getString("SQLSE_E00634.IMP_Invalid_Operation",
                        new Object[] { exchange.getOperation() }));
            }

            mPollMilliSeconds = meta.getJDBCSql().getPollMilliSeconds();
            mSelectSQL = meta.getJDBCSql().getSql();
            /*mPKName = Qualified(meta.getJDBCSql().getPKName());
            mMarkColumnName = Qualified(meta.getJDBCSql().getMarkColumnName());
            mMarkColumnValue = meta.getJDBCSql().getMarkColumnValue();
            mTableName = Qualified(meta.getJDBCSql().getTableName());
            mPollingPostProcessing = meta.getJDBCSql().getPollingPostProcessing();
            mMoveRowToTableName = Qualified(meta.getJDBCSql().getMoveRowToTableName());
            */
            mPKName = meta.getJDBCSql().getPKName();
            mMarkColumnName = meta.getJDBCSql().getMarkColumnName();
            mMarkColumnValue = meta.getJDBCSql().getMarkColumnValue();
            mTableName = meta.getJDBCSql().getTableName();
            mPollingPostProcessing = meta.getJDBCSql().getPollingPostProcessing();
            mMoveRowToTableName = meta.getJDBCSql().getMoveRowToTableName();
            mXAEnabled = meta.getJDBCSql().getTransaction();
            jndiName = epb.getValue(EndpointBean.JDBC_DATABASE_JNDI_NAME);

            // createNewDataSource(mXAEnabled,jndiName,epb);

            if (mXAEnabled.equalsIgnoreCase("XATransaction")) {
                // mtxFlag = startTrasaction();
                getTransactionManager().begin();
            }

            dbDataAccessObject = getDataAccessObject(meta);
            mSelectSQL = dbDataAccessObject.generateSelectQuery(mSelectSQL, mTableName);
            epb.setTableName(mTableName);
            
            connection = getDatabaseConnection(jndiName);
            Transaction tx = getTransactionManager().getTransaction();
            if (isSelectStatement(mSelectSQL)) {
                rs = executeInboundSQLSelect(epb, meta, connection, mTableName, mSelectSQL);
                
                if (rs != null) {
                    final JDBCNormalizer normalizer = new JDBCNormalizer();
                    inMsg = normalizer.normalizeSelectInbound(rs, exchange, meta, epb, mPKName);

                    final List tempList = epb.getProcessList();
                    if (!(tempList.isEmpty())) {
                        // mTxHelper.handleInbound(exchange);
                        exchange.setMessage(inMsg, "in");
                        
                        if (tx != null) {
                            mExchange.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME, tx);
                            getTransactionManager().suspend();
                        }
                        mChannel.send(exchange);
                        epb.getEndpointStatus().incrementSentRequests();
                        // mTableExistsFlag = new Object();
                        InboundMessageProcessor.mInboundExchanges.put(exchangeId, new ListenerMeta(
                                System.currentTimeMillis(), this));
                    } else {
                            if (tx != null) {
                                    try {
                                            tx.commit();
                                    } catch (Exception ex) {
                                            mLogger.log(Level.SEVERE,
                                                            "SQLSE_E00656.IMP_XA_TX_COMMIT_FAILED",
                                                            new Object[] { "commit", ex });
                                            throw ex;
                                    }
                            } else {
                                     if (mXAEnabled.equalsIgnoreCase("XATransaction")) {
             									mLogger.log(Level.WARNING,
                                                    "SQLSE_W00654.IMP_XA_TX_NOT_FOUND_IN_MSG_XCHANGE",
                                                    new Object[] { exchange.getExchangeId() });
									 }
                            }
		    }
                    // mTableExistsFlag = new Object();
                } 
            }
        } catch (final Exception ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("SQLSE_E00663.IMP_ERROR_WHILE_PROCESSING_MEP"), ex);
            Transaction tx = getTransactionManager().getTransaction();
            if(tx != null ) {
                tx.rollback();
            }
        } finally {
            try {
                if (rs != null) {
                    rs.close();
                }
                if (connection != null) {
                    connection.close();
                }
            } catch (final SQLException se) {
                mLogger.log(Level.SEVERE,mMessages.getString("SQLSE_E00628.OMP_Cleanup_Failure"),se);
            }
        }
    }



    public ResultSet executeInboundSQLSelect(final EndpointBean eBean,
                                      final OperationMetaData opMetaData,
                                      Connection connection,
                                      final String mTableName,
                                      String lSelectSQL) throws MessagingException {
        ResultSet rs = null;
        try {
            mLogger.log(Level.INFO, "Executing sql . " + lSelectSQL);

            if ((mMarkColumnName == null) || (mMarkColumnName.equals(""))) {
                // do nothing
            } else {
            	if(mFlagColumnType != null){
                if (mFlagColumnType.equalsIgnoreCase("LONGVARCHAR") || mFlagColumnType.equalsIgnoreCase("CHAR")
                        || mFlagColumnType.equalsIgnoreCase("VARCHAR")) {
                    /*lSelectSQL = lSelectSQL.concat(" where " + mTableName + "." + mMarkColumnName + " != " + "'"
                            + mMarkColumnValue + "'" + "or" + mTableName + "." + mMarkColumnName + "is NULL");*/
                	lSelectSQL = lSelectSQL.concat(" where " + mMarkColumnName + " != " + "'"
                            + mMarkColumnValue + "'" + "or " + mMarkColumnName + " is NULL");
                } else {
                    lSelectSQL = lSelectSQL.concat(" where " + mMarkColumnName + " != "
                            + mMarkColumnValue + "or " + mMarkColumnName + " is NULL");
                }
            	}else{
            		 final String msg = InboundMessageProcessor.mMessages.getString("SQLSE_E00638.IMP_Error_IVALID_ColumnName") + mMarkColumnName;
                     throw new MessagingException(msg, new NamingException());
            }
	}
            ps = connection.prepareStatement(lSelectSQL);
            rs = ps.executeQuery();
	}
        catch (final SQLException ex) {
            final String msg = InboundMessageProcessor.mMessages.getString("SQLSE_E00639.IMP_Failed_Executing_SQL") + lSelectSQL;
            throw new MessagingException(msg, ex);
        } catch (final Exception ex) {
            final String msg = InboundMessageProcessor.mMessages.getString("SQLSE_E00639.IMP_Failed_Executing_SQL") + lSelectSQL;
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
        ResultSet rs = null;
        jndiName = epb.getValue(EndpointBean.JDBC_DATABASE_JNDI_NAME);

        try {
            connection = getDatabaseConnection(jndiName);
            prdtName = DBMetaData.getDBType(connection);

            rs = connection.getMetaData().getColumns(catalog, mSchemaName, mTable, "%");
            
            int noofColCounter = -1;
          //  if(rs==null){
           // 	 final String msg = InboundMessageProcessor.mMessages.getString("IMP_Table_NotExist");
            //     throw new MessagingException(msg, new NamingException());
           // }
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
            	final String msg = InboundMessageProcessor.mMessages.getString("SQLSE_E00636.IMP_Table_NotExist");
                throw new MessagingException(msg, new NamingException());
            }
            if (mPKType == null) {
            	final String msg = InboundMessageProcessor.mMessages.getString("SQLSE_E00637.IMP_PrimaryKey_Error");
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
            final String msg = InboundMessageProcessor.mMessages.getString("SQLSE_E00635.IMP_Error_Lookup") + jndiName;
            throw new MessagingException(msg, ex);
        } catch (final SQLException ex) {
            final String msg = InboundMessageProcessor.mMessages.getString("SQLSE_E00639.IMP_Failed_Executing_SQL");
            throw new MessagingException(msg, ex);
        } catch (final Exception ex) {
            final String msg = InboundMessageProcessor.mMessages.getString("SQLSE_E00639.IMP_Failed_Executing_SQL");
            throw new MessagingException(msg, ex);
        } finally {
            try {
                if (rs != null) {
                    rs.close();
                }
                if (connection != null) {
                    connection.close();
                }
            } catch (final SQLException se) {
                mLogger.log(Level.INFO,
                        InboundMessageProcessor.mMessages.getString("SQLSE_E00628.OMP_Cleanup_Failure"),se);
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
            mLogger.log(Level.INFO, "IMP_locate_EP", new Object[] { serviceName, endpointName });
        }

        return (activatedEndpoint);
    }

    public String Qualified(String str) {
        int len = 0;

        if ((str != null) && (!str.equals(""))) {
            final int i = str.indexOf(".");

            if (i > 0) {
                len = str.length();
                mTable = str.substring(i + 1, len);
                mSchemaName = str.substring(0, i);
                str = "\"" + mSchemaName + "\"" + "." + "\"" + mTable + "\"";

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

        if (!(exchange instanceof InOnly) && !(exchange instanceof InOut)) {
            mLogger.log(Level.SEVERE, "SQLSE_E00647.IMP_Unsupported_exchange_pattern",
                    exchange.getPattern().toString());
            throw new Exception("SQLSE_E00647.IMP_Unsupported_exchange_pattern");
        }

        final String messageId = exchange.getExchangeId();
        try {
            if (InboundMessageProcessor.mInboundExchanges.containsKey(messageId)) {
                // Any status other than 'DONE' is considered an error
                if (exchange.getStatus() != ExchangeStatus.DONE) {
                    final String msgError = "Error occured while getting DONE Response ";
                    throw new Exception(msgError);
                }

                if (exchange.getStatus() == ExchangeStatus.DONE) {
                    try {
                        jndiName = epb.getValue(EndpointBean.JDBC_DATABASE_JNDI_NAME);
                        if (isTransacted && exchange instanceof InOnly) {
                            tx = (Transaction) exchange.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
                            try {
                                // we have to resume the suspended transaction
                                resumeThreadTx(tx);
                            } catch (Exception ex) {
                                // for in-only there's no sending of status back to nmr
                                // failure will be logged
                                mLogger.log(Level.WARNING, "SQLSE_E00651.IMP_RESUME_FAILED",
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
                                        mLogger.log(Level.WARNING, "SQLSE_E00652.IMP_ROLLBACK_FAILED",
                                                new Object[] { ex.getLocalizedMessage() });
                                    }
                                }
                            } catch (Exception ex) {
                                mLogger.log(Level.INFO, "IMP_POST_PROCESS_FAILED",
                                        new Object[] { ex.getLocalizedMessage() });
                            }
                        }
                        connection = getDatabaseConnection(jndiName);
                        if (isTransacted && exchange instanceof InOnly) {
                            connection.setAutoCommit(false);
                        }
                        final List records = epb.getProcessList();
                        for (final Iterator it = records.iterator(); it.hasNext();) {
                            String pkNameRet = (String) it.next();
                            if (mPKType.equalsIgnoreCase("LONGVARCHAR") || mPKType.equalsIgnoreCase("CHAR")
                                    || mPKType.equalsIgnoreCase("VARCHAR")) {
                                pkNameRet = "'" + pkNameRet + "'";
                            }
                            if (mPollingPostProcessing.equalsIgnoreCase("Delete")) {
                                sql = dbDataAccessObject.createQualifiedQuery(mTableName, mMoveRowToTableName,
                                        mMarkColumnName, mMarkColumnValue, mPKName, "DELETE", mFlagColumnType);
                                sql = sql + "=" + pkNameRet;
                                mLogger.log(Level.INFO, "Executing sql . " + sql);
                                ps = connection.prepareStatement(sql);

                                final int delcount = ps.executeUpdate();
                                mLogger.log(Level.SEVERE, "Records deleted are:" + delcount);
                            } else if (mPollingPostProcessing.equalsIgnoreCase("MarkColumn")) {
                                sql = dbDataAccessObject.createQualifiedQuery(mTableName, mMoveRowToTableName,
                                        mMarkColumnName, mMarkColumnValue, mPKName, "UPDATE", mFlagColumnType);
                                sql = sql + "=" + pkNameRet;
                                mLogger.log(Level.INFO, "Executing sql . " + sql);
                                ps = connection.prepareStatement(sql);

                                final int count = ps.executeUpdate();
                                mLogger.log(Level.SEVERE, "Records updated are " + count);
                            } else if (mPollingPostProcessing.equalsIgnoreCase("CopyRow")) {

                                sql = dbDataAccessObject.createQualifiedQuery(mTableName, mMoveRowToTableName,
                                        mMarkColumnName, mMarkColumnValue, mPKName, "INSERT", mFlagColumnType);
                                sql = sql + "=" + pkNameRet;
                                mLogger.log(Level.INFO, "Executing sql . " + sql);
                                ps = connection.prepareStatement(sql);

                                final int count = ps.executeUpdate();
                                mLogger.log(Level.SEVERE, "Records updated are " + count);

                                sql = dbDataAccessObject.createQualifiedQuery(mTableName, mMoveRowToTableName,
                                        mMarkColumnName, mMarkColumnValue, mPKName, "UPDATE", mFlagColumnType);
                                sql = sql + "=" + pkNameRet;
                                mLogger.log(Level.INFO, "Executing sql . " + sql);
                                ps = connection.prepareStatement(sql);

                                final int updatecount = ps.executeUpdate();
                                mLogger.log(Level.SEVERE, "Records updated are " + updatecount);

                            } else if (mPollingPostProcessing.equalsIgnoreCase("MoveRow")) {
                                sql = dbDataAccessObject.createQualifiedQuery(mTableName, mMoveRowToTableName,
                                        mMarkColumnName, mMarkColumnValue, mPKName, "INSERT", mFlagColumnType);
                                sql = sql + "=" + pkNameRet;
                                mLogger.log(Level.INFO, "Executing sql . " + sql);
                                ps = connection.prepareStatement(sql);

                                final int count = ps.executeUpdate();
                                mLogger.log(Level.SEVERE, "Records updated are " + count);

                                sql = dbDataAccessObject.createQualifiedQuery(mTableName, mMoveRowToTableName,
                                        mMarkColumnName, mMarkColumnValue, mPKName, "DELETE", mFlagColumnType);
                                sql = sql + "=" + pkNameRet;
                                mLogger.log(Level.INFO, "Executing sql . " + sql);
                                ps = connection.prepareStatement(sql);

                                final int delcount = ps.executeUpdate();
                                mLogger.log(Level.SEVERE, "Records deleted are:" + delcount);
                            } // else if
                        }
                        if (isTransacted && exchange instanceof InOnly) {
                            try {
                                // As we are the initiator for tx we have to commit
                                commitThreadTx(exchange);
                            } catch (Exception ex) {
                                // for in-only there's no sending of status back to nmr
                                // failure will be logged
                                mLogger.log(Level.WARNING, "SQLSE_E00657.IMP_COMMIT_FAILED",
                                        new Object[] { ex.getLocalizedMessage() });
                            }
                        }

                    } catch (final SQLException ex) {
                        final String msg = InboundMessageProcessor.mMessages.getString("SQLSE_E00639.IMP_Failed_Executing_SQL")
                                + sql;
                        mLogger.log(Level.SEVERE, msg, new Object[] {ex.getLocalizedMessage()});
                        if (isTransacted && exchange instanceof InOnly) {
                            try {
                                // As we are the initiator for tx we have to rollback
                                rollbackThreadTx(exchange);
                            } catch (Exception exception) {
                               mLogger.log(Level.SEVERE, mMessages.getString("SQLSE_E00653.IMP_XA_TX_ROLLBACK_FAILED"), exception);
                            }
                        }
                    } catch (final Exception ex) {
                        final String msg = InboundMessageProcessor.mMessages.getString("SQLSE_E00639.IMP_Failed_Executing_SQL");
                        mLogger.log(Level.SEVERE, msg, new Object[] {ex.getLocalizedMessage()});
                        if (isTransacted && exchange instanceof InOnly) {
                            try {
                                // As we are the initiator for tx we have to rollback
                                rollbackThreadTx(exchange);
                            } catch (Exception exception) {
                                mLogger.log(Level.SEVERE, mMessages.getString("SQLSE_E00653.IMP_XA_TX_ROLLBACK_FAILED"), exception);
                            }
                        }
                        
                    }
                } else {
                    mLogger.log(Level.SEVERE, "IMP_MXCH_BAD_STATUS", new Object[] { exchange.getStatus().toString(),
                            messageId });
                    if (isTransacted && exchange instanceof InOnly) {
                        try {
                            // As we are the initiator for tx we have to rollback
                            rollbackThreadTx(exchange);
                        } catch (Exception ex) {
                            // for in-only there's no sending of status back to nmr
                            mLogger.log(Level.SEVERE, mMessages.getString("SQLSE_E00653.IMP_XA_TX_ROLLBACK_FAILED"), ex);
                        }
                    }
                }

                if (mLogger.isLoggable(Level.INFO)) {
                    mLogger.log(Level.INFO, "SQLSE_E00648.IMP_Remove_exchange_msg_id", messageId);
                }
            } else {
                mLogger.log(Level.SEVERE, "SQLSE_E00646.IMP_Invalid_reply_msgId", messageId);
            }
        } finally {
            InboundMessageProcessor.mInboundExchanges.remove(messageId);
            try {
                if(ps != null) {
                    ps.close();
                }
                if(connection != null) {
                   connection.close();
                }
                
            } catch (final SQLException se) {
                mLogger.log(Level.INFO, mMessages.getString("SQLSE_E00628.OMP_Cleanup_Failure"),se);
            }
        }
    }

    // suspend thread transactional context
    private void resumeThreadTx(Transaction tx) throws Exception {
        if (tx != null) {
            ((TransactionManager) mContext.getTransactionManager()).resume(tx);
            mLogger.log(Level.INFO, "   ", new Object[] { tx.toString() });
        }
    }

    private void rollbackThreadTx(MessageExchange msgXChange) throws Exception {
        if (msgXChange.isTransacted()) {
            Transaction tx = (Transaction) msgXChange.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
            if (tx != null) {
                try {
                    tx.rollback();
                } catch (Exception ex) {
                    mLogger.log(Level.SEVERE, "SQLSE_E00653.IMP_XA_TX_ROLLBACK_FAILED", new Object[] { ex.getLocalizedMessage() });
                    throw ex;
                }
            } else {
				 if (mXAEnabled.equalsIgnoreCase("XATransaction")) {
             		mLogger.log(Level.WARNING, "SQLSE_W00654.IMP_XA_TX_NOT_FOUND_IN_MSG_XCHANGE",
                        new Object[] { msgXChange.getExchangeId() });
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
                    mLogger.log(Level.SEVERE, "SQLSE_E00656.IMP_XA_TX_COMMIT_FAILED", new Object[] { "commit", ex });
                    throw ex;
                }
            } else {
                 if (mXAEnabled.equalsIgnoreCase("XATransaction")) {
             		mLogger.log(Level.WARNING, "SQLSE_W00654.IMP_XA_TX_NOT_FOUND_IN_MSG_XCHANGE",
                        new Object[] { msgXChange.getExchangeId() });
				 }
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
        mLogger.log(Level.INFO, "SQLSE_R00644.IMP_Inbound_stopped");
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
        try{
        switch (ExchangePattern.valueOf(exchange)) {
            case IN_OUT:
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Resending the InOut exchange with group ID '" 
                            + groupId + "' and message ID '" + messageId + "'...");
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
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Resending the InOnly exchange with group ID '" 
                            + groupId + "' and message ID '" + messageId + "'...");
                }
                inMsg = ((InOnly)exchange).getInMessage();
                InOnly inonly = mMsgExchangeFactory.createInOnlyExchange();
                // make sure that the message id has is the same 
                 inonly.setProperty(CRMP_GROUP_ID, groupId);
                 inonly.setProperty(CRMP_MESSAGE_ID, messageId);
                 //processInOnly(inonly, inMsg, operationMetaData);
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

}
