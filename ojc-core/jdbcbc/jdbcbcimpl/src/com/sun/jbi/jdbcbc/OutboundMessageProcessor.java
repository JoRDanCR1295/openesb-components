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
 * @(#)OutboundMessageProcessor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.exchange.ExchangePattern;
import com.sun.jbi.jdbcbc.extensions.JDBCOperationInput;
import java.net.URI;
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.*;
import javax.naming.Context;
import javax.sql.DataSource;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.jdbcbc.transaction.*;

import javax.transaction.xa.XAResource;
import com.sun.jbi.jdbcbc.model.runtime.DBConnectionInfo;
import javax.sql.*;
import javax.transaction.Transaction;
import javax.transaction.TransactionManager;
import javax.transaction.Status;

import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.qos.redelivery.Redelivery;
import com.sun.jbi.common.qos.redelivery.RedeliveryStatus;
import com.sun.jbi.jdbcbc.util.AlertsUtil;
import com.sun.jbi.eManager.provider.EndpointStatus;

/**
 * Process replies/requests received from the SE.
 */
public class OutboundMessageProcessor implements Runnable {
    private static final Messages mMessages = Messages.getMessages(OutboundMessageProcessor.class);

    private static final Logger mLogger = Messages.getLogger(OutboundMessageProcessor.class);

    Map mEndpoints;

    DocumentBuilder mDocBuilder;

    private MessagingChannel mChannel;

    private MessageExchange mExchange;

    private JDBCComponentContext mContext;

    private Map mInboundExchanges;

    DBConnectionInfo dbConnectionInfo;

    XAConnection xaConnection = null;

    private String mXAEnabled = null;

    Connection connection = null;

    //private TransactionManager mTxManager = null;

    private XidImpl xid = null;

    TransactionHelper mtxHelper = null;

    private boolean mtxFlag;

    XAResource xaResource = null;

    // Settings for custom reliability header extensions
    public static final String CUSTOM_RELIABILITY_MESSAGE_ID_PROPERTY = "com.stc.jbi.messaging.messageid"; // NOI18N
    public static final String CUSTOM_RELIABILITY_HEADER_NAMESPACE_URI = "http://schemas.stc.com/ws/2005/07/custrm"; // NOI18N
    public static final String CUSTOM_RELIABILITY_HEADER_LOCAL_NAME = "MessageID"; // NOI18N

	/**
     * JBI message exchange properties for message grouping and sequencing (new CRMP)
     */
    public static final String CRMP_GROUP_ID = "com.sun.jbi.messaging.groupid";
    public static final String CRMP_MESSAGE_ID = "com.sun.jbi.messaging.messageid";
    
	private ReplyListener replyListener = null;
	
	private ArrayList outParamIndex = new ArrayList();
    private HashMap<Integer,String> outParamTypes = new HashMap<Integer,String>();
    private HashMap<Integer,String> outParamNames = new HashMap<Integer,String>(); 
    
	
    protected OutboundMessageProcessor(final MessagingChannel chnl, final MessageExchange exchange, final Map endpoints,
            final JDBCComponentContext context, final Map inboundMessageExchanges) throws ParserConfigurationException {
        mChannel = chnl;
        mEndpoints = endpoints;
        mExchange = exchange;
        mContext = context;
        mInboundExchanges = inboundMessageExchanges;
        //mTxManager = (TransactionManager) context.getTransactionManager();

        final DocumentBuilderFactory docBuilderFact = DocumentBuilderFactory.newInstance();
        mDocBuilder = docBuilderFact.newDocumentBuilder();
        // mtxHelper = new TransactionHelper();
        dbConnectionInfo = new DBConnectionInfo();
    }

    /**
     *
     */
    //@Override
    public void run() {
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "SQLSE_R00606.OMP_Accept_msg", mExchange.getExchangeId());
        }

        try {
            execute();
        } catch (final Exception ex) {
            mLogger.log(Level.SEVERE, OutboundMessageProcessor.mMessages.getString(
                    "SQLSE_E00607.OMP_Unexpected_exception", ex.getLocalizedMessage()), ex);
        }

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.INFO, "SQLSE_R00608.OMP_Complete_processing");
        }
    }

    /**
     * Process the message exchange
     */
    public void execute() {
        if (mExchange != null) {
            final String exchangeId = mExchange.getExchangeId();

            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.log(Level.INFO, "SQLSE_R00606.OMP_Accept_msg", exchangeId);
            }

            final boolean inbound = mInboundExchanges.containsKey(exchangeId);
            final ListenerMeta listenerMeta = (ListenerMeta) mInboundExchanges.get(exchangeId);
            MessageExchangeReplyListener listener = null;

            if (listenerMeta != null) {
                listener = listenerMeta.getMessageExchangeReplyListener();
            }

            if (inbound) {
                final long invocationTime = listenerMeta.getRequestInvocationTime();

                if (mLogger.isLoggable(Level.INFO)) {
                    final long difference = System.currentTimeMillis() - invocationTime;
                    mLogger.log(Level.INFO, "SQLSE_R00609.OMP_Resp_Ex", new Object[] { exchangeId,
                            difference });
                }
            }

            final URI pattern = mExchange.getPattern();

            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.log(Level.INFO, "SQLSE_R00610.OMP_Pattern", new Object[] { exchangeId, pattern });
            }

            final String serviceName = mExchange.getEndpoint().getServiceName().toString();
            final String endpointName = mExchange.getEndpoint().getEndpointName();

            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.fine("Gettin bean for " + serviceName + endpointName);
            }

            String epName = null;

            if (inbound) {
                epName = EndpointBean.getUniqueName(serviceName, endpointName, EndpointBean.ENDPOINT_TYPE_INBOUND);
            } else {
                epName = EndpointBean.getUniqueName(serviceName, endpointName, EndpointBean.ENDPOINT_TYPE_OUTBOUND);
            }

            final EndpointBean epb = (EndpointBean) mEndpoints.get(epName);

			// Create a reply listener and add the ME Support for the same
			if (replyListener == null){
				replyListener = new ReplyListenerImpl(epb);
			}
			MessageExchangeSupport.addReplyListener(exchangeId,replyListener,epb);

            final String status = epb.getValue(EndpointBean.STATUS);

            if (!status.equals(EndpointBean.STATUS_RUNNING)) {
                // If the endpoint is not in the RUNNING state (i.e. is stopped
                // or
                // shutdown), ignore the message
                if (mLogger.isLoggable(Level.INFO)) {
                    mLogger.log(Level.INFO, "SQLSE_R00611.OMP_EP_state");
                }
            } else {

                switch (ExchangePattern.valueOf(mExchange)) {
                case IN_OUT:
                    mLogger.log(Level.INFO, "SQLSE_R00612.OMP_Recv_InOut", mExchange.getExchangeId());
                    if(epb.JDBC_TRANSACTION_REQUIRED == "yes" && mExchange.isTransacted()){
                         // Start of nested diagnostic context prior to processing of message
                        Logger.getLogger("com.sun.EnterContext").fine("context");
                    	processInOutXA((InOut) mExchange, epb);
                        // End of nested diagnostic context prior to processing of message
                        Logger.getLogger("com.sun.ExitContext").fine("context");
                    } else {
                        // Start of nested diagnostic context prior to processing of message
                        Logger.getLogger("com.sun.EnterContext").fine("context");
                    	processInOut((InOut) mExchange, epb);
                        // End of nested diagnostic context prior to processing of message
                        Logger.getLogger("com.sun.ExitContext").fine("context");
                    }
                    break;
                case IN_ONLY:
                    mLogger.log(Level.INFO, "SQLSE_R00613.OMP_Recv_InOnly", mExchange.getExchangeId());

                    if (inbound) {
                        // Start of nested diagnostic context prior to processing of message
                        Logger.getLogger("com.sun.EnterContext").fine("context");
                        processInOnlyInbound((InOnly) mExchange, epb, listener);
                        // End of nested diagnostic context prior to processing of message
                        Logger.getLogger("com.sun.ExitContext").fine("context");
                    } else {
                        // Start of nested diagnostic context prior to processing of message
                        Logger.getLogger("com.sun.EnterContext").fine("context");
                        processInOnly((InOnly) mExchange, epb);
                        // End of nested diagnostic context prior to processing of message
                        Logger.getLogger("com.sun.ExitContext").fine("context");
                    }
                    break;
                case ROBUST_IN_ONLY:
                    mLogger.log(Level.WARNING, "SQLSE_W00614.OMP_Not_supported_inonly",
                            mExchange.getExchangeId());
                    break;
                case IN_OPTIONAL_OUT:
                    mLogger.log(Level.WARNING, "SQLSE_W00615.OMP_Not_supported_outin",
                            mExchange.getExchangeId());
                    break;
                default:
                    mLogger.log(Level.WARNING, "SQLSE_W00617.OMP_Invalid_pattern", exchangeId);
                    return;
                }
            }
        } // if(exchange)
    }

    /**
     * @param inonly
     * @param endpoint
     * @param listener
     */
    protected void processInOnlyInbound(final InOnly inonly,
                                     final EndpointBean endpoint,
                                     final MessageExchangeReplyListener listener) {
        mLogger.info("SQLSE_R00618.OMP_Processing_InOnly_inbound");

        if (inonly.getStatus() == ExchangeStatus.DONE) {
            endpoint.getEndpointStatus().incrementReceivedDones();
        } else if (inonly.getStatus() == ExchangeStatus.ERROR) {
            endpoint.getEndpointStatus().incrementReceivedErrors();
        } else {
            mLogger.log(Level.WARNING, "SQLSE_W00620.OMP_Unexpected_ME_status", new Object[] {
                    inonly.getEndpoint(), inonly.getStatus() });
        }

        try {
            listener.processReplyMessage(inonly);
        } catch (final Exception ex) {
            mLogger.log(Level.SEVERE, "SQLSE_E00619.OMP_Failed_processing_inonly_inbound", ex);
        }
    }

    /**
     * @param inout
     * @param epb
     */
    protected void processInOut(final InOut inout, final EndpointBean epb) {
        ResultSet rs = null;
        Connection connection = null;
        CallableStatement cs = null;

		boolean success = true;
        if (inout.getStatus() == ExchangeStatus.DONE) {
			
			// added for retry support
			updateTallyReceivedReplies(epb);
            // remove the redelivery listener handler - no retry needed.
            MessageExchangeSupport.removeRedeliveryListener(inout.getExchangeId());

            epb.getEndpointStatus().incrementReceivedDones();
        } else if (inout.getStatus() == ExchangeStatus.ERROR) {

			// added for retry support

            updateTallyReceives(epb, false);
            
            // send alerts
            String errorMsg = inout.getError().getMessage();
            if (errorMsg != null) {
                String msg = mMessages.getString("JDBCBC-E00720.Message_exchange_error",
                        new Object[] {
                            String.valueOf(inout.getService()),
                            inout.getEndpoint().getEndpointName(),
                            errorMsg
                });
                mLogger.log(Level.SEVERE, msg);
                AlertsUtil.getAlerter().warning(msg, 
                                                 JDBCBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                                 epb.getDeploymentId(), 
                                                 AlertsUtil.getServerType(),
                                                 AlertsUtil.COMPONENT_TYPE_BINDING,
                                                 NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                                 NotificationEvent.EVENT_TYPE_ALERT,
                                                 "JDBCBC-E00720");
            } else {
                String msg = mMessages.getString("JDBCBC-E00721.Message_exchange_error_no_detail",
                        new Object[] {
                            String.valueOf(inout.getService()),
                            inout.getEndpoint().getEndpointName()
                });
                mLogger.log(Level.SEVERE, msg);
                AlertsUtil.getAlerter().warning(msg, 
                                                 JDBCBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                                 epb.getDeploymentId(), 
                                                 AlertsUtil.getServerType(),
                                                 AlertsUtil.COMPONENT_TYPE_BINDING,
                                                 NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                                 NotificationEvent.EVENT_TYPE_ALERT,
                                                 "JDBCBC-E00721");
            }
            /**
            // let's see if retry is configured or not
            EndpointInfo info = new EndpointInfo(false,
                                                 epb.getEndpointName(),
                                                 null,
                                                 epb.getServiceName(),
                                                 null);
            RedeliveryConfig retryConfig = mChannel.getServiceQuality(info, RedeliveryConfig.class);
            **/
            
            RedeliveryStatus retryStatus = Redelivery.getRedeliveryStatus(inout);
            if (retryStatus != null && retryStatus.getRemainingRetries() > 0) {
            	try {
                    MessageExchangeSupport.notifyOfRedelivery(inout);
                } catch (Exception e) {
                    String groupId = (String)inout.getProperty(CRMP_GROUP_ID);
                    String messageId =  (String)inout.getProperty(CRMP_MESSAGE_ID); 
                    if (mLogger.isLoggable(Level.WARNING)) {
                        String text = mMessages.getString("JDBCBC-E01036.Failed_to_process_redelivery", new Object[] { groupId, messageId });
                        mLogger.log(Level.WARNING, text, e);
                        AlertsUtil.getAlerter().warning(text, 
                                                        JDBCBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                                        epb.getDeploymentId(), 
                                                        AlertsUtil.getServerType(),
                                                        AlertsUtil.COMPONENT_TYPE_BINDING,
                                                        NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                                        NotificationEvent.EVENT_TYPE_ALERT,
                                                        "JDBCBC-E01036");
                    }
                }
            }

			epb.getEndpointStatus().incrementReceivedErrors();
        } else {

			// added for retry support
			try {
				MessageExchangeSupport.notifyOfReply(inout);
			} catch (Exception ex) {
				if (mLogger.isLoggable(Level.WARNING)) {
					String text = mMessages.getString("JDBCBC-E00759.Exception_during_reply_processing", ex.getLocalizedMessage());
					mLogger.log(Level.WARNING, text, ex);
					AlertsUtil.getAlerter().warning(text, 
						JDBCBindingLifeCycle.SHORT_DISPLAY_NAME, 
						epb.getDeploymentId(), 
						AlertsUtil.getServerType(),
						AlertsUtil.COMPONENT_TYPE_BINDING,
						NotificationEvent.OPERATIONAL_STATE_RUNNING, 
						NotificationEvent.EVENT_TYPE_ALERT,
						"JDBCBC-E00759");
				}
				success = false;
			}

            try {
                Map operationNameToMetaData = (Map) epb.getValueObj(EndpointBean.OPERATION_NAME_TO_META_DATA);
                OperationMetaData meta = (OperationMetaData) operationNameToMetaData.get(inout.getOperation().getLocalPart());

                if (meta == null) {
                    throw new MessagingException(mMessages.getString("SQLSE_E00621.OMP_oper_NotDefined")
                            + inout.getOperation());
                }

                final NormalizedMessage inMsg = inout.getInMessage();
                NormalizedMessage outMsg = mExchange.createMessage();
                //boolean success = true;
                String statusMessage = "";
                String jndiName = null;

                try {
                    rs = null;
                    int rowsUpdated = -1;

                    // writeMessage(inMsg, destinationAddress, false);
                    JDBCOperationInput input = meta.getJDBCSql();
                    final String sql = input.getSql();
                    jndiName = epb.getValue(EndpointBean.JDBC_DATABASE_JNDI_NAME);
                    
                    connection = getDatabaseConnection(epb);
                    if (isSelectStatement(sql)) {
                        rs = executeOutboundSQLSelect(inMsg, epb, meta, connection);

                        if (rs != null) {
                            statusMessage = "Success : ResultSet returned ";
                        }

                        final JDBCNormalizer normalizer = new JDBCNormalizer();
                        outMsg = normalizer.normalizeSelect(rs, inout, meta);
                        inout.setOutMessage(outMsg);
                    } else {
                        if (meta.getJDBCOperationInput().getOperationType().equalsIgnoreCase(JDBCOperations.OPERATION_TYPE_EXECUTE.toString())) {
                            cs = executeOutboundProc(inMsg, epb, meta, connection);
                            final JDBCNormalizer normalizer = new JDBCNormalizer();
                            normalizer.setOutParamIndex(outParamIndex);
                            normalizer.setOutParamNames(outParamNames);
                            normalizer.setOutParamTypes(outParamTypes);
                            outMsg = normalizer.normalizeProcedure(cs, inout, meta);
                        } else {
                            rowsUpdated = executeOutboundSQL(inMsg, epb, meta, connection);
                            statusMessage = "Success : " + rowsUpdated + " are updated .";

                            final JDBCNormalizer normalizer = new JDBCNormalizer();
                            outMsg = normalizer.normalize(rowsUpdated, inout, meta);
                        }
                        inout.setOutMessage(outMsg);
                    }
                } catch (final Exception ex) {
                    mLogger.log(Level.SEVERE,
                            mMessages.getString("SQLSE_E00622.OMP_Failed_writing"), ex);
                    // should this populate a full fault instead?
                    // inout.setError(ex);
                    statusMessage = "Failed " + ex.getMessage();
                    success = false;
                    inout.setError(ex);
                    
                }
                
                mChannel.send(inout);
				
				// Added for retry support
				updateTallySends(epb, success);
                if (success) {
                    epb.getEndpointStatus().incrementSentDones();
                } else {
                    epb.getEndpointStatus().incrementSentErrors();
                }
            } catch (final Exception ex) {
                mLogger.log(Level.SEVERE,
                        mMessages.getString("SQLSE_E00623.OMP_Failed_inout"), ex);
            } finally {
                try {
                    if (rs != null) {
                        rs.close();
                    }
                    if (cs != null) {
                        cs.close();
                    }
                    if (connection != null) {
                        connection.close();
                    }
                } catch (SQLException sqlexception) {
                    mLogger.log(Level.SEVERE,
                            mMessages.getString("SQLSE_E00628.OMP_Cleanup_Failure"),
                            sqlexception);
                }
            }
        }
    }

    private void processInOutXA(final InOut inout, final EndpointBean epb) {
        ResultSet rs = null;
        XAConnection xaconnection = null;
        CallableStatement cs = null;
        Transaction transaction = null;

		boolean success = true;

        if (inout.getStatus() == ExchangeStatus.DONE) {
           
			// added for retry support
			updateTallyReceivedReplies(epb);
            // remove the redelivery listener handler - no retry needed.
            MessageExchangeSupport.removeRedeliveryListener(inout.getExchangeId());

            epb.getEndpointStatus().incrementReceivedDones();
        } else if (inout.getStatus() == ExchangeStatus.ERROR) {

			// added for retry support

            updateTallyReceives(epb, false);
            
            // send alerts
            String errorMsg = inout.getError().getMessage();
            if (errorMsg != null) {
                String msg = mMessages.getString("JDBCBC-E00720.Message_exchange_error",
                        new Object[] {
                            String.valueOf(inout.getService()),
                            inout.getEndpoint().getEndpointName(),
                            errorMsg
                });
                mLogger.log(Level.SEVERE, msg);
                AlertsUtil.getAlerter().warning(msg, 
                                                 JDBCBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                                 epb.getDeploymentId(), 
                                                 AlertsUtil.getServerType(),
                                                 AlertsUtil.COMPONENT_TYPE_BINDING,
                                                 NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                                 NotificationEvent.EVENT_TYPE_ALERT,
                                                 "JDBCBC-E00720");
            } else {
                String msg = mMessages.getString("JDBCBC-E00721.Message_exchange_error_no_detail",
                        new Object[] {
                            String.valueOf(inout.getService()),
                            inout.getEndpoint().getEndpointName()
                });
                mLogger.log(Level.SEVERE, msg);
                AlertsUtil.getAlerter().warning(msg, 
                                                 JDBCBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                                 epb.getDeploymentId(), 
                                                 AlertsUtil.getServerType(),
                                                 AlertsUtil.COMPONENT_TYPE_BINDING,
                                                 NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                                 NotificationEvent.EVENT_TYPE_ALERT,
                                                 "JDBCBC-E00721");
            }
            /**
            // let's see if retry is configured or not
            EndpointInfo info = new EndpointInfo(false,
                                                 epb.getEndpointName(),
                                                 null,
                                                 epb.getServiceName(),
                                                 null);
            RedeliveryConfig retryConfig = mChannel.getServiceQuality(info, RedeliveryConfig.class);
            **/
            
            RedeliveryStatus retryStatus = Redelivery.getRedeliveryStatus(inout);
            if (retryStatus != null && retryStatus.getRemainingRetries() > 0) {
            	try {
                    MessageExchangeSupport.notifyOfRedelivery(inout);
                } catch (Exception e) {
                    String groupId = (String)inout.getProperty(CRMP_GROUP_ID);
                    String messageId =  (String)inout.getProperty(CRMP_MESSAGE_ID); 
                    if (mLogger.isLoggable(Level.WARNING)) {
                        String text = mMessages.getString("JDBCBC-E01036.Failed_to_process_redelivery", new Object[] { groupId, messageId });
                        mLogger.log(Level.WARNING, text, e);
                        AlertsUtil.getAlerter().warning(text, 
                                                        JDBCBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                                        epb.getDeploymentId(), 
                                                        AlertsUtil.getServerType(),
                                                        AlertsUtil.COMPONENT_TYPE_BINDING,
                                                        NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                                        NotificationEvent.EVENT_TYPE_ALERT,
                                                        "JDBCBC-E01036");
                    }
                }
            }

			epb.getEndpointStatus().incrementReceivedErrors();
        } else {

			// added for retry support
			try {
				MessageExchangeSupport.notifyOfReply(inout);
			} catch (Exception ex) {
				if (mLogger.isLoggable(Level.WARNING)) {
					String text = mMessages.getString("JDBCBC-E00759.Exception_during_reply_processing", ex.getLocalizedMessage());
					mLogger.log(Level.WARNING, text, ex);
					AlertsUtil.getAlerter().warning(text, 
					JDBCBindingLifeCycle.SHORT_DISPLAY_NAME, 
					epb.getDeploymentId(), 
					AlertsUtil.getServerType(),
					AlertsUtil.COMPONENT_TYPE_BINDING,
					NotificationEvent.OPERATIONAL_STATE_RUNNING, 
					NotificationEvent.EVENT_TYPE_ALERT,
					"JDBCBC-E00759");
				}
				success = false;
			}

            try {
                Map operationNameToMetaData = (Map) epb.getValueObj(EndpointBean.OPERATION_NAME_TO_META_DATA);
                OperationMetaData meta = (OperationMetaData) operationNameToMetaData.get(inout.getOperation().getLocalPart());

                if (meta == null) {
                    throw new MessagingException(mMessages.getString("SQLSE_E00621.OMP_oper_NotDefined")
                            + inout.getOperation());
                }

                final NormalizedMessage inMsg = inout.getInMessage();
                NormalizedMessage outMsg = mExchange.createMessage();

                String statusMessage = "";
                String jndiName = null;

                try {
                    rs = null;
                    int rowsUpdated = -1;

                    // writeMessage(inMsg, destinationAddress, false);
                    JDBCOperationInput input = meta.getJDBCSql();
                    final String sql = input.getSql();
                    jndiName = epb.getValue(EndpointBean.JDBC_DATABASE_JNDI_NAME);
                    if (inout.isTransacted()) {
                        // Removing manual enlistment. Moving to automatic resource enlistment
                        // mtxHelper.handleOutbound(mExchange);
                        // enlistResource(epb);
                        transaction = (Transaction) inout.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
                    }
                    if (transaction != null) {
                        resumeThreadTx(transaction);
                    }
                    
                    xaconnection = getXADatabaseConnection(epb);
                    XAResource xaresource = xaconnection.getXAResource();
                    transaction.enlistResource(xaresource);
                    connection = xaconnection.getConnection();
                    if (isSelectStatement(sql)) {
                        rs = executeOutboundSQLSelect(inMsg, epb, meta, connection);

                        if (rs != null) {
                            statusMessage = "Success : ResultSet returned ";
                        }

                        final JDBCNormalizer normalizer = new JDBCNormalizer();
                        outMsg = normalizer.normalizeSelect(rs, inout, meta);
                        inout.setOutMessage(outMsg);
                    } else {
                        if (meta.getJDBCOperationInput().getOperationType().equalsIgnoreCase(JDBCOperations.OPERATION_TYPE_EXECUTE.toString())) {
                            cs = executeOutboundProc(inMsg, epb, meta, connection);
                            final JDBCNormalizer normalizer = new JDBCNormalizer();
                            outMsg = normalizer.normalizeProcedure(cs, inout, meta);
                        } else {
                            rowsUpdated = executeOutboundSQL(inMsg, epb, meta, connection);
                            statusMessage = "Success : " + rowsUpdated + " are updated .";

                            final JDBCNormalizer normalizer = new JDBCNormalizer();
                            outMsg = normalizer.normalize(rowsUpdated, inout, meta);
                        }
                        inout.setOutMessage(outMsg);

						
                    }
                } catch (final Exception ex) {
                    mLogger.log(Level.WARNING,
                            OutboundMessageProcessor.mMessages.getString("SQLSE_E00622.OMP_Failed_writing"), ex);
                    // should this populate a full fault instead?
                    // inout.setError(ex);
                    statusMessage = "Failed " + ex.getMessage();
                    success = false;
                    inout.setError(ex);
                    if (transaction != null) {
                        rollbackThreadTx(mExchange);
                    }
                }
                if (transaction != null) {
                    getTransactionManager().suspend();
                }
                mChannel.send(inout);

				// Added for retry support
				updateTallySends(epb, success);


                if (success) {
                    epb.getEndpointStatus().incrementSentDones();
                } else {
                    epb.getEndpointStatus().incrementSentErrors();
                }
            } catch (final Exception ex) {
                mLogger.log(Level.WARNING,
                        OutboundMessageProcessor.mMessages.getString("SQLSE_E00623.OMP_Failed_inout"), ex);
            } finally {
                try {
                    if (rs != null) {
                        rs.close();
                    }
                    if (cs != null) {
                        cs.close();
                    }
                    if (connection != null) {
                        connection.close();
                    }
                } catch (SQLException sqlexception) {
                   mLogger.log(Level.SEVERE,
                        mMessages.getString("SQLSE_E00628.OMP_Cleanup_Failure"), sqlexception); 
                }
            }
        }
    }
    
    /**
     * @param inonly
     * @param epb
     */
    protected void processInOnly(final InOnly inonly, final EndpointBean epb) {
        Connection connection = null;
        CallableStatement cs = null;
        Transaction transaction = null;
		boolean success = true;

		if (inonly.getStatus() == ExchangeStatus.DONE) {
           
			// added for retry support
			updateTallyReceivedReplies(epb);
            // remove the redelivery listener handler - no retry needed.
            MessageExchangeSupport.removeRedeliveryListener(inonly.getExchangeId());

            epb.getEndpointStatus().incrementReceivedDones();
        } else if (inonly.getStatus() == ExchangeStatus.ERROR) {

			// added for retry support

            updateTallyReceives(epb, false);
            
            // send alerts
            String errorMsg = inonly.getError().getMessage();
            if (errorMsg != null) {
                String msg = mMessages.getString("JDBCBC-E00720.Message_exchange_error",
                        new Object[] {
                            String.valueOf(inonly.getService()),
                            inonly.getEndpoint().getEndpointName(),
                            errorMsg
                });
                mLogger.log(Level.SEVERE, msg);
                AlertsUtil.getAlerter().warning(msg, 
                                                 JDBCBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                                 epb.getDeploymentId(), 
                                                 AlertsUtil.getServerType(),
                                                 AlertsUtil.COMPONENT_TYPE_BINDING,
                                                 NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                                 NotificationEvent.EVENT_TYPE_ALERT,
                                                 "JDBCBC-E00720");
            } else {
                String msg = mMessages.getString("JDBCBC-E00721.Message_exchange_error_no_detail",
                        new Object[] {
                            String.valueOf(inonly.getService()),
                            inonly.getEndpoint().getEndpointName()
                });
                mLogger.log(Level.SEVERE, msg);
                AlertsUtil.getAlerter().warning(msg, 
                                                 JDBCBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                                 epb.getDeploymentId(), 
                                                 AlertsUtil.getServerType(),
                                                 AlertsUtil.COMPONENT_TYPE_BINDING,
                                                 NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                                 NotificationEvent.EVENT_TYPE_ALERT,
                                                 "JDBCBC-E00721");
            }
            /**
            // let's see if retry is configured or not
            EndpointInfo info = new EndpointInfo(false,
                                                 epb.getEndpointName(),
                                                 null,
                                                 epb.getServiceName(),
                                                 null);
            RedeliveryConfig retryConfig = mChannel.getServiceQuality(info, RedeliveryConfig.class);
            **/
            
            RedeliveryStatus retryStatus = Redelivery.getRedeliveryStatus(inonly);
            if (retryStatus != null && retryStatus.getRemainingRetries() > 0) {
            	try {
                    MessageExchangeSupport.notifyOfRedelivery(inonly);
                } catch (Exception e) {
                    String groupId = (String)inonly.getProperty(CRMP_GROUP_ID);
                    String messageId =  (String)inonly.getProperty(CRMP_MESSAGE_ID); 
                    if (mLogger.isLoggable(Level.WARNING)) {
                        String text = mMessages.getString("JDBCBC-E01036.Failed_to_process_redelivery", new Object[] { groupId, messageId });
                        mLogger.log(Level.WARNING, text, e);
                        AlertsUtil.getAlerter().warning(text, 
                                                        JDBCBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                                        epb.getDeploymentId(), 
                                                        AlertsUtil.getServerType(),
                                                        AlertsUtil.COMPONENT_TYPE_BINDING,
                                                        NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                                        NotificationEvent.EVENT_TYPE_ALERT,
                                                        "JDBCBC-E01036");
                    }
                }
            }

			epb.getEndpointStatus().incrementReceivedErrors();
        }

		// added for retry support
			try {
				MessageExchangeSupport.notifyOfReply(inonly);
			} catch (Exception ex) {
				if (mLogger.isLoggable(Level.WARNING)) {
					String text = mMessages.getString("JDBCBC-E00759.Exception_during_reply_processing", ex.getLocalizedMessage());
					mLogger.log(Level.WARNING, text, ex);
					AlertsUtil.getAlerter().warning(text, 
					JDBCBindingLifeCycle.SHORT_DISPLAY_NAME, 
					epb.getDeploymentId(), 
					AlertsUtil.getServerType(),
					AlertsUtil.COMPONENT_TYPE_BINDING,
					NotificationEvent.OPERATIONAL_STATE_RUNNING, 
					NotificationEvent.EVENT_TYPE_ALERT,
					"JDBCBC-E00759");
				}
				success = false;
			}

        try {
            epb.getEndpointStatus().incrementReceivedRequests();

            final NormalizedMessage inMsg = inonly.getInMessage();
            final Map operationNameToMetaData = (Map) epb.getValueObj(EndpointBean.OPERATION_NAME_TO_META_DATA);
            final OperationMetaData meta = (OperationMetaData) operationNameToMetaData.get(inonly.getOperation().getLocalPart());
            String jndiName = null;

            if (meta == null) {
                throw new MessagingException(OutboundMessageProcessor.mMessages.getString("SQLSE_E00621.OMP_oper_NotDefined")
                        + inonly.getOperation());
            }

            
            jndiName = epb.getValue(EndpointBean.JDBC_DATABASE_JNDI_NAME);

            if (inonly.isTransacted()) {
                // Removing manual enlistment. Moving to automatic resource enlistment
                // mtxHelper.handleOutbound(mExchange);
                // enlistResource(epb);
                transaction = (Transaction) inonly.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
            }

            try {
                if (transaction != null) {
                    getTransactionManager().resume(transaction);
                }
                connection = getDatabaseConnection(epb);
                if(transaction != null) {
                        connection.setAutoCommit(false);
                }
                // writeMessage(inMsg, destinationAddress, false);
                if (meta.getJDBCOperationInput().getOperationType().equalsIgnoreCase(JDBCOperations.OPERATION_TYPE_EXECUTE.toString())) {
                    // executeOutboundProc(inMsg, epb, meta);
                    // Auto enlistment: pass the transaction retrieved from the Message exchange.
                    cs = executeOutboundProc(inMsg, epb, meta, connection);
                } else {
                    // executeOutboundSQL(inMsg, epb, meta);
                    // Auto enlistment: pass the transaction retrieved from the Message exchange.
                    executeOutboundSQL(inMsg, epb, meta, connection);
                }
                inonly.setStatus(ExchangeStatus.DONE);
            } catch (final Exception ex) {
                success = false;
                mLogger.log(Level.WARNING,
                        OutboundMessageProcessor.mMessages.getString("SQLSE_E00622.OMP_Failed_writing"), ex);
                inonly.setError(ex);
                if (transaction != null) {
                    transaction.setRollbackOnly();
                }
            } finally {
                try{
                    if (cs != null) {
                        cs.close();
                    }
                    if (connection != null) {
                        connection.close();
                    }
                }catch(SQLException sqlexception){
                    mLogger.log(Level.SEVERE,
                        mMessages.getString("SQLSE_E00628.OMP_Cleanup_Failure"), sqlexception);
                }
            }

            /*
             * if (inonly.isTransacted()) { mtxHelper.handleInbound(mExchange); }
             */

            if (transaction != null) {
                getTransactionManager().suspend();
            }
            mChannel.send(inonly);

			// Added for retry support
			updateTallySends(epb, success);

            if (success) {
                epb.getEndpointStatus().incrementSentDones();
            } else {
                epb.getEndpointStatus().incrementSentErrors();
            }
        } catch (final Exception ex) {
            mLogger.log(Level.WARNING,
                    OutboundMessageProcessor.mMessages.getString("SQLSE_E00624.OMP_Failed_inonly"), ex);
        }
    }


    /**
     * @param nMsg
     * @param eBean
     * @param opMetaData
     * @return
     * @throws MessagingException
     */
    protected ResultSet executeOutboundSQLSelect(final NormalizedMessage nMsg,
                                       final EndpointBean eBean,
                                       final OperationMetaData opMetaData,
                                       Connection connection) throws MessagingException {
        String sql = null;
        ResultSet rs = null;

        try {
            sql = opMetaData.getJDBCSql().getSql();
            mLogger.log(Level.INFO,
                    OutboundMessageProcessor.mMessages.getString("SQLSE_R00625.OMP_Exec_SQL") + sql);

            final PreparedStatement ps = connection.prepareStatement(sql);

            final JDBCDenormalizer denormalizer = new JDBCDenormalizer();
            denormalizer.denormalizeOutbound(nMsg, opMetaData, ps);
            rs = ps.executeQuery();
	}
        catch (final SQLException ex) {
            final String msg = OutboundMessageProcessor.mMessages.getString("SQLSE_E00626.OMP_Failed_Exec_SQL") + sql;
            throw new MessagingException(msg, ex);
        } catch (final Exception ex) {
            final String msg = OutboundMessageProcessor.mMessages.getString("SQLSE_E00626.OMP_Failed_Exec_SQL") + sql;
            throw new MessagingException(msg, ex);
        }

        return rs;
    }

    /**
     * @param nMsg
     * @param eBean
     * @param opMetaData
     * @return
     * @throws MessagingException
     */
    protected int executeOutboundSQL(final NormalizedMessage nMsg,
                           final EndpointBean eBean,
                           final OperationMetaData opMetaData,
                           Connection connection) throws MessagingException {
        String sql = null;
        int rowsUpdated = -1;

        try {
            sql = opMetaData.getJDBCSql().getSql();
            mLogger.log(Level.INFO,
                    mMessages.getString("SQLSE_R00625.OMP_Exec_SQL") + sql);

            final PreparedStatement ps = connection.prepareStatement(sql);

            final JDBCDenormalizer denormalizer = new JDBCDenormalizer();
            denormalizer.denormalizeOutbound(nMsg, opMetaData, ps);
            rowsUpdated = ps.executeUpdate();
	}
        catch (final SQLException ex) {
            final String msg = mMessages.getString("SQLSE_E00626.OMP_Failed_Exec_SQL") + sql;
            throw new MessagingException(msg, ex);
        } catch (final Exception ex) {
            final String msg = OutboundMessageProcessor.mMessages.getString("SQLSE_E00626.OMP_Failed_Exec_SQL") + sql;
            throw new MessagingException(msg, ex);
        }

        return rowsUpdated;
    }

    /**
     * @param nMsg
     * @param eBean
     * @param opMetaData
     * @return
     * @throws MessagingException
     */
    protected CallableStatement executeOutboundProc(final NormalizedMessage nMsg,
                                          final EndpointBean eBean,
                                          final OperationMetaData opMetaData,
                                          Connection connnection) throws MessagingException {
        String sql = null;
        int rowsUpdated = -1;

        try {
            sql = opMetaData.getJDBCSql().getSql();
            mLogger.log(Level.INFO,
                    mMessages.getString("SQLSE_R00625.OMP_Exec_SQL") + sql);

            final CallableStatement cs = connnection.prepareCall(sql);
            DatabaseMetaData dbmeta = connnection.getMetaData();

            final JDBCDenormalizer denormalizer = new JDBCDenormalizer();
            if (eBean.getValue(EndpointBean.JDBC_DATABASE_NAME) != null) {
                denormalizer.setDatabaseName(eBean.getValue(EndpointBean.JDBC_DATABASE_NAME));
            }
            denormalizer.denormalizeOutboundProc(nMsg, opMetaData, dbmeta, cs);
            outParamIndex = denormalizer.getOutParamIndex();
            outParamTypes = denormalizer.getOutParamTypes();
            outParamNames = denormalizer.getOutParamNames();
            // rowsUpdated = cs.executeUpdate();
            boolean b = cs.execute();
            return cs;
	}
        catch (final SQLException ex) {
            final String msg = OutboundMessageProcessor.mMessages.getString("SQLSE_R00624.OMP_Failed_Exec_SQL") + sql;
            throw new MessagingException(msg, ex);
        } catch (final Exception ex) {
            final String msg = OutboundMessageProcessor.mMessages.getString("SQLSE_R00624.OMP_Failed_Exec_SQL") + sql;
            throw new MessagingException(msg, ex);
        }

        // return rowsUpdated;
    }

    // resumes the transaction
    private void resumeThreadTx(Transaction tx) throws Exception {
        if (tx != null) {
            ((TransactionManager) mContext.getContext().getTransactionManager()).resume(tx);
            mLogger.log(Level.INFO, "   ", new Object[] { tx.toString() });
        }
    }
    
    //rolls back the transaction
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
    
    //commit the transaction
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
        final Context c = mContext.getContext().getNamingContext();

        return c.lookup(jndiName);
    }

    /**
     * @param jndiName
     * @return
     * @throws Exception
     */
    private Connection getDatabaseConnection(final String jndiName) throws Exception {
        final DataSource ds = (DataSource) getDataSourceFromContext(jndiName);
        final Connection con = ds.getConnection();

        return con;
    }

    private Connection getDatabaseConnection(final EndpointBean epbean) throws Exception {
        if (epbean.getValue(EndpointBean.JDBC_DATABASE_JNDI_NAME) == null
                || epbean.getValue(EndpointBean.JDBC_DATABASE_JNDI_NAME).equals("")) {
            final String driverClass = epbean.getValue(EndpointBean.JDBC_DATABASE_DRIVER_CLASS);
            final String dbURL = epbean.getValue(EndpointBean.JDBC_DATABASE_URL);
            final String user = epbean.getValue(EndpointBean.JDBC_DATABASE_USER);
            final String password = epbean.getValue(EndpointBean.JDBC_DATABASE_PASSWORD);
            // Thread.currentThread().getContextClassLoader().loadClass(driverClass).newInstance();
            Class.forName(driverClass).newInstance();

            return DriverManager.getConnection(dbURL, user, password);
        } else {
            final DataSource ds = (DataSource) getDataSourceFromContext(epbean.getValue(EndpointBean.JDBC_DATABASE_JNDI_NAME));
            final Connection con = ds.getConnection();

            return con;
        }
    }
    
    private XAConnection getXADatabaseConnection(final EndpointBean epbean) throws Exception {
    	try {            
        	final XADataSource ds = (XADataSource) getDataSourceFromContext(epbean.getValue(EndpointBean.JDBC_DATABASE_JNDI_NAME));
        	final XAConnection con = ds.getXAConnection();
        	return con;
        }catch(Exception e){
        	mLogger.log(Level.INFO, "Either the JNDI NAME is  NULL or Could not establish connection using JNDI NAME :" + EndpointBean.JDBC_DATABASE_JNDI_NAME);
        }
        return null;
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
    
    private TransactionManager getTransactionManager() {
    	return (TransactionManager)mContext.getContext().getTransactionManager();
    }

	private void updateTallyReceivedReplies(EndpointBean e) {
        if (e != null) {
            EndpointStatus status = e.getEndpointStatus();
            if (status != null) {
                status.incrementReceivedReplies();
            }
        }
    }
    
    private void updateTallyReceivedRequests(EndpointBean e) {
        if (e != null) {
            EndpointStatus status = e.getEndpointStatus();
            if (status != null) {
                status.incrementReceivedRequests();
            }
        }
    }
    
    private void updateTallySentReplies(EndpointBean e) {
        if (e != null) {
            EndpointStatus status = e.getEndpointStatus();
            if (status != null) {
                status.incrementSentReplies();
            }
        }
    }
    
    private void updateTallySentRequests(EndpointBean e) {
        if (e != null) {
            EndpointStatus status = e.getEndpointStatus();
            if (status != null) {
                status.incrementSentRequests();
            }
        }
    }
    
    private void updateTallyReceives(EndpointBean e, boolean successfulReceive) {
        if (e != null) {
            EndpointStatus status = e.getEndpointStatus();
            if (status != null) {
                if (successfulReceive) {
                    status.incrementReceivedDones();
                } else {
                    status.incrementReceivedErrors();
                }
            }
        }
    }
    
    private void updateTallySends(EndpointBean e, boolean successfulSend) {
        if (e != null) {
            EndpointStatus status = e.getEndpointStatus();
            if (status != null) {
                if (successfulSend) {
                    status.incrementSentDones();
                } else {
                    status.incrementSentErrors();
                }
            }
        }
    }

}
