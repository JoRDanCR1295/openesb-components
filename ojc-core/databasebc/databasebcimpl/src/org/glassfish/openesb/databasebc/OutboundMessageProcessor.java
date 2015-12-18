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
package org.glassfish.openesb.databasebc;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.nms.exchange.ExchangePattern;
import org.glassfish.openesb.databasebc.extensions.JDBCOperationInput;
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
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.jbi.messaging.*;
import javax.naming.Context;
import javax.naming.NamingException;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import com.sun.jbi.internationalization.Messages;
import org.glassfish.openesb.databasebc.transaction.*;

import javax.transaction.xa.XAResource;
import org.glassfish.openesb.databasebc.model.runtime.DBConnectionInfo;
import javax.sql.*;
import javax.transaction.Transaction;
import javax.transaction.TransactionManager;

import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.redelivery.Redelivery;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.common.qos.redelivery.RedeliveryStatus;
import org.glassfish.openesb.databasebc.util.AlertsUtil;
import com.sun.jbi.eManager.provider.EndpointStatus;
import org.glassfish.openesb.databasebc.extensions.SPOperationInput;

import net.java.hulp.measure.Probe;

/**
 * Process replies/requests received from the SE.
 */
public class OutboundMessageProcessor implements Runnable {

  private static final Messages mMessages = Messages.getMessages(
          OutboundMessageProcessor.class);
  private static final Logger mLogger = Messages.getLogger(
          OutboundMessageProcessor.class);
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
  PreparedStatement ps = null;
  ResultSet rs = null;
  CallableStatement cs = null;
  //private TransactionManager mTxManager = null;
  private XidImpl xid = null;
  TransactionHelper mtxHelper = null;
  private boolean mtxFlag;
  XAResource xaResource = null;
  // Settings for custom reliability header extensions
  public static final String CUSTOM_RELIABILITY_MESSAGE_ID_PROPERTY =
          "com.stc.jbi.messaging.messageid"; // NOI18N
  public static final String CUSTOM_RELIABILITY_HEADER_NAMESPACE_URI =
          "http://schemas.stc.com/ws/2005/07/custrm"; // NOI18N
  public static final String CUSTOM_RELIABILITY_HEADER_LOCAL_NAME =
          "MessageID"; // NOI18N
  /**
   * JBI message exchange properties for message grouping and sequencing (new CRMP)
   */
  public static final String CRMP_GROUP_ID = "com.sun.jbi.messaging.groupid";
  public static final String CRMP_MESSAGE_ID =
          "com.sun.jbi.messaging.messageid";
  private static final String CLIENT = "Client";
  private static final String SERVER = "Server";
  private ReplyListener replyListener = null;
  private ArrayList outParamIndex = new ArrayList();
  private HashMap<Integer, String> outParamTypes =
          new HashMap<Integer, String>();
  private HashMap<Integer, String> outParamNames =
          new HashMap<Integer, String>();

  protected OutboundMessageProcessor(final MessagingChannel chnl, final MessageExchange exchange, final Map endpoints,
                                     final JDBCComponentContext context, final Map inboundMessageExchanges) throws ParserConfigurationException {
    mChannel = chnl;
    mEndpoints = endpoints;
    mExchange = exchange;
    mContext = context;
    mInboundExchanges = inboundMessageExchanges;
    //mTxManager = (TransactionManager) context.getTransactionManager();

    final DocumentBuilderFactory docBuilderFact = DocumentBuilderFactory.
            newInstance();
    mDocBuilder = docBuilderFact.newDocumentBuilder();
    // mtxHelper = new TransactionHelper();
    dbConnectionInfo = new DBConnectionInfo();
  }

  /**
   *
   */
  //@Override
  public void run() {
    if (mLogger.isLoggable(Level.FINEST))
      mLogger.log(Level.FINEST, "DBBC_R00606.OMP_Accept_msg", mExchange.
              getExchangeId());
    else if (mLogger.isLoggable(Level.FINE))
      mLogger.log(Level.FINE, "DBBC_R00630.OMP_Accept_msg");

    try {
      execute();
    } catch (final Exception ex) {
      mLogger.log(Level.SEVERE, OutboundMessageProcessor.mMessages.getString(
              "DBBC_E00607.OMP_Unexpected_exception", ex.getLocalizedMessage()),
              ex);
      String text = mMessages.getString(
              "DBBC_E00607.OMP_Unexpected_exception", ex.getLocalizedMessage());
      AlertsUtil.getAlerter().warning(text,
              JDBCBindingLifeCycle.SHORT_DISPLAY_NAME,
              null,
              AlertsUtil.getServerType(),
              AlertsUtil.COMPONENT_TYPE_BINDING,
              NotificationEvent.OPERATIONAL_STATE_RUNNING,
              NotificationEvent.EVENT_TYPE_ALERT,
              "DBBC_E00607");
    }

    if (mLogger.isLoggable(Level.INFO))
      mLogger.log(Level.INFO, "DBBC_R00608.OMP_Complete_processing");
  }

  /**
   * Process the message exchange
   */
  public void execute() {
    if (mExchange != null) {
      final String exchangeId = mExchange.getExchangeId();

      if (mLogger.isLoggable(Level.FINE))
        mLogger.log(Level.FINE, "DBBC_R00606.OMP_Accept_msg", exchangeId);
      else if (mLogger.isLoggable(Level.FINE))
        mLogger.log(Level.FINE, "DBBC_R00630.OMP_Accept_msg");

      final boolean inbound = mInboundExchanges.containsKey(exchangeId);
      final ListenerMeta listenerMeta = (ListenerMeta) mInboundExchanges.get(
              exchangeId);
      MessageExchangeReplyListener listener = null;

      if (listenerMeta != null)
        listener = listenerMeta.getMessageExchangeReplyListener();

      if (inbound) {
        final long invocationTime = listenerMeta.getRequestInvocationTime();

        final long difference =
                System.currentTimeMillis() - invocationTime;
        if (mLogger.isLoggable(Level.FINE))
          mLogger.log(Level.FINE, "DBBC_R00609.OMP_Resp_Ex",
                  new Object[]{exchangeId,
                               difference});
        else if (mLogger.isLoggable(Level.INFO))
          mLogger.log(Level.INFO, "DBBC_R00631.OMP_Resp_Ex",
                  new Object[]{exchangeId});
      }

      final URI pattern = mExchange.getPattern();

      if (mLogger.isLoggable(Level.FINE))
        mLogger.log(Level.FINE, "DBBC_R00610.OMP_Pattern",
                new Object[]{exchangeId, pattern});
      else if (mLogger.isLoggable(Level.INFO))
        mLogger.log(Level.INFO, "DBBC_R00632.OMP_Pattern",
                new Object[]{pattern});

      ServiceEndpoint serviceEndpoint = Redelivery.getEndpoint(mExchange);
      final String serviceName =
              serviceEndpoint.getServiceName().toString();
      final String endpointName = serviceEndpoint.getEndpointName();

      if (mLogger.isLoggable(Level.FINE))
        mLogger.fine("Gettin bean for " + serviceName + endpointName);

      String epName = null;

      if (inbound)
        epName = EndpointBean.getUniqueName(serviceName, endpointName,
                EndpointBean.ENDPOINT_TYPE_INBOUND);
      else
        epName = EndpointBean.getUniqueName(serviceName, endpointName,
                EndpointBean.ENDPOINT_TYPE_OUTBOUND);

      final EndpointBean epb = (EndpointBean) mEndpoints.get(epName);



      // Create a reply listener and add the ME Support for the same
      if (replyListener == null)
        replyListener = new ReplyListenerImpl(epb);
      MessageExchangeSupport.addReplyListener(exchangeId, replyListener,
              epb);

      final String status = epb.getValue(EndpointBean.STATUS);

      if (!status.equals(EndpointBean.STATUS_RUNNING)) {
        // If the endpoint is not in the RUNNING state (i.e. is stopped
        // or
        // shutdown), ignore the message
        if (mLogger.isLoggable(Level.INFO))
          mLogger.log(Level.INFO, "DBBC_R00611.OMP_EP_state");
      } else
        switch (ExchangePattern.valueOf(mExchange)) {
          case IN_OUT:
            if (mLogger.isLoggable(Level.FINE))
              mLogger.log(Level.FINE, "DBBC_R00612.OMP_Recv_InOut",
                      mExchange.getExchangeId());
            else if (mLogger.isLoggable(Level.FINE))
              mLogger.log(Level.FINE, "DBBC_R00612.OMP_Recv_InOut",
                      "");
            if (mExchange.isTransacted()) {
              // Start of nested diagnostic context prior to processing of message
              Logger.getLogger("com.sun.EnterContext").fine(
                      "context");
              processInOutXA((InOut) mExchange, epb);
              // End of nested diagnostic context prior to processing of message
              Logger.getLogger("com.sun.ExitContext").fine(
                      "context");
            } else {
              // Start of nested diagnostic context prior to processing of message
              Logger.getLogger("com.sun.EnterContext").fine(
                      "context");
              processInOut((InOut) mExchange, epb);
              // End of nested diagnostic context prior to processing of message
              Logger.getLogger("com.sun.ExitContext").fine(
                      "context");
            }
            break;
          case IN_ONLY:
            if (mLogger.isLoggable(Level.FINE))
              mLogger.log(Level.FINE,
                      "DBBC_R00613.OMP_Recv_InOnly", mExchange.getExchangeId());
            else if (mLogger.isLoggable(Level.INFO))
              mLogger.log(Level.INFO,
                      "DBBC_R00613.OMP_Recv_InOnly", "");

            if (inbound) {
              // Start of nested diagnostic context prior to processing of message
              Logger.getLogger("com.sun.EnterContext").fine(
                      "context");
              processInOnlyInbound((InOnly) mExchange, epb,
                      listener);
              // End of nested diagnostic context prior to processing of message
              Logger.getLogger("com.sun.ExitContext").fine(
                      "context");
            } else {
              // Start of nested diagnostic context prior to processing of message
              Logger.getLogger("com.sun.EnterContext").fine(
                      "context");
              processInOnly((InOnly) mExchange, epb);
              // End of nested diagnostic context prior to processing of message
              Logger.getLogger("com.sun.ExitContext").fine(
                      "context");
            }
            break;
          case ROBUST_IN_ONLY:
            if (mLogger.isLoggable(Level.FINE))
              mLogger.log(Level.WARNING,
                      "DBBC_W00614.OMP_Not_supported_inonly",
                      mExchange.getExchangeId());
            else if (mLogger.isLoggable(Level.INFO))
              mLogger.log(Level.WARNING,
                      "DBBC_W00614.OMP_Not_supported_inonly", "");
            break;
          case IN_OPTIONAL_OUT:
            if (mLogger.isLoggable(Level.FINE))
              mLogger.log(Level.WARNING,
                      "DBBC_W00615.OMP_Not_supported_outin",
                      mExchange.getExchangeId());
            else if (mLogger.isLoggable(Level.INFO))
              mLogger.log(Level.WARNING,
                      "DBBC_W00615.OMP_Not_supported_outin", "");
            break;
          default:
            if (mLogger.isLoggable(Level.FINE))
              mLogger.log(Level.WARNING,
                      "DBBC_W00617.OMP_Invalid_pattern",
                      exchangeId);
            else if (mLogger.isLoggable(Level.INFO))
              mLogger.log(Level.WARNING,
                      "DBBC_W00617.OMP_Invalid_pattern", "");
            return;
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
    mLogger.info("DBBC_R00618.OMP_Processing_InOnly_inbound");

    if (inonly.getStatus() == ExchangeStatus.DONE)
      updateTallyReceives(endpoint, true);
    else if (inonly.getStatus() == ExchangeStatus.ERROR) {
      RedeliveryStatus retryStatus =
              Redelivery.getRedeliveryStatus(inonly);
      if (retryStatus != null && !retryStatus.hasFailed())
        try {
          if (mLogger.isLoggable(Level.WARNING)) {
            String text = mMessages.getString(
                    "DBBC-E01037.Redelivering_message",
                    new Object[]{retryStatus.getRemainingRetries()});
            mLogger.log(Level.WARNING, text);

          }
          MessageExchangeSupport.notifyOfRedelivery(inonly);
        } catch (Exception e) {
          String groupId = (String) inonly.getProperty(CRMP_GROUP_ID);
          String messageId = (String) inonly.getProperty(
                  CRMP_MESSAGE_ID);
          if (mLogger.isLoggable(Level.WARNING)) {
            String text = mMessages.getString(
                    "DBBC-E01036.Failed_to_process_redelivery",
                    new Object[]{groupId, messageId});
            mLogger.log(Level.WARNING, text, e);
            AlertsUtil.getAlerter().warning(text,
                    JDBCBindingLifeCycle.SHORT_DISPLAY_NAME,
                    endpoint.getDeploymentId(),
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "DBBC-E01036");
          }
        }
      else
        try {
          // No more retries...handle all the recourse actions.  Only one that we need to handle in
          // a BC-specific way is the suspending of endpoints
          suspendEndpoint(inonly, endpoint);

          listener.processReplyMessage(inonly);
        } catch (final Exception ex) {
          mLogger.log(Level.SEVERE,
                  "DBBC_E00619.OMP_Failed_processing_inonly_inbound",
                  ex);
        }
      updateTallyReceives(endpoint, false);
      String msg = mMessages.getString(
              "DBBC_E00619.OMP_Failed_processing_inonly_inbound",
              new Object[]{inonly.getEndpoint().getServiceName(),
                           inonly.getEndpoint().getEndpointName(),
                           inonly.getOperation()});

      mLogger.log(Level.WARNING, msg);
      return;
    } else if (mLogger.isLoggable(Level.FINE))
      mLogger.log(Level.WARNING,
              "DBBC_W00620.OMP_Unexpected_ME_status", new Object[]{
                inonly.getEndpoint(), inonly.getStatus()});
    else if (mLogger.isLoggable(Level.INFO))
      mLogger.log(Level.WARNING,
              "DBBC_W00633.OMP_Unexpected_ME_status",
              new Object[]{inonly.getEndpoint()});

    try {
      listener.processReplyMessage(inonly);
    } catch (final Exception ex) {
      mLogger.log(Level.SEVERE,
              "DBBC_E00619.OMP_Failed_processing_inonly_inbound", ex);
    }
  }

  private void suspendEndpoint(InOnly inonly, EndpointBean endpoint) throws Exception {

    ServiceEndpoint serviceEndpoint = Redelivery.getEndpoint(inonly);

    EndpointInfo endpointInfo = EndpointInfo.valueOf(
            serviceEndpoint,
            false); // false = consumer endpoint

    RedeliveryConfig config =
            mContext.getBindingChannel().getServiceQuality(
            endpointInfo, RedeliveryConfig.class);

    if (config != null && config.getFailure() == RedeliveryConfig.Failure.suspend)
      // Suspend endpoint.
      mContext.getDeployer().suspend(endpoint);
  }

  /**
   * @param inout
   * @param epb
   */
  protected void processInOut(final InOut inout, final EndpointBean epb) {
    Connection connection = null;
    String faultCode = null;
    String faultDetail = null;
    boolean success = true;
    if (inout.getStatus() == ExchangeStatus.DONE) {

      // remove the redelivery listener handler - no retry needed.
      MessageExchangeSupport.removeReplyListener(inout.getExchangeId());

      updateTallyReceives(epb, true);
    } else if (inout.getStatus() == ExchangeStatus.ERROR) {

      // added for retry support

      updateTallyReceives(epb, false);

      // send alerts
      String errorMsg = inout.getError().getMessage();
      if (errorMsg != null) {
        String msg = mMessages.getString(
                "DBBC-E00720.Message_exchange_error",
                new Object[]{
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
                "DBBC-E00720");
      } else {
        String msg = mMessages.getString(
                "DBBC-E00721.Message_exchange_error_no_detail",
                new Object[]{
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
                "DBBC-E00721");
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
      if (retryStatus != null && retryStatus.getRemainingRetries() > 0)
        try {
          MessageExchangeSupport.notifyOfRedelivery(inout);
        } catch (Exception e) {
          String groupId = (String) inout.getProperty(CRMP_GROUP_ID);
          String messageId = (String) inout.getProperty(
                  CRMP_MESSAGE_ID);
          if (mLogger.isLoggable(Level.WARNING)) {
            String text = mMessages.getString(
                    "DBBC-E01036.Failed_to_process_redelivery",
                    new Object[]{groupId, messageId});
            mLogger.log(Level.WARNING, text, e);
            AlertsUtil.getAlerter().warning(text,
                    JDBCBindingLifeCycle.SHORT_DISPLAY_NAME,
                    epb.getDeploymentId(),
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "DBBC-E01036");
          }
        }

      //epb.getEndpointStatus().incrementReceivedErrors();
    } else {

      // added for retry support
      try {
        MessageExchangeSupport.notifyOfReply(inout);
      } catch (Exception ex) {
        if (mLogger.isLoggable(Level.WARNING)) {
          String text = mMessages.getString(
                  "DBBC-E00759.Exception_during_reply_processing", ex.
                  getLocalizedMessage());
          mLogger.log(Level.SEVERE, text, ex);
          AlertsUtil.getAlerter().warning(text,
                  JDBCBindingLifeCycle.SHORT_DISPLAY_NAME,
                  epb.getDeploymentId(),
                  AlertsUtil.getServerType(),
                  AlertsUtil.COMPONENT_TYPE_BINDING,
                  NotificationEvent.OPERATIONAL_STATE_RUNNING,
                  NotificationEvent.EVENT_TYPE_ALERT,
                  "DBBC-E00759");
        }
        success = false;
      }

      try {
        updateTallyReceivedRequests(epb);
        Map operationNameToMetaData = (Map) epb.getValueObj(
                EndpointBean.OPERATION_NAME_TO_META_DATA);
        OperationMetaData meta = (OperationMetaData) operationNameToMetaData.get(inout.getOperation().
                getLocalPart());

        if (meta == null)
          throw new MessagingException(mMessages.getString(
                  "DBBC_E00621.OMP_oper_NotDefined") + inout.getOperation());

        final NormalizedMessage inMsg = inout.getInMessage();
        NormalizedMessage outMsg = mExchange.createMessage();
        //boolean success = true;
        String statusMessage = "";
        String jndiName = "";

        try {
          Object[] jndiConn = getDatabaseConnection(inMsg, inout.getExchangeId(), epb);
          jndiName = (String)jndiConn[0];
          connection = (Connection)jndiConn[1];

          rs = null;
          int rowsUpdated = -1;
          /*
           * Modified by Logicoy for [ task #20 ] DB BC - Insert statement should return primary key auto-increment value inserted
           */
          String generatedKeyValue = "";
          String outputValue = "";

          // writeMessage(inMsg, destinationAddress, false);
          JDBCOperationInput input = meta.getJDBCSql();
          if (input != null) {
            final String sql = input.getSql();
            if (meta.getJDBCOperationInput().getOperationType().
                equalsIgnoreCase(JDBCOperations.OPERATION_TYPE_SELECT.toString()) ||
                meta.getJDBCOperationInput().getOperationType().
                equalsIgnoreCase(JDBCOperations.OPERATION_TYPE_FIND.toString())) {
              try {
                rs = executeOutboundSQLSelect(inMsg, epb, meta, jndiName, connection);
              } catch (final SQLException ex) {
                faultCode = SERVER;
                faultDetail = mMessages.getString(
                        "DBBC_E00626.OMP_Failed_Exec_SQL") + ex.
                        getLocalizedMessage();
                String faultString = mMessages.getString(
                        "DBBC_E00706.JDBCDN_Failed_Denormalize", new Object[]{
                          inout.getExchangeId(), epb.getValue(
                          EndpointBean.ENDPOINT_NAME),
                          inout.getOperation(), ex.getLocalizedMessage()});
                AlertsUtil.getAlerter().warning(faultString,
                        JDBCBindingLifeCycle.SHORT_DISPLAY_NAME,
                        null,
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "DBBC_E00706");
                throw new Exception(faultString, ex);
              } catch (final Exception ex) {
                faultCode = CLIENT;
                faultDetail = mMessages.getString(
                        "DBBC_E00626.OMP_Failed_Exec_SQL") + ex.
                        getLocalizedMessage();
                String faultString = mMessages.getString(
                        "DBBC_E00706.JDBCDN_Failed_Denormalize", new Object[]{
                          inout.getExchangeId(), epb.getValue(
                          EndpointBean.ENDPOINT_NAME),
                          inout.getOperation(), ex.getLocalizedMessage()});
                throw new Exception(faultString, ex);
              }

              if (rs != null)
                statusMessage = "Success : ResultSet returned ";

              final JDBCNormalizer normalizer =
                      new JDBCNormalizer();
              Probe normalizationMeasurement = null;
              try {
                normalizationMeasurement =
                        Probe.info(getClass(),
                        epb.getUniqueName(),
                        JDBCBindingLifeCycle.PERF_CAT_NORMALIZATION);

                outMsg = normalizer.normalizeSelect(rs, inout,
                        meta, connection.getMetaData().
                        getDriverName());
              } catch (Exception e) {
                faultCode = SERVER;
                faultDetail =
                        "Unable to normalize response from the external service.";
                String faultString = mMessages.getString(
                        "DBBC_E00702.JDBCN_Failed_NM", new Object[]{
                          inout.getExchangeId(), epb.getValue(
                          EndpointBean.ENDPOINT_NAME),
                          inout.getOperation().toString()});
                AlertsUtil.getAlerter().warning(faultString,
                        JDBCBindingLifeCycle.SHORT_DISPLAY_NAME,
                        null,
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "DBBC_E00702");
                throw new Exception(faultString, e);
              } finally {
                if (normalizationMeasurement != null)
                  normalizationMeasurement.end();
              }
            } else {
              try {
                /*
                 * Modified by Logicoy for [ task #20 ] DB BC - Insert statement should return primary key auto-increment value inserted
                 */
                String generatedKey = meta.getJDBCSql().
                        getGeneratedKey();
                if (meta.getJDBCOperationInput().getOperationType().
                  equalsIgnoreCase(JDBCOperations.OPERATION_TYPE_INSERT.toString()) &&
                  generatedKey != null && !"".equals(generatedKey)) {
                  generatedKeyValue = executeOutboundSQLWithGeneratedKeys(inMsg, epb, meta, jndiName, connection);
                  outputValue = generatedKeyValue;
                  statusMessage =
                          "Success : Generated Key =  " + generatedKeyValue;
                } else {
                  rowsUpdated = executeOutboundSQL(inMsg, epb, meta, jndiName, connection);
                  statusMessage =
                          "Success : " + rowsUpdated + " are updated .";
                  outputValue = String.valueOf(rowsUpdated);
                }
              } catch (final SQLException ex) {
                faultCode = SERVER;
                faultDetail = mMessages.getString(
                        "DBBC_E00626.OMP_Failed_Exec_SQL");
                String faultString = mMessages.getString(
                        "DBBC_E00706.JDBCDN_Failed_Denormalize", new Object[]{
                          inout.getExchangeId(), epb.getValue(
                          EndpointBean.ENDPOINT_NAME),
                          inout.getOperation().toString(), ex.
                          getLocalizedMessage()});
                AlertsUtil.getAlerter().warning(faultString,
                        JDBCBindingLifeCycle.SHORT_DISPLAY_NAME,
                        null,
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "DBBC_E00706");
                throw new Exception(faultString, ex);
              } catch (final Exception ex) {
                faultCode = CLIENT;
                faultDetail = mMessages.getString(
                        "DBBC_E00626.OMP_Failed_Exec_SQL");
                String faultString = mMessages.getString(
                        "DBBC_E00706.JDBCDN_Failed_Denormalize", new Object[]{
                          inout.getExchangeId(), epb.getValue(
                          EndpointBean.ENDPOINT_NAME),
                          inout.getOperation().toString()});
                throw new Exception(faultString, ex);
              }


              final JDBCNormalizer normalizer =
                      new JDBCNormalizer();
              Probe normalizationMeasurement = null;
              try {
                normalizationMeasurement =
                        Probe.info(getClass(),
                        epb.getUniqueName(),
                        JDBCBindingLifeCycle.PERF_CAT_NORMALIZATION);
                /*
                 * Modified by Logicoy for [ task #20 ] DB BC - Insert statement should return primary key auto-increment value inserted
                 */
                outMsg =
                        normalizer.normalize(outputValue, inout,
                        meta);
              } catch (Exception e) {
                faultCode = SERVER;
                faultDetail =
                        "Unable to normalize response from the external service.";
                String faultString =
                        mMessages.getString(
                        "DBBC_E00702.JDBCN_Failed_NM",
                        new Object[]{
                          inout.getExchangeId(), epb.getValue(
                          EndpointBean.ENDPOINT_NAME),
                          mExchange.getOperation().toString(), e.
                          getLocalizedMessage()});
                AlertsUtil.getAlerter().warning(faultString,
                        JDBCBindingLifeCycle.SHORT_DISPLAY_NAME,
                        null,
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "DBBC_E00702");
                throw new Exception(faultString, e);
              } finally {
                if (normalizationMeasurement != null)
                  normalizationMeasurement.end();
              }
            }
            inout.setOutMessage(outMsg);
          } else {
            SPOperationInput spInput =
                    meta.getJDBCSPOperationInput();
            if (spInput != null) {
              cs = executeOutboundProc(inMsg, epb, meta, jndiName, connection);
              final JDBCNormalizer normalizer =
                      new JDBCNormalizer();
              final JDBCDenormalizer denormalizer =
                      new JDBCDenormalizer();
              if (connection.getMetaData().getDatabaseProductName().
                      toLowerCase().contains("sql server") || connection.
                      getMetaData().getDatabaseProductName().
                      toLowerCase().contains("adaptive server"))
                if (outParamIndex.size() == 0) {
                  outParamNames.put(1, denormalizer.getProcName(meta.
                          getJDBCSPOperationInput().
                          getExecutionString()));
                  outParamTypes.put(1, "RESULTSET");
                  outParamIndex.add(
                          Integer.valueOf(Double.valueOf(1).intValue()));
                } /*else
              for (int i = 1; i <= outParamIndex.size(); i++)
              if ((outParamNames.get(i) != null) && (outParamNames.get(i).
              equalsIgnoreCase(
              "RETURN_VALUE"))) {
              outParamNames.put(i, denormalizer.getProcName(meta.
              getJDBCSPOperationInput().
              getExecutionString()));
              outParamTypes.put(i, "RETURN_VALUE");
              }*/
              normalizer.setOutParamIndex(outParamIndex);
              normalizer.setOutParamNames(outParamNames);
              normalizer.setOutParamTypes(outParamTypes);
              Probe normalizationMeasurement = null;
              try {
                normalizationMeasurement =
                        Probe.info(getClass(),
                        epb.getUniqueName(),
                        JDBCBindingLifeCycle.PERF_CAT_NORMALIZATION);
                outMsg = normalizer.normalizeProcedure(cs, inout, meta, connection.getMetaData().
                        getDatabaseProductName().toLowerCase());
              } catch (final SQLException ex) {
                faultCode = SERVER;
                faultDetail =
                        "Unable to normalize response from the external service.";
                String faultString = mMessages.getString(
                        "DBBC_E00727.JDBCN_Failed_NM_SQL", new Object[]{
                          ex.getClass().getName(),
                          ex.getLocalizedMessage()
                        });
                throw new Exception(faultString, ex);
              } catch (Exception e) {
                faultCode = SERVER;
                faultDetail =
                        "Unable to normalize response from the external service.";
                String faultString =
                        mMessages.getString(
                        "DBBC_E00702.JDBCN_Failed_NM", new Object[]{
                          inout.getExchangeId(), epb.getValue(
                          EndpointBean.ENDPOINT_NAME),
                          mExchange.getOperation().toString()});
                AlertsUtil.getAlerter().warning(faultString,
                        JDBCBindingLifeCycle.SHORT_DISPLAY_NAME,
                        null,
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "DBBC_E00702");
                throw new Exception(faultString, e);
              } finally {
                if (normalizationMeasurement != null)
                  normalizationMeasurement.end();

              }
              inout.setOutMessage(outMsg);

            }
          }
        } catch (final Exception ex) {
          mLogger.log(Level.SEVERE,
                  mMessages.getString("DBBC_E00622.OMP_Failed_writing"),
                  ex);
          // should this populate a full fault instead?
          AlertsUtil.getAlerter().warning(mMessages.getString(
                  "DBBC_E00622.OMP_Failed_writing"),
                  JDBCBindingLifeCycle.SHORT_DISPLAY_NAME,
                  null,
                  AlertsUtil.getServerType(),
                  AlertsUtil.COMPONENT_TYPE_BINDING,
                  NotificationEvent.OPERATIONAL_STATE_RUNNING,
                  NotificationEvent.EVENT_TYPE_ALERT,
                  "DBBC_E00622");
          // inout.setError(ex);
          statusMessage = "Failed " + ex.getMessage();
          success = false;
          setErrorInExchange(inout, faultCode, faultDetail, ex);
          inout.setStatus(ExchangeStatus.ERROR);

        }

        mChannel.send(inout);

        if (success)
          updateTallySentReplies(epb);
        else
          epb.getEndpointStatus().incrementSentErrors();
      } catch (final Exception ex) {
        mLogger.log(Level.SEVERE,
                mMessages.getString("DBBC_E00623.OMP_Failed_inout"), ex);
        throw new RuntimeException(ex);
      } finally {
        try {
          if (rs != null)
            rs.close();
          if (cs != null)
            cs.close();
          if (ps != null)
            ps.close();
          if (connection != null)
            connection.close();
        } catch (SQLException sqlexception) {
          mLogger.log(Level.SEVERE,
                  mMessages.getString(
                  "DBBC_E00628.OMP_Cleanup_Failure"),
                  sqlexception);
        }
      }
    }
  }

  private void processInOutXA(final InOut inout, final EndpointBean epb) {
    mLogger.log(Level.INFO, "Entering processInOutXA");
    XAConnection xaconnection = null;
    Transaction transaction = null;
    String faultCode = null;
    String faultDetail = null;
    boolean success = true;

    if (inout.getStatus() == ExchangeStatus.DONE) {

      // remove the redelivery listener handler - no retry needed.
      MessageExchangeSupport.removeReplyListener(inout.getExchangeId());

      updateTallyReceives(epb, true);
    } else if (inout.getStatus() == ExchangeStatus.ERROR) {

      // added for retry support

      updateTallyReceives(epb, false);

      // send alerts
      String errorMsg = inout.getError().getMessage();
      if (errorMsg != null) {
        String msg = mMessages.getString(
                "DBBC-E00720.Message_exchange_error",
                new Object[]{
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
                "DBBC-E00720");
      } else {
        String msg = mMessages.getString(
                "DBBC-E00721.Message_exchange_error_no_detail",
                new Object[]{
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
                "DBBC-E00721");
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
      if (retryStatus != null && retryStatus.getRemainingRetries() > 0)
        try {
          MessageExchangeSupport.notifyOfRedelivery(inout);
        } catch (Exception e) {
          String groupId = (String) inout.getProperty(CRMP_GROUP_ID);
          String messageId = (String) inout.getProperty(
                  CRMP_MESSAGE_ID);
          if (mLogger.isLoggable(Level.WARNING)) {
            String text = mMessages.getString(
                    "DBBC-E01036.Failed_to_process_redelivery",
                    new Object[]{groupId, messageId});
            mLogger.log(Level.WARNING, text, e);
            AlertsUtil.getAlerter().warning(text,
                    JDBCBindingLifeCycle.SHORT_DISPLAY_NAME,
                    epb.getDeploymentId(),
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "DBBC-E01036");
          }
        }

      //epb.getEndpointStatus().incrementReceivedErrors();
    } else {

      // added for retry support
      try {
        MessageExchangeSupport.notifyOfReply(inout);
      } catch (Exception ex) {
        if (mLogger.isLoggable(Level.WARNING)) {
          String text = mMessages.getString(
                  "DBBC-E00759.Exception_during_reply_processing", ex.
                  getLocalizedMessage());
          mLogger.log(Level.SEVERE, text, ex);
          AlertsUtil.getAlerter().warning(text,
                  JDBCBindingLifeCycle.SHORT_DISPLAY_NAME,
                  epb.getDeploymentId(),
                  AlertsUtil.getServerType(),
                  AlertsUtil.COMPONENT_TYPE_BINDING,
                  NotificationEvent.OPERATIONAL_STATE_RUNNING,
                  NotificationEvent.EVENT_TYPE_ALERT,
                  "DBBC-E00759");
        }
        success = false;
      }

      try {
        updateTallyReceivedRequests(epb);
        Map operationNameToMetaData = (Map) epb.getValueObj(
                EndpointBean.OPERATION_NAME_TO_META_DATA);
        OperationMetaData meta = (OperationMetaData) operationNameToMetaData.get(inout.getOperation().
                getLocalPart());

        if (meta == null)
          throw new MessagingException(mMessages.getString(
                  "DBBC_E00621.OMP_oper_NotDefined") + inout.getOperation());

        final NormalizedMessage inMsg = inout.getInMessage();
        NormalizedMessage outMsg = mExchange.createMessage();

        String statusMessage = "";
        String jndiName = null;

        try {
          Object[] jndiConn = getDatabaseConnection(inMsg, inout.getExchangeId(), epb);
          jndiName = (String)jndiConn[0];
          connection = (Connection)jndiConn[1];

          rs = null;
          int rowsUpdated = -1;

          String generatedKeyValue = "";
          String outputValue = "";

          // writeMessage(inMsg, destinationAddress, false);
          JDBCOperationInput input = meta.getJDBCSql();
          if (input != null) {
            final String sql = input.getSql();

            if (inout.isTransacted())
              // Removing manual enlistment. Moving to automatic resource enlistment
              // mtxHelper.handleOutbound(mExchange);
              // enlistResource(epb);
              transaction =
                      (Transaction) inout.getProperty(
                      MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
            if (transaction != null)
              resumeThreadTx(transaction);

            connection.setAutoCommit(true);

            /* PP: Glassfish does not return a XADataSource and always returns
             * a DataSource30 object which does not implement getXAResource() method
            xaconnection = getXADatabaseConnection(epb);
            XAResource xaresource = xaconnection.getXAResource();
            transaction.enlistResource(xaresource);
            connection = xaconnection.getConnection();*/

            if (meta.getJDBCOperationInput().getOperationType().
                equalsIgnoreCase(JDBCOperations.OPERATION_TYPE_SELECT.toString()) ||
                meta.getJDBCOperationInput().getOperationType().
                equalsIgnoreCase(JDBCOperations.OPERATION_TYPE_FIND.toString())) {
              try {
                rs = executeOutboundSQLSelect(inMsg, epb, meta, jndiName, connection);
              } catch (final SQLException ex) {
                faultCode = SERVER;
                faultDetail = mMessages.getString(
                        "DBBC_E00626.OMP_Failed_Exec_SQL");
                String faultString = mMessages.getString(
                        "DBBC_E00706.JDBCDN_Failed_Denormalize", new Object[]{
                          inout.getExchangeId(), epb.getValue(
                          EndpointBean.ENDPOINT_NAME),
                          inout.getOperation().toString(), ex.
                          getLocalizedMessage()
                        });
                processException(ex, transaction, inout, epb,
                        faultCode, faultDetail);
                AlertsUtil.getAlerter().warning(faultDetail,
                        JDBCBindingLifeCycle.SHORT_DISPLAY_NAME,
                        null,
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "DBBC_E00626");
                throw new Exception(faultString, ex);
              } catch (final Exception ex) {
                faultCode = CLIENT;
                faultDetail = mMessages.getString(
                        "DBBC_E00626.OMP_Failed_Exec_SQL");
                String faultString = mMessages.getString(
                        "DBBC_E00706.JDBCDN_Failed_Denormalize", new Object[]{
                          inout.getExchangeId(), epb.getValue(
                          EndpointBean.ENDPOINT_NAME),
                          inout.getOperation().toString()
                        });
                processException(ex, transaction, inout, epb,
                        faultCode, faultDetail);
                throw new Exception(faultString, ex);
              }
              if (rs != null)
                statusMessage = "Success : ResultSet returned ";

              final JDBCNormalizer normalizer =
                      new JDBCNormalizer();
              Probe normalizationMeasurement = null;

              try {
                normalizationMeasurement =
                        Probe.info(getClass(),
                        epb.getUniqueName(),
                        JDBCBindingLifeCycle.PERF_CAT_NORMALIZATION);

                outMsg = normalizer.normalizeSelect(rs, inout,
                        meta, connection.getMetaData().
                        getDriverName());
              } catch (Exception e) {
                faultCode = SERVER;
                faultDetail =
                        "Unable to normalize response from the external service.";
                String faultString = mMessages.getString(
                        "DBBC_E00702.JDBCN_Failed_NM", new Object[]{
                          inout.getExchangeId(), epb.getValue(
                          EndpointBean.ENDPOINT_NAME),
                          inout.getOperation().toString(), e.getLocalizedMessage()
                        });
                processException(e, transaction, inout, epb,
                        faultCode, faultDetail);
                AlertsUtil.getAlerter().warning(faultString,
                        JDBCBindingLifeCycle.SHORT_DISPLAY_NAME,
                        null,
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "DBBC_E00702");
                throw new Exception(faultString, e);
              } finally {
                if (normalizationMeasurement != null)
                  normalizationMeasurement.end();
              }
              inout.setOutMessage(outMsg);
            } else {
              if (meta.getJDBCOperationInput().getOperationType().
                      equalsIgnoreCase(JDBCOperations.OPERATION_TYPE_EXECUTE.
                      toString())) {
                try {
                  cs = executeOutboundProc(inMsg, epb, meta, jndiName, connection);
                } catch (final SQLException ex) {
                  faultCode = SERVER;
                  faultDetail = mMessages.getString(
                          "DBBC_E00626.OMP_Failed_Exec_SQL");
                  String faultString = mMessages.getString(
                          "DBBC_E00706.JDBCDN_Failed_Denormalize", new Object[]{
                            inout.getExchangeId(), epb.getValue(
                            EndpointBean.ENDPOINT_NAME),
                            inout.getOperation().toString(), ex.
                            getLocalizedMessage()
                          });
                  processException(ex, transaction, inout, epb,
                          faultCode, faultDetail);
                  AlertsUtil.getAlerter().warning(faultString,
                          JDBCBindingLifeCycle.SHORT_DISPLAY_NAME,
                          null,
                          AlertsUtil.getServerType(),
                          AlertsUtil.COMPONENT_TYPE_BINDING,
                          NotificationEvent.OPERATIONAL_STATE_RUNNING,
                          NotificationEvent.EVENT_TYPE_ALERT,
                          "DBBC_E00626");
                  throw new Exception(faultString, ex);
                } catch (final Exception ex) {
                  faultCode = CLIENT;
                  faultDetail = mMessages.getString(
                          "DBBC_E00626.OMP_Failed_Exec_SQL");
                  String faultString = mMessages.getString(
                          "DBBC_E00706.JDBCDN_Failed_Denormalize", new Object[]{
                            inout.getExchangeId(), epb.getValue(
                            EndpointBean.ENDPOINT_NAME),
                            inout.getOperation().toString()
                          });
                  processException(ex, transaction, inout, epb,
                          faultCode, faultDetail);
                  throw new Exception(faultString, ex);
                }
                final JDBCNormalizer normalizer =
                        new JDBCNormalizer();
                Probe normalizationMeasurement = null;
                try {
                  normalizationMeasurement =
                          Probe.info(getClass(),
                          epb.getUniqueName(),
                          JDBCBindingLifeCycle.PERF_CAT_NORMALIZATION);
                  outMsg = normalizer.normalizeProcedure(cs,
                          inout, meta, connection.getMetaData().
                          getDatabaseProductName());
                } catch (Exception e) {
                  faultCode = SERVER;
                  faultDetail =
                          "Unable to normalize response from the external service.";
                  String faultString = mMessages.getString(
                          "DBBC_E00702.JDBCN_Failed_NM", new Object[]{
                            inout.getExchangeId(), epb.getValue(
                            EndpointBean.ENDPOINT_NAME),
                            inout.getOperation().toString()
                          });
                  processException(e, transaction, inout, epb,
                          faultCode, faultDetail);
                  AlertsUtil.getAlerter().warning(faultString,
                          JDBCBindingLifeCycle.SHORT_DISPLAY_NAME,
                          null,
                          AlertsUtil.getServerType(),
                          AlertsUtil.COMPONENT_TYPE_BINDING,
                          NotificationEvent.OPERATIONAL_STATE_RUNNING,
                          NotificationEvent.EVENT_TYPE_ALERT,
                          "DBBC_E00702");
                  throw new Exception(faultString, e);
                } finally {
                  if (normalizationMeasurement != null)
                    normalizationMeasurement.end();
                }
              } else {
                try {
                  /*
                   * Modified by Logicoy for [ task #20 ] DB BC - Insert statement should return primary key auto-increment value inserted
                   */
                  String generatedKey = meta.getJDBCSql().
                          getGeneratedKey();
                  if (meta.getJDBCOperationInput().getOperationType().
                    equalsIgnoreCase(JDBCOperations.OPERATION_TYPE_INSERT.toString()) &&
                    generatedKey != null && !"".equals(generatedKey)) {
                    generatedKeyValue = executeOutboundSQLWithGeneratedKeys(inMsg, epb, meta, jndiName, connection);
                    outputValue = generatedKeyValue;
                    statusMessage =
                            "Success : Generated Key =  " + generatedKeyValue;
                  } else {
                    rowsUpdated = executeOutboundSQL(inMsg, epb, meta, jndiName, connection);
                    statusMessage =
                            "Success : " + rowsUpdated + " are updated .";
                    outputValue =
                            String.valueOf(rowsUpdated);
                  }
                } catch (final SQLException ex) {
                  faultCode = SERVER;
                  faultDetail = mMessages.getString(
                          "DBBC_E00626.OMP_Failed_Exec_SQL");
                  String faultString = mMessages.getString(
                          "DBBC_E00706.JDBCDN_Failed_Denormalize", new Object[]{
                            inout.getExchangeId(), epb.getValue(
                            EndpointBean.ENDPOINT_NAME),
                            inout.getOperation().toString(), ex.
                            getLocalizedMessage()
                          });
                  processException(ex, transaction, inout, epb,
                          faultCode, faultDetail);
                  AlertsUtil.getAlerter().warning(faultString,
                          JDBCBindingLifeCycle.SHORT_DISPLAY_NAME,
                          null,
                          AlertsUtil.getServerType(),
                          AlertsUtil.COMPONENT_TYPE_BINDING,
                          NotificationEvent.OPERATIONAL_STATE_RUNNING,
                          NotificationEvent.EVENT_TYPE_ALERT,
                          "DBBC_E00706");
                } catch (final Exception ex) {
                  faultCode = CLIENT;
                  faultDetail = mMessages.getString(
                          "DBBC_E00626.OMP_Failed_Exec_SQL");
                  String faultString = mMessages.getString(
                          "DBBC_E00706.JDBCDN_Failed_Denormalize", new Object[]{
                            inout.getExchangeId(), epb.getValue(
                            EndpointBean.ENDPOINT_NAME),
                            inout.getOperation().toString()
                          });
                  processException(ex, transaction, inout, epb,
                          faultCode, faultDetail);
                  throw new Exception(faultString, ex);
                }


                final JDBCNormalizer normalizer =
                        new JDBCNormalizer();
                Probe normalizationMeasurement = null;
                try {
                  /*
                   * Modified by Logicoy for [ task #20 ] DB BC - Insert statement should return primary key auto-increment value inserted
                   */
                  outMsg = normalizer.normalize(outputValue,
                          inout, meta);
                } catch (Exception e) {
                  faultCode = SERVER;
                  faultDetail =
                          "Unable to normalize response from the external service.";
                  String faultString = mMessages.getString(
                          "DBBC_E00702.JDBCN_Failed_NM", new Object[]{
                            inout.getExchangeId(), epb.getValue(
                            EndpointBean.ENDPOINT_NAME),
                            inout.getOperation().toString()
                          });
                  AlertsUtil.getAlerter().warning(faultString,
                          JDBCBindingLifeCycle.SHORT_DISPLAY_NAME,
                          null,
                          AlertsUtil.getServerType(),
                          AlertsUtil.COMPONENT_TYPE_BINDING,
                          NotificationEvent.OPERATIONAL_STATE_RUNNING,
                          NotificationEvent.EVENT_TYPE_ALERT,
                          "DBBC_E00702");
                  processException(e, transaction, inout, epb,
                          faultCode, faultDetail);
                  throw new Exception(faultString, e);
                } finally {
                  if (normalizationMeasurement != null)
                    normalizationMeasurement.end();
                }
              }
              inout.setOutMessage(outMsg);
            }
          } else {
            SPOperationInput spInput =
                    meta.getJDBCSPOperationInput();
            if (spInput != null) {
              if (inout.isTransacted())
                // Removing manual enlistment. Moving to automatic resource enlistment
                // mtxHelper.handleOutbound(mExchange);
                // enlistResource(epb);
                transaction =
                        (Transaction) inout.getProperty(
                        MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
              if (transaction != null)
                resumeThreadTx(transaction);
              connection.setAutoCommit(true);
              cs = executeOutboundProc(inMsg, epb, meta, jndiName, connection);
              final JDBCNormalizer normalizer =
                      new JDBCNormalizer();
              final JDBCDenormalizer denormalizer =
                      new JDBCDenormalizer();
              if (connection.getMetaData().getDatabaseProductName().
                      toLowerCase().contains("sql server") || connection.
                      getMetaData().getDatabaseProductName().
                      toLowerCase().contains("adaptive server"))
                if (outParamIndex.size() == 0) {
                  outParamNames.put(1, denormalizer.getProcName(meta.
                          getJDBCSPOperationInput().
                          getExecutionString()));
                  outParamTypes.put(1, "RESULTSET");
                  outParamIndex.add(
                          Integer.valueOf(Double.valueOf(1).intValue()));
                } else
                  for (int i = 1; i <= outParamIndex.size(); i++)
                    if ((outParamNames.get(i) != null) && (outParamNames.get(i).
                            equalsIgnoreCase(
                            "RETURN_VALUE"))) {
                      outParamNames.put(i, denormalizer.getProcName(meta.
                              getJDBCSPOperationInput().
                              getExecutionString()));
                      outParamTypes.put(i, "RESULTSET");
                    }
              normalizer.setOutParamIndex(outParamIndex);
              normalizer.setOutParamNames(outParamNames);
              normalizer.setOutParamTypes(outParamTypes);
              Probe normalizationMeasurement = null;
              try {
                normalizationMeasurement =
                        Probe.info(getClass(),
                        epb.getUniqueName(),
                        JDBCBindingLifeCycle.PERF_CAT_NORMALIZATION);
                outMsg = normalizer.normalizeProcedure(cs, inout, meta, connection.getMetaData().
                        getDatabaseProductName().toLowerCase());
              } catch (Exception e) {
                faultCode = SERVER;
                faultDetail =
                        "Unable to normalize response from the external service.";
                String faultString =
                        mMessages.getString(
                        "DBBC_E00702.JDBCN_Failed_NM", new Object[]{
                          inout.getExchangeId(), epb.getValue(
                          EndpointBean.ENDPOINT_NAME),
                          mExchange.getOperation().toString()});
                processException(e, transaction, inout, epb,
                        faultCode, faultDetail);
                throw new Exception(faultString, e);
              } finally {
                if (normalizationMeasurement != null)
                  normalizationMeasurement.end();

              }
              inout.setOutMessage(outMsg);

            }
          }
        } catch (final Exception ex) {
          mLogger.log(Level.SEVERE,
                  OutboundMessageProcessor.mMessages.getString(
                  "DBBC_E00622.OMP_Failed_writing"), ex.getLocalizedMessage());
          success = false;
          setErrorInExchange(inout, faultCode, faultDetail, ex);
          inout.setStatus(ExchangeStatus.ERROR);
        }
      } catch (final Exception ex) {
        mLogger.log(Level.SEVERE,
                OutboundMessageProcessor.mMessages.getString(
                "DBBC_E00623.OMP_Failed_inout"), ex);
        throw new RuntimeException(ex);
      } finally {
        try {
          getTransactionManager().suspend();
          mChannel.send(inout);


          if (success)
            updateTallySentReplies(epb);
          else
            epb.getEndpointStatus().incrementSentErrors();
          if (rs != null)
            rs.close();
          if (cs != null)
            cs.close();

          if (ps != null)
            ps.close();
          if (connection != null)
            connection.close();
        } catch (SQLException sqlexception) {
          if (mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.SEVERE, mMessages.getString(
                    "DBBC_E00628.OMP_Cleanup_Failure"), sqlexception);
          else if (mLogger.isLoggable(Level.INFO))
            mLogger.log(Level.SEVERE, mMessages.getString(
                    "DBBC_E00628.OMP_Cleanup_Failure"));
        } catch (Exception ex) {
          if (mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.SEVERE, mMessages.getString(
                    "DBBC_E00628.OMP_Cleanup_Failure"), ex);
          else if (mLogger.isLoggable(Level.INFO))
            mLogger.log(Level.SEVERE, mMessages.getString(
                    "DBBC_E00628.OMP_Cleanup_Failure"));

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
    Transaction transaction = null;
    boolean success = true;
    String faultCode = null;
    String faultDetail = null;
    if (inonly.getStatus() == ExchangeStatus.DONE)
      // remove the redelivery listener handler - no retry needed.
      MessageExchangeSupport.removeReplyListener(inonly.getExchangeId());
    else if (inonly.getStatus() == ExchangeStatus.ERROR) {

      // added for retry support
      updateTallyReceives(epb, false);

      // send alerts
      String errorMsg = inonly.getError().getMessage();
      if (errorMsg != null) {
        String msg = mMessages.getString(
                "DBBC-E00720.Message_exchange_error",
                new Object[]{
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
                "DBBC-E00720");
      } else {
        String msg = mMessages.getString(
                "DBBC-E00721.Message_exchange_error_no_detail",
                new Object[]{
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
                "DBBC-E00721");
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
      RedeliveryStatus retryStatus =
              Redelivery.getRedeliveryStatus(inonly);
      if (retryStatus != null && retryStatus.getRemainingRetries() > 0) {
        try {
          MessageExchangeSupport.notifyOfRedelivery(inonly);
        } catch (Exception e) {
          String groupId = (String) inonly.getProperty(CRMP_GROUP_ID);
          String messageId = (String) inonly.getProperty(
                  CRMP_MESSAGE_ID);
          if (mLogger.isLoggable(Level.WARNING)) {
            String text = mMessages.getString(
                    "DBBC-E01036.Failed_to_process_redelivery",
                    new Object[]{groupId, messageId});
            mLogger.log(Level.WARNING, text, e);
            AlertsUtil.getAlerter().warning(text,
                    JDBCBindingLifeCycle.SHORT_DISPLAY_NAME,
                    epb.getDeploymentId(),
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "DBBC-E01036");
          }
        }
        updateTallyReceives(epb, false);
      }
    }

    // added for retry support
    try {
      MessageExchangeSupport.notifyOfReply(inonly);
    } catch (Exception ex) {
      if (mLogger.isLoggable(Level.WARNING)) {
        String text = mMessages.getString(
                "DBBC-E00759.Exception_during_reply_processing", ex.
                getLocalizedMessage());
        mLogger.log(Level.SEVERE, text, ex);
        AlertsUtil.getAlerter().warning(text,
                JDBCBindingLifeCycle.SHORT_DISPLAY_NAME,
                epb.getDeploymentId(),
                AlertsUtil.getServerType(),
                AlertsUtil.COMPONENT_TYPE_BINDING,
                NotificationEvent.OPERATIONAL_STATE_RUNNING,
                NotificationEvent.EVENT_TYPE_ALERT,
                "DBBC-E00759");
      }
      success = false;
    }

    try {
      updateTallyReceivedRequests(epb);

      final NormalizedMessage inMsg = inonly.getInMessage();
      final Map operationNameToMetaData = (Map) epb.getValueObj(
              EndpointBean.OPERATION_NAME_TO_META_DATA);
      final OperationMetaData meta = (OperationMetaData) operationNameToMetaData.get(inonly.getOperation().
              getLocalPart());

      if (meta == null)
        throw new MessagingException(OutboundMessageProcessor.mMessages.
                getString("DBBC_E00621.OMP_oper_NotDefined") + inonly.
                getOperation());


      if (inonly.isTransacted())
        // Removing manual enlistment. Moving to automatic resource enlistment
        // mtxHelper.handleOutbound(mExchange);
        // enlistResource(epb);
        transaction = (Transaction) inonly.getProperty(
                MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);

      try {
        if (transaction != null)
          getTransactionManager().resume(transaction);
        Object[] jndiConn = getDatabaseConnection(inMsg, inonly.getExchangeId(), epb);
        String jndiName = (String)jndiConn[0];
        connection = (Connection)jndiConn[1];

        if (transaction != null)
          connection.setAutoCommit(true);
        // writeMessage(inMsg, destinationAddress, false);
        if (meta.getJDBCOperationInput().getOperationType().
                equalsIgnoreCase(
                JDBCOperations.OPERATION_TYPE_EXECUTE.toString()))
          // Auto enlistment: pass the transaction retrieved from the Message exchange.
          cs = executeOutboundProc(inMsg, epb, meta, jndiName, connection);
        else
          // Auto enlistment: pass the transaction retrieved from the Message exchange.
          executeOutboundSQL(inMsg, epb, meta, jndiName, connection);
        inonly.setStatus(ExchangeStatus.DONE);
      } catch (final SQLException ex) {
        processException(ex, transaction, inonly, epb, faultCode,
                faultDetail);
      } catch (final MessagingException ex) {
        processException(ex, transaction, inonly, epb, faultCode,
                faultDetail);
      } catch (final Exception ex) {
        processException(ex, transaction, inonly, epb, faultCode,
                faultDetail);
      } finally {
        try {
          if (cs != null)
            cs.close();

          if (ps != null)
            ps.close();
          if (connection != null)
            connection.close();
        } catch (SQLException sqlexception) {
          if (mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.SEVERE, mMessages.getString(
                    "DBBC_E00628.OMP_Cleanup_Failure"), sqlexception);
          else if (mLogger.isLoggable(Level.INFO))
            mLogger.log(Level.SEVERE, mMessages.getString(
                    "DBBC_E00628.OMP_Cleanup_Failure"));
        }
      }

      /*
       * if (inonly.isTransacted()) { mtxHelper.handleInbound(mExchange); }
       */

      if (transaction != null)
        getTransactionManager().suspend();
      mChannel.send(inonly);


      if (success)
        updateTallySends(epb, success);
      else
        epb.getEndpointStatus().incrementSentErrors();
    } catch (final Exception ex) {
      if (mLogger.isLoggable(Level.FINE))
        mLogger.log(Level.SEVERE, mMessages.getString(
                "DBBC_E00624.OMP_Failed_inonly"), ex);
      else if (mLogger.isLoggable(Level.INFO))
        mLogger.log(Level.SEVERE, mMessages.getString(
                "DBBC_E00624.OMP_Failed_inonly"));

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
                                               final String jndiName,
                                               Connection connection) throws SQLException, MessagingException {
    String sql = null;
    try {
      sql = opMetaData.getJDBCSql().getSql();
      mLogger.log(Level.INFO,
              OutboundMessageProcessor.mMessages.getString(
              "DBBC_R00625.OMP_Exec_SQL") + sql);

      ps = connection.prepareStatement(sql);

      final JDBCDenormalizer denormalizer = new JDBCDenormalizer();
      Probe denormalizationMeasurement = Probe.info(getClass(),
              eBean.getUniqueName(),
              JDBCBindingLifeCycle.PERF_CAT_DENORMALIZATION);

      denormalizer.denormalizeOutbound(nMsg, getDBName(eBean, nMsg), jndiName,
              opMetaData, ps);

      if (denormalizationMeasurement != null)
        denormalizationMeasurement.end();

      rs = ps.executeQuery();
    } catch (final SQLException ex) {
      final String msg = OutboundMessageProcessor.mMessages.getString(
              "DBBC_E00626.OMP_Failed_Exec_SQL") + sql +
              " Reason: " + ex.getLocalizedMessage() + " SQLState: " + ex.
              getSQLState() + " ErrorCode: " + ex.getErrorCode();
      throw new SQLException(msg);
    } catch (final MessagingException ex) {
      final String msg = OutboundMessageProcessor.mMessages.getString(
              "DBBC_E00626.OMP_Failed_Exec_SQL") + sql +
              " Reason: " + ex.getLocalizedMessage();
      throw new MessagingException(msg, ex);
    } catch (final Exception ex) {
      final String msg = OutboundMessageProcessor.mMessages.getString(
              "DBBC_E00626.OMP_Failed_Exec_SQL") + sql +
              " Reason: " + ex.getLocalizedMessage();
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
                                   final String jndiName,
                                   Connection connection) throws SQLException, MessagingException {
    String sql = null;
    int rowsUpdated = -1;

    try {
      sql = opMetaData.getJDBCSql().getSql();
      mLogger.log(Level.INFO, mMessages.getString(
              "DBBC_R00625.OMP_Exec_SQL") + sql);

      ps = connection.prepareStatement(sql);

      final JDBCDenormalizer denormalizer = new JDBCDenormalizer();
      Probe denormalizationMeasurement = Probe.info(getClass(),
              eBean.getUniqueName(),
              JDBCBindingLifeCycle.PERF_CAT_DENORMALIZATION);

      denormalizer.denormalizeOutbound(nMsg, getDBName(eBean, nMsg), jndiName,
              opMetaData, ps);
      if (denormalizationMeasurement != null)
        denormalizationMeasurement.end();

      rowsUpdated = ps.executeUpdate();
    } catch (final SQLException ex) {
      final String msg = mMessages.getString(
              "DBBC_E00626.OMP_Failed_Exec_SQL") + sql +
              " Reason: " + ex.getLocalizedMessage() + " SQLState: " + ex.
              getSQLState() + " ErrorCode: " + ex.getErrorCode();
      throw new SQLException(msg);
    } catch (final MessagingException ex) {
      final String msg = OutboundMessageProcessor.mMessages.getString(
              "DBBC_E00626.OMP_Failed_Exec_SQL") + sql +
              " Reason: " + ex.getLocalizedMessage();
      throw new MessagingException(msg, ex);
    } catch (final Exception ex) {
      final String msg = OutboundMessageProcessor.mMessages.getString(
              "DBBC_E00626.OMP_Failed_Exec_SQL") + sql +
              " Reason: " + ex.getLocalizedMessage();
      throw new MessagingException(msg, ex);
    }
    return rowsUpdated;
  }

  /*
   * Added by Logicoy for [ task #20 ] DB BC - Insert statement should return primary key auto-increment value inserted
   */
  /**
   * @param nMsg
   * @param eBean
   * @param opMetaData
   * @return
   * @throws MessagingException
   */
  protected String executeOutboundSQLWithGeneratedKeys(final NormalizedMessage nMsg,
                                                       final EndpointBean eBean,
                                                       final OperationMetaData opMetaData,
                                                       final String jndiName,
                                                       Connection connection) throws SQLException, MessagingException {
    mLogger.log(Level.INFO, "Entering executeOutboundSQLWithGeneratedKeys");
    String sql = null;
    int rowsUpdated = -1;
    String generatedKeyValue = null;

    try {
      sql = opMetaData.getJDBCSql().getSql();
      mLogger.log(Level.INFO, mMessages.getString(
              "DBBC_R00625.OMP_Exec_SQL") + sql);

      String outputColumn = opMetaData.getJDBCSql().getGeneratedKey();

      ps = connection.prepareStatement(sql, new String[]{outputColumn});

      final JDBCDenormalizer denormalizer = new JDBCDenormalizer();
      Probe denormalizationMeasurement = Probe.info(getClass(),
              eBean.getUniqueName(),
              JDBCBindingLifeCycle.PERF_CAT_DENORMALIZATION);

      denormalizer.denormalizeOutbound(nMsg, getDBName(eBean, nMsg), jndiName,
              opMetaData, ps);
      if (denormalizationMeasurement != null)
        denormalizationMeasurement.end();


      rowsUpdated = ps.executeUpdate();
      ResultSet rs = ps.getGeneratedKeys();
      if (rs != null && rs.next())
        generatedKeyValue = rs.getString(1);
    } catch (final SQLException ex) {
      final String msg = mMessages.getString(
              "DBBC_E00626.OMP_Failed_Exec_SQL") + sql +
              " Reason: " + ex.getLocalizedMessage() + " SQLState: " + ex.
              getSQLState() + " ErrorCode: " + ex.getErrorCode();
      throw new SQLException(msg);
    } catch (final MessagingException ex) {
      final String msg = OutboundMessageProcessor.mMessages.getString(
              "DBBC_E00626.OMP_Failed_Exec_SQL") + sql +
              " Reason: " + ex.getLocalizedMessage();
      throw new MessagingException(msg, ex);
    } catch (final Exception ex) {
      final String msg = OutboundMessageProcessor.mMessages.getString(
              "DBBC_E00626.OMP_Failed_Exec_SQL") + sql +
              " Reason: " + ex.getLocalizedMessage();
      throw new MessagingException(msg, ex);
    }
    mLogger.log(Level.INFO, "Exiting executeOutboundSQLWithGeneratedKeys");
    return generatedKeyValue;
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
                                                  final String jndiName,
                                                  Connection connnection) throws SQLException, MessagingException {
    String sql = null;
    int rowsUpdated = -1;

    try {
      sql = opMetaData.getJDBCSPOperationInput().getExecutionString();
      mLogger.log(Level.INFO, mMessages.getString(
              "DBBC_R00625.OMP_Exec_SQL") + sql);

      cs = connnection.prepareCall(sql);
      DatabaseMetaData dbmeta = connnection.getMetaData();

      final JDBCDenormalizer denormalizer = new JDBCDenormalizer();

      if (eBean.getValue(EndpointBean.JDBC_DATABASE_NAME) != null)
        denormalizer.setDatabaseName(eBean.getValue(
                EndpointBean.JDBC_DATABASE_NAME));
      Probe denormalizationMeasurement = Probe.info(getClass(),
              eBean.getUniqueName(),
              JDBCBindingLifeCycle.PERF_CAT_DENORMALIZATION);
      denormalizer.denormalizeOutboundProc(nMsg, opMetaData, dbmeta, jndiName, cs);
      if (denormalizationMeasurement != null)
        denormalizationMeasurement.end();

      outParamIndex = denormalizer.getOutParamIndex();
      outParamTypes = denormalizer.getOutParamTypes();
      outParamNames = denormalizer.getOutParamNames();
      // rowsUpdated = cs.executeUpdate();
      boolean b = cs.execute();
      return cs;
    } catch (final SQLException ex) {
      final String msg = OutboundMessageProcessor.mMessages.getString(
              "DBBC_R00624.OMP_Failed_Exec_SQL") + sql +
              " Reason: " + ex.getLocalizedMessage() + " SQLState: " + ex.
              getSQLState() + " ErrorCode: " + ex.getErrorCode();
      throw new SQLException(msg);
    } catch (final MessagingException ex) {
      final String msg = OutboundMessageProcessor.mMessages.getString(
              "DBBC_E00626.OMP_Failed_Exec_SQL") + sql +
              " Reason: " + ex.getLocalizedMessage();
      throw new MessagingException(msg, ex);
    } catch (final Exception ex) {
      final String msg = OutboundMessageProcessor.mMessages.getString(
              "DBBC_R00624.OMP_Failed_Exec_SQL") + sql +
              " Reason: " + ex.getLocalizedMessage();
      throw new MessagingException(msg, ex);
    }

    // return rowsUpdated;
    }

  // resumes the transaction
  private void resumeThreadTx(Transaction tx) throws Exception {
    if (tx != null) {
      ((TransactionManager) mContext.getContext().getTransactionManager()).
              resume(tx);
      mLogger.log(Level.INFO, "   ", new Object[]{tx.toString()});
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
    if (epbean.getValue(EndpointBean.JDBC_DATABASE_JNDI_NAME) == null || epbean.
            getValue(EndpointBean.JDBC_DATABASE_JNDI_NAME).equals("")) {
      final String driverClass = epbean.getValue(
              EndpointBean.JDBC_DATABASE_DRIVER_CLASS);
      final String dbURL = epbean.getValue(EndpointBean.JDBC_DATABASE_URL);
      final String user = epbean.getValue(EndpointBean.JDBC_DATABASE_USER);
      final String password = epbean.getValue(
              EndpointBean.JDBC_DATABASE_PASSWORD);
      // Thread.currentThread().getContextClassLoader().loadClass(driverClass).newInstance();
      Class.forName(driverClass).newInstance();

      return DriverManager.getConnection(dbURL, user, password);
    } else {
      final DataSource ds = (DataSource) getDataSourceFromContext(epbean.
              getValue(EndpointBean.JDBC_DATABASE_JNDI_NAME));
      final Connection con = ds.getConnection();

      return con;
    }
  }

  private Connection getDatabaseConnection(NormalizedMessage nm)
          throws SQLException, NamingException {
    Object p = nm.getProperty(
            JDBCComponentContext.NM_PROP_DATABASEBC_CONNECTION_JNDI_NAME);
    String jndiName = null;
    Connection conn = null;
    if (p != null)
      jndiName = p.toString();
    if (jndiName != null) {
      DataSource ds = (DataSource) getDataSourceFromContext(jndiName);
      conn = ds.getConnection();
    }
    return conn;
  }

  private Object[] getDatabaseConnection(NormalizedMessage inMsg, String exchangeId, EndpointBean epb) throws Exception
  {
    String jndiName = null;
    Connection connection = null;
    String faultDetail = null;
    try {
      jndiName = (String)inMsg.getProperty(JDBCComponentContext.NM_PROP_DATABASEBC_CONNECTION_JNDI_NAME);
      if (jndiName != null) {
        mLogger.log(Level.INFO,
                OutboundMessageProcessor.mMessages.getString(
                "DBBC_R00629.OMP_UsedJNDI") + jndiName);
        connection = getDatabaseConnection(inMsg);
      } else {
        jndiName = epb.getValue(EndpointBean.JDBC_DATABASE_JNDI_NAME);
        mLogger.log(Level.INFO,
                OutboundMessageProcessor.mMessages.getString(
                "DBBC_R00629.OMP_UsedJNDI") + jndiName);
        connection = getDatabaseConnection(epb);
      }
    } catch (Exception e) {
      String faultString = mMessages.getString(
              "DBBC_E00627.OMP_Failed_LookUp_JNDI", new Object[]{
              jndiName, exchangeId, epb.getValue(EndpointBean.ENDPOINT_NAME),
              mExchange.getOperation().toString()});
      AlertsUtil.getAlerter().warning(faultString,
              JDBCBindingLifeCycle.SHORT_DISPLAY_NAME,
              null,
              AlertsUtil.getServerType(),
              AlertsUtil.COMPONENT_TYPE_BINDING,
              NotificationEvent.OPERATIONAL_STATE_RUNNING,
              NotificationEvent.EVENT_TYPE_ALERT,
              "DBBC_E00627");
      throw e;
    }
    return new Object[] { jndiName, connection };
  }

  private XAConnection getXADatabaseConnection(final EndpointBean epbean) throws Exception {
    try {
      final DataSource ds = (DataSource) getDataSourceFromContext(epbean.
              getValue(EndpointBean.JDBC_DATABASE_JNDI_NAME));
      if (ds instanceof XADataSource) {
        XAConnection con = ((XADataSource) ds).getXAConnection();
        return con;
      }
    } catch (Exception e) {
      if (mLogger.isLoggable(Level.FINEST))
        mLogger.log(Level.SEVERE, "Either the JNDI NAME is  NULL or Could not establish connection using JNDI NAME :" +
                EndpointBean.JDBC_DATABASE_JNDI_NAME, e);
      else if (mLogger.isLoggable(Level.FINE))
        mLogger.log(Level.SEVERE, "Either the JNDI NAME is  NULL or Could not establish connection using JNDI NAME :" +
                EndpointBean.JDBC_DATABASE_JNDI_NAME + " Reason: " + e.
                getLocalizedMessage());
      else if (mLogger.isLoggable(Level.INFO))
        mLogger.log(Level.SEVERE,
                "Either the JNDI NAME is  NULL or Could not establish connection using JNDI NAME :");
    }
    return null;
  }

  private TransactionManager getTransactionManager() {
    return (TransactionManager) mContext.getContext().getTransactionManager();
  }

  private boolean processException(final Exception ex, Transaction transaction,
                                   final MessageExchange inonly, final EndpointBean epb,
                                   String faultCode, String faultDetail) {
    boolean success = false;
    mLogger.log(Level.WARNING, OutboundMessageProcessor.mMessages.getString(
            "DBBC_E00622.OMP_Failed_writing"), ex);

    try {
      if (transaction != null)
        transaction.setRollbackOnly();
    } catch (javax.transaction.SystemException e) {
      //ignore since the code below will take care of setting error on ME.
    }
    faultCode = CLIENT;
    faultDetail = mMessages.getString(
            "DBBC_E00706.JDBCDN_Failed_Denormalize");
    String faultString = mMessages.getString(
            "DBBC_E00706.JDBCDN_Failed_Denormalize", new Object[]{inonly.
              getExchangeId(), epb.getValue(EndpointBean.ENDPOINT_NAME), inonly.
              getOperation().
              toString()});
    setErrorInExchange(inonly, faultCode, faultDetail, ex);
    try {
      inonly.setStatus(ExchangeStatus.ERROR);
    } catch (MessagingException mex) {
      //ignore
    }
    return success;
  }

  private void updateTallyReceivedReplies(EndpointBean e) {
    if (e != null) {
      EndpointStatus status = e.getEndpointStatus();
      if (status != null)
        status.incrementReceivedReplies();
    }
  }

  private void updateTallyReceivedRequests(EndpointBean e) {
    if (e != null) {
      EndpointStatus status = e.getEndpointStatus();
      if (status != null)
        status.incrementReceivedRequests();
    }
  }

  private void updateTallySentReplies(EndpointBean e) {
    if (e != null) {
      EndpointStatus status = e.getEndpointStatus();
      if (status != null)
        status.incrementSentReplies();
    }
  }

  private void updateTallySentRequests(EndpointBean e) {
    if (e != null) {
      EndpointStatus status = e.getEndpointStatus();
      if (status != null)
        status.incrementSentRequests();
    }
  }

  private void updateTallyReceives(EndpointBean e, boolean successfulReceive) {
    if (e != null) {
      EndpointStatus status = e.getEndpointStatus();
      if (status != null)
        if (successfulReceive)
          status.incrementReceivedDones();
        else
          status.incrementReceivedErrors();
    }
  }

  private void updateTallySends(EndpointBean e, boolean successfulSend) {
    if (e != null) {
      EndpointStatus status = e.getEndpointStatus();
      if (status != null)
        if (successfulSend)
          status.incrementSentDones();
        else
          status.incrementSentErrors();
    }
  }

  private String getDBName(EndpointBean eBean, NormalizedMessage nMsg)
          throws javax.naming.NamingException, java.sql.SQLException {
    if (eBean.getValue(EndpointBean.JDBC_DATABASE_JNDI_NAME) == null || eBean.
            getValue(EndpointBean.JDBC_DATABASE_JNDI_NAME).equals(""))
      return dbConnectionInfo.getDataBaseName(eBean.getValue(
              EndpointBean.JDBC_DATABASE_URL), null, null);
    else {
      String jndiName = eBean.getValue(
              EndpointBean.JDBC_DATABASE_JNDI_NAME);
      if (nMsg.getProperty(
              JDBCComponentContext.NM_PROP_DATABASEBC_CONNECTION_JNDI_NAME) != null)
        jndiName = nMsg.getProperty(
                JDBCComponentContext.NM_PROP_DATABASEBC_CONNECTION_JNDI_NAME).
                toString();
      return dbConnectionInfo.getDataBaseName(null, null,
              (DataSource) getDataSourceFromContext(jndiName));

    }
  }

  private void setErrorInExchange(MessageExchange ex, String faultCode,
                                  String faultDetail, Exception e) {
    ex.setError(e);
    ex.setProperty("com.sun.jbi.crl.faultcode", faultCode);
    ex.setProperty("com.sun.jbi.crl.faultstring", e.getMessage());
    ex.setProperty("com.sun.jbi.crl.faultactor", "sun-database-binding");
    ex.setProperty("com.sun.jbi.crl.faultdetail",
            faultDetail + e.getMessage());
  }
}
