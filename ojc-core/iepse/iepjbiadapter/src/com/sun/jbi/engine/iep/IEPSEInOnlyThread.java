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
 * @(#)IEPSEInOnlyThread.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.xml.namespace.QName;

import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.engine.iep.core.runtime.operator.OperatorConstants;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.exchange.ExchangePattern;

/*
 * IEPSEInOnlyThread.java
 *
 * Created on 2005 02 09
 *
 * @author  Bing Lu
 */
public class IEPSEInOnlyThread extends Thread implements OperatorConstants {

    public enum FaultCode {

        Client, Server, VersionMismatch
    };
    private static final Messages mMessages = Messages.getMessages(IEPSEInOnlyThread.class);
    private static final Logger mLogger = Messages.getLogger(IEPSEInOnlyThread.class);
    private boolean mRunning;
    private boolean mStopped;
    private Object mStoppedMonitor = new Object();
    private ExtendedComponentContext mExtendedContext;

    /** Creates a new instance of IEPSEInOnlyThread */
    public IEPSEInOnlyThread(ExtendedComponentContext extendedContext) {
        mExtendedContext = extendedContext;
    }

    // A ===> (1. inOnly X) ==> B ----------------> (3. inOnly Y) --> C
    // A <=== (2. X.done) <=== B <================= (4. Y.done  ) <== C
    private void process(final InOnly inOnly,
            InputOperationTable inputOperationTable) {

        if (inOnly.getStatus() == ExchangeStatus.ACTIVE) {
            if (mLogger.isLoggable(Level.FINE)) {
                QName serviceName = (inOnly.getEndpoint() != null) ? inOnly.getEndpoint().getServiceName() : inOnly.getService();
                mLogger.log(Level.FINE, mMessages.getString(
                        "IEPSEInOnlyThread.exId_service_operation", new Object[]{
                            inOnly.getExchangeId(), serviceName, inOnly.getOperation()
                        }));
            }
            // 1. Receive inOnly X from A
            // 2. Send X.done or X.error to
            // A=======================================
            inputOperationTable.process(inOnly);
            return;
        }

        if (mLogger.isLoggable(Level.FINE)) {
            QName serviceName = (inOnly.getEndpoint() != null) ? inOnly.getEndpoint().getServiceName() : inOnly.getService();
            String msgId = (String) inOnly.getProperty(ServiceQuality.MESSAGE_ID);

            mLogger.log(Level.FINE, mMessages.getString(
                    "IEPSEInOnlyThread.status_msgId_service_operation",
                    new Object[]{msgId, serviceName, inOnly.getOperation(),
                        inOnly.getExchangeId()
                    }));
        }

        // 4. Receive Y.done from C III (Seen IEPSEOutOnlyThread)
        ExchangeTable exTable = mExtendedContext.getExchangeTable();
        ExchangeRecord record = exTable.getInOnly(inOnly);

        // see IEPSEServiceUnitManager.init()
        String deployName = record.getOperator().getPlan().getInstanceId();
        DeploymentRecord dr = mExtendedContext.getDeploymentTable().getRecordByDeployName(deployName);

        if (dr == null) {
            /*
             * since record is still in db and it is not pending any more (not
             * in mExchangeTable any more), mOutOnlyThread will send it again
             * because it is the oldest one in db. This may result in duplicate.
             * Use XA transaction for once and only once delivery.
             */
            return;
        }

        record.updateStatusInOnlyToReceive(inOnly);
        exTable.deleteInOnly(inOnly);

        try {
            if (record.isTransacted()) {
                //For Tx message remove the record from database and commit the tx
                record.updateDBForTxMessage(inOnly, mExtendedContext.getConfigProperties());
            } else if (record.isTransacted() == false && record.hasAcknowledgedAll()) {
                //For non-tx, we wait for all the DONEs to be received before we remove the 
                //records from the database
                record.updateDBForNonTxMessages(mExtendedContext.getConfigProperties());
            }

        } catch (Exception e) {
            /*
             * TODO: this could run into 1. Db connectivity - network/server
             * issues 2. Db data integrity issues 3. XA error - Transaction
             * could be set to rollbackonly by partner, invalid state. etc
             * Handle them, and localize the msg that is logged
             */
            mLogger.log(Level.SEVERE,
                    "Failed while processing MEx ACKs for ouput operators",
                    e);
        }

        if (record.hasAcknowledgedAll()) {
            exTable.deleteRecord(record.getOperator());
        }

        //IEPSEOutOnlyThread if it is waiting. It calls wait if no 
        //Output operator to process for all the IEP process deployed
        mExtendedContext.getOutOnlyThread().messageConfirmed();
    }

    @Override
    public void run() {
        InputOperationTable inputOpTable = new InputOperationTable(mExtendedContext);
        mRunning = true;
        mStopped = false;
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("IEPSEInOnlyThread.Started_IEP_service_engine_in_only_thread"));
        }

        DeliveryChannel channel = mExtendedContext.getDeliveryChannel();
        if (channel == null) {
            return;
        }

        try {
            while (mRunning) {
                MessageExchange exchange = null;
                try {
                    exchange = channel.accept(1000);
                } catch (MessagingException e) {
                    mLogger.log(Level.SEVERE, mMessages.getString("IEPSEInOnlyThread.Caught_exception_accepting_message_on_channel"), e);
                    continue;
                }

                if (exchange == null) {
                    // time is expired or mChannel.close() is called
                    continue;
                }

                try {
                    while (mRunning) {
                        switch (ExchangePattern.valueOf(exchange)) {
                            case IN_OUT:
                                mLogger.log(Level.WARNING, mMessages.getString("IEPSEInOnlyThread.In_out_message_is_not_supported", exchange.getExchangeId()));
                                break;
                            case IN_ONLY:
                                if (mLogger.isLoggable(Level.FINE)) {
                                    mLogger.log(Level.FINE, mMessages.getString("IEPSEInOnlyThread.Received_in_only_message", exchange.getExchangeId()));
                                }
                                process((InOnly) exchange, inputOpTable);
                                break;
                            case ROBUST_IN_ONLY:
                                mLogger.log(Level.WARNING, mMessages.getString("IEPSEInOnlyThread.Robust_in_only_is_not_supported", exchange.getExchangeId()));
                                break;
                            default:
                                mLogger.log(Level.SEVERE, mMessages.getString("IEPSEInOnlyThread.Received_invalid_pattern_info", exchange.getExchangeId()));
                        }
                        // 1 millisecond is the smallest time period to specify
                        exchange = channel.accept(1);
                        if (exchange == null) {
                            // time is expired or mChannel.close() is called
                            break;
                        }
                    }
                    inputOpTable.batchProcess();

                    //This will clear the InputOperation object from the cache. The InputOperation object
                    //is only cached for the duration during which the batch is processed. For XA, there
                    //is no batch, so ideally we need not cache the InputOperation object. But this is
                    //harmless.
                    inputOpTable.clear();
                } catch (MessagingException ex) {
                    mLogger.log(Level.SEVERE, mMessages.getString("IEPSEInOnlyThread.Caught_exception_accepting_message_on_channel"), ex);
                }

            }
        } catch (Exception e) {
            if (mRunning) {
                mLogger.log(Level.SEVERE, mMessages.getString("IEPSEInOnlyThread.Caught_exception_in_run"), e);
            }
        } finally {
            mStopped = true;
            synchronized (mStoppedMonitor) {
                mStoppedMonitor.notifyAll();
            }
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSEInOnlyThread.IEP_service_engine_in_only_thread_finished"));
            }
        }
    }

    public void stopAndWait() {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("IEPSEInOnlyThread.Ceasing_IEP_service_engine_in_only_thread"));
        }
        mRunning = false;
        while (!mStopped) {
            synchronized (mStoppedMonitor) {
                try {
                    mStoppedMonitor.wait();
                } catch (InterruptedException e) {
                }
            }
        }
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("IEPSEInOnlyThread.Ceased_IEP_service_engine_in_only_thread_successfully"));
        }
    }
}
