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
 * @(#)IEPSEOutOnlyThread.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep;

import java.sql.Connection;
import java.sql.Timestamp;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.transaction.TransactionManager;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;

import org.w3c.dom.Document;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.messaging.BaseExchangeTemplates;
import com.sun.jbi.engine.iep.core.runtime.IEPEngine;
import com.sun.jbi.engine.iep.core.runtime.operator.Notification;
import com.sun.jbi.engine.iep.core.runtime.operator.Notifier;
import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.OperatorConstants;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.util.NameUtil;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import com.sun.jbi.engine.iep.core.runtime.util.XmlUtil;
import com.sun.jbi.internationalization.Messages;

public class IEPSEOutOnlyThread extends Thread implements OperatorConstants {

    private static final Messages mMessages = Messages.getMessages(IEPSEOutOnlyThread.class);
    private static final Logger mLogger = Messages.getLogger(IEPSEOutOnlyThread.class);
    private boolean mRunning;
    private Object mRunningMonitor = new Object();
    private boolean mStopped;
    private Object mStoppedMonitor = new Object();
    private long mTimeUnit = 1000L;
    private ExtendedComponentContext mExtendedContext;
    private MessageExchangeFactory mMessageExchangeFactory;

    /** Creates a new instance of IEPSEOutOnlyThread */
    public IEPSEOutOnlyThread(ExtendedComponentContext extendedContext) {
        mExtendedContext = extendedContext;
        DeliveryChannel channel = mExtendedContext.getDeliveryChannel();
        mMessageExchangeFactory = channel.createExchangeFactory();
    }

    // A ---> (1. inOnly X) --> B =================> (2. inOnly Y) --> C 
    // A <--- (4. X.done) <---- B <----------------- (3. Y.done  ) <-- C
    private void process(final Connection con, Operator op, Timestamp timestamp){
        // Initiate the message exchange
        ExchangeTable exTable = mExtendedContext.getExchangeTable();
        ExchangeRecord record = exTable.getRecord(op);
        if(record != null) {
            //No decison is made what needs to be done for the MEx that are failed
            //those records can be found in  record.mInOnlyToReceive after record.updateDB is called.
            return;
        }

        if (((Notifier)op).getCacheSize() == 0) {
            // Read from db if nothing in cache
            ((Notifier)op).fetch(con, timestamp);
            if (((Notifier)op).getCacheSize() == 0) {
                return;
            }
        }

        ServiceEndpoint endpoint = deriveServiceEndpoint(op);
        String operation = NameUtil.makeJavaId(op.getName());
        
        // 2. Send batch of inOnlys Y to C ===========================================
        // Retrieve many rows from db if any
        EndpointInfo epInfo = new EndpointInfo(false, endpoint.getEndpointName(),
                null, endpoint.getServiceName(), null);

        int maxCurrentLimit = mExtendedContext.getMaxConcurrencyLimit(epInfo);
        TransactionManager tm = null;
        boolean transactd = false;
        if(mExtendedContext.isTransactedOutput()){
            tm = mExtendedContext.getTransactionManager();
            transactd = true;
        }
        
        List<Notification> batch = ((Notifier) op).getNotificationBatch(maxCurrentLimit);

        record = new ExchangeRecord(op, transactd, tm, batch.size());

        if (mLogger.isLoggable(Level.FINE)) {
            int batchSize =  batch.size();
            for (int index=0; index < batchSize; index ++) {
                Notification notification = batch.get(index);
                mLogger.log(Level.FINE, mMessages.getString("IEPSEOutOnlyThread.Output",
                        new Object[]{
                        index + 1, batchSize, notification.getId(), 
                        XmlUtil.toXml((Document)notification.getEvent(), "UTF-8", false)}));
            }
        }

        for (Notification notification : batch) {
            try {
                sendNotification(notification, endpoint, operation, exTable,
                        record, tm);
            } catch (Exception e) {
                mLogger.log(java.util.logging.Level.SEVERE, mMessages.getString(
                                "IEPSEOutOnlyThread.Operator_fails_to_process",
                                op.getName()), e);
                record.setErrorOnMessage(notification.getId());
            }
        }

    }

    private void sendNotification(Notification notification,
            ServiceEndpoint endpoint, String operation, ExchangeTable exTable,
            ExchangeRecord record, TransactionManager tm)
            throws MessagingException {
        Document outputDoc = (Document)notification.getEvent();
        Source input = new DOMSource(outputDoc);
        String msgId = notification.getId();
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("IEPSEOutOnlyThread.msgId_service_operation",
                    new Object[]{msgId, endpoint.getServiceName(), operation}));
        }
        
        QName qualOpName = new QName(
                endpoint.getServiceName().getNamespaceURI(), operation);
        BaseExchangeTemplates et = new BaseExchangeTemplates(endpoint,
                qualOpName, true, null, msgId, input,
                mMessageExchangeFactory);
        et.setTransactionManager(tm);

        synchronized(exTable){
            //Notification is added to record prior to DC.send, 
            //it is done to record error on the notification if DC.send fails 
            record.addInOnlyToReceive(msgId, notification);
            mExtendedContext.getDeliveryChannel().send(et);
            exTable.addInOnly(msgId, record); // II         
        }
    }
    
    private ServiceEndpoint deriveServiceEndpoint(Operator op) {
        String opName = op.getOperation() ;
        
        String serviceNamespaceURI = op.getPlan().getInstanceId();
        String serviceLocalPart = "OutputPl_" + opName;
        QName service = new QName(serviceNamespaceURI, serviceLocalPart);
        
        String endpointName = "OutputRn_" + opName;
        ServiceEndpoint endpoint = mExtendedContext.getComponentContext().getEndpoint(service, endpointName);
        
        if(endpoint == null) {
        	throw new RuntimeException(mMessages.getString("IEPSEOutOnlyThread.Outbound_Service_Endpoint_Not_Found", new Object [] {service, endpointName}));
        }
        
        return endpoint;
    }

    @Override
    public void run() {
        mRunning = true;
        mStopped = false;
        IEPEngine engine = mExtendedContext.getEngine();
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("IEPSEOutOnlyThread.Started_IEP_service_engine_out_only_thread"));
        }
        Connection con = null;
        try {
            con = Util.getConnection(mExtendedContext.getConfigProperties());
            while (mRunning) {
                synchronized (mRunningMonitor) {
                    try {
                        mRunningMonitor.wait(mTimeUnit);
                    } catch (InterruptedException e) {
                        mLogger.log(java.util.logging.Level.WARNING,
                                mMessages.getString("IEPSEOutOnlyThread.IEPSEOutOnlyThread_is_interrupted"),
                                e);
                    }
                }
                if (!mRunning) {
                    return;
                }
                List recordList = mExtendedContext.getDeploymentTable().getRecordList();
                for (int i = 0, I = recordList.size(); i < I; i++) {
                    DeploymentRecord dr = (DeploymentRecord) recordList.get(i);
                    if (!dr.isStarted()) {
                        continue;
                    }
                    String instanceId = dr.getDeployName();
                    if (!engine.isScheduled(instanceId)) {
                        continue;
                    }
                    QueryPlan plan = dr.getPlan();
                    try {
                        Timestamp newTimestamp = Util.getPrevTimestampToCheck(con, plan.getId());
                        List<Notifier> opList = plan.getNotifierList();
                        for (int j = 0, J = opList.size(); j < J; j++) {
                            Notifier op = opList.get(j);
                            process(con, (Operator)op, newTimestamp);
                        }
                    } catch (Exception e) {
                        mLogger.log(java.util.logging.Level.SEVERE,
                                mMessages.getString("IEPSEOutOnlyThread.Plan_fails_to_output", plan.getInstanceId()),
                                e);
                    }
                }
                Thread.yield();
            }
        } catch (Exception e) {
            mLogger.log(java.util.logging.Level.SEVERE,
                    mMessages.getString("IEPSEOutOnlyThread.IEPSEOutOnlyThread_failed"),
                    e);
        } finally {
            mStopped = true;
            synchronized (mStoppedMonitor) {
                mStoppedMonitor.notifyAll();
            }
            Util.close(con);
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSEOutOnlyThread.IEP_service_engine_out_only_thread_finished"));
            }
        }
    }

    public synchronized void stopAndWait() {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("IEPSEOutOnlyThread.Ceasing_IEP_service_engine_out_only_thread"));
        }
        mRunning = false;
        synchronized (mRunningMonitor) {
            mRunningMonitor.notifyAll();
        }
        while (!mStopped) {
            synchronized (mStoppedMonitor) {
                try {
                    mStoppedMonitor.wait();
                } catch (InterruptedException e) {
                }
            }
        }
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("IEPSEOutOnlyThread.IEP_service_engine_out_only_thread_ceased"));
        }
    }

    public synchronized void messageConfirmed() {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("IEPSEOutOnlyThread.Message_confirmed"));
        }
        synchronized (mRunningMonitor) {
            mRunningMonitor.notifyAll();
        }
    }
}
