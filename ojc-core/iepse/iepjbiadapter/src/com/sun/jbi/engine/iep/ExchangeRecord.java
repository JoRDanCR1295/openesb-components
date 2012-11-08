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
 * @(#)ExchangeRecord.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep;

import java.sql.Connection;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.MessageExchange;
import javax.transaction.Transaction;
import javax.transaction.TransactionManager;

import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.redelivery.Redelivery;
import com.sun.jbi.common.qos.redelivery.RedeliveryStatus;
import com.sun.jbi.engine.iep.core.runtime.operator.Notification;
import com.sun.jbi.engine.iep.core.runtime.operator.Notifier;
import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import com.sun.jbi.internationalization.Messages;

/**
 * ExchangeRecord.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class ExchangeRecord {

    private static final Logger mLogger = Messages.getLogger(ExchangeRecord.class);
    private static final Messages mMessages = Messages.getMessages(ExchangeRecord.class);
    private Operator mOperator;
    private Map<String, Notification> mInOnlyToReceive;
    private boolean error = false;
    private boolean transacted = false;
    private int ackCount = 0;
    private TransactionManager tm;
    private boolean failed = true;
    private int batchSize = 0;

    public ExchangeRecord(Operator operator, boolean transacted, TransactionManager tm, int size) {
        mOperator = operator;
        mInOnlyToReceive = Collections.synchronizedMap(new HashMap<String, Notification>());
        this.transacted = transacted;
        this.tm = tm;
        batchSize = size;
    }

    public Operator getOperator() {
        return mOperator;
    }

    public void addInOnlyToReceive(String msgId, Notification notification) {
        mInOnlyToReceive.put(msgId, notification);
    }

    public void updateStatusInOnlyToReceive(InOnly inOnly) {
        String msgId = (String) inOnly.getProperty(ServiceQuality.MESSAGE_ID);
        Notification notification = mInOnlyToReceive.get(msgId);
        if (inOnly.getStatus() == ExchangeStatus.ERROR) {
            error = true;
            notification.setError(true);
            RedeliveryStatus redeliveryStatus = Redelivery.getRedeliveryStatus(inOnly);
            if (redeliveryStatus.hasFailed()) {
                failed = true;
            }
        }
        notification.setTransaction(inOnly.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME));
        synchronized (this) {
            ackCount++;
        }
    }

    public void setErrorOnMessage(String msgId) {
        Notification notification = mInOnlyToReceive.get(msgId);
        error = true;
        notification.setError(true);
        synchronized (this) {
            ackCount++;
        }
    }

    public boolean hasAcknowledgedAll() {
        return (batchSize == ackCount);
    }

    public boolean isTransacted() {
        return transacted;
    }

    public void updateDBForTxMessage(InOnly inOnly, Properties props) throws Exception {

        String msgId = (String) inOnly.getProperty(ServiceQuality.MESSAGE_ID);
        Notification notification = mInOnlyToReceive.get(msgId);
        Connection con = null;

        if (!notification.isError()) {
            Transaction tx = (Transaction) notification.getTransaction();
            tm.resume(tx);
            /*
             * If the transaction is XA, then the connection must be acquired within the
             * tx boundaries, that is, after the tm.begin() or tm.resume(). This is a
             * requirement for using GlassFish's automatic enlistment feature.
             */
            try {
                con = Util.getXaConnection(props);
                ((Notifier) mOperator).removeNotification(con, notification);
                //Close the connection within the tx boundary. This is a good practice
                //while using the GlassFish's automatic enlistment feature.
                Util.close(con);
                tx.commit();
                mInOnlyToReceive.remove(notification.getId());
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, mMessages.getString("ExchangeRecord.Notification_removed", notification.getId()));
                }
            } catch (Exception e) {
                mLogger.log(Level.SEVERE, mMessages.getString("ExchangeRecord.Exception_completing_outbound_tx"), e);
                Util.close(con);
                tx.rollback();
            }
        } else {
            throw new Exception(mMessages.getString("ExchangeRecord.Notification_error", notification.getId()));
        }
    }

    public void updateDBForNonTxMessages(Properties props) throws Exception {
        Collection<Notification> notifications = mInOnlyToReceive.values();
        ArrayList<Notification> ackNotificationList = new ArrayList<Notification>();
        Connection con = null;
        if (!error) {
            ackNotificationList.addAll(notifications);
            try {
                con = Util.getConnection(props);
                con.setAutoCommit(false);
                ((Notifier) mOperator).removeNotificationBatch(con, ackNotificationList);
                con.commit();
                mInOnlyToReceive.clear();
            } finally {
                Util.close(con);
            }
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("ExchangeRecord.Notification_batch_removed") + ackNotificationList);
            }
            return;
        }

        for (Notification notification : notifications) {
            if (!notification.isError()) {
                try {
                    con = Util.getConnection(props);
                    con.setAutoCommit(false);
                    ((Notifier) mOperator).removeNotification(con, notification);
                    con.commit();
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, mMessages.getString("ExchangeRecord.Notification_removed", notification.getId()));
                    }
                } finally {
                    Util.close(con);
                }
                // assumption msgid is constructed from notification.getId()
                ackNotificationList.add(notification);
            }
        }

        for (Notification notification : ackNotificationList) {
            mInOnlyToReceive.remove(notification.getId());
        }

        if (!mInOnlyToReceive.isEmpty()) {
            notifications = mInOnlyToReceive.values();
            String msg = mMessages.getString("ExchangeRecord.Notification_batch_error");
            for (Notification notification : notifications) {
                msg = msg + notification.getId() + ", ";
            }
            throw new Exception(msg);
        }
    }
}
