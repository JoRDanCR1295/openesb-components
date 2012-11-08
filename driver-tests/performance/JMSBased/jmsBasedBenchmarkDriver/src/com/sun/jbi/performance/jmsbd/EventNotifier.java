/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.performance.jmsbd;

import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.OperatorConstants;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.Timestamp;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Properties;
import javax.jms.DeliveryMode;
import javax.jms.QueueSender;
import javax.jms.QueueSession;
import javax.jms.TextMessage;

/**
 *
 * @author Bing Lu
 */
public class EventNotifier implements Runnable, OperatorConstants {
    static final int mServerConnectionRetries = 10;
    static final int mServerConectionRetryInterval = 2000;
    private static int mRetryCount = 0;

    private QueryPlan mPlan;
    private Operator mOperator;
    private String mJmsServerHostName;
    private String mJmsServerPort;
    private String mQueueName;
    private Properties mProperties;
    private Thread mThread;
    private boolean mRunning;
    private Object mRunningMonitor = new Object();
    private boolean mStopped;
    private Object mStoppedMonitor = new Object();

    private String mGetBatchStr;
    private String mDeleteBatchStr;
    private int mColCnt;

    public EventNotifier(QueryPlan plan, Operator operator, String queueName, Properties properties) {
        mPlan = plan;
        mOperator = operator;
        mQueueName = queueName;
        mProperties = properties;
        mJmsServerHostName = mProperties.getProperty("JmsServerHostName");
        mJmsServerPort = mProperties.getProperty("JmsServerPort");

        StringBuffer sb = new StringBuffer();
        sb.append("SELECT * FROM ");
        sb.append(operator.getQueueName());
        sb.append(" WHERE " + COL_TIMESTAMP + " <= ?  ORDER BY " + COL_SEQID);
        mGetBatchStr = sb.toString();

        sb = new StringBuffer();
        sb.append("DELETE FROM ");
        sb.append(operator.getQueueName());
        sb.append(" WHERE " + COL_SEQID + " <= ?");
        mDeleteBatchStr = sb.toString();

        mColCnt = mOperator.getOutputSchema().getColumnCount();
    }

    public void start() {
        mThread = new Thread(this, "Event collector");
        mThread.start();
    }

    public void run() {
        HashMap hashMap = null;
        Connection con = null;
        PreparedStatement getBatchStmt = null;
        PreparedStatement deleteBatchStmt = null;
        try {
            con = Util.getConnection(mProperties);
            con.setAutoCommit(false);
            getBatchStmt = con.prepareStatement(mGetBatchStr);
            deleteBatchStmt = con.prepareStatement(mDeleteBatchStr);

            hashMap = JMSHelper.createQueueSender(mJmsServerHostName, mJmsServerPort, mQueueName);
            QueueSender queueSender = (QueueSender)hashMap.get("QueueSender");
            QueueSession queueSession = (QueueSession)hashMap.get("QueueSession");

            TextMessage receivedTextMessage = null;
            mRunning = true;
            mStopped = false;
            while (mRunning) {
                synchronized (mRunningMonitor) {
                    try {
                        mRunningMonitor.wait(200);
                    } catch (InterruptedException e) {
                    }
                }
                if (!mRunning) {
                    return;
                }

                batchProcess(con, getBatchStmt, deleteBatchStmt, queueSession, queueSender);
            }
        } catch (Exception e) {
            if (mRunning) {
                e.printStackTrace();
            }
        } finally {
            Util.close(getBatchStmt);
            Util.close(deleteBatchStmt);
            Util.close(con);
            JMSHelper.closeConnectionAndSession(hashMap);
            mStopped = true;
            synchronized (mStoppedMonitor) {
                mStoppedMonitor.notifyAll();
            }
            System.out.println("Event notifier is stopped successfully");
        }
    }

    public void stopAndWait() {
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
    }

    private void batchProcess(Connection con, PreparedStatement getBatchStmt, PreparedStatement deleteBatchStmt, 
            QueueSession queueSession, QueueSender queueSender)
    {
        ResultSet rs = null;
        LinkedList<Object[]> rowList = new LinkedList<Object[]>();
        try {
            Timestamp newTimestamp = Util.getPrevTimestampToCheck(con, mPlan.getId());
            getBatchStmt.setTimestamp(1, newTimestamp);
            rs = getBatchStmt.executeQuery();
            long maxSeqId = -1;
            while (rs.next()) {
                Object[] row = new Object[mColCnt]; 
                for (int i = 0; i < mColCnt; i++) {
                    row[i] = rs.getObject(i + 1);
                }
                Long seqId = rs.getLong(mColCnt + 1);
                if (maxSeqId < seqId) {
                    maxSeqId = seqId;
                }
                rowList.add(row);
            }

            deleteBatchStmt.setLong(1, maxSeqId);
            deleteBatchStmt.executeUpdate();
            con.commit();
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            Util.close(rs);
        }

        try {
            for (Object[] row : rowList) {
                StringBuffer sb = new StringBuffer();
                for (int i = 0; i < mColCnt; i++) {
                    if (i > 0) {
                        sb.append(",");
                    }
                    sb.append(row[i]);
                }
                TextMessage textMessage = queueSession.createTextMessage();
                textMessage.setJMSDeliveryMode(DeliveryMode.NON_PERSISTENT);
                textMessage.setText(sb.toString());
                queueSender.send(textMessage);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

}
