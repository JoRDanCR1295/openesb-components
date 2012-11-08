/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.performance.jmsbd;

import com.sun.jbi.engine.iep.core.runtime.operator.Inserter;
import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Properties;
import java.util.StringTokenizer;
import javax.jms.JMSException;
import javax.jms.QueueReceiver;
import javax.jms.QueueSession;
import javax.jms.TextMessage;

/**
 *
 * @author Bing Lu
 */
public class EventCollector implements Runnable {
    static final int mServerConnectionRetries = 10;
    static final int mServerConectionRetryInterval = 2000;
    private static int mRetryCount = 0;

    private Operator mOperator;
    private String mJmsServerHostName;
    private String mJmsServerPort;
    private String mQueueName;
    private Properties mProperties;
    private Thread mThread;
    private boolean mRunning;
    private boolean mStopped;
    private Object mStoppedMonitor = new Object();

    private List<Object[]> mRowList = new LinkedList<Object[]>();


    public EventCollector(Operator operator, String queueName, Properties properties) {
        mOperator = operator;
        mQueueName = queueName;
        mProperties = properties;
        mJmsServerHostName = mProperties.getProperty("JmsServerHostName");
        mJmsServerPort = mProperties.getProperty("JmsServerPort");
    }

    public void start() {
        mThread = new Thread(this, "Event collector");
        mThread.start();
    }

    public void run() {
        HashMap hashMap = null;
        Connection con = null;
        PreparedStatement insertStmt = null;
        try {
            con = Util.getConnection(mProperties);
            con.setAutoCommit(false);
            insertStmt = ((Inserter) mOperator).getInsertStatement(con, true);
            hashMap = JMSHelper.createQueueReceiver(mJmsServerHostName, mJmsServerPort, mQueueName);
            QueueReceiver queueReceiver = (QueueReceiver)hashMap.get("QueueReceiver");
            QueueSession queueSession = (QueueSession)hashMap.get("QueueSession");

            TextMessage receivedTextMessage = null;
            mRunning = true;
            mStopped = false;
            while (mRunning) {
                try {
                    receivedTextMessage = (TextMessage) queueReceiver.receive(1000);
                    if (receivedTextMessage == null) {
                        continue;
                    }
                } catch (JMSException e) {
                    hashMap = retryConnection();
                    if (hashMap != null) {
                        // reset retry count
                        mRetryCount = 0;
                        queueReceiver = (QueueReceiver)hashMap.get("QueueReceiver");
                        queueSession = (QueueSession)hashMap.get("QueueSession");
                        continue;
                    } else {
                        throw e;
                    }
                }

                try {
                    while (mRunning) {
                        process(receivedTextMessage);
                        // 1 millisecond is the smallest time period to specify
                        receivedTextMessage = (TextMessage) queueReceiver.receive(1);
                        if (receivedTextMessage == null) {
                            // time is expired 
                            break;
                        }
                    }
                    batchProcess(con, insertStmt);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        } catch (Exception e) {
            if (mRunning) {
                e.printStackTrace();
            }
        } finally {
            Util.close(insertStmt);
            Util.close(con);
            JMSHelper.closeConnectionAndSession(hashMap);
            mStopped = true;
            synchronized (mStoppedMonitor) {
                mStoppedMonitor.notifyAll();
            }
            System.out.println("Event collector is stopped successfully");
        }
    }

    public void stopAndWait() {
        mRunning = false;
        while (!mStopped) {
            synchronized (mStoppedMonitor) {
                try {
                    mStoppedMonitor.wait();
                } catch (InterruptedException e) {
                }
            }
        }
    }

    private void process(TextMessage msg) {
        try {
            String txt = msg.getText();
            StringTokenizer st = new StringTokenizer(txt, ",");
            ArrayList list = new ArrayList();
            while (st.hasMoreTokens()) {
                list.add(st.nextToken());
            }
            mRowList.add(list.toArray());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void batchProcess(Connection con, PreparedStatement insertStmt) throws Exception {
        insertStmt.clearBatch();
        for (Object[] row : mRowList) {
            for (int i = 0; i < row.length; i++) {
                insertStmt.setObject(i+1, row[i]);
            }
            insertStmt.addBatch();
        }
        insertStmt.executeBatch();
        con.commit();
        mRowList.clear();
    }

    private HashMap retryConnection() {
        try {
            if (mRetryCount++ < mServerConnectionRetries) {
                System.out.println("Waiting " + mServerConectionRetryInterval + " secs before restarting JMS server");
                try {
                    Thread.sleep(mServerConectionRetryInterval * 1000);
                } catch(InterruptedException e) {
                    System.out.println("Exception occurred while the thread was sleeping.");
                }

                return JMSHelper.createQueueReceiver(mJmsServerHostName, mJmsServerPort, mQueueName);
            }
        } catch (Exception e) {
            if (mRetryCount < mServerConnectionRetries) {
                return retryConnection();
            }
        }
        return null;
    }
}
