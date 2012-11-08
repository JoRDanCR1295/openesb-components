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
 * @(#)BenchmarkDriver.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.performance.jmsbd;

import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.FileHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * BenchmarkDriver.java
 *
 * @author Bing Lu
 *
 * Created on July 27, 2007, 10:04 AM
 */
public class BenchmarkDriver implements Configuration {

    private static Logger LOGGER = Logger.getLogger(BenchmarkDriver.class.getName());
    private Properties mProperties = new Properties();
    private List<Properties> mSenderList = new ArrayList<Properties>();
    private List<Properties> mReceiverList = new ArrayList<Properties>();
    private Object mDoneToken = new Object();

    /**
     *
     * @param propFile
     * @throws java.lang.Exception
     */
    public BenchmarkDriver(String propFileName) throws Exception {
        //Load the properties
        File propFile = new File(propFileName);
        String benchmarkDir = propFile.getParentFile().getAbsolutePath();
        mProperties.load(propFile.toURI().toURL().openStream());
        mProperties.setProperty("log-file", benchmarkDir + File.separator + mProperties.getProperty("log-file"));
        Handler logFile = new FileHandler(mProperties.getProperty("log-file"));
        logFile.setFormatter(new LogFormatter());
        LOGGER.addHandler(logFile);
        setLogLevel(LOGGER, mProperties.getProperty("logging-level"));

        LOGGER.log(Level.INFO, "*** Configurations *** ");
        int senderCount = PropertyUtil.getint(mProperties, "sender.count", 0);
        LOGGER.log(Level.INFO, "senders: " + senderCount);
        for (int i = 0; i < senderCount; i++) {
            Properties prop = new Properties();
            String prefix = "sender." + i + ".";
            prop.setProperty(NAME, mProperties.getProperty(prefix + NAME));
            prop.setProperty(JMS_SERVER_HOST_NAME, mProperties.getProperty(prefix + JMS_SERVER_HOST_NAME));
            prop.setProperty(JMS_SERVER_PORT, mProperties.getProperty(prefix + JMS_SERVER_PORT));
            prop.setProperty(JMS_QUEUE, mProperties.getProperty(prefix + JMS_QUEUE));
            prop.setProperty(BATCH_SIZE, mProperties.getProperty(prefix + BATCH_SIZE));
            prop.setProperty(TEMPLATE_FILE, benchmarkDir + File.separator + mProperties.getProperty(prefix + TEMPLATE_FILE));
            prop.setProperty(MESSAGE_DELIVERY_MODE, mProperties.getProperty(prefix + MESSAGE_DELIVERY_MODE));
            mSenderList.add(prop);
            LOGGER.log(Level.INFO, NAME + " : " + prop.getProperty(NAME));
            LOGGER.log(Level.INFO, "\t" + JMS_SERVER_HOST_NAME + " : " + prop.getProperty(JMS_SERVER_HOST_NAME));
            LOGGER.log(Level.INFO, "\t" + JMS_SERVER_PORT + " : " + prop.getProperty(JMS_SERVER_PORT));
            LOGGER.log(Level.INFO, "\t" + JMS_QUEUE + " : " + prop.getProperty(JMS_QUEUE));
            LOGGER.log(Level.INFO, "\t" + BATCH_SIZE + ": " + prop.getProperty(BATCH_SIZE));
            LOGGER.log(Level.INFO, "\t" + TEMPLATE_FILE + " : " + prop.getProperty(TEMPLATE_FILE));
            LOGGER.log(Level.INFO, "\t" + MESSAGE_DELIVERY_MODE + " : " + prop.getProperty(MESSAGE_DELIVERY_MODE));
        }

        int receiverCount = PropertyUtil.getint(mProperties, "receiver.count", 0);
        LOGGER.log(Level.INFO, "\nreceivers: " + receiverCount);
        for (int i = 0; i < receiverCount; i++) {
            Properties prop = new Properties();
            prop.put(DONE_TOKEN, mDoneToken);
            String prefix = "receiver." + i + ".";
            prop.setProperty(NAME, mProperties.getProperty(prefix + NAME));
            prop.setProperty(JMS_SERVER_HOST_NAME, mProperties.getProperty(prefix + JMS_SERVER_HOST_NAME));
            prop.setProperty(JMS_SERVER_PORT, mProperties.getProperty(prefix + JMS_SERVER_PORT));
            prop.setProperty(JMS_QUEUE, mProperties.getProperty(prefix + JMS_QUEUE));
            prop.setProperty(BATCH_SIZE, mProperties.getProperty(prefix + BATCH_SIZE));
            mReceiverList.add(prop);
            LOGGER.log(Level.INFO, NAME + " : " + prop.getProperty(NAME));
            LOGGER.log(Level.INFO, "\t" + JMS_SERVER_HOST_NAME + " : " + prop.getProperty(JMS_SERVER_HOST_NAME));
            LOGGER.log(Level.INFO, "\t" + JMS_SERVER_PORT + " : " + prop.getProperty(JMS_SERVER_PORT));
            LOGGER.log(Level.INFO, "\t" + JMS_QUEUE + " : " + prop.getProperty(JMS_QUEUE));
            LOGGER.log(Level.INFO, "\t" + BATCH_SIZE + ": " + prop.getProperty(BATCH_SIZE));
        }
    }

    /**
     * This would test by feeding the messages to the queue in batches.
     *
     * @throws IOException
     * @throws Exception
     */
    private void run() throws Exception {
        LOGGER.log(Level.INFO, "***** Results ******* ");
        int batchSize = 0;
        for (Properties prop : mSenderList) {
            batchSize += PropertyUtil.getint(prop, BATCH_SIZE, 0);
        }
        int totalBatches = Integer.parseInt(mProperties.getProperty("total-batches"));

        long totalTime = 0;
        for (int i = 0; i < totalBatches; i++) {
            List<JMSMessageSender> senderList = new ArrayList<JMSMessageSender>();
            List<JMSMessageReceiver> receiverList = new ArrayList<JMSMessageReceiver>();
            LOGGER.log(Level.INFO, "Starting benchmark run (" + i + ") .. ");
            for (int j = 0; j < mSenderList.size(); j++) {
                Properties prop = mSenderList.get(j);
                JMSMessageSender sender = new JMSMessageSender(prop);
                sender.setName(prop.getProperty(NAME));
                senderList.add(sender);
            }
            for (int j = 0; j < mReceiverList.size(); j++) {
                Properties prop = mReceiverList.get(j);
                JMSMessageReceiver receiver = new JMSMessageReceiver(prop);
                receiver.setName(prop.getProperty(NAME));
                receiverList.add(receiver);
            }
            // Start receivers first
            for (int j = 0; j < receiverList.size(); j++) {
                receiverList.get(j).start();
            }
            // Start timer
            long startTime = System.currentTimeMillis();
            // Start senders
            for (int j = 0; j < senderList.size(); j++) {
                senderList.get(j).start();
            }
            // Each receiver notifies Driver once it is done, and records its own endTime
            while (!isDone(receiverList)) {
                synchronized (mDoneToken) {
                    try {
                        mDoneToken.wait(1000);
                    } catch (InterruptedException e) {
                    }
                }
            }
            long endTime = 0L;
            for (JMSMessageReceiver r : receiverList) {
                if (endTime < r.getEndTime()) {
                    endTime = r.getEndTime();
                }
            }
            double throughput = batchSize * 1000.0 / (endTime - startTime);
            totalTime += (endTime - startTime);
            LOGGER.log(Level.INFO, "Successfully completed benchmark run: " + i + " in " + (endTime - startTime) + " miliseconds. Throughput(msg per second): " + throughput);
        }
        double avgThroughput = batchSize * totalBatches * 1000.0 / totalTime;
        LOGGER.log(Level.INFO, "Benchmark completed. Total number of runs: " + totalBatches + ". Average throughput (msg per second): " + avgThroughput);
    }

    private boolean isDone(List<JMSMessageReceiver> receiverList) {
        for (JMSMessageReceiver receiver : receiverList) {
            if (!receiver.isDone()) {
                return false;
            }
        }
        return true;
    }

    private void setLogLevel(Logger logger, String logLevel) {
        //Options [OFF|SEVERE|WARNING|INFO|CONFIG|FINE|FINER|FINEST|ALL|]
        if (logLevel.equalsIgnoreCase("OFF")) {
            logger.setLevel(Level.OFF);
        } else if (logLevel.equalsIgnoreCase("INFO")) {
            logger.setLevel(Level.INFO);
        } else if (logLevel.equalsIgnoreCase("FINE")) {
            logger.setLevel(Level.FINE);
        } else if (logLevel.equalsIgnoreCase("FINER")) {
            logger.setLevel(Level.FINER);
        } else if (logLevel.equalsIgnoreCase("FINEST")) {
            logger.setLevel(Level.FINEST);
        }
    }

    /**
     *
     * @param args
     * @throws java.lang.Exception
     */
    public static void main(String[] args) throws Exception {
        if (args == null || args.length < 1) {
            System.out.println("Usage: java com.sun.jbi.jmsbd.BenchmarkDriver <properties file>");
            System.exit(0);
        }

        BenchmarkDriver driver = new BenchmarkDriver(args[0]);
        driver.run();
        LOGGER.log(Level.INFO, " Benchmark completed. Exiting ... ");

        // The following is required as there will be running threads which started the servers (JMS server)
        // and the program would not exit.
        System.exit(0);
    }
}
