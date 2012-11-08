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

package com.sun.jbi.performance.jdbcjmsbd;

import java.io.File;
import java.io.IOException;
import java.util.Properties;
import java.util.logging.FileHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;
import javax.jms.Session;
import javax.jms.TextMessage;

/**
 * BenchmarkDriver.java
 *
 * Created on July 27, 2007, 10:04 AM
 */
public class JDBCJMSBenchmarkDriver {

    private static Logger LOGGER = Logger.getLogger(JDBCJMSBenchmarkDriver.class.getName());
    private Properties properties = new Properties();

    /**
     *
     * @param propFile
     * @throws java.lang.Exception
     */
    public JDBCJMSBenchmarkDriver(String propFileName) throws Exception {
        //Load the properties
        File propFile = new File(propFileName);
        properties.load(propFile.toURL().openStream());
        properties.setProperty("InputSQLFile", 
                propFile.getParentFile().getAbsolutePath() + File.separator + properties.getProperty("InputSQLFile"));
        
        properties.setProperty("InputDataFile", 
                propFile.getParentFile().getAbsolutePath() + File.separator + properties.getProperty("InputDataFile"));
        
        
        properties.setProperty("LogFile", 
                propFile.getParentFile().getAbsolutePath() + File.separator + properties.getProperty("LogFile"));
        Logger parentLogger = Logger.getLogger(getClass().getPackage().getName());
        Handler logFile = new FileHandler(properties.getProperty("LogFile"));
        logFile.setFormatter(new LogFormatter());
        parentLogger.addHandler(logFile);
        setLogLevel(parentLogger, properties.getProperty("LoggingLevel"));

        LOGGER.log(Level.INFO, "*** Configurations *** ");

        LOGGER.log(Level.INFO, "JmsServerHostName : " + properties.getProperty("JmsServerHostName"));
        LOGGER.log(Level.INFO, "JmsServerPort : " + properties.getProperty("JmsServerPort"));
        LOGGER.log(Level.INFO, "ConsumerQueue : " + properties.getProperty("ConsumerQueue"));
        LOGGER.log(Level.INFO, "InputSQLFile : " + properties.getProperty("InputSQLFile"));
        LOGGER.log(Level.INFO, "InputDataFile : " + properties.getProperty("InputDataFile"));
        LOGGER.log(Level.INFO, "JDBCDriverFile : " + properties.getProperty("JDBCDriverFile"));
        LOGGER.log(Level.INFO, "JDBCDriverClassName : " + properties.getProperty("JDBCDriverClassName"));
        LOGGER.log(Level.INFO, "DBUrl : " + properties.getProperty("DBUrl"));
        LOGGER.log(Level.INFO, "DBUser : " + properties.getProperty("DBUser"));
        LOGGER.log(Level.INFO, "DBPassword : " + properties.getProperty("DBPassword"));
        LOGGER.log(Level.INFO, "InputCommitSize : " + properties.getProperty("InputCommitSize"));
        
        LOGGER.log(Level.INFO, "InputBatchSize: " + Integer.parseInt(properties.getProperty("InputBatchSize")));
        LOGGER.log(Level.INFO, "OutputBatchSize: " + Integer.parseInt(properties.getProperty("OutputBatchSize")));
        LOGGER.log(Level.INFO, "TotalBatches: " + Integer.parseInt(properties.getProperty("TotalBatches")));

    }

   /**
     * This would test by feeding the messages to the queue in batches.
     *
     * @throws IOException
     * @throws Exception
     */
    private void run() throws Exception {
        LOGGER.log(Level.INFO, "***** Results ******* ");
        int batchSize = Integer.parseInt(properties.getProperty("InputBatchSize"));
        int totalBatches = Integer.parseInt(properties.getProperty("TotalBatches"));

        Thread messageSender = null;
        Thread messageReceiver = null;
        long totalTime = 0;
        for (int i = 0; i < totalBatches; i++) {
            LOGGER.log(Level.INFO, "Starting benchmark run (" + i + ") .. Message batch size (" + batchSize + ")");
            messageSender = new JDBCTableDataInserter(properties);
            messageReceiver = new JMSMessageReceiver(properties);
            long startTime = System.currentTimeMillis();
            messageSender.setName("JDBC Table Data Inserter");
            messageSender.start();

            messageReceiver.setName("JMS Message Receiver");
            messageReceiver.start();

            /*
             *It is expected that the sender has completed sending messages before the receiver returns,
             *as the receiver would not return before receiving all the messages, hence we dont need to
             *join on the sender.
             */
            messageSender.join();
            messageReceiver.join();
            long endTime = System.currentTimeMillis();
            double throughput = batchSize * 1000.0 / (endTime - startTime);
            totalTime += (endTime - startTime);
            LOGGER.log(Level.INFO, "Successfully completed benchmark run: " + i + " in " + (endTime - startTime) + " miliseconds. Throughput(msg per second): " + throughput);
        }
        double avgThroughput = batchSize * totalBatches  * 1000.0 / totalTime;
        LOGGER.log(Level.INFO, "Benchmark completed. Total number of runs: " + totalBatches + ". Average throughput (msg per second): " + avgThroughput);
    }

    /**
     * @param currentIteration
     * @return
     */
    private boolean continueExecution(int currentIteration) {
        int totalBatches = Integer.parseInt(properties.getProperty("TotalBatches"));
        if (totalBatches == -1) {
            return true;
        } else {
            return currentIteration < totalBatches;
        }
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
        //args = new String[] {"C:\\sun\\open-esb\\open-jbi\\open-jbi-components\\driver-tests\\performance\\JDBCJMSBased\\benchmarks\\iepse\\externalTablePolling\\pollTable\\benchmark.properties"};
        if (args == null || args.length < 1) {
            System.out.println("Usage: java com.sun.jbi.jmsbd.BenchmarkDriver <properties file>");
            System.exit(0);
        }

        JDBCJMSBenchmarkDriver driver = new JDBCJMSBenchmarkDriver(args[0]);
        driver.run();
        LOGGER.log(Level.INFO, " Benchmark completed. Exiting ... ");

        // The following is required as there will be running threads which started the servers (JMS server)
        // and the program would not exit.
        System.exit(0);
    }
}
