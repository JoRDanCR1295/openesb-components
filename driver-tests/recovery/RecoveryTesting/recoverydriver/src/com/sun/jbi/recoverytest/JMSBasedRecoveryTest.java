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
 * @(#)JMSBasedRecoveryTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.recoverytest;

import java.io.File;
import java.io.IOException;
import java.util.Properties;
import java.util.logging.FileHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;
import javax.jms.Session;
import javax.jms.TextMessage;

public class JMSBasedRecoveryTest {
    
    private static Logger LOGGER = Logger.getLogger(JMSBasedRecoveryTest.class.getName());
    private Properties properties = new Properties();
    private Properties testCaseProperties = new Properties();
    
    /**
     *
     * @param testPropFile
     * @throws java.lang.Exception
     */
    public JMSBasedRecoveryTest(String testPropFile) throws Exception {
        init(testPropFile);
    }
    
    private void init(String testPropFile) throws Exception {
        
        //Load the properties
        properties.load(new File(testPropFile).toURL().openStream());        
        testCaseProperties.load(new File(properties.getProperty("TestCasePropertyFile")).toURL().openStream());
        
        Logger parentLogger = Logger.getLogger("com.sun.jbi.recoverytest");
        Handler logFile = new FileHandler(properties.getProperty("TestLogFile"));
        logFile.setFormatter(new LogFormatter());
        parentLogger.addHandler(logFile);
        setLogLevel(parentLogger, properties.getProperty("loggingLevel"));
        
        LOGGER.log(Level.INFO, "*** Test configurations *** ");
        
        LOGGER.log(Level.INFO, "BatchSize: " + Integer.parseInt(properties.getProperty("BatchSize")));
        LOGGER.log(Level.INFO, "Iterations: " + Integer.parseInt(properties.getProperty("Iterations")));
        
        LOGGER.log(Level.INFO, "StartServers (start the appservers and database server): "
                + properties.getProperty("StartServers"));
        
        LOGGER.log(Level.INFO, "KillAppServer: " + properties.getProperty("KillAppServer"));
        
        LOGGER.log(Level.INFO, "WaitTimeBeforeASStart : " + properties.getProperty("WaitTimeBeforeASStart"));
        LOGGER.log(Level.INFO, "WaitTimeAfterASStart : " + properties.getProperty("WaitTimeAfterASStart"));
        LOGGER.log(Level.INFO, "MaxRandomWaitTimeBeforeASCrash : " + properties.getProperty("MaxRandomWaitTimeBeforeASCrash"));
        
        LOGGER.log(Level.INFO, "JmsServerHostName : " + properties.getProperty("JmsServerHostName"));
        LOGGER.log(Level.INFO, "JmsServerPort : " + properties.getProperty("JmsServerPort"));
        
        LOGGER.log(Level.INFO, "MessageTimeout: " + properties.getProperty("MessageTimeout"));
        LOGGER.log(Level.INFO, "IterationsAfterWhichToDeleteRows: " + properties.getProperty("IterationsAfterWhichToDeleteRows"));
        
        LOGGER.log(Level.INFO, "AllowDuplicateMessages " + properties.getProperty("AllowDuplicateMessages"));
        
        LOGGER.log(Level.INFO, "******************************************* ");
        LOGGER.log(Level.INFO, "**** Test case configurations (from test case file) : " + properties.getProperty("TestCasePropertyFile") + "****");
        LOGGER.log(Level.INFO, "Queue name where messages will be published : " + testCaseProperties.getProperty("PublisherQueue"));
        LOGGER.log(Level.INFO, "Queue name from where the messages will be consumed : " + testCaseProperties.getProperty("ConsumerQueue"));
        LOGGER.log(Level.INFO, "******************************************* ");
    }
    
    /**
     * This would test by feeding the messages to the queue in batches.
     *
     * @param testCaseProperties
     * @throws IOException
     * @throws Exception
     */
    private void runTest() throws IOException, Exception {
        LOGGER.log(Level.INFO, "***** Test Results ******* ");        
        startServers();
        setUpAppserverCrashAndStartTasks();
        
        int batchSize = Integer.parseInt(properties.getProperty("BatchSize"));
        String iterations = properties.getProperty("Iterations");
        int iterAfterWhichToDleteRows = Integer.parseInt(properties.getProperty("IterationsAfterWhichToDeleteRows"));
        
        Thread messageSender = null;
        Thread messageReceiver = null;
        boolean isFileBased = Boolean.parseBoolean(testCaseProperties.getProperty("IsFileBased", "false"));
        int currentIteration = 1;
        do {
            LOGGER.log(Level.INFO, "Starting test run (" + currentIteration + ") .. Message batch size (" + batchSize + ")");
            if (isFileBased) {
                messageSender = new FileBasedJMSMessageSender(properties, testCaseProperties);            
                messageReceiver = new FileBasedJMSMessageReceiver(properties, testCaseProperties);
            } else {
                messageSender = new JMSMessageSender(properties, testCaseProperties);            
                messageReceiver = new JMSMessageReceiver(properties, testCaseProperties);
            }
            messageSender.setName("JMS Message Sender");
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
            LOGGER.log(Level.INFO, "Successfully completed test run: " + currentIteration);
            
            if(iterAfterWhichToDleteRows != -1 && (currentIteration % iterAfterWhichToDleteRows == 0)) {
                Utility.deleteDataFromDB(properties.getProperty("DatabaseUrl"), 
                        properties.getProperty("DatabaseUsername"),
                        properties.getProperty("DatabasePassword"));            
            }

        } while (continueExecution(currentIteration++));
        
        LOGGER.log(Level.INFO," Test completed. Total number of runs: " + (currentIteration - 1));
    }
    
    /**
     * @param currentIteration
     * @return
     */
    private boolean continueExecution(int currentIteration) {
        int iterations = Integer.parseInt(properties.getProperty("Iterations"));
        if (iterations == -1) {
            return true;
        } else {
            return currentIteration < iterations;
        }
    }
    
    private void setUpAppserverCrashAndStartTasks() {
        
        if("yes".equalsIgnoreCase(properties.getProperty("KillAppServer"))){
            AppserverStartAndCrash appServerStartAndCrashThread = new AppserverStartAndCrash(properties);
            appServerStartAndCrashThread.setPriority(9);
            appServerStartAndCrashThread.start();
        }
    }
    
    private void startServers() throws Exception {
        
        if ("yes".equalsIgnoreCase(properties.getProperty("StartServers"))) {
            Utility.startJMSServer(properties);
            
            if (properties.getProperty("DatabaseUrl").startsWith("jdbc:derby://")) {
                Utility.startDBServer(properties);
            }
            
            Utility.startAppServer(properties);
            
            LOGGER.log(Level.INFO, "Waiting " + Integer.parseInt(properties.getProperty("WaitTimeAfterASStart")) + " secs for the AppServer to complete startup");
            try {
                Thread.sleep(Integer.parseInt(properties.getProperty("WaitTimeAfterASStart")) * 1000);
            } catch(InterruptedException e) {
                LOGGER.log(Level.SEVERE, "Exception occurred while the thread was sleeping.", e);
            }            
        }
    }
    
    private void setLogLevel(Logger logger, String logLevel) {
        //Options [OFF|SEVERE|WARNING|INFO|CONFIG|FINE|FINER|FINEST|ALL|]
        if (logLevel.equalsIgnoreCase("OFF")) {logger.setLevel(Level.OFF);} 
        else if (logLevel.equalsIgnoreCase("SEVERE")) {logger.setLevel(Level.SEVERE);} 
        else if (logLevel.equalsIgnoreCase("WARNING")) {logger.setLevel(Level.WARNING);} 
        else if (logLevel.equalsIgnoreCase("INFO")) {logger.setLevel(Level.INFO);} 
        else if (logLevel.equalsIgnoreCase("CONFIG")) {logger.setLevel(Level.CONFIG);} 
        else if (logLevel.equalsIgnoreCase("FINE")) {logger.setLevel(Level.FINE);} 
        else if (logLevel.equalsIgnoreCase("FINER")) {logger.setLevel(Level.FINER);} 
        else if (logLevel.equalsIgnoreCase("FINEST")) {logger.setLevel(Level.FINEST);} 
        else if (logLevel.equalsIgnoreCase("ALL")) {logger.setLevel(Level.ALL);}
    }
    
    /**
     * @throws java.lang.Exception
     */
    protected void tearDown() throws Exception {
    }
    
    /*
     * In development ... Currently not used
     */ 
    private void runJMSInOutWithBPELStopAndStart(boolean shutdownAndStart) throws Exception {
        System.out.println("read the consumer and publisher names from the properties file");
        System.exit(0);
        Object[] objs = null; //JMSHelper.createQueuesAndSession(properties.getProperty("JmsServerHostName"), 
                //properties.getProperty("JmsServerPort"), null, null);
        MessageProducer producer = (MessageProducer) objs[0];
        MessageConsumer consumer = (MessageConsumer) objs[1];
        Session producerSession = (Session) objs[2];
        long i = 0;
        TextMessage msg = null;
        do {
            msg = producerSession.createTextMessage();
            msg.setText("This is message " + (i + 1));
            System.out.println("Sending message: " + msg.getText());
            producer.send(msg);
            
            if (shutdownAndStart) {
                //shutdownAndStartComponent();
            } else {
                //stopAndStartComponent();
            }
            
            Message returnedMesg = consumer.receive();
            msg = (TextMessage) returnedMesg;
            System.out.println("received message: " + msg.getText());
            i++;
        } while (true);
    }
    
    /**
     *
     * @param args
     * @throws java.lang.Exception
     */
    public static void main(String args[]) throws Exception {
        if (args == null || args.length < 1) {
            System.out.println("Usage: JMSBasedRecoveryTest <properties file>");
            System.exit(0);
        }
        
        JMSBasedRecoveryTest recoveryTest = new JMSBasedRecoveryTest(args[0]);
        recoveryTest.runTest();
        LOGGER.log(Level.INFO," Test completed. Exiting ... ");
        
        // The following is required as there will be running threads which started the servers (JMS server)
        // and the program would not exit.
        System.exit(1);
    }
}
