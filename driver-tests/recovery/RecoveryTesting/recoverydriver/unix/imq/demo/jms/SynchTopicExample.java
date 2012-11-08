/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.  
 *
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with 
 * your own identifying information: 
 * "Portions Copyrighted [year] [name of copyright owner]"
 */

/*
 * @(#)SynchTopicExample.java	1.5 08/01/06
 *
 * Copyright 2006 Sun Microsystems, Inc.  All Rights Reserved.
 */ 
 
import javax.jms.*;

/**
 * The SynchTopicExample class demonstrates the simplest form of the 
 * publish/subscribe model: the producer publishes a message, and the 
 * consumer reads it using a synchronous receive.
 * <p>
 * The program contains a SimpleProducer class, a SynchConsumer class, a
 * main method, and a method that runs the consumer and producer
 * threads.
 * <p>
 * Specify a topic name on the command line when you run the program.
 * <p>
 * The program calls methods in the SampleUtilities class.
 *
 * @author Kim Haase
 * @version 1.7, 08/18/00
 */
public class SynchTopicExample {
    String  topicName = null;
    int     exitResult = 0;

    /**
     * The SynchConsumer class fetches a single message from a topic using 
     * synchronous message delivery.
     *
     * @author Kim Haase
     * @version 1.7, 08/18/00
     */
    public class SynchConsumer extends Thread {

        /**
         * Runs the thread.
         */
        public void run() {
            ConnectionFactory    connectionFactory = null;
            Connection           connection = null;
            Session              session = null;
            Topic                topic = null;
            MessageConsumer      msgConsumer = null;
            final boolean        NOLOCAL = true;
            TextMessage          inMessage = null;
            TextMessage          outMessage = null;
            MessageProducer      msgProducer = null;

            /*
             * Obtain connection factory.
             * Create connection.
             * Create session from connection; false means session is not
             * transacted.
             * Obtain topic name.
             */
            try {
                connectionFactory = 
                    SampleUtilities.getConnectionFactory();
                connection = 
                    connectionFactory.createConnection();
                session = connection.createSession(false, 
                    Session.AUTO_ACKNOWLEDGE);
                topic = SampleUtilities.getTopic(topicName, session);
            } catch (Exception e) {
                System.out.println("Connection problem: " + e.toString());
                if (connection != null) {
                    try {
                        connection.close();
                    } catch (JMSException ee) {}
                }
    	        System.exit(1);
            } 

            /*
             * Create consumer, then start message delivery.  Consumer is
             * non-local so that it won't receive the message we publish.
             * Wait for text message to arrive, then display its contents.
             * Close connection and exit.
             */
            try {
                msgConsumer = 
                    session.createConsumer(topic, null, NOLOCAL);
                connection.start();

                inMessage = (TextMessage) msgConsumer.receive();
                System.out.println("CONSUMER THREAD: Reading message: " 
                                   + inMessage.getText());

                /* 
                 * Notify producer that we received a message and it
                 * can stop broadcasting.
                 */
                msgProducer = session.createProducer(topic);
                outMessage = session.createTextMessage();
                outMessage.setText("Done");
                msgProducer.send(outMessage);
            } catch (JMSException e) {
                System.out.println("Exception occurred: " + e.toString());
                exitResult = 1;
            } finally {
                if (connection != null) {
                    try {
                        connection.close();
                    } catch (JMSException e) {
                        exitResult = 1;
                    }
                }
            }   	    
        }
    }

    /**
     * The SimpleProducer class publishes a single message to a topic. 
     *
     * @author Kim Haase
     * @version 1.7, 08/18/00
     */
    public class SimpleProducer extends Thread {

        /**
         * Runs the thread.
         */
        public void run() {
            ConnectionFactory    connectionFactory = null;
            Connection           connection = null;
            Session              session = null;
            Topic                topic = null;
            MessageConsumer      producerControlConsumer = null;
            final boolean        NOLOCAL = true;
            MessageProducer      msgProducer =  null;
            TextMessage          sentMessage = null;
            final String         MSG_TEXT = new String("Here is a message ");
            Message              receivedMessage = null;

            /*
             * Obtain connection factory.
             * Create connection.
             * Create session from connection; false means session is not
             * transacted.
             * Obtain topic name.
             */
            try {
                connectionFactory = 
                    SampleUtilities.getConnectionFactory();
                connection = 
                    connectionFactory.createConnection();
                session = connection.createSession(false, 
                    Session.AUTO_ACKNOWLEDGE);
                topic = SampleUtilities.getTopic(topicName, session);
            } catch (Exception e) {
                System.out.println("Connection problem: " + e.toString());
                if (connection != null) {
                    try {
                        connection.close();
                    } catch (JMSException ee) {}
                }
    	        System.exit(1);
            } 

            /*
             * Create non-local consumer to receive "Done" message from
             * another connection; start delivery.
             * Create producer and text message.
             * Set message text, display it, and publish message.
             * Close connection and exit.
             */
            try {
                producerControlConsumer = 
                    session.createConsumer(topic, null, NOLOCAL);
                connection.start();

                /*
                 * Publish a message once per second until consumer 
                 * reports that it has finished receiving messages.
                 */
                msgProducer = session.createProducer(topic);
                sentMessage = session.createTextMessage();
                for (int i = 1; receivedMessage == null; i++) {
                    sentMessage.setText(MSG_TEXT + i);
                    System.out.println("PRODUCER THREAD: Publishing message: " 
                                       + sentMessage.getText());
                    msgProducer.send(sentMessage);
                    try { Thread.sleep(1000); } catch (InterruptedException ie){}
                    receivedMessage = producerControlConsumer.receiveNoWait();
                }
            } catch (JMSException e) {
                System.out.println("Exception occurred: " + e.toString());
                exitResult = 1;
            } finally {
                if (connection != null) {
                    try {
                        connection.close();
                    } catch (JMSException e) {
                        exitResult = 1;
                    }
                }
            }
        }
    }
    
    /**
     * Instantiates the consumer and producer classes and starts their
     * threads.
     * Calls the join method to wait for the threads to die.
     * <p>
     * It is essential to start the consumer before starting the producer.
     * In the publish/subscribe model, a consumer can ordinarily receive only 
     * messages published while it is active. 
     */
    public void run_threads() {
        SynchConsumer  synchConsumer = new SynchConsumer();
        SimpleProducer  simpleProducer = new SimpleProducer();

        synchConsumer.start();
        simpleProducer.start();
        try {
            synchConsumer.join();
            simpleProducer.join();
        } catch (InterruptedException e) {}
    }

    /**
     * Reads the topic name from the command line and displays it.  The
     * topic must have been created by the jmsadmin tool.
     * Calls the run_threads method to execute the program threads.
     * Exits program.
     *
     * @param args	the topic used by the example
     */
    public static void main(String[] args) {
        SynchTopicExample  ste = new SynchTopicExample();
        
        if (args.length != 1) {
    	    System.out.println("Usage: java SynchTopicExample <topic_name>");
    	    System.exit(1);
    	}
        ste.topicName = new String(args[0]);
        System.out.println("Topic name is " + ste.topicName);

    	ste.run_threads();
    	SampleUtilities.exit(ste.exitResult);
    }
}
