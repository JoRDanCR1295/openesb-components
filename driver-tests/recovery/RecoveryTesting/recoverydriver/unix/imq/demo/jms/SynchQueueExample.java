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
 * @(#)SynchQueueExample.java	1.6 08/01/06
 *
 * Copyright 2006 Sun Microsystems, Inc.  All Rights Reserved.
 */ 

import javax.jms.*;

/**
 * The SynchQueueExample class consists only of a main method, which fetches 
 * one or more messages from a queue using synchronous message delivery.  Run 
 * this program in conjunction with SenderToQueue.  Specify a queue name on the
 * command line when you run the program.
 * <p>
 * The program calls methods in the SampleUtilities class.
 *
 * @author Kim Haase
 * @version 1.7, 08/14/00
 */
public class SynchQueueExample {

    /**
     * Main method.
     *
     * @param args	the queue used by the example
     */
    public static void main(String[] args) {
        String               queueName = null;
        ConnectionFactory    connectionFactory = null;
        Connection           connection = null;
        Session              session = null;
        Queue                queue = null;
        MessageConsumer      msgConsumer = null;
        TextMessage          message = null;
        int                  exitResult = 0;
                
    	/*
    	 * Read queue name from command line and display it.
    	 */
    	if (args.length != 1) {
    	    System.out.println("Usage: java SynchQueueExample <queue_name>");
    	    System.exit(1);
    	}
    	queueName = new String(args[0]);
    	System.out.println("Queue name is " + queueName);
    	    
        /*
         * Obtain connection factory.
         * Create connection.
         * Create session from connection; false means session is not
         * transacted.
         * Obtain queue name.
         */
    	try {
    	    connectionFactory = 
    	        SampleUtilities.getConnectionFactory();
    	    connection = 
    	        connectionFactory.createConnection();
            session = connection.createSession(false, 
                Session.AUTO_ACKNOWLEDGE);
            queue = SampleUtilities.getQueue(queueName, session);
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
         * Create consumer, then start message delivery.
	 * Receive all text messages from queue until
	 * a non-text message is received indicating end of
	 * message stream.
         * Close connection and exit.
         */
        try {
            msgConsumer = session.createConsumer(queue);
            connection.start();
	    while (true) {
		Message m = msgConsumer.receive();
		if (m instanceof TextMessage) {
		    message = (TextMessage) m;
		    System.out.println("Reading message: " + message.getText());
		} else {
                    // Non-text control message indicates end of messages.
 		    break;
		}
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
    	SampleUtilities.exit(exitResult);
    }
}