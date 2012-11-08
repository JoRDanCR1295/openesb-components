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
 * @(#)ObjectMessages.java	1.5 08/01/06
 *
 * Copyright 2006 Sun Microsystems, Inc.  All Rights Reserved.
 */ 

import javax.jms.*;

/**
 * The ObjectMessages class consists only of a main method, which demonstrates
 * that mutable objects are copied, not passed by reference, when you use them 
 * to create message objects.
 * <p>
 * The example uses only an ObjectMessage and a BytesMessage, but the same is
 * true for all message formats.
 *
 * @author Kim Haase
 * @version 1.4, 08/09/00
 */
public class ObjectMessages {

    /**
     * Main method.  Takes no arguments.
     */
    public static void main(String[] args) {
        ConnectionFactory    connectionFactory = null;
        Connection           connection = null;
        Session              session = null;
        ObjectMessage        objectMessage = null;
        String               object = "A String is an object.";
        BytesMessage         bytesMessage = null;
        byte[]               byteArray = {3, 5, 7, 9, 11};
        final int            ARRLEN = 5;
        int                  length = 0;
        byte[]               inByteData = new byte[ARRLEN];
        int                  exitResult = 0;

    	try {
            connectionFactory = 
                SampleUtilities.getConnectionFactory();
    	    connection = 
    	        connectionFactory.createConnection();
    	    session = connection.createSession(false, 
    	        Session.AUTO_ACKNOWLEDGE);
    	} catch (Exception e) {
            System.out.println("Connection problem: " + e.toString());
            if (connection != null) {
                try {
                    connection.close();
                } catch (JMSException ee) {}
            }
    	    System.exit(1);
    	} 

        try {
    	    /* 
    	     * Create an ObjectMessage from a String.
    	     * Modify the original object.
    	     * Read the message, proving that the object in the message
             * has not changed.
             */
    	    objectMessage = session.createObjectMessage();
    	    System.out.println("Writing ObjectMessage with string:  " + object);
    	    objectMessage.setObject(object);
    	    object = "I'm a different String now.";
    	    System.out.println("Changed string; object is now:  " + object);
            System.out.println("ObjectMessage contains:  " + 
                (String) objectMessage.getObject()); 

    	    /* 
    	     * Create a BytesMessage from an array.
    	     * Modify an element of the original array.
    	     * Reset and read the message, proving that contents of the message
             * have not changed.
    	     */
    	    bytesMessage = session.createBytesMessage();
    	    System.out.print("Writing BytesMessage with array: ");
            for (int i = 0; i < ARRLEN; i++) {
                System.out.print(" " + byteArray[i]);
    	    }
    	    System.out.println();
    	    bytesMessage.writeBytes(byteArray);
    	    byteArray[1] = 13;
    	    System.out.print("Changed array element; array is now: ");
            for (int i = 0; i < ARRLEN; i++) {
                System.out.print(" " + byteArray[i]);
    	    }
    	    System.out.println();
    	    bytesMessage.reset();
            length = bytesMessage.readBytes(inByteData);
            System.out.print("BytesMessage contains: ");
            for (int i = 0; i < length; i++) {
                System.out.print(" " + inByteData[i]);
            }
    	    System.out.println();
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
