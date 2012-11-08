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
 * @(#)MessageFormats.java	1.5 08/01/06
 *
 * Copyright 2006 Sun Microsystems, Inc.  All Rights Reserved.
 */ 

import javax.jms.*;

/**
 * The MessageFormats class consists only of a main method, which creates and 
 * then reads messages in all supported JMS message formats:  BytesMessage, 
 * TextMessage, MapMessage, StreamMessage, and ObjectMessage.  It does not send 
 * the messages.
 * <p>
 * Before it can read a BytesMessage or StreamMessage that has not been sent,
 * the program must call reset() to put the message body in read-only mode 
 * and reposition the stream.
 *
 * @author Kim Haase
 * @version 1.4, 08/09/00
 */
public class MessageFormats {

    /**
     * Main method.  Takes no arguments.
     */
    public static void main(String[] args) {
        ConnectionFactory    connectionFactory = null;
        Connection           connection = null;
        Session              session = null;
        BytesMessage         bytesMessage = null;
        byte[]               byteData = {-128, 127, -1, 0, 1, -64, 64};
        int                  length = 0;
        byte[]               inByteData = new byte[7];
        TextMessage          textMessage = null;
        String               msgText = "This is a text message.";
        MapMessage           mapMessage = null;
        StreamMessage        streamMessage = null;
        ObjectMessage        objectMessage = null;
        String               object = "A String is an object.";
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
    	     * Create a BytesMessage, then write it from an array of
    	     * bytes (signed 8-bit integers). 
    	     * Reset the message for reading, then read the bytes into a
    	     * second array.
    	     * A BytesMessage is an undifferentiated stream of bytes that can
    	     * be read in various formats.
    	     */
    	    bytesMessage = session.createBytesMessage();
    	    bytesMessage.writeBytes(byteData);
    	    bytesMessage.reset();
            length = bytesMessage.readBytes(inByteData);
            System.out.println("Reading BytesMessage " + length
                + " bytes long:");
            for (int i = 0; i < length; i++) {
                System.out.print("  " + inByteData[i]);
            }
    	    System.out.println();
    	    
    	    /* 
    	     * Create, write, and display the contents of a TextMessage.
    	     * A TextMessage contains a String of any length. 
    	     */
    	    textMessage = session.createTextMessage();
    	    textMessage.setText(msgText);
            System.out.println("Reading TextMessage:");
            System.out.println(" " + textMessage.getText());
    	    
    	    /* 
    	     * Create and write a MapMessage, then display its contents in
    	     * a different order.
    	     * A MapMessage contains a series of name/value pairs.
    	     * The name is a string; the value can be of various types.
    	     * The receiving program can read any or all of the values,
    	     * in any order.
    	     */
    	    mapMessage = session.createMapMessage();
    	    mapMessage.setString("Message type", "Map");
    	    mapMessage.setInt("An Integer", 3456);
    	    mapMessage.setDouble("A Double", 1.23456789);
            System.out.println("Reading MapMessage in a different order"
                + " from the way it was generated:");
            System.out.println(" Type: " 
                + mapMessage.getString("Message type"));
            System.out.println(" Double: "
                + mapMessage.getDouble("A Double"));
            System.out.println(" Integer: " 
                + mapMessage.getInt("An Integer"));
   	        
    	    /* 
    	     * Create and write a StreamMessage.
    	     * Reset the message for reading and display the values.
    	     * A StreamMessage can also contain values of various types.
    	     * They must be read in the same order in which they were
    	     * written.
    	     */
    	    streamMessage = session.createStreamMessage();
    	    streamMessage.writeString("Stream message");
    	    streamMessage.writeDouble(123.456789e222);
    	    streamMessage.writeInt(223344);
    	    streamMessage.reset();
            System.out.println("Reading StreamMessage in the order"
                + " in which it was generated:");
            System.out.println(" String: " 
                + streamMessage.readString());
            System.out.println(" Double: "
                + streamMessage.readDouble());
            System.out.println(" Integer: " 
                + streamMessage.readInt());
    	    
    	    /* 
    	     * Create an ObjectMessage from a String object, then
    	     * display its contents.
    	     * An ObjectMessage can contain any Java object.  This example
    	     * uses a String for the sake of simplicity.  The program that
    	     * reads the object casts it to the appropriate type.
    	     */
    	    objectMessage = session.createObjectMessage();
    	    objectMessage.setObject(object);
            System.out.println("Reading ObjectMessage:");
            System.out.println(" " + (String) objectMessage.getObject()); 

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
