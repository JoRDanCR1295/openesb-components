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
 * your own identifying information: Portions Copyright 
 * [year] [name of copyright owner]
 */

/*
 * @(#)SendSOAPMessage.java	1.5 08/01/06
 *
 * Copyright 2006 Sun Microsystems, Inc.  All Rights Reserved.
 */ 

import javax.xml.messaging.URLEndpoint;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPMessage;
import javax.xml.soap.SOAPEnvelope;
import javax.xml.soap.SOAPBody;
import javax.xml.soap.SOAPPart;
import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPConnectionFactory;
import javax.xml.soap.SOAPConnection;

/**
 * This example demonstrates a hello world example for using JAXM API.
 */
public class SendSOAPMessage {

    /**
     * send a simple soap message with JAXM API.
     */
    public void sendMessage (String url) {

        try {
            /**
             * Construct a default SOAP message factory.
             */
            MessageFactory mf = MessageFactory.newInstance();
            /**
             * Create a SOAP message object.
             */
            SOAPMessage soapMessage = mf.createMessage();
            /**
             * Get SOAP part.
             */
            SOAPPart soapPart = soapMessage.getSOAPPart();
            /**
             * Get SOAP envelope.
             */
            SOAPEnvelope soapEnvelope = soapPart.getEnvelope();

            /**
             * Get SOAP body.
             */
            SOAPBody soapBody = soapEnvelope.getBody();

            /**
             * Add child element with the specified name.
             */
            SOAPElement element = soapBody.addChildElement("HelloWorld");

            /**
             * Add text message
             */
            element.addTextNode("Welcome to SunOne Web Services!");

            soapMessage.saveChanges();

            /**
             * Construct a default SOAP connection factory.
             */
            SOAPConnectionFactory connectionFactory = SOAPConnectionFactory.newInstance();

            /**
             * Get SOAP connection.
             */
            SOAPConnection soapConnection = connectionFactory.createConnection();

            /**
             * Construct endpoint object.
             */
            URLEndpoint endpoint = new URLEndpoint (url);

            /**
             * Send SOAP message.
             */
            SOAPMessage resp = soapConnection.call(soapMessage, endpoint);

            /**
             * Print response to the std output.
             */
            resp.writeTo( System.out );

            /**
             * close the connection
             */
            soapConnection.close();

        } catch (java.io.IOException ioe) {
            ioe.printStackTrace();
        } catch (SOAPException soape) {
            soape.printStackTrace();
        }
    }

    public static void main (String args[]) {

        String url = "http://localhost:8080/imqSOAPexamples/SOAPEchoServlet";

        if (args.length > 0) {
            url = args[0];
        } else {
            System.out.println("Usage: " +
                "\tjava SendSOAPMessage <SOAP servlet url>\n" +
                "e.g.\n\tjava SendSOAPMessage http://localhost:8080/imqSOAPexamples/SOAPEchoServlet"
                );
            System.exit(1);
        }

        SendSOAPMessage ssm = new SendSOAPMessage();
        ssm.sendMessage(url);
    }

}
