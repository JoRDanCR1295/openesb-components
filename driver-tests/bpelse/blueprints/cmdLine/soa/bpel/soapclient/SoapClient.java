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
 * @(#)SoapClient.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Properties;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.MimeHeaders;
import javax.xml.soap.SOAPConnection;
import javax.xml.soap.SOAPConnectionFactory;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPMessage;
import javax.xml.soap.SOAPPart;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;


public class SoapClient {
    /**
     * Test of inbound SOAP Request processing.
     */
    private static MessageFactory messageFactory;
    private static SOAPConnectionFactory soapConnFactory;
    private static SOAPConnection connection;
    
    static {
        try {
            messageFactory = MessageFactory.newInstance();
            soapConnFactory = SOAPConnectionFactory.newInstance();
            connection = soapConnFactory.createConnection();
        } catch (SOAPException ex) {
            ex.printStackTrace();
        }
    }
    
    public static void testInboundSOAPRequest(String propFilePath, String commonPropsFilePath) throws Exception {
        try {
            FileInputStream fis = null;
            Properties props = null;
            try {
                fis = new FileInputStream(new File(propFilePath));
                props = new java.util.Properties();
                props.load(fis);
            } finally {
                try {
                    if (fis != null) {
                        fis.close();
                    }
                } catch (Exception ex) {
                    // ignore
                }
            }
            
            String soapAction = (String) props.get("soapaction");
            String destination = (String) props.get("destination");
            String inputFilePath = (String) props.get("inputfile");
            String outputFilePath = (String) props.get("outputfile");

            try {
                fis = new FileInputStream(new File(commonPropsFilePath));
                props = new java.util.Properties();
                props.load(fis);
            } finally {
                try {
                    if (fis != null) {
                        fis.close();
                    }
                } catch (Exception ex) {
                    // ignore
                }
            }            
            String httpDefaultPort = ((String) props.get("HttpDefaultPort"));
            if(httpDefaultPort == null || httpDefaultPort.trim().equals("")) {
            	System.out.println("Please specify a value for the property HttpDefaultPort in the file " + commonPropsFilePath);
            	return;	
            }
            
            
            destination = destination.replaceAll("\\$\\{HttpDefaultPort\\}", httpDefaultPort);
            System.out.println("destination : " + destination);
            sendAndCheck(destination, inputFilePath, outputFilePath, soapAction);        
        } catch (Exception ex) {
            System.out.println(" Failed.");
            throw ex;
        } catch (Error er) {
            System.out.println(" Failed.");
            throw er;
        }
    }
    
    /**
     * Send a soap message read in from the given input file, compare the output 
     * to the given output file
     */
    private static void sendAndCheck(String destination, String inputFilePath, String outputFilePath, String soapAction) throws Exception {
        SOAPMessage message = loadMessage(inputFilePath);
        SOAPMessage response = sendMessage(destination, message, soapAction);
        outputResponse(response, inputFilePath, outputFilePath);
    }

    private static void outputResponse(SOAPMessage response, String inputFilePath, String outputFilePath) {
        try {
            if (outputFilePath == null || outputFilePath.equals("")) {
                outputFilePath = inputFilePath + ".out";
            }
            System.out.println("output file: " + outputFilePath);
            File outputFile = new File(outputFilePath);
            FileOutputStream fos = new FileOutputStream(outputFile);
            TransformerFactory transformerFactory = TransformerFactory.newInstance();
            Transformer transformer = transformerFactory.newTransformer();

            if (response != null) {
                SOAPPart replySOAPPart = response.getSOAPPart();
                Source sourceContent = replySOAPPart.getContent();
                StreamResult result = new StreamResult(fos);
                transformer.transform(sourceContent, result);
            }
        } catch (IOException ex) {
            ex.printStackTrace();
        } catch (TransformerConfigurationException ex) {
            ex.printStackTrace();
        } catch (SOAPException ex) {
            ex.printStackTrace();
        } catch (TransformerException ex) {
            ex.printStackTrace();
        }
    }
    
    /**
     * read in a soap message from the given input file
     */    
    private static SOAPMessage loadMessage(String testMsgFileName) throws SOAPException, IOException {
        //Create and populate the message from a file
        SOAPMessage message = messageFactory.createMessage();
        SOAPPart soapPart = message.getSOAPPart();
        StreamSource preppedMsgSrc = new StreamSource(new FileInputStream(testMsgFileName));
        soapPart.setContent(preppedMsgSrc);
        message.saveChanges();
        return message;
    }
    
    /**
     * Send a soap message 
     * @param destination URL to send to 
     * @param message message to send
     * @param expectedHttpStatus expected http status code or null if success is expected
     * @return reply soap message
     */    
    private static SOAPMessage sendMessage(String destination, SOAPMessage message, String soapAction) throws SOAPException {
        
        // Add soapAction if not null
        if (soapAction != null) {
            MimeHeaders hd = message.getMimeHeaders();
            hd.setHeader("SOAPAction", soapAction);
        }
        
        // Send the message and get a reply
        SOAPMessage reply = null;
        long start = 0;
       
        boolean httpSuccess = true;
        try {
            reply = connection.call(message, destination);
        } catch (SOAPException ex) {
            ex.printStackTrace();
        }
        long end = 0;
                
        return reply;
    }

    public final static void main(String[] args) {
        if (args.length != 2) {
            System.out.println("Please specify the two arguments. Usage: java SoapClient test.properties common.properties");
            return;
        }
        
        String propFilePath = args[0];
        System.out.println("test file path: " + propFilePath);
        String commonPropsFilePath = args[1];
                
        try {
            SoapClient.testInboundSOAPRequest(propFilePath, commonPropsFilePath);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}
