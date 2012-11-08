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
 * @(#)Simulator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.simulator;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Serializable;
import java.io.StringReader;

import javax.xml.soap.MessageFactory;
import javax.xml.soap.MimeHeaders;
import javax.xml.soap.SOAPConnection;
import javax.xml.soap.SOAPConnectionFactory;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPMessage;
import javax.xml.soap.SOAPPart;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

/**
 * @author ylee
 * @author graj
 *
 */
public class Simulator implements Serializable {
    private String destinationURI;
    private String soapAction;
    private String soapRequest;

    private SOAPConnectionFactory soapConnectionFactory;
    private SOAPConnection soapConnection;
    private MessageFactory messageFactory;


    /**
     *
     */
    public Simulator() {
    }

    /**
     * @param destinationuri
     * @param action
     * @param request
     */
    public Simulator(String destinationuri, String soapAction, String soapRequestPayload) {
        this.destinationURI = destinationuri;
        this.soapAction = soapAction;
        this.soapRequest = soapRequestPayload;
    }

    public void initialize() throws SOAPException {
        this.soapConnectionFactory = SOAPConnectionFactory.newInstance();
        this.soapConnection = this.soapConnectionFactory.createConnection();
        this.messageFactory = MessageFactory.newInstance();
    }

    public void cleanup() throws SOAPException {
        this.soapConnection.close();
        this.soapConnection = null;
    }

    public String convertToXML(ByteArrayOutputStream input) {
        final String STANDALONE_XML = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>";
        final String NORMAL_XML = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";

        String result = STANDALONE_XML;
        String output = input.toString();
        if(output != null) {
            if(output.startsWith(NORMAL_XML) == true) {
                result = this.replace(output, NORMAL_XML, STANDALONE_XML);
            } else
            if(output.startsWith(STANDALONE_XML) == true) {
                result = output;
            } else {
                result += output;
            }
        }
        return result;
    }

    /**
     *
     * @param str
     * @param pattern
     * @param replace
     * @return
     */
    private String replace(String str, String pattern, String replace) {
        int s = 0;
        int e = 0;
        StringBuffer result = new StringBuffer();

        while ((e = str.indexOf(pattern, s)) >= 0) {
            result.append(str.substring(s, e));
            result.append(replace);
            s = e+pattern.length();
        }
        result.append(str.substring(s));
        return result.toString();
    }


    public ByteArrayOutputStream execute() throws SOAPException, IOException, TransformerException {
        SOAPMessage soapMessage = null;
        SOAPMessage reply = null;
        ByteArrayOutputStream outputStream = null;
        this.initialize();
        soapMessage = this.createMessage(this.soapRequest);
        reply = this.sendMessage(this.destinationURI, soapMessage, this.soapAction);
        this.cleanup();
        outputStream = extractReply(reply);
        return outputStream;
    }

    /**
     * Create a SOAP Message
     *
     * @param messageString
     * @return
     * @throws SOAPException
     * @throws IOException
     */
    private SOAPMessage createMessage(String messageString) throws SOAPException, IOException {
        //Create and populate the message
        SOAPMessage message = messageFactory.createMessage();
        SOAPPart soapPart = message.getSOAPPart();
        StringReader stream = new StringReader (messageString);
        StreamSource preparedMessageSource = new StreamSource(stream);
        soapPart.setContent(preparedMessageSource);
        message.saveChanges();
        OutputStream outputStream = new ByteArrayOutputStream();
        message.writeTo(outputStream);
        //System.out.println("///////////////////////////////////////////////");
        //System.out.println("// -- Request --");
        //System.out.println("///////////////////////////////////////////////");
        //System.out.println(outputStream.toString());
        //System.out.println("///////////////////////////////////////////////");
        return message;
    }

    /**
     * Send a SOAP Message
     *
     * @param destination URL to send to
     * @param message message to send
     * @param theSoapAction
     *
     * @return reply soap message
     * @throws SOAPException
     */
    private SOAPMessage sendMessage(String destination,
                            SOAPMessage message,
                            String theSoapAction) throws SOAPException {
        boolean httpSuccess = true;

        // Add soapAction if not null
        if (theSoapAction != null) {
            MimeHeaders headers = message.getMimeHeaders();
            headers.setHeader("SOAPAction", theSoapAction);
        }

        // Send the message and get a reply
        SOAPMessage reply = null;
        long start = 0;
        start = System.currentTimeMillis();
        reply = this.soapConnection.call(message, destination);
        long end = 0;
        end = System.currentTimeMillis();
        //System.out.println("Duration of call:" + (end - start) + " msec.");
        return reply;
    }

    /**
     * Extract the SOAPMessage content into a ByteArrayOutputStream
     * @param reply
     * @return
     * @throws TransformerException
     * @throws SOAPException
     */
    private ByteArrayOutputStream extractReply(SOAPMessage reply) throws TransformerException, SOAPException {
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

        TransformerFactory transformerFactory = TransformerFactory.newInstance();
        Transformer transformer = transformerFactory.newTransformer();
        if (reply != null) {
            SOAPPart replySOAPPart = reply.getSOAPPart();
            Source sourceContent = replySOAPPart.getContent();
            StreamResult result = new StreamResult(outputStream);
            transformer.transform(sourceContent, result);
        }
        //System.out.println("///////////////////////////////////////////////");
       // System.out.println("// -- Response --");
        //System.out.println("///////////////////////////////////////////////");
        //System.out.println(outputStream.toString());
        //System.out.println("///////////////////////////////////////////////");


        return outputStream;
    }


    /**
     * @return Returns the destinationURI.
     */
    public String getDestinationURI() {
        return this.destinationURI;
    }



    /**
     * @param destinationURI The destinationURI to set.
     */
    public void setDestinationURI(String destinationURI) {
        this.destinationURI = destinationURI;
    }



    /**
     * @return Returns the soapAction.
     */
    public String getSoapAction() {
        return this.soapAction;
    }



    /**
     * @param soapAction The soapAction to set.
     */
    public void setSoapAction(String soapAction) {
        this.soapAction = soapAction;
    }



    /**
     * @return Returns the soapRequest.
     */
    public String getSoapRequest() {
        return this.soapRequest;
    }



    /**
     * @param soapRequest The soapRequest to set.
     */
    public void setSoapRequest(String soapRequest) {
        this.soapRequest = soapRequest;
    }



    /**
     * @param args
     */
    public static void main(String[] args) {
        String destinationURI = null;
        String soapAction = null;
        String soapRequest = null;
        ByteArrayOutputStream output = null;

        destinationURI = "http://api.google.com/search/beta2";
        soapAction = null;
        soapRequest = "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:xsi=\"http://www.w3.org/1999/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/1999/XMLSchema\"><SOAP-ENV:Body><doGoogleSearch SOAP-ENV:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\"><key xsi:type=\"xsd:string\">00000000000000000000000000000000</key><q xsi:type=\"xsd:string\">shrdlu winograd maclisp teletype</q><start xsi:type=\"xsd:int\">0</start><maxResults xsi:type=\"xsd:int\">10</maxResults><filter xsi:type=\"xsd:boolean\">true</filter><restrict xsi:type=\"xsd:string\"></restrict><safeSearch xsi:type=\"xsd:boolean\">false</safeSearch><lr xsi:type=\"xsd:string\"></lr><ie xsi:type=\"xsd:string\">latin1</ie><oe xsi:type=\"xsd:string\">latin1</oe></doGoogleSearch></SOAP-ENV:Body></SOAP-ENV:Envelope>";
        Simulator simulator = new Simulator(destinationURI, soapAction, soapRequest);
        try {
            output = simulator.execute();
            String result = simulator.convertToXML(output);
            System.out.println(result);
        } catch (SOAPException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
            System.out.println("Message is: "+e.getMessage());
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
            System.out.println("Message is: "+e.getMessage());
        } catch (TransformerException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
            System.out.println("Message is: "+e.getMessage());
        }


    }

}
