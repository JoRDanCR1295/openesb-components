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
 * @(#)SMTPNormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/***************************************************************************
 *
 *          Copyright (c) 2005, SeeBeyond Technology Corporation,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ***************************************************************************/
package com.sun.jbi.smtpbc;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.mail.Address;
import javax.mail.Session;
import javax.mail.internet.MimeMessage;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.sax.SAXSource;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.sun.encoder.Encoder;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.impl.NodeListImpl;
import com.sun.jbi.smtpbc.extensions.SMTPConstants;
import com.sun.jbi.smtpbc.extensions.SMTPOperation;
import com.sun.jbi.smtpbc.extensions.SMTPOperationInput;
import com.sun.jbi.smtpbc.util.XmlUtil;



/**
 *
 *
 * @author       Alexander Fung
 * @version      
 *
 */
public class SMTPNormalizer {

	private static final Messages mMessages = Messages.getMessages(SMTPNormalizer.class);	
	private static final Logger mLogger = Messages.getLogger(SMTPNormalizer.class);
	
    public NormalizedMessage normalize (final byte[] byteMessage,
                                        final MessageExchange exchange,
                                        final QName operationName,
                                        final Endpoint endpoint) 
        throws Exception {

    	//mLogger.fine("Message Arrived="+ new String(byteMessage));

        final InputStream is = new ByteArrayInputStream(byteMessage);
        final MimeMessage message =
            new MimeMessage(Session.getDefaultInstance(new Properties()),
                            is);

        // Get metadata on how to convert the MimeMessage into a NormalizedMessage
        final SMTPOperation operation = (SMTPOperation)endpoint.getSMTPOperations().get(operationName);
        final SMTPOperationInput opInput = endpoint.getSMTPOperationInput(operation);
        final Service service  = endpoint.getDefinition().getService(endpoint.getServiceName());
        final Port port = service.getPort(QName.valueOf(endpoint.getEndpointName()).getLocalPart());
        final PortType portType = port.getBinding().getPortType();
        
        // Grab the operation that matches the operationName.  There actually may
        // be more than one operation with the same name (but different input/output)
        // names.  We need to fix this so that we uniquely identify which operation we're
        // going after
        final Iterator it = portType.getOperations().iterator();
        Message wsdlMessage = null;
        while (it.hasNext()) {
            final Operation op = (Operation)it.next();
            if (op.getName().equals(operationName.toString()) ||
                op.getName().equals(operationName.getLocalPart())) {
                wsdlMessage = op.getInput().getMessage();
            }
        }


        final WrapperBuilder builder = HelperFactory.createBuilder();
        builder.initialize(null,
                           wsdlMessage,
                           null);

        // Add the subject part
        final String subject = message.getSubject();
        if (opInput.getSubject() != null) {
            try {
                // Check if it's XML first
                final InputStream is2 = new ByteArrayInputStream(subject.getBytes("US-ASCII"));
                final Document document = createDocumentFromInputStream(is2);

                final Element element = document.getDocumentElement();
                builder.addPart(opInput.getSubject(),
                        element);
            } catch (final Exception ex) {
                // It's not XML, so just do it as a TextNode
                final Document document = createDocument();
                        
                final Text textNode = document.createTextNode(subject);
                builder.addPart(opInput.getSubject(),
                        new NodeListImpl(textNode));
            }
        }

        // Add the message part
        if (opInput.getMessage() != null) {
        	if(SMTPConstants.SMTP_USE_TYPE_ENCODED.equals(opInput.getSmtpUseType())){
        		final Map partMappings = endpoint.getMessagePartEncoderMapping();
                final Encoder encoder = (Encoder) partMappings.get(wsdlMessage.getQName() + opInput.getMessage());
                if (encoder == null) {
                    throw new Exception(SMTPNormalizer.mMessages.getString("SMTPNMR_Invalid_encodingStyle"));
                }
                
                // Decode raw data and add message part
                
                final InputStream is2 = message.getInputStream();
            	int bytesRead = 0;
                final byte[] buffer = new byte[512];
                final StringBuffer contentBuffer = new StringBuffer();
                while((bytesRead = is2.read(buffer,0,512)) != -1){
                	contentBuffer.append(new String(buffer, 0, bytesRead));
                }

                //Remove the CRLF character in the begining of the content if it contains
                final int firstIndex = contentBuffer.indexOf("\r\n");
                if(firstIndex ==0)
                {
                	contentBuffer.delete(0,firstIndex+2);
                }
                
                //The content always comes with two CRLF characters at the end of data. 
                final int lastIndex = contentBuffer.lastIndexOf("\r\n\r\n");
                if(lastIndex !=-1 ){
                	contentBuffer.delete(lastIndex,contentBuffer.length());
                }
                
                final Source source = encoder.decodeFromString(contentBuffer.toString());
                final Element element = getRootElement(source);
                builder.addPart(opInput.getMessage(), element);
        	}
        	else{
	            try {
	
	                // Check if it's XML first
	            	
					final InputStream is2 = message.getInputStream();   
					
					Document document = createDocumentFromInputStream(is2);
	                final Element element = document.getDocumentElement();
	                builder.addPart(opInput.getMessage(),
	                        element);
	            } catch (final Exception ex) {
	                // It's not XML, so just do it as a TextNode
	                final Document document = createDocument();
                        
	            	
	                final InputStream is2 = message.getInputStream();   
	                int bytesRead = 0;
	                final byte[] buffer = new byte[512];
	                final StringBuffer contentBuffer = new StringBuffer();
	                while ((bytesRead = is2.read(buffer, 0, 512)) != -1) {
	                    contentBuffer.append(new String(buffer, 0, bytesRead));
	                }
	                
	                final Text textNode = document.createTextNode(contentBuffer.toString());
	                builder.addPart(opInput.getMessage(),
	                        new NodeListImpl(textNode));
	            }
	        }
        }

        // Add the from part
        String fromList = "";


        if (opInput.getFrom() != null) {
            try {
                final Address[] from = message.getFrom();
                for (Address element : from) {
                    fromList = fromList + element.toString() + " ";
                }
                fromList = fromList.trim();
            } catch (final Exception ex) {
                // What to do here if the from list is bad?
            }

            try {
                // Check if it's XML first
                final InputStream is2 = new ByteArrayInputStream(fromList.getBytes("US-ASCII"));
                final Document document = createDocumentFromInputStream(is2);

                final Element element = document.getDocumentElement();
                builder.addPart(opInput.getFrom(),
                                element);
            } catch (final Exception ex) {
                // It's not XML, so just do it as a TextNode
                final Document document = createDocument();

                final Text textNode = document.createTextNode(fromList);
                builder.addPart(opInput.getFrom(),
                                new NodeListImpl(textNode));
            }
        }
        
        String toList = "";
        if (opInput.getTo() != null) {
            try {
                final Address[] to = message.getRecipients(javax.mail.Message.RecipientType.TO);
                for (Address element : to) {
                	toList = toList + element.toString() + ",";
                }
                toList = toList.trim();
            } catch (final Exception ex) {
                ex.printStackTrace();
            }
            try {
                // Check if it's XML first
                final InputStream is2 = new ByteArrayInputStream(toList.getBytes("US-ASCII"));
                final Document document = createDocumentFromInputStream(is2);

                final Element element = document.getDocumentElement();
                builder.addPart(opInput.getTo(),
                                element);
            } catch (final Exception ex) {
                // It's not XML, so just do it as a TextNode
                final Document document = createDocument();

                final Text textNode = document.createTextNode(toList);
                builder.addPart(opInput.getTo(),
                                new NodeListImpl(textNode));
            }
        }
        
        String ccList = "";
        if (opInput.getCc() != null) {
            try {
                final Address[] cc = message.getRecipients(javax.mail.Message.RecipientType.CC);
                if(cc != null){
                    for (Address element : cc) {
                        ccList = ccList + element.toString() + ",";
                    }
                    ccList = ccList.trim();
                }

            } catch (final Exception ex) {
                ex.printStackTrace();
            }
            try {
                // Check if it's XML first
                final InputStream is2 = new ByteArrayInputStream(ccList.getBytes("US-ASCII"));
                final Document document = createDocumentFromInputStream(is2);

                final Element element = document.getDocumentElement();
                builder.addPart(opInput.getCc(),
                                element);
            } catch (final Exception ex) {
                // It's not XML, so just do it as a TextNode
                final Document document = createDocument();

                final Text textNode = document.createTextNode(ccList);
                builder.addPart(opInput.getCc(),
                                new NodeListImpl(textNode));
            }
        }
        
        final NormalizedMessage retMessage = exchange.createMessage();
        retMessage.setContent(new DOMSource(builder.getResult()));

        return retMessage;
    }
    
    private Element getRootElement(final Source source) throws Exception {
        Element root = null;
        if (source instanceof DOMSource) {
            final Node sourceNode = ((DOMSource) source).getNode();
            if (sourceNode instanceof Element) {
                root = (Element) sourceNode;
            } else if (sourceNode instanceof Document) {
                root = ((Document) sourceNode).getDocumentElement();
            }
        } else if (source instanceof SAXSource) {
            // convert Source to DOMResult
            try {
                final DOMResult result = XmlUtil.transformToDOMResult(source);
                root = ((Document) result.getNode()).getDocumentElement();
            } catch (final Exception e) {
                throw new Exception("SMTPNMR_Failed_ConvertToDOM");
            }
        } else {
            throw new Exception(SMTPNormalizer.mMessages.getString("SMTPNMR_unsupported_source"));
        }

        return root;
    }
    
    private Document createDocumentFromInputStream(InputStream inputStream) 
    	throws IOException, SAXException,ParserConfigurationException{
    	
		byte[] buffer = new byte[512];
		int bytesRead = 0;
        final StringBuffer contentBuffer = new StringBuffer();
        while((bytesRead = inputStream.read(buffer,0,512)) != -1){
        	contentBuffer.append(new String(buffer, 0, bytesRead));
        }
		
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setNamespaceAware(true);
		
		Document document = factory.newDocumentBuilder().parse(new InputSource(new StringReader(contentBuffer.toString())));
		
		return document;

    }
    
    private Document createDocument() throws ParserConfigurationException{
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setNamespaceAware(true);
		Document document = factory.newDocumentBuilder().newDocument();
		return document;

    }
    
}
