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
 * @(#)SMTPDenormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc;


import java.io.StringWriter;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.namespace.QName;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import com.sun.encoder.Encoder;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperParser;
import com.sun.jbi.smtpbc.extensions.MailTo;
import com.sun.jbi.smtpbc.extensions.Mailbox;
import com.sun.jbi.smtpbc.extensions.SMTPConstants;
import com.sun.jbi.smtpbc.extensions.SMTPOperation;
import com.sun.jbi.smtpbc.extensions.SMTPOperationInput;
import com.sun.jbi.smtpbc.extservice.EmailAddress;
import com.sun.jbi.smtpbc.extservice.EmailConfiguration;
import com.sun.jbi.smtpbc.extservice.EmailData;
import com.sun.jbi.smtpbc.extservice.EmailMessage;

/**
 *  SMTPDenormalizer converts Normalized Messages to messages that can be
 *  interpreted by the External Service Provider.  This class utilizes
 *  metadata information from the WSDL to properly convert Normalized
 *  Messages.
 * *
 *  @author       Alexander Fung
 *  @version      
 * *
 */
public class SMTPDenormalizer {
    private static final Messages mMessages =
                Messages.getMessages(SMTPDenormalizer.class);
    
    private static final Logger mLogger =
                Messages.getLogger(SMTPDenormalizer.class);
    
    public EmailData denormalize( final NormalizedMessage normalizedMessage, final QName operationName, final Endpoint endpoint )
    throws Exception {
        final EmailConfiguration config = new EmailConfiguration();
        final EmailMessage message = new EmailMessage();
        
        
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

        // Convert the normalizedMessage into a DOM object
        final DOMResult result = new DOMResult();
        final Source src = normalizedMessage.getContent();
        if (src != null) {
            final TransformerFactory fact = TransformerFactory.newInstance();
            final Transformer transformer = fact.newTransformer();
            transformer.transform( src, result );
        }
        final Node node = result.getNode();
        Document normalizedDoc = null;
        if (node instanceof Document) {
            normalizedDoc = (Document) node;
        } else {
            normalizedDoc = ((Element) node).getOwnerDocument();
        }
        // Use the WrapperParser to help in parsing out the Parts
        final WrapperParser wrapperParser = HelperFactory.createParser();
        wrapperParser.parse( normalizedDoc, endpoint.getDefinition() );
        // Go through our SMTP extensibility elements to see what parts
        // go where.
        SMTPOperation operation = null;
        if(operation == null)
            operation=(SMTPOperation) endpoint.getSMTPOperations().get( operationName );
        if(operation==null){
            operation=(SMTPOperation) endpoint.getSMTPOperations().get( operationName.valueOf(operationName.getLocalPart()));
        }
        if (operation == null) {
            throw new Exception(SMTPDenormalizer.mMessages.getString("SMTPDNMR_Invalid_opname",
                                                    new Object[] {operationName,
                                                                  endpoint.getEndpointName()}));
        }
        // Am I denormalizing the input or output message?  Does it
        // depend on what MEP I'm using and what the Endpoint type is? For
        // now, I'm going to send out the input message, but I'll have to
        // revisit this.
        final SMTPOperationInput opInput = endpoint.getSMTPOperationInput( operation );
        if (opInput == null) {
            throw new Exception(SMTPDenormalizer.mMessages.getString("SMTPDNMR_No_Binding_Input",
                                                    operationName));
        }
        // Now fill in the EmailMessage object
        // First, fill in the message body
        // If the mailto URL has body in its URL, that body takes precedence
        final MailTo location = endpoint.getSMTPAddress().getLocation();
        if(location.getBody() != null && !"".equals(location.getBody()))
        {
            message.setMsgText(location.getBody());
        }
        else
        {
            final String messagePart = opInput.getMessage();
            if (messagePart != null) {
        	if(SMTPConstants.SMTP_USE_TYPE_ENCODED.equals(opInput.getSmtpUseType())){
	    		final Map partMappings = endpoint.getMessagePartEncoderMapping();
	    		final Encoder encoder = (Encoder)partMappings.get(wsdlMessage.getQName() + opInput.getMessage());
	            if (encoder == null) {
	                throw new Exception(SMTPDenormalizer.mMessages.getString("SMTPDNMR_Invalid_encodingStyle"));
	            }
	            // Encode DOM source to raw data format
	            final NodeList n1 = wrapperParser.getPartNodes(messagePart);
	            final Node aNode = n1.item(0);
	            
	            final Source source = new DOMSource(aNode);
	            final String messageText = encoder.encodeToString(source);
	            message.setMsgText(messageText);
                }else{
                    final NodeList nl = wrapperParser.getPartNodes( messagePart );
                    message.setMsgText( transformPart( nl ) );
                }
            }
        }

        // Fill in the message subject
        if(location.getSubject() != null && !"".equals(location.getSubject()))
        {
            message.setSubject(location.getSubject());
        }else
        {
            final String subjectPart = opInput.getSubject();
            if (subjectPart != null) {
                final NodeList nl = wrapperParser.getPartNodes( subjectPart );
                message.setSubject( transformPart( nl ) );
            }
        }
        // Fill in the from field
        final String fromPart = opInput.getFrom();
        String fromAddress = null;
        if (fromPart != null) {
            final NodeList nl = wrapperParser.getPartNodes( fromPart );
            if(nl.item(0) != null){
                Mailbox fromMailbox = new Mailbox(nl.item(0).getTextContent());
                fromAddress = fromMailbox.getNormalizedAddressSpec();
                message.getFrom().setAddress(fromAddress);
            }
        }
        // Fill in the To: field.

        final String toPart = opInput.getTo();
        String toAddresses = null;
        if(toPart != null){
            
        	final NodeList n1 = wrapperParser.getPartNodes(toPart);
        	toAddresses = transformPart( n1 );
                String [] toEmailAddresses = null;
                
                if(toAddresses.indexOf(',') != -1){
                    toEmailAddresses = toAddresses.split(",");
                }else if(toAddresses.indexOf(';') != -1){
                    toEmailAddresses = toAddresses.split(";");
                }else{
                    message.addTo(new EmailAddress(toAddresses,""));
                    // Set the "from" field
                    if (fromAddress == null) {
                        message.getFrom().setAddress( toAddresses );
                    } else {
                        message.getFrom().setAddress( fromAddress );
                    }
                   
                }
                
                if(toEmailAddresses != null){
                    int len = toEmailAddresses.length;
                    for(int i=0 ; i < len ; i++){
                        message.addTo(new EmailAddress(toEmailAddresses[i],""));
                        
                        // Set the "from" field
                        if (fromAddress == null) {
                            message.getFrom().setAddress( toEmailAddresses[i] );
                        } else {
                            message.getFrom().setAddress( fromAddress );
                        }
                    }
                }

        }
        
        final String ccPart = opInput.getCc();
        String ccAddresses = null;
        if(ccPart != null){
        	final NodeList n1 = wrapperParser.getPartNodes(ccPart);
        	ccAddresses = transformPart( n1 );
                String [] ccEmailAddresses = null;
                
                if(ccAddresses.indexOf(',') != -1){
                    ccEmailAddresses = ccAddresses.split(",");
                }else if(toAddresses.indexOf(';') != -1){
                    ccEmailAddresses = ccAddresses.split(";");
                }else{
                    message.addCc(new EmailAddress(ccAddresses,""));
                }
                
                if(ccEmailAddresses != null){
                    int len = ccEmailAddresses.length;
                    for(int i=0 ; i < len ; i++){
                        message.addCc(new EmailAddress(ccEmailAddresses[i],""));
                    }
                }
        }
        
        final String bccPart = opInput.getBcc();
        String bccAddresses = null;
        if(bccPart != null){
        	final NodeList n1 = wrapperParser.getPartNodes(bccPart);
        	bccAddresses = transformPart( n1 );
                
                String [] bccEmailAddresses = null;
                
                if(bccAddresses.indexOf(',') != -1){
                    bccEmailAddresses = bccAddresses.split(",");
                }else if(toAddresses.indexOf(';') != -1){
                    bccEmailAddresses = bccAddresses.split(";");
                }else{
                    message.addBcc(new EmailAddress(bccAddresses,""));
                }
                
                if(bccEmailAddresses != null){
                    int len = bccEmailAddresses.length;
                    for(int i=0 ; i < len ; i++){
                        message.addBcc(new EmailAddress(bccEmailAddresses[i],""));
                    }
                }
                
        }        

        if (SMTPDenormalizer.mLogger.isLoggable(Level.INFO)) {
            SMTPDenormalizer.mLogger.log(Level.INFO, "SMTPDNMR_Location_From_WSDL", location);
        }
        if ((toPart == null && ccPart == null && bccPart == null) && location != null) {
            final Iterator mailboxes = location.getMailbox().iterator();
            while (mailboxes.hasNext()) {
                final Mailbox box = (Mailbox) mailboxes.next();
                final String emailAddress = box.getNormalizedAddressSpec();
                if (SMTPDenormalizer.mLogger.isLoggable(Level.INFO)) {
                    SMTPDenormalizer.mLogger.log(Level.INFO,
                                "SMTPDNMR_Email_Addr_SendTo",
                                emailAddress );
                }
                final EmailAddress to = new EmailAddress( emailAddress, "" );
                message.addTo( to );
                // Set the "from" field
                if (fromAddress == null) {
                    message.getFrom().setAddress( emailAddress );
                } else {
                    message.getFrom().setAddress( fromAddress );
                }
            }
            final Iterator ccmailboxes = location.getCCMailbox().iterator();
            while (ccmailboxes.hasNext()) {
                final Mailbox box = (Mailbox) ccmailboxes.next();
                final String emailAddress = box.getNormalizedAddressSpec();
                if (SMTPDenormalizer.mLogger.isLoggable(Level.INFO)) {
                    SMTPDenormalizer.mLogger.log(Level.INFO,
                                "SMTPDNMR_Email_Addr_CCTo",
                                emailAddress);
                }
                final EmailAddress cc = new EmailAddress( emailAddress, "" );
                message.addCc( cc );
            }
            final Iterator bccmailboxes = location.getBCCMailbox().iterator();
            while (bccmailboxes.hasNext()) {
                final Mailbox box = (Mailbox) bccmailboxes.next();
                final String emailAddress = box.getNormalizedAddressSpec();
                if (SMTPDenormalizer.mLogger.isLoggable(Level.INFO)) {
                    SMTPDenormalizer.mLogger.log(Level.INFO,
                                "SMTPDNMR_Email_Addr_BCCTo",
                                emailAddress);
                }
                final EmailAddress bcc = new EmailAddress( emailAddress, "" );
                message.addBcc( bcc );
            }
        } 
        if(toPart == null && ccPart == null && bccPart == null && location == null)
        {
            throw new Exception(SMTPDenormalizer.mMessages.getString("SMTPDNMR_Location_Required"));
        }
        
        // This is also broken.  When we have multiple recipients, we
        // can't just set it to one!
        /*
          mLogger.info("Setting the smtp server to be the same as the " +
          "destination of the email address");
          String smtpServer =
          emailAddress.substring(emailAddress.indexOf("@") + 1,
          emailAddress.length());
         */
        // fix the above we have new attribute smtpserv        
        final String smtpServer = endpoint.getSMTPAddress().getSMTPServer();
        if ((smtpServer == null) || (smtpServer.length() == 0)) {
            throw new Exception(SMTPDenormalizer.mMessages.getString("SMTPDNMR_Server_Required"));
        }
        if (SMTPDenormalizer.mLogger.isLoggable(Level.INFO)) {
            SMTPDenormalizer.mLogger.log(Level.INFO, "SMTP_Server", smtpServer );
        }
        config.setHostSend(smtpServer);
        
        final int smtpPort = endpoint.getSMTPAddress().getSMTPPort();
        
        Integer smtpPortInt = new Integer(smtpPort);
        if(smtpPortInt != null ) 
          config.setPortSend(smtpPort);
               
        
        final boolean useSSL = endpoint.getSMTPAddress().getUseSSL();
        
        if (SMTPDenormalizer.mLogger.isLoggable(Level.INFO)) {
            SMTPDenormalizer.mLogger.log(Level.INFO, "SMTP_UseSSL", useSSL );
        }
        config.setUseSSL(useSSL);
        if(useSSL){
           if(smtpPortInt == 25)
            config.setPortSendSSL(465);
        }
        final String userName = endpoint.getSMTPAddress().getUserName();
        config.setUserSend(userName);
        final String password = endpoint.getSMTPAddress().getPassword();
        config.setPasswordSend(password);
        return new EmailData( config, message );
    }
    
    public String transformPart( final NodeList parts )
    throws Exception {
    	if(parts.item(0) == null){
    		return "";
    	}
        final StringWriter out = new StringWriter();
        final TransformerFactory tFactory = TransformerFactory.newInstance();
        final Transformer trans = tFactory.newTransformer();
        trans.setOutputProperty( OutputKeys.ENCODING, "UTF-8" );
        trans.setOutputProperty( OutputKeys.INDENT, "yes" );
        trans.setOutputProperty( OutputKeys.METHOD, "xml" );
        trans.setOutputProperty( OutputKeys.OMIT_XML_DECLARATION, "yes" );
        // Just output the first node in the list for now.
        final Source source = new DOMSource( parts.item( 0 ) );
        final StreamResult result = new StreamResult( out );
        trans.transform( source, result );
        out.flush();
        out.close();
        return out.toString();
    }
    
}
