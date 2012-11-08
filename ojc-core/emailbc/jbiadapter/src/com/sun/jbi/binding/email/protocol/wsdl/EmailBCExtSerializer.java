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
 * @(#)EmailBCExtSerializer.java 
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.binding.email.protocol.wsdl;

import com.sun.jbi.binding.email.protocol.ApplicationVariableNotDefinedException;
import java.io.PrintWriter;
import java.io.Serializable;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.xml.namespace.QName;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.ibm.wsdl.Constants;
import com.ibm.wsdl.util.xml.DOMUtils;
import com.sun.jbi.binding.email.protocol.EmailBCConstants;
import com.sun.jbi.binding.email.protocol.EmailUtil;
import com.sun.jbi.common.qos.config.ComponentConfig;

/**
 * @author Harry Liu (Harry.Liu@sun.com)
 */
public class EmailBCExtSerializer
    implements ExtensionSerializer, ExtensionDeserializer, Serializable {
    private static final long serialVersionUID = 1L;    

    private ComponentConfig mConfig;

    public EmailBCExtSerializer(ComponentConfig config) {
        super();
        this.mConfig = config;
    }

    /**
     * @see javax.wsdl.extensions.ExtensionSerializer#marshall(java.lang.Class, javax.xml.namespace.QName, javax.wsdl.extensions.ExtensibilityElement, java.io.PrintWriter, javax.wsdl.Definition, javax.wsdl.extensions.ExtensionRegistry)
     */
    public void marshall(Class parentType, QName elementType, ExtensibilityElement extension,
            PrintWriter pw, Definition def, ExtensionRegistry extReg)
        throws WSDLException {
        if (extension == null) {
            return;
        }
        if (extension instanceof IMAPBinding) {
            pw.print("      <email:IMAPbinding");
            final Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }
            pw.println("/>");
        }else if (extension instanceof POP3Binding){
            pw.print("      <email:POP3binding");
            final Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }
            pw.println("/>");
        }else if (extension instanceof SMTPBinding){
            pw.print("      <email:SMTPbinding");
            final Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }
            pw.println("/>");
        } else if (extension instanceof IMAPOperation) {
            pw.print("      <email:IMAPoperation");
            final Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }
            pw.println("/>");
        }else if (extension instanceof POP3Operation) {
            pw.print("      <email:POP3operation");
            final Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }
            pw.println("/>");
        }else if (extension instanceof SMTPOperation) {
            pw.print("      <email:SMTPoperation");
            final Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }
            pw.println("/>");
        } else if (extension instanceof IMAPOperationInput) {
        } else if (extension instanceof POP3OperationInput) {
        } else if (extension instanceof SMTPOperationInput) {

        } else if (extension instanceof IMAPAddress) {
            final IMAPAddress imapAddress = (IMAPAddress) extension;
            pw.print("      <email:IMAPaddress");
            final Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }

            final String emailServer = imapAddress.getHostName();
            if (emailServer != null)
            {
                DOMUtils.printAttribute(IMAPAddress.ATTR_EMAIL_HOSTNAME, emailServer, pw);
            }

            final int port = imapAddress.getPort();
            if( port != 0)
            {
                DOMUtils.printAttribute(IMAPAddress.ATTR_EMAIL_PORT,String.valueOf(port),pw);
            }

            final String userName = imapAddress.getUserName();
            if( userName != null)
            {
                DOMUtils.printAttribute(IMAPAddress.ATTR_EMAIL_USERNAME,userName,pw);
            }

            final String password = imapAddress.getPassword();
            if( password != null)
            {
                DOMUtils.printAttribute(IMAPAddress.ATTR_EMAIL_PASSWORD,password,pw);
            }            

            final Boolean useSSL = imapAddress.getUseSSL();
            if( useSSL != null)
            {
                DOMUtils.printAttribute(IMAPAddress.ATTR_EMAIL_USESSL,useSSL.toString(),pw);
            }

            final String mailFolder = imapAddress.getMailFolder();
            if( mailFolder != null)
            {
                DOMUtils.printAttribute(IMAPAddress.ATTR_IMAP_MAIL_FOLDER,mailFolder,pw);
            }            

            final String msgAckMode = imapAddress.getMessageAckMode();
            if( msgAckMode != null)
            {
                DOMUtils.printAttribute(IMAPAddress.ATTR_IMAP_MESSAGE_ACK_MODE,msgAckMode,pw);
            }     

            final int maxMessageCount = imapAddress.getMaxMessageCount();
            if( maxMessageCount != 0)
            {
                DOMUtils.printAttribute(IMAPAddress.ATTR_IMAP_MAX_MESSAGE_COUNT,String.valueOf(maxMessageCount),pw);
            }                 


            pw.println("/>");

        } else if (extension instanceof POP3Address) {
            final POP3Address pop3Address = (POP3Address) extension;
            pw.print("      <email:POP3address");
            final Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }

            final String emailServer = pop3Address.getHostName();
            if (emailServer != null)
            {
                DOMUtils.printAttribute(POP3Address.ATTR_EMAIL_HOSTNAME, emailServer, pw);
            }

            final int port = pop3Address.getPort();
            if( port != 0)
            {
                DOMUtils.printAttribute(POP3Address.ATTR_EMAIL_PORT,String.valueOf(port),pw);
            }

            final String userName = pop3Address.getUserName();
            if( userName != null)
            {
                DOMUtils.printAttribute(POP3Address.ATTR_EMAIL_USERNAME,userName,pw);
            }

            final String password = pop3Address.getPassword();
            if( password != null)
            {
                DOMUtils.printAttribute(POP3Address.ATTR_EMAIL_PASSWORD,password,pw);
            }            

            final Boolean useSSL = pop3Address.getUseSSL();
            if( useSSL != null)
            {
                DOMUtils.printAttribute(POP3Address.ATTR_EMAIL_USESSL,useSSL.toString(),pw);
            }

            final String mailFolder = pop3Address.getMailFolder();
            if( mailFolder != null)
            {
                DOMUtils.printAttribute(POP3Address.ATTR_POP3_MAIL_FOLDER,mailFolder,pw);
            }            

            final String msgAckMode = pop3Address.getMessageAckMode();
            if( msgAckMode != null)
            {
                DOMUtils.printAttribute(POP3Address.ATTR_POP3_MESSAGE_ACK_MODE,msgAckMode,pw);
            }     

            final int maxMessageCount = pop3Address.getMaxMessageCount();
            if( maxMessageCount != 0)
            {
                DOMUtils.printAttribute(POP3Address.ATTR_POP3_MAX_MESSAGE_COUNT,String.valueOf(maxMessageCount),pw);
            }                 


            pw.println("/>");
        } else if (extension instanceof SMTPAddress) {
            final SMTPAddress smtpAddress = (SMTPAddress) extension;
            pw.print("      <email:SMTPaddress");
            final Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }

            final String location = smtpAddress.getLocation();
            if (location != null) {
                DOMUtils.printAttribute(SMTPAddress.ATTR_SMTP_LOCATION, location, pw);
            }
            
            final String emailServer = smtpAddress.getHostName();
            if (emailServer != null)
            {
                DOMUtils.printAttribute(SMTPAddress.ATTR_EMAIL_HOSTNAME, emailServer, pw);
            }

            final int port = smtpAddress.getPort();
            if( port != 0)
            {
                DOMUtils.printAttribute(SMTPAddress.ATTR_EMAIL_PORT,String.valueOf(port),pw);
            }

            final String userName = smtpAddress.getUserName();
            if( userName != null)
            {
                DOMUtils.printAttribute(SMTPAddress.ATTR_EMAIL_USERNAME,userName,pw);
            }

            final String password = smtpAddress.getPassword();
            if( password != null)
            {
                DOMUtils.printAttribute(SMTPAddress.ATTR_EMAIL_PASSWORD,password,pw);
            }            

            final Boolean useSSL = smtpAddress.getUseSSL();
            if( useSSL != null)
            {
                DOMUtils.printAttribute(SMTPAddress.ATTR_EMAIL_USESSL,useSSL.toString(),pw);
            }


            pw.println("/>");
        }

    }

    /**
     * @see javax.wsdl.extensions.ExtensionDeserializer#unmarshall(java.lang.Class, javax.xml.namespace.QName, org.w3c.dom.Element, javax.wsdl.Definition, javax.wsdl.extensions.ExtensionRegistry)
     */
    public ExtensibilityElement unmarshall(final Class parentType, final QName elementType,
            final Element element, final Definition def, final ExtensionRegistry extReg)
        throws WSDLException {

        ExtensibilityElement returnValue = null;

        try {
            if (IMAPBinding.QNAME_IMAP_BINDING.equals(elementType)) {
                final IMAPBinding imapBinding = new IMAPBinding();
                returnValue = imapBinding;
    
            } else if (POP3Binding.QNAME_POP3_BINDING.equals(elementType)) {
                final POP3Binding pop3Binding = new POP3Binding();
    
                returnValue = pop3Binding;
            } else if (SMTPBinding.QNAME_SMTP_BINDING.equals(elementType)) {
                final SMTPBinding smtpBinding = new SMTPBinding();
    
                returnValue = smtpBinding;
    
            } else if (IMAPOperation.QNAME_IMAP_OPERATION.equals(elementType)) {
    
                final IMAPOperation imapOperation = new IMAPOperation();
                returnValue = imapOperation;
    
            } else if (POP3Operation.QNAME_POP3_OPERATION.equals(elementType)) {
    
                final POP3Operation pop3Operation = new POP3Operation();
                returnValue = pop3Operation;
            } else if (SMTPOperation.QNAME_SMTP_OPERATION.equals(elementType)) {
    
                final SMTPOperation smtpOperation = new SMTPOperation();
                returnValue = smtpOperation;
    
            } else if (IMAPOperationInput.QNAME_IMAP_OPERATION_INPUT.equals(elementType)) {
    
                final IMAPOperationInput imapOperationInput = new IMAPOperationInput();
    
                String message = DOMUtils.getAttribute(element, IMAPOperationInput.ATTR_EMAIL_BODY);
                //message = EmailUtil.resolveAppVarsValue(message);
                if (message != null) {
                    imapOperationInput.setMessage(message);
                }
    
                String subject = DOMUtils.getAttribute(element, IMAPOperationInput.ATTR_EMAIL_SUBJECT);
                //subject = EmailUtil.resolveAppVarsValue(subject);
                if (subject != null) {
                    imapOperationInput.setSubject(subject);
                }
    
                String from = DOMUtils.getAttribute(element, IMAPOperationInput.ATTR_EMAIL_FROM);
                //from = EmailUtil.resolveAppVarsValue(from);
                if (from != null) {
                    imapOperationInput.setFrom(from);
                }
    
                String to = DOMUtils.getAttribute(element, IMAPOperationInput.ATTR_EMAIL_TO);
                //to = EmailUtil.resolveAppVarsValue(to);
                if (to != null) {
                    imapOperationInput.setTo(to);
                }
    
                String cc = DOMUtils.getAttribute(element, IMAPOperationInput.ATTR_EMAIL_CC);
                //cc = EmailUtil.resolveAppVarsValue(cc);
                if (cc != null) {
                    imapOperationInput.setCc(cc);
                }
                
                String bcc = DOMUtils.getAttribute(element, IMAPOperationInput.ATTR_EMAIL_BCC);
                //bcc = EmailUtil.resolveAppVarsValue(bcc);
                if (bcc != null) {
                    imapOperationInput.setBcc(bcc);
                }            
    
                String newsgroups = DOMUtils.getAttribute(element, IMAPOperationInput.ATTR_EMAIL_NEWSGROUPS);
                //newsgroup = EmailUtil.resolveAppVarsValue(newsgroup);
                if (newsgroups != null) {
                    imapOperationInput.setNewsgroups(newsgroups);
                }            
    
                String handleNMAttachments = DOMUtils.getAttribute(element, IMAPOperationInput.ATTR_EMAIL_HANDLE_NM_ATTACHMENTS);
                handleNMAttachments = EmailUtil.resolveAppVarsValue(handleNMAttachments, mConfig);
                if (handleNMAttachments != null) {
                    imapOperationInput.setHandleNMAttachments(Boolean.parseBoolean(handleNMAttachments));
                }
    
                String saveAttachmentsToDir = DOMUtils.getAttribute(element, IMAPOperationInput.ATTR_IMAP_SAVE_ATTACHMENTS_TO_DIR);
                //saveAttachmentsToDir = EmailUtil.resolveAppVarsValue(saveAttachmentsToDir);
                if (saveAttachmentsToDir != null) {
                    imapOperationInput.setSaveAttachmentsToDir(saveAttachmentsToDir);
                }            
    
                // process WSDL attachments
                // <email:IMAPattachment partName="..." saveToFile="...">
                NodeList nodeList = element.getElementsByTagNameNS(
                        IMAPAttachment.QNAME_IMAP_ATTACHMENT.getNamespaceURI(),
                        IMAPAttachment.QNAME_IMAP_ATTACHMENT.getLocalPart());
                if (null != nodeList && 0 < nodeList.getLength()) {
                    IMAPAttachment[] attachments = new IMAPAttachment[nodeList.getLength()];
                    Element attachmentElement = null;
                    IMAPAttachment attachment = null;
                    for (int i = 0; i < nodeList.getLength(); i++) {
                        attachmentElement = (Element) nodeList.item(i);
                        attachment = new IMAPAttachment();
                        
                        String partName = DOMUtils.getAttribute(attachmentElement, IMAPAttachment.ATTR_IMAP_ATTACHMENT_PARTNAME);
                        //partName = EmailUtil.resolveAppVarsValue(partName);
                        if (partName != null) {
                            attachment.setPartName(partName);
                        }
            
                        String saveToFile = DOMUtils.getAttribute(attachmentElement, IMAPAttachment.ATTR_IMAP_ATTACHMENT_SAVE_TO_FILE);
                        //saveToFile = EmailUtil.resolveAppVarsValue(saveToFile);
                        if (saveToFile != null) {
                            attachment.setSaveToFile(saveToFile);
                        }
                        
                        attachments[i] = attachment;
                    } // end of for
                    
                    imapOperationInput.setAttachments(attachments);
                } // end of if
                
                returnValue = imapOperationInput;
            } else if (POP3OperationInput.QNAME_POP3_OPERATION_INPUT.equals(elementType)) {
    
                final POP3OperationInput pop3OperationInput = new POP3OperationInput();
    
                String message = DOMUtils.getAttribute(element, POP3OperationInput.ATTR_EMAIL_BODY);
                //message = EmailUtil.resolveAppVarsValue(message);
                if (message != null) {
                    pop3OperationInput.setMessage(message);
                }
    
                String subject = DOMUtils.getAttribute(element, POP3OperationInput.ATTR_EMAIL_SUBJECT);
                //subject = EmailUtil.resolveAppVarsValue(subject);
                if (subject != null) {
                    pop3OperationInput.setSubject(subject);
                }
    
                String from = DOMUtils.getAttribute(element, POP3OperationInput.ATTR_EMAIL_FROM);
                //from = EmailUtil.resolveAppVarsValue(from);
                if (from != null) {
                    pop3OperationInput.setFrom(from);
                }
    
                String to = DOMUtils.getAttribute(element, POP3OperationInput.ATTR_EMAIL_TO);
                //to = EmailUtil.resolveAppVarsValue(to);
                if (to != null) {
                    pop3OperationInput.setTo(to);
                }
    
                String cc = DOMUtils.getAttribute(element, POP3OperationInput.ATTR_EMAIL_CC);
                //cc = EmailUtil.resolveAppVarsValue(cc);
                if (cc != null) {
                    pop3OperationInput.setCc(cc);
                }
                
                String bcc = DOMUtils.getAttribute(element, POP3OperationInput.ATTR_EMAIL_BCC);
                //bcc = EmailUtil.resolveAppVarsValue(bcc);
                if (bcc != null) {
                    pop3OperationInput.setBcc(bcc);
                }            
    
                String newsgroups = DOMUtils.getAttribute(element, POP3OperationInput.ATTR_EMAIL_NEWSGROUPS);
                //newsgroup = EmailUtil.resolveAppVarsValue(newsgroup);
                if (newsgroups != null) {
                    pop3OperationInput.setNewsgroups(newsgroups);
                }            
    
                String handleNMAttachments = DOMUtils.getAttribute(element, POP3OperationInput.ATTR_EMAIL_HANDLE_NM_ATTACHMENTS);
                handleNMAttachments = EmailUtil.resolveAppVarsValue(handleNMAttachments, mConfig);
                if (handleNMAttachments != null) {
                    pop3OperationInput.setHandleNMAttachments(Boolean.parseBoolean(handleNMAttachments));
                }
    
                String saveAttachmentsToDir = DOMUtils.getAttribute(element, POP3OperationInput.ATTR_POP3_SAVE_ATTACHMENTS_TO_DIR);
                //saveAttachmentsToDir = EmailUtil.resolveAppVarsValue(saveAttachmentsToDir);
                if (saveAttachmentsToDir != null) {
                    pop3OperationInput.setSaveAttachmentsToDir(saveAttachmentsToDir);
                }            
    
                // process WSDL attachments
                // <email:POP3attachment partName="..." saveToFile="...">
                NodeList nodeList = element.getElementsByTagNameNS(
                        POP3Attachment.QNAME_POP3_ATTACHMENT.getNamespaceURI(),
                        POP3Attachment.QNAME_POP3_ATTACHMENT.getLocalPart());
                if (null != nodeList && 0 < nodeList.getLength()) {
                    POP3Attachment[] attachments = new POP3Attachment[nodeList.getLength()];
                    Element attachmentElement = null;
                    POP3Attachment attachment = null;
                    for (int i = 0; i < nodeList.getLength(); i++) {
                        attachmentElement = (Element) nodeList.item(i);
                        attachment = new POP3Attachment();
                        
                        String partName = DOMUtils.getAttribute(attachmentElement, POP3Attachment.ATTR_POP3_ATTACHMENT_PARTNAME);
                        //partName = EmailUtil.resolveAppVarsValue(partName);
                        if (partName != null) {
                            attachment.setPartName(partName);
                        }
            
                        String saveToFile = DOMUtils.getAttribute(attachmentElement, POP3Attachment.ATTR_POP3_ATTACHMENT_SAVE_TO_FILE);
                        //saveToFile = EmailUtil.resolveAppVarsValue(saveToFile);
                        if (saveToFile != null) {
                            attachment.setSaveToFile(saveToFile);
                        }
                        
                        attachments[i] = attachment;
                    } // end of for
                    
                    pop3OperationInput.setAttachments(attachments);
                } // end of if
                
                returnValue = pop3OperationInput;
    
            } else if (SMTPOperationInput.QNAME_SMTP_OPERATION_INPUT.equals(elementType)) {
    
                final SMTPOperationInput smtpOperationInput = new SMTPOperationInput();
    
                String message = DOMUtils.getAttribute(element, SMTPOperationInput.ATTR_EMAIL_BODY);
                //message = EmailUtil.resolveAppVarsValue(message);
                if (message != null) {
                    smtpOperationInput.setMessage(message);
                }
    
                String subject = DOMUtils.getAttribute(element, SMTPOperationInput.ATTR_EMAIL_SUBJECT);
                //subject = EmailUtil.resolveAppVarsValue(subject);
                if (subject != null) {
                    smtpOperationInput.setSubject(subject);
                }
    
                String from = DOMUtils.getAttribute(element, SMTPOperationInput.ATTR_EMAIL_FROM);
                //from = EmailUtil.resolveAppVarsValue(from);
                if (from != null) {
                    smtpOperationInput.setFrom(from);
                }
    
                String to = DOMUtils.getAttribute(element, SMTPOperationInput.ATTR_EMAIL_TO);
                //to = EmailUtil.resolveAppVarsValue(to);
                if (to != null) {
                    smtpOperationInput.setTo(to);
                }
    
                String cc = DOMUtils.getAttribute(element, SMTPOperationInput.ATTR_EMAIL_CC);
                //cc = EmailUtil.resolveAppVarsValue(cc);
                if (cc != null) {
                    smtpOperationInput.setCc(cc);
                }
                
                String bcc = DOMUtils.getAttribute(element, SMTPOperationInput.ATTR_EMAIL_BCC);
                //bcc = EmailUtil.resolveAppVarsValue(bcc);
                if (bcc != null) {
                    smtpOperationInput.setBcc(bcc);
                }     
    
                String newsgroups = DOMUtils.getAttribute(element, SMTPOperationInput.ATTR_EMAIL_NEWSGROUPS);
                //newsgroup = EmailUtil.resolveAppVarsValue(newsgroup);
                if (newsgroups != null) {
                    smtpOperationInput.setNewsgroups(newsgroups);
                }            
    
                String charset = DOMUtils.getAttribute(element, SMTPOperationInput.ATTR_SMTP_CHARSET);
                //charset = EmailUtil.resolveAppVarsValue(charset);
                if (charset != null){
                    smtpOperationInput.setCharset(charset);
                }
                
                String useType = DOMUtils.getAttribute(element, SMTPOperationInput.ATTR_SMTP_USE_TYPE);
                //useType = EmailUtil.resolveAppVarsValue(useType);
                if (useType != null){
                    smtpOperationInput.setSmtpUseType(useType);
                }
    
                String encodingStyle = DOMUtils.getAttribute(element, SMTPOperationInput.ATTR_SMTP_ENCODING_STYLE);
                //encodingStyle = EmailUtil.resolveAppVarsValue(encodingStyle);
                if (encodingStyle != null) {
                    smtpOperationInput.setEncodingStyle(encodingStyle);
                }

                String handleNMAttachments = DOMUtils.getAttribute(element, SMTPOperationInput.ATTR_EMAIL_HANDLE_NM_ATTACHMENTS);
                handleNMAttachments = EmailUtil.resolveAppVarsValue(handleNMAttachments, mConfig);
                if (handleNMAttachments != null) {
                    smtpOperationInput.setHandleNMAttachments(Boolean.parseBoolean(handleNMAttachments));
                }
    
                String sendOption = DOMUtils.getAttribute(element, SMTPOperationInput.ATTR_SMTP_SEND_OPTION);
                //sendOption = EmailUtil.resolveAppVarsValue(sendOption);
                if (sendOption != null) {
                    smtpOperationInput.setSendOption(sendOption);
                }

                String embedImagesInHtml = DOMUtils.getAttribute(element, SMTPOperationInput.ATTR_SMTP_EMBED_IMAGES_IN_HTML);
                embedImagesInHtml = EmailUtil.resolveAppVarsValue(embedImagesInHtml, mConfig);
                if (embedImagesInHtml != null) {
                    smtpOperationInput.setEmbedImagesInHtml(Boolean.parseBoolean(embedImagesInHtml));
                }
    
                // process WSDL attachments
                // <email:SMTPattachment disposition="" contentType="..." partName="..." readFromFile="...">
                NodeList nodeList = element.getElementsByTagNameNS(
                        SMTPAttachment.QNAME_SMTP_ATTACHMENT.getNamespaceURI(),
                        SMTPAttachment.QNAME_SMTP_ATTACHMENT.getLocalPart());
                if (null != nodeList && 0 < nodeList.getLength()) {
                    SMTPAttachment[] attachments = new SMTPAttachment[nodeList.getLength()];
                    Element attachmentElement = null;
                    SMTPAttachment attachment = null;
                    for (int i = 0; i < nodeList.getLength(); i++) {
                        attachmentElement = (Element) nodeList.item(i);
                        attachment = new SMTPAttachment();
                        
                        String disposition = DOMUtils.getAttribute(attachmentElement, SMTPAttachment.ATTR_SMTP_ATTACHMENT_DISPOSITION);
                        //disposition = EmailUtil.resolveAppVarsValue(disposition);
                        if (disposition != null) {
                            attachment.setDisposition(disposition);
                        }
            
                        String transferEncoding = DOMUtils.getAttribute(attachmentElement, SMTPAttachment.ATTR_SMTP_ATTACHMENT_TRANSFER_ENCODING);
                        //transferEncoding = EmailUtil.resolveAppVarsValue(transferEncoding);
                        if (transferEncoding != null) {
                            attachment.setTransferEncoding(transferEncoding);
                        }
            
                        String contentType = DOMUtils.getAttribute(attachmentElement, SMTPAttachment.ATTR_SMTP_ATTACHMENT_CONTENT_TYPE);
                        //contentType = EmailUtil.resolveAppVarsValue(contentType);
                        if (contentType != null) {
                            attachment.setContentType(contentType);
                        }
            
                        String partName = DOMUtils.getAttribute(attachmentElement, SMTPAttachment.ATTR_SMTP_ATTACHMENT_CONTENT_PART);
                        //partName = EmailUtil.resolveAppVarsValue(partName);
                        if (partName != null) {
                            attachment.setAttachmentContentPart(partName);
                        }

                        String attachmentFileNamePart = DOMUtils.getAttribute(attachmentElement, SMTPAttachment.ATTR_SMTP_ATTACHMENT_FILE_NAME_PART);
                        if (attachmentFileNamePart != null) {
                            attachment.setAttachmentFileNamePart(attachmentFileNamePart);
                        }
            
                        String readFromFile = DOMUtils.getAttribute(attachmentElement, SMTPAttachment.ATTR_SMTP_ATTACHMENT_READ_FROM_FILE);
                        //readFromFile = EmailUtil.resolveAppVarsValue(readFromFile);
                        if (readFromFile != null) {
                            attachment.setReadFromFile(readFromFile);
                        }
                        
                        attachments[i] = attachment;
                    } // end of for
                    
                    smtpOperationInput.setAttachments(attachments);
                } // end of if
                
                returnValue = smtpOperationInput;
                
            } else if (IMAPAddress.QNAME_IMAP_ADDRESS.equals(elementType)) {
                final IMAPAddress imapAddress = new IMAPAddress();

                String emailServer = DOMUtils.getAttribute(element, IMAPAddress.ATTR_EMAIL_HOSTNAME);
                //emailServer = EmailUtil.resolveAppConfigValue(emailServer, IMAPAddress.ATTR_EMAIL_HOSTNAME, mAppConfigName);
                //emailServer = EmailUtil.resolveAppVarsValue(emailServer);
                if (emailServer != null) {
                    imapAddress.setHostName(emailServer);
                }

                String port = DOMUtils.getAttribute(element, IMAPAddress.ATTR_EMAIL_PORT);
                //port = EmailUtil.resolveAppConfigValue(port, IMAPAddress.ATTR_EMAIL_PORT, mAppConfigName);
                port = EmailUtil.resolveAppVarsValue(port, mConfig);
                if (port != null) {
                    imapAddress.setPort(Integer.parseInt(port));
                }
    
                String username = DOMUtils.getAttribute(element, IMAPAddress.ATTR_EMAIL_USERNAME);
                //username = EmailUtil.resolveAppConfigValue(username, IMAPAddress.ATTR_EMAIL_USERNAME, mAppConfigName);
                //username = EmailUtil.resolveAppVarsValue(username);
                if (username != null) {
                    imapAddress.setUserName(username);
                }
    
                String password = DOMUtils.getAttribute(element, IMAPAddress.ATTR_EMAIL_PASSWORD);
                //password = EmailUtil.resolveAppConfigValue(password, IMAPAddress.ATTR_EMAIL_PASSWORD, mAppConfigName);
                //password = EmailUtil.resolveAppVarsValue(password);
                if (password != null) {
                    imapAddress.setPassword(password);
                }
    
                String useSSL = DOMUtils.getAttribute(element, IMAPAddress.ATTR_EMAIL_USESSL);
                //useSSL = EmailUtil.resolveAppConfigValue(useSSL, IMAPAddress.ATTR_EMAIL_USESSL, mAppConfigName);
                useSSL = EmailUtil.resolveAppVarsValue(useSSL, mConfig);
                if (useSSL != null) {
                    imapAddress.setUseSSL(Boolean.parseBoolean(useSSL));
                } else {
                    imapAddress.setUseSSL(Boolean.FALSE);
                }
    
                String mailFolder = DOMUtils.getAttribute(element, IMAPAddress.ATTR_IMAP_MAIL_FOLDER);
                //mailFolder = EmailUtil.resolveAppVarsValue(mailFolder);
                if (mailFolder != null) {
                    imapAddress.setMailFolder(mailFolder);
                }
    
                String msgAckMode = DOMUtils.getAttribute(element, IMAPAddress.ATTR_IMAP_MESSAGE_ACK_MODE);
                //msgAckMode = EmailUtil.resolveAppVarsValue(msgAckMode);
                if (msgAckMode != null) {
                    imapAddress.setMessageAckMode(msgAckMode);
                } else {
                    imapAddress.setMessageAckMode(EmailBCConstants.ACK_MODE_AUTOMATIC);
                }
    
                String msgAckOperation = DOMUtils.getAttribute(element, IMAPAddress.ATTR_IMAP_MESSAGE_ACK_OPERATION);
                //msgAckOperation = EmailUtil.resolveAppVarsValue(msgAckOperation);
                if (msgAckOperation != null) {
                    imapAddress.setMessageAckOperation(msgAckOperation);
                } else {
                    imapAddress.setMessageAckOperation(EmailBCConstants.ACK_OP_MARK_AS_READ);
                }
    
                String maxMessageCount = DOMUtils.getAttribute(element, IMAPAddress.ATTR_IMAP_MAX_MESSAGE_COUNT);
                maxMessageCount = EmailUtil.resolveAppVarsValue(maxMessageCount, mConfig);
                if (maxMessageCount != null) {
                    imapAddress.setMaxMessageCount(Integer.parseInt(maxMessageCount));
                }
    
                String pollingInterval = DOMUtils.getAttribute(element,IMAPAddress.ATTR_IMAP_POLLING_INTERVAL);
                pollingInterval = EmailUtil.resolveAppVarsValue(pollingInterval, mConfig);
                if (pollingInterval != null) {
                    imapAddress.setPollingInterval(Integer.parseInt(pollingInterval));
                } else {
                    imapAddress.setPollingInterval(1);
                }
    
                returnValue = imapAddress;
            } else if (POP3Address.QNAME_POP3_ADDRESS.equals(elementType)) {
                final POP3Address pop3Address = new POP3Address();
    
                String emailServer = DOMUtils.getAttribute(element, POP3Address.ATTR_EMAIL_HOSTNAME);
                //emailServer = EmailUtil.resolveAppConfigValue(emailServer, POP3Address.ATTR_EMAIL_HOSTNAME, mAppConfigName);
                //emailServer = EmailUtil.resolveAppVarsValue(emailServer);
                if (emailServer != null) {
                    pop3Address.setHostName(emailServer);
                }
    
                String port = DOMUtils.getAttribute(element, POP3Address.ATTR_EMAIL_PORT);
                //port = EmailUtil.resolveAppConfigValue(port, POP3Address.ATTR_EMAIL_PORT, mAppConfigName);
                port = EmailUtil.resolveAppVarsValue(port, mConfig);
                if (port != null) {
                    pop3Address.setPort(Integer.parseInt(port));
                }
    
                String username = DOMUtils.getAttribute(element, POP3Address.ATTR_EMAIL_USERNAME);
                //username = EmailUtil.resolveAppConfigValue(username, POP3Address.ATTR_EMAIL_USERNAME, mAppConfigName);
                //username = EmailUtil.resolveAppVarsValue(username);
                if (username != null) {
                    pop3Address.setUserName(username);
                }
    
                String password = DOMUtils.getAttribute(element, POP3Address.ATTR_EMAIL_PASSWORD);
                //password = EmailUtil.resolveAppConfigValue(password, POP3Address.ATTR_EMAIL_PASSWORD, mAppConfigName);
                //password = EmailUtil.resolveAppVarsValue(password);
                if (password != null) {
                    pop3Address.setPassword(password);
                }
    
                String useSSL = DOMUtils.getAttribute(element, POP3Address.ATTR_EMAIL_USESSL);
                //useSSL = EmailUtil.resolveAppConfigValue(useSSL, POP3Address.ATTR_EMAIL_USESSL, mAppConfigName);
                useSSL = EmailUtil.resolveAppVarsValue(useSSL, mConfig);
                if (useSSL != null) {
                    pop3Address.setUseSSL(Boolean.parseBoolean(useSSL));
                } else {
                    pop3Address.setUseSSL(Boolean.FALSE);
                }
    
                String mailFolder = DOMUtils.getAttribute(element, POP3Address.ATTR_POP3_MAIL_FOLDER);
                //mailFolder = EmailUtil.resolveAppVarsValue(mailFolder);
                if (mailFolder != null) {
                    pop3Address.setMailFolder(mailFolder);
                }
    
                String msgAckMode = DOMUtils.getAttribute(element, POP3Address.ATTR_POP3_MESSAGE_ACK_MODE);
                //msgAckMode = EmailUtil.resolveAppVarsValue(msgAckMode);
                if (msgAckMode != null) {
                    pop3Address.setMessageAckMode(msgAckMode);
                } else {
                    pop3Address.setMessageAckMode(EmailBCConstants.ACK_MODE_AUTOMATIC);
                }
    
                String msgAckOperation = DOMUtils.getAttribute(element, POP3Address.ATTR_POP3_MESSAGE_ACK_OPERATION);
                //msgAckOperation = EmailUtil.resolveAppVarsValue(msgAckOperation);
                if (msgAckOperation != null) {
                    pop3Address.setMessageAckOperation(msgAckOperation);
                } else {
                    pop3Address.setMessageAckOperation(EmailBCConstants.ACK_OP_MARK_AS_READ);
                }
    
                String maxMessageCount = DOMUtils.getAttribute(element, POP3Address.ATTR_POP3_MAX_MESSAGE_COUNT);
                maxMessageCount = EmailUtil.resolveAppVarsValue(maxMessageCount, mConfig);
                if (maxMessageCount != null) {
                    pop3Address.setMaxMessageCount(Integer.parseInt(maxMessageCount));
                }
    
                String pollingInterval = DOMUtils.getAttribute(element,POP3Address.ATTR_POP3_POLLING_INTERVAL);
                pollingInterval = EmailUtil.resolveAppVarsValue(pollingInterval, mConfig);
                if (pollingInterval != null) {
                    pop3Address.setPollingInterval(Integer.parseInt(pollingInterval));
                } else {
                    pop3Address.setPollingInterval(1);
                }
    
                returnValue = pop3Address;
    
            } else if (SMTPAddress.QNAME_SMTP_ADDRESS.equals(elementType)) {
                final SMTPAddress smtpAddress = new SMTPAddress();
    
                String location = DOMUtils.getAttribute(element, SMTPAddress.ATTR_SMTP_LOCATION);
                //location = EmailUtil.resolveAppConfigValue(location, SMTPAddress.ATTR_SMTP_LOCATION, mAppConfigName);
                //location = EmailUtil.resolveAppVarsValue(location);
                if (location != null) {
                    smtpAddress.setLocation(location);
                }
    
                String emailServer = DOMUtils.getAttribute(element, SMTPAddress.ATTR_EMAIL_HOSTNAME);
                //emailServer = EmailUtil.resolveAppConfigValue(emailServer, SMTPAddress.ATTR_EMAIL_HOSTNAME, mAppConfigName);
                //emailServer = EmailUtil.resolveAppVarsValue(emailServer);
                if (emailServer != null) {
                    smtpAddress.setHostName(emailServer);
                }
    
                String port = DOMUtils.getAttribute(element, SMTPAddress.ATTR_EMAIL_PORT);
                //port = EmailUtil.resolveAppConfigValue(port, SMTPAddress.ATTR_EMAIL_PORT, mAppConfigName);
                port = EmailUtil.resolveAppVarsValue(port, mConfig);
                if (port != null) {
                    smtpAddress.setPort(Integer.parseInt(port));
                }
    
                String username = DOMUtils.getAttribute(element, SMTPAddress.ATTR_EMAIL_USERNAME);
                //username = EmailUtil.resolveAppConfigValue(username, SMTPAddress.ATTR_EMAIL_USERNAME, mAppConfigName);
                //username = EmailUtil.resolveAppVarsValue(username);
                if (username != null) {
                    smtpAddress.setUserName(username);
                }
    
                String password = DOMUtils.getAttribute(element, SMTPAddress.ATTR_EMAIL_PASSWORD);
                //password = EmailUtil.resolveAppConfigValue(password, SMTPAddress.ATTR_EMAIL_PASSWORD, mAppConfigName);
                //password = EmailUtil.resolveAppVarsValue(password);
                if (password != null) {
                    smtpAddress.setPassword(password);
                }
    
                String useSSL = DOMUtils.getAttribute(element, SMTPAddress.ATTR_EMAIL_USESSL);
                //useSSL = EmailUtil.resolveAppConfigValue(useSSL, SMTPAddress.ATTR_EMAIL_USESSL, mAppConfigName);
                useSSL = EmailUtil.resolveAppVarsValue(useSSL, mConfig);
                if (useSSL != null) {
                    smtpAddress.setUseSSL(Boolean.parseBoolean(useSSL));
                } else {
                    smtpAddress.setUseSSL(Boolean.FALSE);
                }
    
                returnValue = smtpAddress;
            }
        } catch (ApplicationVariableNotDefinedException ex) {
            throw new WSDLException(WSDLException.PARSER_ERROR,
                    "Failed to unmarshal ExtensibilityElement ",
                    ex);
        }

        return returnValue;
    }

    public void registerSerializers(EmailBCExtensionRegistry registry){
        registry.registerSerializer(Binding.class, IMAPBinding.QNAME_IMAP_BINDING, this);
        registry.registerDeserializer(Binding.class, IMAPBinding.QNAME_IMAP_BINDING, this);
        registry.mapExtensionTypes(Binding.class, IMAPBinding.QNAME_IMAP_BINDING, IMAPBinding.class);

        registry.registerSerializer(BindingOperation.class, IMAPOperation.QNAME_IMAP_OPERATION, this);
        registry.registerDeserializer(BindingOperation.class, IMAPOperation.QNAME_IMAP_OPERATION, this);        
        registry.mapExtensionTypes(BindingOperation.class, IMAPOperation.QNAME_IMAP_OPERATION, IMAPOperation.class);

        registry.registerSerializer(BindingInput.class, IMAPOperationInput.QNAME_IMAP_OPERATION_INPUT, this);
        registry.registerDeserializer(BindingInput.class, IMAPOperationInput.QNAME_IMAP_OPERATION_INPUT, this);
        registry.mapExtensionTypes(BindingInput.class, IMAPOperationInput.QNAME_IMAP_OPERATION_INPUT, IMAPOperationInput.class);

        registry.registerSerializer(Port.class, IMAPAddress.QNAME_IMAP_ADDRESS, this);
        registry.registerDeserializer(Port.class, IMAPAddress.QNAME_IMAP_ADDRESS, this);
        registry.mapExtensionTypes(Port.class, IMAPAddress.QNAME_IMAP_ADDRESS, IMAPAddress.class);

        registry.registerSerializer(Binding.class, POP3Binding.QNAME_POP3_BINDING, this);
        registry.registerDeserializer(Binding.class, POP3Binding.QNAME_POP3_BINDING, this);
        registry.mapExtensionTypes(Binding.class, POP3Binding.QNAME_POP3_BINDING, POP3Binding.class);

        registry.registerSerializer(BindingOperation.class, POP3Operation.QNAME_POP3_OPERATION, this);
        registry.registerDeserializer(BindingOperation.class, POP3Operation.QNAME_POP3_OPERATION, this);        
        registry.mapExtensionTypes(BindingOperation.class, POP3Operation.QNAME_POP3_OPERATION, POP3Operation.class);

        registry.registerSerializer(BindingInput.class, POP3OperationInput.QNAME_POP3_OPERATION_INPUT, this);
        registry.registerDeserializer(BindingInput.class, POP3OperationInput.QNAME_POP3_OPERATION_INPUT, this);
        registry.mapExtensionTypes(BindingInput.class, POP3OperationInput.QNAME_POP3_OPERATION_INPUT, POP3OperationInput.class);

        registry.registerSerializer(Port.class, POP3Address.QNAME_POP3_ADDRESS, this);
        registry.registerDeserializer(Port.class, POP3Address.QNAME_POP3_ADDRESS, this);
        registry.mapExtensionTypes(Port.class, POP3Address.QNAME_POP3_ADDRESS, POP3Address.class);

        registry.registerSerializer(Binding.class, 	SMTPBinding.QNAME_SMTP_BINDING, this);
        registry.registerDeserializer(Binding.class, SMTPBinding.QNAME_SMTP_BINDING, this);
        registry.mapExtensionTypes(Binding.class, SMTPBinding.QNAME_SMTP_BINDING, POP3Binding.class);

        registry.registerSerializer(BindingOperation.class, SMTPOperation.QNAME_SMTP_OPERATION, this);
        registry.registerDeserializer(BindingOperation.class, SMTPOperation.QNAME_SMTP_OPERATION, this);        
        registry.mapExtensionTypes(BindingOperation.class, SMTPOperation.QNAME_SMTP_OPERATION, POP3Operation.class);

        registry.registerSerializer(BindingInput.class, SMTPOperationInput.QNAME_SMTP_OPERATION_INPUT, this);
        registry.registerDeserializer(BindingInput.class, SMTPOperationInput.QNAME_SMTP_OPERATION_INPUT, this);
        registry.mapExtensionTypes(BindingInput.class, SMTPOperationInput.QNAME_SMTP_OPERATION_INPUT, POP3OperationInput.class);

        registry.registerSerializer(Port.class, SMTPAddress.QNAME_SMTP_ADDRESS, this);
        registry.registerDeserializer(Port.class, SMTPAddress.QNAME_SMTP_ADDRESS, this);
        registry.mapExtensionTypes(Port.class, SMTPAddress.QNAME_SMTP_ADDRESS, SMTPAddress.class);
    }

}
