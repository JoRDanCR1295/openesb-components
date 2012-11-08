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
 * @(#)EmailDenormalizer.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.binding.email.protocol.send.smtp;

import java.net.URISyntaxException;
import java.net.URL;
import java.util.Date;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.activation.FileDataSource;
import javax.activation.URLDataSource;
import javax.jbi.messaging.NormalizedMessage;
import javax.mail.MessagingException;
import javax.mail.Multipart;
import javax.mail.Session;
import javax.mail.internet.ContentType;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;
import javax.mail.internet.MimePart;
import javax.mail.internet.MimeMessage.RecipientType;
import javax.mail.internet.ParseException;
import javax.mail.util.ByteArrayDataSource;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import org.xml.sax.SAXException;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import javax.xml.transform.TransformerException;
import javax.xml.parsers.ParserConfigurationException;

import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderException;
import com.sun.jbi.binding.email.I18n;
import com.sun.jbi.common.util.Base64Utils;
import com.sun.jbi.binding.email.protocol.ApplicationVariableNotDefinedException;
import com.sun.jbi.binding.email.protocol.EmailBCConstants;
import com.sun.jbi.binding.email.protocol.EmailUtil;
import com.sun.jbi.binding.email.protocol.wsdl.SMTPAddress;
import com.sun.jbi.binding.email.protocol.wsdl.SMTPAttachment;
import com.sun.jbi.binding.email.protocol.wsdl.SMTPOperation;
import com.sun.jbi.binding.email.protocol.wsdl.SMTPOperationInput;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperParser;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;
import com.sun.jbi.nms.wsdl11wrapper.util.WrapperUtil;
import java.util.List;
import static com.sun.jbi.binding.email.protocol.EmailBCConstants.EmailNMProperty;

/**
 * EmailDenormalizer converts Normalized Messages to messages that can be
 * interpreted by the External Service Provider.  This class utilizes
 * metadata information from the WSDL to properly convert Normalized
 * Messages.
 *  
 * @author Harry Liu (Harry.Liu@sun.com)
 */
//Content Types (RFC2045/2046/2049/2387):
//1. discrete top-level:
//1.1. text/  ... plain, richtext, enriched, html, xml, ...
//1.2. image/  ... jpeg, gif, png, ...
//1.3. audio/  ... basic, ...
//1.4. video/  ... mpeg, ...
//1.5. application/  ... PostScript, octet-stream, ...
//
//2. composite top-level:
//2.1. multipart/  ... mixed, alternative, parallel, digest, related, ...
//2.2. message/  ... rfc822, partial, external-body, ...
//
//3. Experimental: x- ...
//
public class EmailDenormalizer {

    private static final Logger logger = Logger.getLogger(EmailDenormalizer.class.getName());
    private Session mSession;
    private NormalizedMessage mNormalizedMessage;
    private QName mOperationName;
    private SMTPEndpoint mEndpoint;
    private SMTPOperationInput mOperationInput;
    private String mDefaultCharset;
    private SMTPMailTo mMailtoAttr;
    private boolean mEmbedImagesInHtml;
    private String mSendOption;
    ///private WSMessage mJbiMessage;
    private WrapperParser mWrapperParser;
    private final static QName BASE64BINARY_1991_QNAME = new QName("http://www.w3.org/1999/XMLSchema", "base64Binary");
    private final static QName BASE64BINARY_2001_QNAME = new QName("http://www.w3.org/2001/XMLSchema", "base64Binary");

    public EmailDenormalizer(Session session, NormalizedMessage normalizedMessage,
            QName operationName, SMTPEndpoint endpoint) {
        this.mSession = session;
        this.mNormalizedMessage = normalizedMessage;
        this.mOperationName = operationName;
        this.mEndpoint = endpoint;
    }

    /**
     * @param normalizedMessage
     * @param operationName
     * @param endpoint
     * @return
     * @throws Exception
     */
    public MimeMessage denormalize() throws EmailDenormalizerException {
        EndpointInfo info = this.mEndpoint.getInfo();
//        // <service name="smtpService1">
//        Service service = this.mEndpoint.getDefinition().getService(info.getServiceName());
//        // <port name="smtpPort1" binding="tns:smtpBinding1">
//        Port port = service.getPort(QName.valueOf(info.getEndpointName()).getLocalPart());
//        // <binding name="smtpBinding1" type="tns:smtpPortType1">
//        // <portType name="smtpPortType1">
//        PortType portType = port.getBinding().getPortType();

//        Iterator it = portType.getOperations().iterator();
//        Message wsdlMessage = null;
//        while (it.hasNext()) {
//            // <operation name="smtpOperation1">
//            Operation op = (Operation)it.next();
//            if (op.getName().equals(this.mOperationName.toString()) ||
//                    op.getName().equals(this.mOperationName.getLocalPart())) {
//                // <input name="input1" message="tns:smtpOperationRequest1"/>
//                // <message name="smtpOperationRequest1">
//                wsdlMessage = op.getInput().getMessage();
//                break;
//            }
//        }

        // <email:SMTPoperation/>    
        SMTPOperation operation = (SMTPOperation) this.mEndpoint.getEmailOperations().get(this.mOperationName);

        if (null == operation) {
            operation = (SMTPOperation) this.mEndpoint.getEmailOperations().get(QName.valueOf(this.mOperationName.getLocalPart()));
        }

        if (null == operation) {
            throw new EmailDenormalizerException(I18n.loc("EMAILBC-7017: Invalid operation name: {0} for endpoint: {1}",
                    this.mOperationName,
                    info.getEndpointName()));
        }

        // <email:SMTPinput message="bodyPart" subject="subjectPart" from="fromPart" to="toPart"/>
        this.mOperationInput = (SMTPOperationInput) operation.getInput();
        if (this.mOperationInput == null) {
            throw new EmailDenormalizerException(I18n.loc("EMAILBC-7018: No binding input for operation: {0}",
                    this.mOperationName));
        }

        try {
            // charset -- apply to subject and body, also default to attachment "text/*" content
            this.mDefaultCharset = this.mOperationInput.getCharset();
            this.mDefaultCharset = resolveValue(this.mDefaultCharset, SMTPOperationInput.ATTR_SMTP_CHARSET, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_CHARSET);
            if ("".equals(this.mDefaultCharset)) {
                this.mDefaultCharset = null;
            }

            // location="mailto:user@domain?subject=abc&cc=user2@domain2"
            String locationAttr = this.mEndpoint.getSMTPAddress().getLocation();
            locationAttr = resolveValue(locationAttr, SMTPAddress.ATTR_SMTP_LOCATION, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_LOCATION);
            if (null != locationAttr && 0 != locationAttr.length()) {
                this.mMailtoAttr = new SMTPMailTo(locationAttr);
            }

            Source contentSrc = this.mNormalizedMessage.getContent();
            Node node = EmailUtil.transformToNode(contentSrc);
            Document contentDoc = EmailUtil.getDocument(node);
            ///this.mJbiMessage = MessageUtil.createWSMessage(contentSrc, wsdlMessage);
            this.mWrapperParser = HelperFactory.createParser();
            this.mWrapperParser.parse(contentDoc, this.mEndpoint.getDefinition());
            ///

            MimeMessage mimeMsg = new MimeMessage(this.mSession);

            this.handleMessageHeaders(mimeMsg);
            this.handleMessageContent(mimeMsg);
            I18n.fine(logger, "EMAILBC-1007: Completed drafting the email message.");
            return mimeMsg;
        } catch (ApplicationVariableNotDefinedException e) {
            throw wrapException(e);
        } catch (URISyntaxException e) {
            throw wrapException(e);
        } catch (TransformerException e) {
            throw wrapException(e);
        } catch (WrapperProcessingException e) {
            throw wrapException(e);
        } catch (EncoderException e) {
            throw wrapException(e);
        } catch (IOException e) {
            throw wrapException(e);
        } catch (MessagingException e) {
            throw wrapException(e);
        } catch (SAXException e) {
            throw wrapException(e);
        } catch (ParserConfigurationException e) {
            throw wrapException(e);
        } catch (Exception e) {
            throw wrapException(e);
        }

    }

    /**
     * @param mimeMsg
     * @throws Exception
     */
    private void handleMessageHeaders(MimeMessage mimeMsg) throws WrapperProcessingException, TransformerException, IOException, MessagingException, ApplicationVariableNotDefinedException, EmailDenormalizerException {
        // NM Properties and AppConfig first, then Parts values, then mailtoAttr   (dynamic ... static)

        this.handleHeaderSubject(mimeMsg);
        this.handleHeaderFrom(mimeMsg);
        this.handleHeaderTo(mimeMsg);
        this.handleHeaderCc(mimeMsg);
        this.handleHeaderBcc(mimeMsg);
        this.handleHeaderNewsgroups(mimeMsg);

        if (null == mimeMsg.getAllRecipients() || 0 == mimeMsg.getAllRecipients().length) {
            throw new EmailDenormalizerException(I18n.loc("EMAILBC-7019: No valid recipient address is specified for this email message."));
        }

        //?mimeMsg.setContentID(cid);
        //?mimeMsg.setReplyTo(addresses);
        //?mimeMsg.setSender(address);
        mimeMsg.setSentDate(new Date());
        mimeMsg.setHeader("X-Mailer", "Open JBI Email Binding Component");

    }

    /**
     * @param mimeMsg
     * @throws Exception
     */
    private void handleHeaderSubject(MimeMessage mimeMsg) throws ApplicationVariableNotDefinedException, WrapperProcessingException, TransformerException, IOException, MessagingException {
        // Fill in the mail "subject"
        String subjectPartValue = resolveValue(null, SMTPOperationInput.ATTR_EMAIL_SUBJECT, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_SUBJECT);
        if (null != subjectPartValue) {
            I18n.finer(logger, "EMAILBC-2003: Resolved \"Subject\": [{0}].", subjectPartValue);
        } else {
            subjectPartValue = "";
            String subjectPartName = this.mOperationInput.getSubject();
            if (null != subjectPartName && 0 != subjectPartName.length()) {
                ///Element subjectPartObj = this.mJbiMessage.getPart(subjectPartName);
                NodeList subjectPartObj = this.mWrapperParser.getPartNodes(subjectPartName);
                if (null != subjectPartObj && 0 != subjectPartObj.getLength()) {
                    subjectPartValue = EmailUtil.transformToTextPlain(subjectPartObj.item(0), null);
                    I18n.finer(logger, "EMAILBC-2004: Derived from wsdl part: {1}, \"Subject\" : [{0}].", subjectPartValue, subjectPartName);
                } else {
                    I18n.warning(logger, "EMAILBC-6001: The WSDL message does not define corresponding part for input attribute {0}. An empty value is assumed.", subjectPartName);
                }
            } else {
                if (null != this.mMailtoAttr && null != this.mMailtoAttr.getSubject()) {
                    subjectPartValue = this.mMailtoAttr.getSubject();
                    I18n.finer(logger, "EMAILBC-2005: Derived from location attribute, \"Subject\": [{0}].", subjectPartValue);
                } else {
                    I18n.warning(logger, "EMAILBC-6002: No \"Subject\" is specified.");
                }
            }
        }
        mimeMsg.setSubject(subjectPartValue, this.mDefaultCharset);
    }

    /**
     * @param mimeMsg
     * @throws Exception
     */
    private void handleHeaderFrom(MimeMessage mimeMsg) throws ApplicationVariableNotDefinedException, WrapperProcessingException, TransformerException, IOException {
        // Fill in the "from" field
        String fromAddr = resolveValue(null, SMTPOperationInput.ATTR_EMAIL_FROM, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_FROM);
        if (null != fromAddr) {
            I18n.finer(logger, "EMAILBC-2006: Resolved \"From\": [{0}].", fromAddr);
        } else {
            fromAddr = "";
            String fromPartName = this.mOperationInput.getFrom();
            if (null != fromPartName && 0 != fromPartName.length()) {
                ///Element fromPartObj = this.mJbiMessage.getPart(fromPartName);
                NodeList fromPartObj = this.mWrapperParser.getPartNodes(fromPartName);
                if (null != fromPartObj && 0 != fromPartObj.getLength()) {
                    fromAddr = EmailUtil.transformToTextPlain(fromPartObj.item(0), null);
                    I18n.finer(logger, "EMAILBC-2007: Derived from wsdl part: {1}, \"From\": [{0}].", fromAddr, fromPartName);
                } else {
                    I18n.warning(logger, "EMAILBC-6001: The WSDL message does not define corresponding part for input attribute {0}. An empty value is assumed.", fromPartName);
                }
            } else {
                if (null != this.mMailtoAttr && null != this.mMailtoAttr.getFromMailbox()) {
                    fromAddr = this.mMailtoAttr.getFromMailbox().getNormalizedAddressSpec();
                    I18n.finer(logger, "EMAILBC-2008: Derived from location attribute, \"From\" : [{0}].", fromAddr);
                } else {
                    I18n.warning(logger, "EMAILBC-6003: No \"From\" address is specified.");
                }
            }
        }
        if (null != fromAddr && 0 != fromAddr.length()) {
            try {
                //?InternetAddress[] fromPartAddress = InternetAddress.parse(fromAddr);
                //?mimeMsg.addFrom(fromPartAddress);
                InternetAddress fromPartAddress = new InternetAddress(fromAddr);
                mimeMsg.setFrom(fromPartAddress);
            } catch (Exception e) {
                I18n.warning(logger, "EMAILBC-6004: Header \"From\" is not added because the value [{0}] does not represent valid address(es)", e,
                        fromAddr);
            }
        }
    }

    /**
     * @param mimeMsg
     * @throws Exception
     */
    private void handleHeaderTo(MimeMessage mimeMsg) throws ApplicationVariableNotDefinedException, WrapperProcessingException, TransformerException, IOException {
        // Fill in the "to" field.
        String toPartValue = resolveValue(null, SMTPOperationInput.ATTR_EMAIL_TO, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_TO);
        if (null != toPartValue) {
            I18n.finer(logger, "EMAILBC-2009: Resolved \"To\": [{0}].", toPartValue);
        } else {
            toPartValue = "";
            String toPartName = this.mOperationInput.getTo();
            if (null != toPartName && 0 != toPartName.length()) {
                ///Element toPartObj = this.mJbiMessage.getPart(toPartName);
                NodeList toPartObj = this.mWrapperParser.getPartNodes(toPartName);
                if (null != toPartObj && 0 != toPartObj.getLength()) {
                    toPartValue = EmailUtil.transformToTextPlain(toPartObj.item(0), null);
                    I18n.finer(logger, "EMAILBC-2010: Derived from wsdl part: {1}, \"To\": [{0}].", toPartValue, toPartName);
                } else {
                    I18n.warning(logger, "EMAILBC-6001: The WSDL message does not define corresponding part for input attribute {0}. An empty value is assumed.", toPartName);
                }
            } else {
                if (null != this.mMailtoAttr && null != this.mMailtoAttr.getMailbox()) {
                    toPartValue = createCommaSeperatedAddresses(this.mMailtoAttr.getMailbox());
                    I18n.finer(logger, "EMAILBC-2011: Derived from location attribute, \"To\": [{0}].", toPartValue);
                } else {
                    I18n.warning(logger, "EMAILBC-6005: No \"To\" address is specified.");
                }
            }
        }
        if (null != toPartValue && 0 != toPartValue.length()) {
            try {
                mimeMsg.addRecipients(RecipientType.TO, toPartValue);
            } catch (Exception e) {
                I18n.warning(logger, "EMAILBC-6006: Recipient(s) is not added to \"To\" because the value [{0}] does not represent valid address(es)", e,
                        toPartValue);
            }
        }
    }

    /**
     * @param mimeMsg
     * @throws Exception
     */
    private void handleHeaderCc(MimeMessage mimeMsg) throws ApplicationVariableNotDefinedException, WrapperProcessingException, TransformerException, IOException {
        // Fill in the "cc" field.
        String ccPartValue = resolveValue(null, SMTPOperationInput.ATTR_EMAIL_CC, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_CC);
        if (null != ccPartValue) {
            I18n.finer(logger, "EMAILBC-2012: Resolved \"cc\": [{0}].", ccPartValue);
        } else {
            ccPartValue = "";
            String ccPartName = this.mOperationInput.getCc();
            if (null != ccPartName && 0 != ccPartName.length()) {
                ///Element ccPartObj = this.mJbiMessage.getPart(ccPartName);
                NodeList ccPartObj = this.mWrapperParser.getPartNodes(ccPartName);
                if (null != ccPartObj && 0 != ccPartObj.getLength()) {
                    ccPartValue = EmailUtil.transformToTextPlain(ccPartObj.item(0), null);
                    I18n.finer(logger, "EMAILBC-2013: Derived from wsdl part: {1}, \"Cc\": [{0}].", ccPartValue, ccPartName);
                } else {
                    I18n.warning(logger, "EMAILBC-6001: The WSDL message does not define corresponding part for input attribute {0}. An empty value is assumed.", ccPartName);
                }
            } else {
                if (null != this.mMailtoAttr && null != this.mMailtoAttr.getCCMailbox()) {
                    ccPartValue = createCommaSeperatedAddresses(this.mMailtoAttr.getCCMailbox());
                    I18n.finer(logger, "EMAILBC-2014: Derived from location attribute, \"Cc\": [{0}].", ccPartValue);
                } else {
                    I18n.finer(logger, "EMAILBC-2015: No \"cc\" is specified.");
                }
            }
        }
        if (null != ccPartValue && 0 != ccPartValue.length()) {
            try {
                mimeMsg.addRecipients(RecipientType.CC, ccPartValue);
            } catch (Exception e) {
                I18n.warning(logger, "EMAILBC-6007: Recipient(s) is not added to \"Cc\" because the value [{0}] does not represent valid address(es)", e,
                        ccPartValue);
            }
        }
    }

    /**
     * @param mimeMsg
     * @throws Exception
     */
    private void handleHeaderBcc(MimeMessage mimeMsg) throws ApplicationVariableNotDefinedException, WrapperProcessingException, TransformerException, IOException {
        // Fill in the "bcc" field.
        String bccPartValue = resolveValue(null, SMTPOperationInput.ATTR_EMAIL_BCC, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_BCC);
        if (null != bccPartValue) {
            I18n.finer(logger, "EMAILBC-2016: Resolved \"Bcc\": [{0}].", bccPartValue);
        } else {
            bccPartValue = "";
            String bccPartName = this.mOperationInput.getBcc();
            if (null != bccPartName && 0 != bccPartName.length()) {
                ///Element bccPartObj = this.mJbiMessage.getPart(bccPartName);
                NodeList bccPartObj = this.mWrapperParser.getPartNodes(bccPartName);
                if (null != bccPartObj && 0 != bccPartObj.getLength()) {
                    bccPartValue = EmailUtil.transformToTextPlain(bccPartObj.item(0), null);
                    I18n.finer(logger, "EMAILBC-2017: Derived from wsdl part: {1}, \"Bcc\": [{0}].", bccPartValue, bccPartName);
                } else {
                    I18n.warning(logger, "EMAILBC-6001: The WSDL message does not define corresponding part for input attribute {0}. An empty value is assumed.", bccPartName);
                }
            } else {
                if (null != this.mMailtoAttr && null != this.mMailtoAttr.getBCCMailbox()) {
                    bccPartValue = createCommaSeperatedAddresses(mMailtoAttr.getBCCMailbox());
                    I18n.finer(logger, "EMAILBC-2018: Derived from location attribute, \"Bcc\": [{0}].", bccPartValue);
                } else {
                    I18n.finer(logger, "EMAILBC-2019: No \"Bcc\" is specified.");
                }
            }
        }
        if (null != bccPartValue && 0 != bccPartValue.length()) {
            try {
                mimeMsg.addRecipients(RecipientType.BCC, bccPartValue);
            } catch (Exception e) {
                I18n.warning(logger, "EMAILBC-6008: Recipient(s) is not added to \"Bcc\" because the value [{0}] does not represent valid address(es)", e,
                        bccPartValue);
            }
        }
    }

    private String createCommaSeperatedAddresses(Collection<SMTPMailbox> mailBoxes) {
        StringBuilder csaBuilder = new StringBuilder();
        for (SMTPMailbox mailBox : mailBoxes) {
            csaBuilder.append(mailBox.getNormalizedAddressSpec()).append(",");
        }
        if (!mailBoxes.isEmpty()) {
            csaBuilder.deleteCharAt(csaBuilder.length() - 1);
        }
        return csaBuilder.toString();
    }

    /**
     * @param mimeMsg
     * @throws Exception
     */
    private void handleHeaderNewsgroups(MimeMessage mimeMsg) throws ApplicationVariableNotDefinedException, WrapperProcessingException, TransformerException, IOException {
        // Fill in the "newsgroup" field.
        String ngPartValue = resolveValue(null, SMTPOperationInput.ATTR_EMAIL_NEWSGROUPS, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_NEWSGROUP);
        if (null != ngPartValue) {
            I18n.finer(logger, "EMAILBC-2020: Resolved \"Newsgroups\": [{0}].", ngPartValue);
        } else {
            ngPartValue = "";
            String ngPartName = this.mOperationInput.getNewsgroups();
            if (null != ngPartName && 0 != ngPartName.length()) {
                ///Element ngPartObj = this.mJbiMessage.getPart(ngPartName);
                NodeList ngPartObj = this.mWrapperParser.getPartNodes(ngPartName);
                if (null != ngPartObj && 0 != ngPartObj.getLength()) {
                    ngPartValue = EmailUtil.transformToTextPlain(ngPartObj.item(0), null);
                    I18n.finer(logger, "EMAILBC-2021: Derived from wsdl part: {1}, \"Newsgroups\" : [{0}].", ngPartValue, ngPartName);
                } else {
                    I18n.warning(logger, "EMAILBC-6001: The WSDL message does not define corresponding part for input attribute {0}. An empty value is assumed.", ngPartName);
                }
            } else {
                if (null != this.mMailtoAttr && null != this.mMailtoAttr.getNewsgroups()) {
                    ngPartValue = this.mMailtoAttr.getNewsgroups();
                    I18n.finer(logger, "EMAILBC-2022: Derived from location attribute, \"Newsgroups\": [{0}].", ngPartValue);
                } else {
                    I18n.finer(logger, "EMAILBC-2023: No \"newsgroups\" is specified.");
                }
            }
        }
        if (null != ngPartValue && 0 != ngPartValue.length()) {
            try {
                mimeMsg.addRecipients(RecipientType.NEWSGROUPS, ngPartValue);
            } catch (Exception e) {
                I18n.warning(logger, "EMAILBC-6009: Recipient(s) is not added to \"newsgroups\" because the value [{0}] does not represent valid address(es)", e,
                        ngPartValue);
            }
        }
    }

    /**
     * @param mimeMsg
     * @throws Exception
     */
    // multipart/mixed 
    //                -> text/plain
    //                -> text/html
    //                -> image/jpeg
    //                -> image/gif
    //                -> image/png
    //                -> multipart/alternative
    //                -> multipart/related
    private void handleMessageContent(MimeMessage mimeMsg) throws ApplicationVariableNotDefinedException, WrapperProcessingException, EmailDenormalizerException, EncoderException, TransformerException, IOException, MessagingException, SAXException, ParserConfigurationException, Exception {
        // NM Properties and AppConfig first, then Parts values, then mailtoAttr   (dynamic ... static)
        EndpointInfo info = this.mEndpoint.getInfo();
        Service service = this.mEndpoint.getDefinition().getService(info.getServiceName());
        Port port = service.getPort(QName.valueOf(info.getEndpointName()).getLocalPart());
        PortType portType = port.getBinding().getPortType();
        Message wsdlMessage = null;
        for (Iterator it = portType.getOperations().iterator(); it.hasNext();) {
            Operation op = (Operation) it.next();
            if (op.getName().equals(this.mOperationName.toString()) ||
                    op.getName().equals(this.mOperationName.getLocalPart())) {
                wsdlMessage = op.getInput().getMessage();
                break;
            }
        }

        String bodyPartValue = resolveValue(null, SMTPOperationInput.ATTR_EMAIL_BODY, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_BODY);
        boolean isBinary = false;
        String attachmentFileName = "";
        if (null != bodyPartValue) {
            I18n.finer(logger, "EMAILBC-2024: Resolved \"Message body\" using nm property : [{0}].", bodyPartValue);
        } else {
            bodyPartValue = "";
            String bodyPartName = this.mOperationInput.getMessage();
            if (null != bodyPartName && 0 != bodyPartName.length()) {
                bodyPartValue = getPartValueForBody(wsdlMessage, bodyPartName);
            } else {
                if (null != this.mMailtoAttr && null != this.mMailtoAttr.getBody()) {
                    bodyPartValue = this.mMailtoAttr.getBody();
                    I18n.finer(logger, "EMAILBC-2027: Derived from location attribute defined in SMTP address, Message body : [{0}].", bodyPartValue);
                }
                if (bodyPartValue == null || bodyPartValue.trim().length() == 0) {
                    //Using whatever is the value of the first part to create a body.
                    //This is helpful when doing bc to bc direct connections, read the part, and send as body,
                    //if binary, then send as attachment.
                    List parts = wsdlMessage.getOrderedParts(null);
                    if (parts != null && parts.size() > 0) {
                        Part part = (Part) parts.get(0);
                        String firstPartName = part.getName();
                        QName bodyPartType = part.getTypeName();
                        isBinary = bodyPartType != null &&
                                (bodyPartType.equals(BASE64BINARY_1991_QNAME) || bodyPartType.equals(BASE64BINARY_2001_QNAME));
                        bodyPartValue = getPartValueForBody(wsdlMessage, firstPartName);
                        attachmentFileName = firstPartName;
                        I18n.finer(logger, "EMAILBC-2036: Assuming first part '{0}' value for , Message body : [{1}].", firstPartName, bodyPartValue);
                    } else {
                        I18n.warning(logger, "EMAILBC-6011: No \"Message body\" is specified.");
                    }
                }
            }
        }

        boolean hasNMAttachments = false;
        Set<String> nmAttachmentNames = this.mNormalizedMessage.getAttachmentNames();
        if (null != nmAttachmentNames && 0 < nmAttachmentNames.size()) {
            hasNMAttachments = true;
        }

        boolean hasWSDLAttachments = false;
        SMTPAttachment[] wsdlAttachments = this.mOperationInput.getAttachments();
        if (null != wsdlAttachments && 0 < wsdlAttachments.length) {
            hasWSDLAttachments = true;
        }

        boolean hasNMPropertyAttachment = false;
        String paths = (String) mNormalizedMessage.getProperty(EmailNMProperty.OUTBOUND_ATTACHMENT_FILEPATHS.getKey());
        if (paths != null && !paths.trim().equals("")) {
            hasNMPropertyAttachment = true;
        }


        String booleanString = Boolean.toString(this.mOperationInput.getHandleNMAttachments());
        booleanString = this.resolveValue(booleanString, SMTPOperationInput.ATTR_EMAIL_HANDLE_NM_ATTACHMENTS, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_HANDLE_NM_ATTACHMENTS);
        boolean handleNMAttach = Boolean.parseBoolean(booleanString);

        booleanString = Boolean.toString(this.mOperationInput.getEmbedImagesInHtml());
        booleanString = this.resolveValue(booleanString, SMTPOperationInput.ATTR_SMTP_EMBED_IMAGES_IN_HTML, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_EMBED_IMAGES_IN_HTML);
        this.mEmbedImagesInHtml = Boolean.parseBoolean(booleanString);

        String sendOption = this.mOperationInput.getSendOption();
        this.mSendOption = this.resolveValue(sendOption, SMTPOperationInput.ATTR_SMTP_SEND_OPTION, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_SEND_OPTION);

        // without attachment ... single level-1 part
        if (!isBinary && !hasWSDLAttachments && !(hasNMAttachments && handleNMAttach) && !hasNMPropertyAttachment) {
            // main body only
            // text/plain, text/html, or multipart/alternative (text/plain + text/html)
            if (EmailBCConstants.SMTP_SEND_OPTION_HTML_ONLY.equalsIgnoreCase(this.mSendOption)) {
                this.handleTextHtml(mimeMsg, bodyPartValue);
            } else if (EmailBCConstants.SMTP_SEND_OPTION_XML_ONLY.equalsIgnoreCase(this.mSendOption)) {
                this.handleTextXml(mimeMsg, bodyPartValue);
            } else if (EmailBCConstants.SMTP_SEND_OPTION_BOTH_TEXT_AND_HTML.equalsIgnoreCase(this.mSendOption) || EmailBCConstants.SMTP_SEND_OPTION_BOTH_TEXT_AND_XML.equalsIgnoreCase(this.mSendOption)) {
                this.handleMultipartAlternative(mimeMsg, bodyPartValue, bodyPartValue, bodyPartValue);
            } else {
                // treat all others as text/plain
                this.handleTextPlain(mimeMsg, bodyPartValue);
            }
            return;
        }

        // with attachments ... multiple level-1 parts
        MimeMultipart masterMultipart = new MimeMultipart("mixed");
        masterMultipart.setPreamble(EmailBCConstants.PREAMBLE);
        mimeMsg.setContent(masterMultipart);

        // handle/add the first/main level-1 part
        if (isBinary) {
            I18n.warning(logger, "EMAILBC-6016: Binary values will result in garbled text in the email body");
        }
        this.handleMultipartMixedFirstPart(masterMultipart, bodyPartValue);
        // handle/add attachments
        if (hasNMAttachments && handleNMAttach) {
            this.handleNMAttachments(masterMultipart, nmAttachmentNames);
        }

        if (hasWSDLAttachments) {
            this.handleWSDLAttachments(masterMultipart, wsdlAttachments);
        }

        if (hasNMPropertyAttachment) {
            this.handleNMPropertyWSDLAttachments(masterMultipart, paths);
        }

    }

    private byte[] getPartIfAvailableAsAttachment(Node aNode, NormalizedMessage nm) throws IOException {
        if (!WrapperUtil.isNodeXopInclude(aNode)) {
            return null;
        }
        String contentId = WrapperUtil.getXopContentId(aNode);
        DataHandler dataHandler = nm.getAttachment(contentId);
        InputStream in = dataHandler.getInputStream();
        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        byte[] bytes = new byte[128];
        int len;
        while ((len = in.read(bytes)) != -1) {
            bout.write(bytes, 0, len);
        }
        //reset inputstream if somewants to read again
        in.reset();
        return bout.toByteArray();
    }

    // multipart/mixed ... add the first/main level-1 part
    private void handleMultipartMixedFirstPart(MimeMultipart masterMultipart, String firstPartValue) throws MessagingException {
        MimeBodyPart bodyPart = new MimeBodyPart();
        // the first part
        // text/plain, text/html, or multipart/alternative (text/plain + text/html)
        if (EmailBCConstants.SMTP_SEND_OPTION_HTML_ONLY.equalsIgnoreCase(this.mSendOption)) {
            this.handleTextHtml(bodyPart, firstPartValue);
        } else if (EmailBCConstants.SMTP_SEND_OPTION_XML_ONLY.equalsIgnoreCase(this.mSendOption)) {
            this.handleTextXml(bodyPart, firstPartValue);
        } else if (EmailBCConstants.SMTP_SEND_OPTION_BOTH_TEXT_AND_HTML.equalsIgnoreCase(this.mSendOption) || EmailBCConstants.SMTP_SEND_OPTION_BOTH_TEXT_AND_XML.equalsIgnoreCase(this.mSendOption)) {
            this.handleMultipartAlternative(bodyPart, firstPartValue, firstPartValue, firstPartValue);
        } else {
            // treat all others as text/plain
            this.handleTextPlain(bodyPart, firstPartValue);
        }
        masterMultipart.addBodyPart(bodyPart);

        return;
    }

    // (text/plain + text/html), (text/plain + text/xml)
    private void handleMultipartAlternative(
            MimePart container, String plainPartValue, String htmlPartValue, String xmlPartValue) throws MessagingException {
        // container could be MimeBodyPart or MimeMessage
        // plainPartValue, htmlPartValue and xmlPartValue could be different or same

        MimeMultipart alternativePart = new MimeMultipart("alternative");
        alternativePart.setPreamble(EmailBCConstants.PREAMBLE);

        MimeBodyPart plainBodyPart = new MimeBodyPart();
        this.handleTextPlain(plainBodyPart, plainPartValue);
        alternativePart.addBodyPart(plainBodyPart);

        if (EmailBCConstants.SMTP_SEND_OPTION_BOTH_TEXT_AND_HTML.equalsIgnoreCase(this.mSendOption)) {
            MimeBodyPart htmlBodyPart = new MimeBodyPart();
            this.handleTextHtml(htmlBodyPart, htmlPartValue);
            alternativePart.addBodyPart(htmlBodyPart);
        } else {
            MimeBodyPart xmlBodyPart = new MimeBodyPart();
            this.handleTextXml(xmlBodyPart, xmlPartValue);
            alternativePart.addBodyPart(xmlBodyPart);
        }

        container.setContent(alternativePart);
    }

    // text/plain
    private void handleTextPlain(MimePart container, String plainPartValue) throws MessagingException {
        // container could be MimeBodyPart or MimeMessage

        boolean doTransform = false;  //?? revisit later
        if (doTransform) {
            plainPartValue = EmailUtil.transformToTextPlain(
                    plainPartValue,
                    this.mDefaultCharset,
                    container.getContentType());
        }

        container.setText(plainPartValue, this.mDefaultCharset, "plain");
        return;
    }

    // text/html, or multipart/related (text/html + image/*)
    private void handleTextHtml(MimePart container, String htmlPartValue) throws MessagingException {
        // container could be MimeBodyPart or MimeMessage

        boolean doTransform = false;  //?? revisit later
        if (doTransform) {
            htmlPartValue = EmailUtil.transformToTextHtml(
                    htmlPartValue,
                    this.mDefaultCharset);
        }

        if (this.mEmbedImagesInHtml) {
            this.handleMultipartRelated(container, htmlPartValue);
        } else {
            container.setText(htmlPartValue, this.mDefaultCharset, "html");
        }
        return;
    }

    // text/xml
    private void handleTextXml(MimePart container, String xmlPartValue) throws MessagingException {
        // container could be MimeBodyPart or MimeMessage

        boolean doTransform = false;  //?? revisit later
        if (doTransform) {
            xmlPartValue = EmailUtil.transformToTextXml(
                    xmlPartValue,
                    this.mDefaultCharset);
        }

        ///////////////////////////////////////
        // "text/xml" vs "application/xml" ?
        ///////////////////////////////////////

        // just use "text/xml", let receivers to decide 
        // whether handle it specially or same as "text/plain"
        container.setText(xmlPartValue, this.mDefaultCharset, "xml");
        /*
        // seems "application/xml" is more specific but currently
        // it is not as well-known as "text/xml" (things may change later :-),
        // for the email clients don't understand "application/xml", they may 
        // render the content as an attachment, for example, with name "attachment.xml".
        String type = "application/xml";
        if (null != this.mDefaultCharset && !"".equals(this.mDefaultCharset)) {
        type = type + "; charset=" + this.mDefaultCharset;
        }
        container.setContent(xmlPartValue, type);
         */

        return;
    }

    // multipart/related 
    //                   -> text/html
    //                   -> image/jpeg
    //                   -> image/gif
    //                   -> image/png
    private void handleMultipartRelated(MimePart container, String htmlPartValue) throws MessagingException {
        // container could be MimeBodyPart or MimeMessage

        //TODO is it actual html format? simple guess/peek?

        // does it have tag <img src="..." border="..." alt="..." > ?
        int imgStart = 0;
        int imgEnd = 0;
        int srcStart = 0;
        int srcEnd = 0;
        int pos = 0;
        String img = null;
        String imgSrc = null;
        MimeBodyPart htmlBodyPart = new MimeBodyPart();
        MimeMultipart relatedPart = new MimeMultipart("related");
        relatedPart.addBodyPart(htmlBodyPart);

        StringBuffer cidList = new StringBuffer();
        while (true) {
            // cid approach (RFC2387/2557)
            imgStart = htmlPartValue.toLowerCase().indexOf("<img ", imgStart);
            if (imgStart < 0) {
                break;
            }
            imgEnd = htmlPartValue.indexOf(">", imgStart);
            if (imgEnd < 0) {
                break;
            }
            img = htmlPartValue.substring(imgStart, imgEnd + 1);
            srcStart = img.toLowerCase().indexOf(" src");
            if (srcStart < 0) {
                break;
            }
            srcStart = img.indexOf("\"", srcStart);
            if (srcStart < 0) {
                break;
            }
            srcStart++;
            srcEnd = img.indexOf("\"", srcStart);
            if (srcEnd < 0) {
                break;
            }
            // move forward
            imgStart = imgStart + 1;

            imgSrc = img.substring(srcStart, srcEnd);
            if (imgSrc.startsWith("<cid")) {
                // replaced previously, skip it
                continue;
            }
            try {
                // image part
                MimeBodyPart imageBodyPart = new MimeBodyPart();
                URLDataSource imageDS = new URLDataSource(new URL(imgSrc));
                DataHandler imageDH = new DataHandler(imageDS);
                imageBodyPart.setDataHandler(imageDH);
                //need it? imageBodyPart.setDisposition(Part.INLINE);
                String cid = UUID.randomUUID().toString();
                // Content-ID: <MyUUID>
                imageBodyPart.setContentID("<" + cid + ">");
                relatedPart.addBodyPart(imageBodyPart);

                // replace <img src="http://www.sun.com/logo.gif"> with
                //         <img src="cid:MyUUID">
                String imgReplaced = img.replace(imgSrc, "cid:" + cid);
                htmlPartValue = htmlPartValue.replace(img, imgReplaced);
                I18n.finer(logger, "EMAILBC-2028: Image tag is replaced: {0} ==> {1}",
                        img, imgReplaced);
                //
                if (cidList.length() > 0) {
                    cidList.append(";").append(cid);
                } else {
                    cidList.append(cid);
                }
            } catch (Exception e) {
                I18n.warning(logger, "EMAILBC-6012: Failed to embed/resolve src {0} image {1}", e,
                        imgSrc, img);
                continue;
            } // end of try
        } // end of while

        // is any of src= really resolved/replaced?
        if (cidList.length() > 0) {
            // multipart/related
            container.setContent(relatedPart);
            //
            // text/html
            htmlBodyPart.setText(htmlPartValue, this.mDefaultCharset, "html");
            String htmlBodyPartCid = UUID.randomUUID().toString();
            htmlBodyPart.setHeader("include-cid-list", cidList.toString()); // my own usage
            htmlBodyPart.setContentID("<" + htmlBodyPartCid + ">");
            String containerContentType = container.getContentType();
            // ;start="<MyHtmlBodyPartCid>"
            containerContentType = containerContentType + ";start=\"<" + htmlBodyPartCid + "\">";
        } else {
            // nothing is changed
            // relatedPart is ignored, fall back to regular text/html ...
            container.setText(htmlPartValue, this.mDefaultCharset, "html");
        }
        return;
    }

    private void handleNMAttachments(Multipart masterPart, Set<String> nmAttachmentNames) {
        // We don't care whether it is a regular NM attachment or it is an attachment
        // referenced by a "xop:include" part (created by upstream components e.g. FileBC/HttpSoapBC/etc).
        // Both re supported.
        MimeBodyPart attachmentPart = null;
        for (String name : nmAttachmentNames) {
            try {
                DataHandler dh = this.mNormalizedMessage.getAttachment(name);
                //! the upstream components may construct DataHandler incorrectly, e.g. wrong ContentType,
                //! that will cause transport error
                //! verify it now and correct it if needed
                String contentType = dh.getContentType();
                try {
                    // simply parse it
                    new ContentType(contentType);
                } catch (Exception e) {
                    contentType = "application/octet-stream"; // for unknown type per spec
                    ByteArrayDataSource ds = new ByteArrayDataSource(dh.getInputStream(), contentType);
                    dh = new DataHandler(ds);
                }

                attachmentPart = new MimeBodyPart();
                attachmentPart.setDataHandler(dh);
                // although not fileName, it also sets "name" parameter in header "Content-type" for backward compatibility
                // note: this NM attachment name is out of EmailBC's control,
                // it may look like an executable file name (e.g. abcd@org.glassfish.openesb.com),
                // it may not be allowed by many SMTP server, see http://mail.google.com/support/bin/answer.py?answer=6590
                // append a suffix ".jbi"
                String fileName = name;
                //remove the cid tag.
                if (name.startsWith("cid:")) {
                    fileName = name.substring(4);
                }
                if (fileName.indexOf(".") == -1) {
                    fileName += ".jbi";
                }
                attachmentPart.setFileName(fileName);
                attachmentPart.setDescription("Attachment from JBI Normalized Message");
                masterPart.addBodyPart(attachmentPart);
            } catch (Exception e) {
                I18n.severe(logger, "EMAILBC-7010: Failed to handle NM attachment [{0}]: {1}", e, name, e.getMessage());
                //?throw e;
            }
        }

    }

    private void handleWSDLAttachments(Multipart masterPart, SMTPAttachment[] wsdlAttachments) {
        MimeBodyPart attachmentPart = null;
        SMTPAttachment attachment = null;
        for (int i = 0; i < wsdlAttachments.length; i++) {
            attachment = (SMTPAttachment) wsdlAttachments[i];
            try {
                // "partName" takes precedence over "readFromFile"

                // could be AppVars but not AppConfig nor NM property because we may have multiple attachments
                String attachmentContentPart = EmailUtil.resolveAppVarsValue(attachment.getAttachmentContentPart(), mEndpoint.getContext().getConfiguration());
                String fileName = EmailUtil.resolveAppVarsValue(attachment.getReadFromFile(), mEndpoint.getContext().getConfiguration());
                String contentType = EmailUtil.resolveAppVarsValue(attachment.getContentType(), mEndpoint.getContext().getConfiguration());
                String disposition = EmailUtil.resolveAppVarsValue(attachment.getDisposition(), mEndpoint.getContext().getConfiguration());
                if ((null == attachmentContentPart || 0 == attachmentContentPart.length()) &&
                        (null == fileName || 0 == fileName.length())) {
                    I18n.severe(logger, "EMAILBC-7011: Attachment content is not specified. You can specify it by use of attribute {0} or {1}.", null,
                            SMTPAttachment.ATTR_SMTP_ATTACHMENT_READ_FROM_FILE,
                            SMTPAttachment.ATTR_SMTP_ATTACHMENT_CONTENT_PART);
                    continue;
                }

                String attachmentFileNamePart = attachment.getAttachmentFileNamePart();
                String attachmentFileName = null;
                if (null != attachmentFileNamePart && 0 != attachmentFileNamePart.length()) {
                    ///Element subjectPartObj = this.mJbiMessage.getPart(subjectPartName);
                    NodeList attachmentFileNameNode = this.mWrapperParser.getPartNodes(attachmentFileNamePart);
                    if (null != attachmentFileNameNode && 0 != attachmentFileNameNode.getLength()) {
                        attachmentFileName = EmailUtil.transformToTextPlain(attachmentFileNameNode.item(0), null);
                        I18n.finer(logger, "EMAILBC-2047: Derived from wsdl part: {1}, \"attachmentFileNamePart\" : [{0}].", attachmentFileName, attachmentFileNamePart);
                    } else {
                        I18n.warning(logger, "EMAILBC-6001: The WSDL message does not define corresponding part for input attribute {0}. An empty value is assumed.", attachmentFileNamePart);
                    }
                }
                ;
                //
                ////////////////////////
                // from "readFromFile"
                ////////////////////////
                if (null == attachmentContentPart || 0 == attachmentContentPart.length()) {
                    I18n.finer(logger, "EMAILBC-2029: Derived attachment from file name: [{0}].", fileName);

                    //TODO: apply encoder here

                    attachmentPart = new MimeBodyPart();
                    try {
                        attachmentPart.setDisposition(disposition);
                    } catch (Exception e) {
                        //ignore
                    }

                    // it is ok to simply attachFile(), Content Type will be derived up on JAF typeMap
                    // but intend to apply the configured contentType (although it is rarely needed)
                    //attachmentPart.attachFile(fileName);

                    DataSource ds = new EmailFileDataSource(fileName, contentType);
                    DataHandler dh = new DataHandler(ds);
                    attachmentPart.setDataHandler(dh);
                    attachmentPart.setFileName(ds.getName()); // base name
                    attachmentPart.setDescription("Attachment from a file " + ds.getName());
                    masterPart.addBodyPart(attachmentPart);
                    continue;
                }
                //
                ///////////////////////
                // from "partName"
                ///////////////////////
                String attachmentContentValue = null;
                ///Element partObj = this.mJbiMessage.getPart(partName);
                NodeList attachmentContentNode = this.mWrapperParser.getPartNodes(attachmentContentPart);
                if (null != attachmentContentNode && 0 != attachmentContentNode.getLength()) {
                    Node node = attachmentContentNode.item(0);
                    if (WrapperUtil.isNodeXopInclude(node)) {
                        byte[] bytes = getPartIfAvailableAsAttachment(node, mNormalizedMessage);
                        attachmentContentValue = new String(bytes);
                    } else {
                        attachmentContentValue = EmailUtil.transformToTextPlain(attachmentContentNode.item(0), null);
                    }
                    I18n.finer(logger, "EMAILBC-2030: Derived attachment from message part: [{0}].", attachmentContentValue);
                } else {
                    attachmentContentValue = "";
                    I18n.warning(logger, "EMAILBC-6013: The WSDL message does not define corresponding part for input attachment attribute {0}. An empty value is assumed.", attachmentContentPart);
                }

                try {
                    // simply parse it
                    new ContentType(contentType);
                } catch (Exception e) {
                    contentType = "application/octet-stream"; // for unknown type per spec
                }

                // charset is already covered by contentType for the case "text/*" with charset
                //TODO: apply encoder here
                ByteArrayDataSource ds = new ByteArrayDataSource(attachmentContentValue, contentType);

                DataHandler dh = new DataHandler(ds);
                attachmentPart = new MimeBodyPart();
                try {
                    attachmentPart.setDisposition(disposition);
                } catch (Exception e) {
                    //ignore
                }
                attachmentPart.setDataHandler(dh);
                //If file name is not set, the content part name is set as the file name.
                if (attachmentFileName == null) {
                    attachmentFileName = attachmentContentPart;
                }
                // although not fileName, it also sets "name" parameter in header "Content-type" for backward compatibility
                attachmentPart.setFileName(attachmentFileName);
                attachmentPart.setDescription("Attachment from WSDL message part " + attachmentContentPart);
                masterPart.addBodyPart(attachmentPart);
            } catch (ApplicationVariableNotDefinedException e) {
                I18n.severe(logger, e.getLocalizedMessage(), e);
//                logger.log(Level.SEVERE,
//                        I18n.loc("EMAILBC-9999: Failed to handle WSDL attachment [{0}]: {1}", name, e.getMessage()), e);
//                //?throw e;
            } catch (IOException e) {
                I18n.severe(logger, e.getLocalizedMessage(), e);
            } catch (MessagingException e) {
                I18n.severe(logger, e.getLocalizedMessage(), e);
            } catch (TransformerException e) {
                I18n.severe(logger, e.getLocalizedMessage(), e);
            } catch (WrapperProcessingException e) {
                I18n.severe(logger, e.getLocalizedMessage(), e);
            }
        }

    }

    private String resolveValue(String oldValue,
            String wsdlFieldName,
            String nmPropertyName) throws ApplicationVariableNotDefinedException {

        return EmailUtil.resolveValue(oldValue,
                wsdlFieldName,
                this.mEndpoint.getApplicationConfiguration(),
                nmPropertyName,
                this.mNormalizedMessage, mEndpoint.getContext().getConfiguration());
    }

    private EmailDenormalizerException wrapException(Exception e) {
        EmailDenormalizerException e1 = new EmailDenormalizerException(e.getLocalizedMessage());
        e1.initCause(e);
        return e1;
    }

    private void handleNMPropertyWSDLAttachments(MimeMultipart masterMultipart, String paths) {
        if (!paths.startsWith("\"") || !paths.endsWith("\"")) {
            I18n.severe(logger, "EMAILBC-7030: The attachment file paths attribute value should start and end with double quote '\"' character : {0}", null, paths);
            I18n.severe(logger, "EMAILBC-7037: Some attachments could not be attached", null);
        } else {
            paths = paths.substring(1, paths.length() - 1);
            String[] pathArr = paths.split("\"\\s*\"");
            for (String path : pathArr) {
                try {
                    MimeBodyPart attachmentPart = new MimeBodyPart();
                    try {
                        attachmentPart.setDisposition(MimeBodyPart.ATTACHMENT);
                    } catch (Exception e) {
                        //ignore
                    }
                    // it is ok to simply attachFile(), Content Type will be derived up on JAF typeMap
                    // but intend to apply the configured contentType (although it is rarely needed)
                    attachmentPart.attachFile(path);
                    masterMultipart.addBodyPart(attachmentPart);
                } catch (IOException ex) {
                    I18n.severe(logger, ex.getLocalizedMessage(), ex);
                } catch (MessagingException ex) {
                    I18n.severe(logger, ex.getLocalizedMessage(), ex);
                }
            }
        }
    }

    private String getPartValueForBody(Message wsdlMessage, String bodyPartName) throws WrapperProcessingException, IOException, EmailDenormalizerException, SAXException, ParserConfigurationException, EncoderException, TransformerException, Exception {
        String bodyPartValue = "";
        // if its Binary
        Part bodyPart = wsdlMessage.getPart(bodyPartName);

        NodeList partObj = this.mWrapperParser.getPartNodes(bodyPartName);
        Node node = (Node) partObj.item(0);
        QName bodyPartType = bodyPart.getTypeName();
        // Check if message type is binary
        boolean isBinary = bodyPartType != null &&
                (bodyPartType.equals(BASE64BINARY_1991_QNAME) || bodyPartType.equals(BASE64BINARY_2001_QNAME));
        if (isBinary) {
            if (WrapperUtil.isNodeXopInclude(node)) {
                byte[] bytes = getPartIfAvailableAsAttachment(node, mNormalizedMessage);
                bodyPartValue = new String(bytes);
            } else {
                bodyPartValue = Base64Utils.base64Decode(node.getNodeValue());
            }
        } else {
            if (EmailBCConstants.SMTP_USE_TYPE_ENCODED.equals(this.mOperationInput.getSmtpUseType())) {
                // TODO here
                Map partMappings = this.mEndpoint.getMessagePartEncoderMapping();
                Encoder encoder = (Encoder) partMappings.get(wsdlMessage.getQName() + bodyPartName);
                if (encoder == null) {
                    throw new EmailDenormalizerException(I18n.loc("EMAILBC-7020: Invalid encoding style: [{0}]", this.mOperationInput.getEncodingStyle()));
                }
                if (null != partObj && 0 != partObj.getLength()) {
                    byte[] bytes = getPartIfAvailableAsAttachment(node, mNormalizedMessage);
                    Source source;
                    if (bytes != null) {
                        Document doc = EmailUtil.buildDocument(bytes);
                        source = new DOMSource(doc);
                    } else {
                        source = new DOMSource(node);
                    }
                    bodyPartValue = encoder.encodeToString(source);
                    I18n.finer(logger, "EMAILBC-2025: Derived and encoded from wsdl part, \"Message body\": [{0}].", bodyPartValue);
                } else {
                    I18n.warning(logger, "EMAILBC-6010: The WSDL message does not define part: {0}. An empty value is assumed.", bodyPartName);
                }
            } else {
                if (null != partObj && 0 != partObj.getLength()) {
                    //TODO? try other types? transformToTextXml/Html ?
                    bodyPartValue = EmailUtil.transformToTextPlain(node, null);
                    I18n.finer(logger, "EMAILBC-2026: Derived from wsdl part: {1}, \"Message body\" : [{0}].", bodyPartValue, bodyPartName);
                } else {
                    I18n.warning(logger, "EMAILBC-6010: The WSDL message does not define part: {0}. An empty value is assumed.", bodyPartName);
                }
            }
        }
        return bodyPartValue;
    }

    /**
     * FileDataSource possibly with the specified Content Type
     */
    public static class EmailFileDataSource extends FileDataSource {

        private String mContentType;

        /**
         * @param fileName
         * @param contentType
         */
        public EmailFileDataSource(String fileName, String contentType) {
            super(fileName);
            this.mContentType = contentType;
        }

        /**
         * @see javax.activation.FileDataSource#getContentType()
         */
        @Override
        public String getContentType() {
            try {
                if (null != this.mContentType && 0 != this.mContentType.trim().length()) {
                    new ContentType(this.mContentType);
                    return this.mContentType;
                }
            } catch (Exception e) {
                // fall back to JAF
            }
            return super.getContentType();
        }
    }
}
