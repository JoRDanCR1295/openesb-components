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
 * @(#)EmailNormalizer.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.binding.email.protocol.receive;

import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;
import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Iterator;

import java.util.logging.Level;
import javax.mail.MessagingException;
import javax.mail.Multipart;
import javax.mail.Part;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeUtility;
import javax.mail.internet.MimeMessage.RecipientType;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.namespace.QName;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.dom.DOMSource;

import org.w3c.dom.Document;
import org.w3c.dom.Text;

import com.sun.jbi.binding.email.EmailBCEndpoint;
import com.sun.jbi.binding.email.I18n;
import com.sun.jbi.binding.email.protocol.ApplicationVariableNotDefinedException;
import com.sun.jbi.binding.email.protocol.EmailBCConstants;
import com.sun.jbi.binding.email.protocol.EmailBindingComponentConfigurationException;
import com.sun.jbi.binding.email.protocol.EmailUtil;
import com.sun.jbi.binding.email.protocol.wsdl.EmailAttachment;
import com.sun.jbi.binding.email.protocol.wsdl.EmailOperation;
import com.sun.jbi.binding.email.protocol.wsdl.EmailOperationInput;
import com.sun.jbi.binding.email.protocol.wsdl.IMAPAttachment;
import com.sun.jbi.binding.email.protocol.wsdl.IMAPOperationInput;
import com.sun.jbi.binding.email.protocol.wsdl.POP3Attachment;
import com.sun.jbi.binding.email.protocol.wsdl.POP3OperationInput;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.impl.NodeListImpl;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.logging.Logger;
import javax.activation.DataHandler;
import javax.mail.internet.MimeMessage;
import javax.mail.util.ByteArrayDataSource;
import javax.xml.transform.Source;

import static com.sun.jbi.binding.email.protocol.EmailBCConstants.EmailNMProperty;

/**
 * @author Harry Liu (Harry.Liu@sun.com)
 */
public class EmailNormalizer {

    private static final Logger logger = Logger.getLogger(EmailNormalizer.class.getName());
    private javax.mail.Message mMimeMsg;
    private QName mOperationName;
    private EmailBCEndpoint mEndpoint;
    // internal usage
    private String mMailBody;
    private boolean mGotMailBody = false;
    private Document mNormDocument;
    private EmailOperationInput mOperationInput;
    private Message mWsdlMessageDef;
    ///private WSMessage mJbiMessage;
    private WrapperBuilder mWrapperBuilder;
    private ArrayList<Part> mEmailAttachmentParts;
    private Map<String, Object> mNMProperties = new HashMap<String, Object>();
    private Map<String, DataHandler> mNMAttachments = new HashMap<String, DataHandler>();

    public EmailNormalizer(javax.mail.Message mimeMsg,
            QName operationName,
            EmailBCEndpoint endpoint) {
        this.mMimeMsg = mimeMsg;
        this.mOperationName = operationName;
        this.mEndpoint = endpoint;
    }

    public Source getSource() throws EmailNormalizerException {
        // validate
        EndpointInfo info = this.mEndpoint.getInfo();
        // <service name="imapService1">
        Service service = mEndpoint.getDefinition().getService(info.getServiceName());
        // <port name="imapPort1" binding="tns:imapBinding1">
        Port port = service.getPort(info.getEndpointName());
        // <binding name="imapBinding1" type="tns:imapPortType1">
        // <portType name="imapPortType1">
        PortType portType = port.getBinding().getPortType();

        // Grab the operation that matches the operationName.
        Iterator it = portType.getOperations().iterator();
        while (it.hasNext()) {
            // <operation name="imapOperation1">
            Operation op = (Operation) it.next();
            if (op.getName().equals(mOperationName.toString()) ||
                    op.getName().equals(mOperationName.getLocalPart())) {
                // <input name="input1" message="tns:imapOperationRequest1"/>
                // <message name="imapOperationRequest1">
                this.mWsdlMessageDef = op.getInput().getMessage();
            }
        }

        // <email:IMAPoperation/>
        EmailOperation operation = (EmailOperation) mEndpoint.getEmailOperations().get(mOperationName);

        // <email:IMAPinput message="bodyPart" subject="subjectPart" from="fromPart" to="toPart"/>
        this.mOperationInput = operation.getInput();
        try {
            ///Document document = EmailUtil.newDocument();
            ///this.mJbiMessage = MessageUtil.createWSMessage(new DOMSource(document), wsdlMessage);
            this.mNormDocument = EmailUtil.newDocument();
            this.mWrapperBuilder = HelperFactory.createBuilder();
            this.mWrapperBuilder.initialize(null, this.mWsdlMessageDef, null);
            this.mEmailAttachmentParts = new ArrayList<Part>();
            handleMessageHeaders();
            handleMessageContent(mMimeMsg); // might be called recursively
            handleMessageAttachments();
            return new DOMSource(this.mWrapperBuilder.getResult());
        } catch (ParserConfigurationException ex) {
            throw wrapException(ex);
        } catch (WrapperProcessingException ex) {
            throw wrapException(ex);
        } catch (MessagingException ex) {
            throw wrapException(ex);
        } catch (UnsupportedEncodingException ex) {
            throw wrapException(ex);
        } catch (IOException ex) {
            throw wrapException(ex);
        } catch (ApplicationVariableNotDefinedException ex) {
            throw wrapException(ex);
        } catch (javax.jbi.messaging.MessagingException ex) {
            throw wrapException(ex);
        } catch (EmailBindingComponentConfigurationException ex) {
            throw wrapException(ex);
        }
    }

    public Map<String, Object> getNMProperties() {
        return mNMProperties;
    }

    private EmailNormalizerException wrapException(Exception e) {
        EmailNormalizerException e1 = new EmailNormalizerException(e.getLocalizedMessage());
        e1.initCause(e);
        return e1;
    }

    private void handleMessageHeaders() throws WrapperProcessingException {

        handleHeaderSubject();
        handleHeaderFrom();
        handleHeaderTo();
        handleHeaderCc();
        handleHeaderBcc();
        handleHeaderNewsgroups();
    }

    private void handleHeaderSubject() throws WrapperProcessingException {
        String partName = this.mOperationInput.getSubject();
        if (null == partName) {
            return;
        }

        String partValue = "";
        try {
            partValue = mMimeMsg.getSubject();
        } catch (MessagingException e) {
            // continue
        }
        if (null == partValue) {
            partValue = "";
        }

        Text textNode = mNormDocument.createTextNode(partValue);
        this.mWrapperBuilder.addPart(partName, new NodeListImpl(textNode));
        ///Element part = WrapperUtil.createJBIWrappedPart(document, textNode);
        ///jbiMessage.setPart(partName, part);

    }

    private void handleHeaderFrom() throws WrapperProcessingException {
        String partName = this.mOperationInput.getFrom();
        if (null == partName) {
            return;
        }

        String partValue = "";
        try {
            partValue = InternetAddress.toString(mMimeMsg.getFrom());
        } catch (MessagingException e) {
            // continue
        }
        if (null == partValue) {
            partValue = "";
        }

        Text textNode = mNormDocument.createTextNode(partValue);
        this.mWrapperBuilder.addPart(partName, new NodeListImpl(textNode));
        ///Element part = WrapperUtil.createJBIWrappedPart(document, textNode);
        ///jbiMessage.setPart(partName, part);

    }

    private void handleHeaderTo() throws WrapperProcessingException {
        String partName = this.mOperationInput.getTo();
        if (null == partName) {
            return;
        }

        String partValue = "";
        try {
            partValue = InternetAddress.toString(mMimeMsg.getRecipients(RecipientType.TO));
        } catch (MessagingException e) {
            // continue
        }
        if (null == partValue) {
            partValue = "";
        }

        Text textNode = mNormDocument.createTextNode(partValue);
        this.mWrapperBuilder.addPart(partName, new NodeListImpl(textNode));
        ///Element part = WrapperUtil.createJBIWrappedPart(document, textNode);
        ///jbiMessage.setPart(partName, part);

    }

    private void handleHeaderCc() throws WrapperProcessingException {
        String partName = this.mOperationInput.getCc();
        if (null == partName) {
            return;
        }

        String partValue = "";
        try {
            partValue = InternetAddress.toString(mMimeMsg.getRecipients(RecipientType.CC));
        } catch (MessagingException e) {
            // continue
        }
        if (null == partValue) {
            partValue = "";
        }

        Text textNode = mNormDocument.createTextNode(partValue);
        this.mWrapperBuilder.addPart(partName, new NodeListImpl(textNode));
        ///Element part = WrapperUtil.createJBIWrappedPart(document, textNode);
        ///jbiMessage.setPart(partName, part);

    }

    private void handleHeaderBcc() throws WrapperProcessingException {
        String partName = this.mOperationInput.getBcc();
        if (null == partName) {
            return;
        }

        String partValue = "";
        try {
            partValue = InternetAddress.toString(mMimeMsg.getRecipients(RecipientType.BCC));
        } catch (MessagingException e) {
            // continue
        }
        if (null == partValue) {
            partValue = "";
        }

        Text textNode = mNormDocument.createTextNode(partValue);
        this.mWrapperBuilder.addPart(partName, new NodeListImpl(textNode));
        ///Element part = WrapperUtil.createJBIWrappedPart(document, textNode);
        ///jbiMessage.setPart(partName, part);

    }

    private void handleHeaderNewsgroups() throws WrapperProcessingException {
        String partName = this.mOperationInput.getNewsgroups();
        if (null == partName) {
            return;
        }

        String partValue = "";
        try {
            partValue = InternetAddress.toString(mMimeMsg.getRecipients(RecipientType.NEWSGROUPS));
        } catch (MessagingException e) {
            // continue
        }
        if (null == partValue) {
            partValue = "";
        }

        Text textNode = mNormDocument.createTextNode(partValue);
        this.mWrapperBuilder.addPart(partName, new NodeListImpl(textNode));
        ///Element part = WrapperUtil.createJBIWrappedPart(document, textNode);
        ///jbiMessage.setPart(partName, part);

    }

    private void handleMessageContent(Part container)
            throws MessagingException, IOException, WrapperProcessingException {

        if (container.isMimeType("text/*")) {
            if (container.isMimeType("text/html")) {
                handleTextHtml(container);
            } else if (container.isMimeType("text/xml")) {
                handleTextXml(container);
            } else {
                handleTextPlain(container);
            }
        } else if (container.isMimeType("image/*")) {
            // no special handling
            handleTextPlain(container);
        } else if (container.isMimeType("audio/*")) {
            // no special handling
            handleTextPlain(container);
        } else if (container.isMimeType("video/*")) {
            // no special handling
            handleTextPlain(container);
        } else if (container.isMimeType("application/*")) {
            if (container.isMimeType("application/xml")) {
                handleTextXml(container);
            } else {
                handleTextPlain(container);
            }
        } else if (container.isMimeType("multipart/*")) {
            if (container.isMimeType("multipart/alternative")) {
                handleMultipartAlternative(container);
            } else if (container.isMimeType("multipart/related")) {
                handleMultipartRelated(container);
            } else {
                handleMultipartMixed(container);
            }
        } else if (container.isMimeType("message/*")) {
            if (container.isMimeType("message/rfc822")) {
                // no special handling?
                handleTextPlain(container);
            } else {
                handleTextPlain(container);
            }
        } else if (container.isMimeType("x-*/*")) {
            // no special handling
            handleTextPlain(container);
        } else {
            // unknown ... treat as text/plain or error
            handleTextPlain(container);
        }

    }

    /**
     * It handles "text/plain" content type and
     * it also works as a fall-back for many other content types.
     * @param container
     * @throws Exception
     */
    private void handleTextPlain(Part container) throws MessagingException, IOException, WrapperProcessingException {
        // container could be MimeMessage, MimePart, MimeBodyPart, Multipart

        String partName = this.mOperationInput.getMessage();
        if (null == partName) {
            // possible if users don't want to see the email body
            this.mGotMailBody = true;
            return;
        }

        if (Part.ATTACHMENT.equalsIgnoreCase(container.getDisposition())) {
            // it is an attachment
            return;
        }

        boolean doTransform = false; //?? revisit later
        if (doTransform) {
            this.mMailBody = EmailUtil.transformToTextPlain(container.getInputStream(), container.getContentType());
        } else {
            Object content = container.getContent();
            if (!(content instanceof String)) {
                // could be some unknown content types
                this.mMailBody = EmailUtil.toText(container.getInputStream(), null);
            } else {
                // common case
                this.mMailBody = (String) content;
            }
        }

        I18n.finer(logger, "EMAILBC-2040: Content type: '{0}'. Text: [{1}].",
                container.getContentType(),
                this.mMailBody);

        Text textNode = this.mNormDocument.createTextNode(this.mMailBody);
        this.mWrapperBuilder.addPart(partName, new NodeListImpl(textNode));
        ///Element part = WrapperUtil.createJBIWrappedPart(this.mNormDocument, textNode);
        ///this.mJbiMessage.setPart(partName, part);

        this.mGotMailBody = true;

        return;
    }

    private void handleTextHtml(Part container) throws MessagingException, IOException, WrapperProcessingException {
        // container could be MimeMessage, MimePart, MimeBodyPart, Multipart

        String partName = this.mOperationInput.getMessage();
        if (null == partName) {
            // possible if users don't want to see the email body
            this.mGotMailBody = true;
            return;
        }

        if (Part.ATTACHMENT.equalsIgnoreCase(container.getDisposition())) {
            // it is an attachment
            return;
        }

        boolean doTransform = false; //?? revisit later
        if (doTransform) {
            this.mMailBody = EmailUtil.transformToTextHtml(container.getInputStream());
        } else {
            Object content = container.getContent();
            if (!(content instanceof String)) {
                // could be some unknown content types
                this.mMailBody = EmailUtil.toText(container.getInputStream(), null);
            } else {
                // common case
                this.mMailBody = (String) content;
            }
        }

        I18n.finer(logger, "EMAILBC-2041: Content type: '{0}'. Html: [{1}].",
                container.getContentType(),
                this.mMailBody);

        Text textNode = this.mNormDocument.createTextNode(this.mMailBody);
        this.mWrapperBuilder.addPart(partName, new NodeListImpl(textNode));
        ///Element part = WrapperUtil.createJBIWrappedPart(this.mNormDocument, textNode);
        ///this.mJbiMessage.setPart(partName, part);

        this.mGotMailBody = true;

        return;
    }

    private void handleTextXml(Part container) throws MessagingException, IOException, WrapperProcessingException {
        // container could be MimeMessage, MimePart, MimeBodyPart, Multipart

        String partName = this.mOperationInput.getMessage();
        if (null == partName) {
            // possible if users don't want to see the email body
            this.mGotMailBody = true;
            return;
        }

        if (Part.ATTACHMENT.equalsIgnoreCase(container.getDisposition())) {
            // it is an attachment
            return;
        }

        boolean doTransform = false; //?? revisit later
        if (doTransform) {
            this.mMailBody = EmailUtil.transformToTextXml(container.getInputStream());
        } else {
            Object content = container.getContent();
            if (!(content instanceof String)) {
                // could be some unknown content types
                this.mMailBody = EmailUtil.toText(container.getInputStream(), null);
            } else {
                // common case
                this.mMailBody = (String) content;
            }
        }

        I18n.finer(logger, "EMAILBC-2042: Content type: '{0}'. Xml: [{1}].",
                container.getContentType(),
                this.mMailBody);

        Text textNode = this.mNormDocument.createTextNode(this.mMailBody);
        this.mWrapperBuilder.addPart(partName, new NodeListImpl(textNode));
        ///Element part = WrapperUtil.createJBIWrappedPart(this.mNormDocument, textNode);
        ///this.mJbiMessage.setPart(partName, part);

        /*
        //TODO: check xml anyType, element IMAPMessage and POP3Message, type tIMAPMessage and tPOP3Message
        javax.wsdl.Part wsdlPart = this.mWsdlMessageDef.getPart(this.mOperationInput.getMessage());
        if (null != wsdlPart.getElementName()) {
        QName elementName = wsdlPart.getElementName();
        //is it IMAPMessage or POP3Message?
        // if yes, special handling ...
        Document document = EmailUtil.newDocument();
        Element element = document.createElement(...);
        // this element will have structure of IMAPMessage and POP3Message
        // its sub elements are: subject, to, from, body, attachment, etc
        // element.body = this.mMessageBody;
        } else {
        QName typeName = wsdlPart.getTypeName();
        //is it tIMAPMessage or tPOP3Message?
        // if yes, special handling ... populate IMAPMessage or POP3Message
        }
         */

        this.mGotMailBody = true;

        return;
    }

    private void handleMultipartAlternative(Part container) throws MessagingException, IOException, WrapperProcessingException {
        // container could be MimeMessage, MimePart, MimeBodyPart, Multipart

        if (Part.ATTACHMENT.equalsIgnoreCase(container.getDisposition())) {
            // it is an attachment
            return;
        }

        Object content = container.getContent();
        if (!(content instanceof Multipart)) {
            // something must be wrong
            // best try
            handleTextPlain(container);
        } else {
            Multipart multipart = (Multipart) container.getContent();
            int partsCount = multipart.getCount();
            Part partUnderstand = null;
            Part thisPart = null;
            for (int i = 0; i < partsCount; i++) {
                // favor "text/xml", then "application/xml",
                // then "text/html", then "text/plain", then "multipart/*"
                thisPart = multipart.getBodyPart(i);
                if (thisPart.isMimeType("text/xml")) {
                    partUnderstand = thisPart;
                    break;
                }
                if (thisPart.isMimeType("application/xml")) {
                    if (null == partUnderstand) {
                        partUnderstand = thisPart;
                        continue;
                    }
                    continue;
                }
                if (thisPart.isMimeType("text/html")) {
                    if (null == partUnderstand) {
                        partUnderstand = thisPart;
                        continue;
                    }
                    if (!partUnderstand.isMimeType("application/xml")) {
                        // overwrite
                        partUnderstand = thisPart;
                    }
                    continue;
                }
                if (thisPart.isMimeType("text/plain")) {
                    if (null == partUnderstand) {
                        partUnderstand = thisPart;
                        continue;
                    }
                    if (!partUnderstand.isMimeType("application/xml") &&
                            !partUnderstand.isMimeType("text/html")) {
                        // overwrite
                        partUnderstand = thisPart;
                    }
                    continue;
                }
                if (thisPart.isMimeType("multipart/*")) {
                    if (null == partUnderstand) {
                        partUnderstand = thisPart;
                        continue;
                    }
                    if (!partUnderstand.isMimeType("application/xml") &&
                            !partUnderstand.isMimeType("text/html") &&
                            !partUnderstand.isMimeType("text/plain")) {
                        // overwrite
                        partUnderstand = thisPart;
                    }
                }
            } // end of for

            // the best try for non-understand type
            if (null == partUnderstand) {
                partUnderstand = multipart.getBodyPart(partsCount - 1);
            }

            ////////////////////////
            // recursive call
            ////////////////////////
            this.handleMessageContent(partUnderstand);

        } // end of if

    }

    private void handleMultipartRelated(Part container) throws MessagingException, IOException, WrapperProcessingException {
        // container could be MimeMessage, MimePart, MimeBodyPart, Multipart

        if (Part.ATTACHMENT.equalsIgnoreCase(container.getDisposition())) {
            // it is an attachment
            return;
        }

        Object content = container.getContent();
        if (!(content instanceof Multipart)) {
            // something must be wrong
            // best try
            handleTextPlain(container);
        } else {
            Multipart multipart = (Multipart) container.getContent();
            int partsCount = multipart.getCount();
            Part part = null;
            // the first part is text/html, the other parts are the images
            for (int i = 0; i < partsCount; i++) {
                part = multipart.getBodyPart(i);
                if (0 == i) { // the first one
                    handleTextHtml(part);
                    continue;
                }
                // TODO: more handling ... refer denormalizer
                this.mEmailAttachmentParts.add(part);

            } // end of for
        }

    }

    private void handleMultipartMixed(Part container) throws MessagingException, IOException, WrapperProcessingException {
        // container could be MimeMessage, MimePart, MimeBodyPart, Multipart

        if (Part.ATTACHMENT.equalsIgnoreCase(container.getDisposition())) {
            // it is an attachment
            return;
        }

        Object content = container.getContent();
        if (!(content instanceof Multipart)) {
            // something must be wrong
            // best try
            handleTextPlain(container);
        } else {

            Multipart multipart = (Multipart) container.getContent();
            int partsCount = multipart.getCount();
            Part part = null;
            for (int i = 0; i < partsCount; i++) {
                part = multipart.getBodyPart(i);
                if (!this.mGotMailBody) {

                    ////////////////////////
                    // recursive call
                    ////////////////////////
                    this.handleMessageContent(part);
                    // now this.mGotMailBody should be true
                    continue;
                }

                //?handle attachments only for level-1 parts ... too strict?
                //?if (container instanceof MimeMessage) {
                if (!container.isMimeType("mutipart/alternative")) {
                    this.mEmailAttachmentParts.add(part);
                }
                //?}
            }
        }

    }

    private void handleMessageAttachments() throws ApplicationVariableNotDefinedException, MessagingException, WrapperProcessingException, UnsupportedEncodingException, IOException, javax.jbi.messaging.MessagingException, EmailBindingComponentConfigurationException {
        String booleanString = Boolean.toString(this.mOperationInput.getHandleNMAttachments());
        booleanString = this.resolveValue(
                booleanString, EmailOperationInput.ATTR_EMAIL_HANDLE_NM_ATTACHMENTS);
        boolean handleNMAttach = Boolean.parseBoolean(booleanString);
        String saveToDir = null;
        if (this.mOperationInput instanceof IMAPOperationInput) {
            saveToDir = ((IMAPOperationInput) this.mOperationInput).getSaveAttachmentsToDir();
            saveToDir = this.resolveValue(saveToDir, IMAPOperationInput.ATTR_IMAP_SAVE_ATTACHMENTS_TO_DIR);
        } else if (this.mOperationInput instanceof POP3OperationInput) {
            saveToDir = ((POP3OperationInput) this.mOperationInput).getSaveAttachmentsToDir();
            saveToDir = this.resolveValue(saveToDir, POP3OperationInput.ATTR_POP3_SAVE_ATTACHMENTS_TO_DIR);
        } else {
            throw new EmailBindingComponentConfigurationException(I18n.loc("EMAILBC-7015: Invalid operation input definition in WSDL: {0}. It must be IMAPOperationInput or POP3OperationInput.",
                    this.mOperationInput.getElementType()));
        }
        if (saveToDir != null) {
            saveToDir = saveToDir.trim();
            if (!saveToDir.endsWith(File.separator)) {
                saveToDir += File.separator;
            }
            String messageId = ((MimeMessage) mMimeMsg).getMessageID();

            Date currentDate = new Date();
            saveToDir += "s" + currentDate.getTime();//;
        }

        Part attachment = null;
        for (int i = 0; i < this.mEmailAttachmentParts.size(); i++) {
            attachment = this.mEmailAttachmentParts.get(i);
            if (handleNMAttach) {
                //TODO: Reenable this after the QOS lib is fixed to handle attachments.
                //this.handleNMAttachment(attachment);
            }
            if (null != saveToDir && !"".equals(saveToDir)) {
                this.handleWSDLAttachmentDir(attachment, saveToDir);
            }
        }
        mNMProperties.put(EmailNMProperty.INBOUND_ATTACHMENTS_COUNT.getKey(), Integer.valueOf(mEmailAttachmentParts.size()));
        String paths = (String) mNMProperties.get(EmailBCConstants.EmailNMProperty.INBOUND_ATTACHMENTS.getKey());
        if (paths == null) {
            mNMProperties.put(EmailNMProperty.INBOUND_ATTACHMENTS.getKey(), "");
        } else if (paths.endsWith(",")) {
            paths = paths.substring(0, paths.length() - 1);
            mNMProperties.put(EmailNMProperty.INBOUND_ATTACHMENTS.getKey(), paths);
        }
    }

    private void handleNMAttachment(Part attachment) throws MessagingException, WrapperProcessingException, javax.jbi.messaging.MessagingException {
        String attachmentName = attachment.getFileName();
        if (null == attachmentName || "".equals(attachmentName)) {
            attachmentName = UUID.randomUUID().toString();
        }

        // note: Email may have multiple attachments with same name,
        //       while NM attachment should have unique name
        String append = "";
        int seq = 0;
        while (null != getNMAttachments().get(attachmentName + append)) {
            // the name is occupied, try another one
            seq++;
            append = "" + seq;
        }
        attachmentName = attachmentName + append;

        //??? Some downstream components (e.g. BpelSE/FileBC/HttpSoapBC) assume that
        // NM attachments are referenced "xop:include" message parts, otherwise NM
        // attachments are ignored or even NM gets messy (bpelse bug).
        // To optionally feed these components for backward compatibility, make "xop:include" part as an option. ???
        boolean xopInclude = true;
        if (xopInclude) {
            attachmentName = "cid:" + attachmentName;
            String messagePartName = mOperationInput.getAttachmentsPart();//"AttachmentPart"; // hard-coded? serve multiple parts
            this.mWrapperBuilder.addPartWithAttachment(messagePartName, attachmentName);
        }

        ByteArrayDataSource ds;
        try {
            ds = new ByteArrayDataSource(attachment.getDataHandler().getInputStream(), attachment.getContentType());
            getNMAttachments().put(attachmentName, new DataHandler(ds));
        } catch (IOException ex) {
            Logger.getLogger(EmailNormalizer.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    private void handleWSDLAttachmentDir(Part attachment, String saveToDir) throws MessagingException, UnsupportedEncodingException, IOException {
        // part content to file
        //String partContent = EmailUtil.toString(attachment.getInputStream());
        File saveToDirectory = new File(saveToDir);
        if (!saveToDirectory.exists()) {
            if (!saveToDirectory.mkdirs()) {
                I18n.severe(logger, "EMAILBC-7031: Cannot create the directory structure \"{0}\".", null, saveToDir);
            }
        }
        String attachmentName = attachment.getFileName();
        if (null == attachmentName || "".equals(attachmentName)) {
            attachmentName = "Attachment";
        } else {
            attachmentName = MimeUtility.decodeText(attachmentName);
        }

        int seq = 0;
        String append = "";
        File file = null;
        while (true) {
            file = new File(saveToDir, attachmentName + append);
            if (!file.exists()) {
                break;
            }
            seq++;
            append = "" + seq;
        }

        if (attachment instanceof MimeBodyPart) {
            ((MimeBodyPart) attachment).saveFile(file);
            String attachments = "";
            if (mNMProperties.containsKey(EmailNMProperty.INBOUND_ATTACHMENTS.getKey())) {
                attachments = (String) mNMProperties.get(EmailNMProperty.INBOUND_ATTACHMENTS.getKey());
            }
            attachments += file.toURI().toURL() + ",";
            mNMProperties.put(EmailNMProperty.INBOUND_ATTACHMENTS.getKey(), attachments);
        } else {
            // no reason to come here
            // but we can write file on our own code if needed
            }
    }

    private String resolveValue(String oldValue, String wsdlFieldName) throws ApplicationVariableNotDefinedException {
        String newValue = oldValue;
        newValue = EmailUtil.resolveAppConfigValue(newValue, wsdlFieldName, this.mEndpoint.getApplicationConfiguration());
        newValue = EmailUtil.resolveAppVarsValue(newValue, mEndpoint.getContext().getConfiguration());
        return newValue;
    }

    private void tbd_handleWSDLAttachment(Part attachment, EmailAttachment wsdlAttachmentInput) throws EmailBindingComponentConfigurationException, IOException, MessagingException, WrapperProcessingException {
        String partName = null;
        String saveToFile = null;
        if (wsdlAttachmentInput instanceof IMAPAttachment) {
            IMAPAttachment imapAttachment = (IMAPAttachment) wsdlAttachmentInput;
            partName = imapAttachment.getPartName();
            saveToFile = imapAttachment.getSaveToFile();
        } else if (wsdlAttachmentInput instanceof POP3Attachment) {
            POP3Attachment imapAttachment = (POP3Attachment) wsdlAttachmentInput;
            partName = imapAttachment.getPartName();
            saveToFile = imapAttachment.getSaveToFile();
        } else {
            throw new EmailBindingComponentConfigurationException(I18n.loc("EMAILBC-7016: Invalid attachment definition in WSDL: {0}. " +
                    "It must be IMAPAttachment or POP3Attachment.",
                    wsdlAttachmentInput.getElementType()));
        }

        if (null != partName && !"".equals(partName)) {
            tbd_handleWSDLAttachmentPart(attachment, partName);
        }

        if (null != saveToFile && !"".equals(saveToFile)) {
            tbd_handleWSDLAttachmentFile(attachment, saveToFile);
        }

    }

    private void tbd_handleWSDLAttachmentPart(Part attachment, String partName) throws IOException, MessagingException, WrapperProcessingException {
        // part content to jbi message part
        String partContent = EmailUtil.toText(attachment.getInputStream(), null);
        Text textNode = this.mNormDocument.createTextNode(partContent);
        this.mWrapperBuilder.addPart(partName, new NodeListImpl(textNode));
    }

    private void tbd_handleWSDLAttachmentFile(Part attachment, String saveToFile) throws IOException, MessagingException {
        // part content to file
        //String partContent = EmailUtil.toString(attachment.getInputStream());
        boolean isDir = false;
        if (saveToFile.endsWith("/") || saveToFile.endsWith("\\")) {
            isDir = true;
        }
        String fullFileName = "";
        if (isDir) {
            fullFileName = saveToFile + attachment.getFileName();
        } else {
            fullFileName = saveToFile;
        }

        new File(fullFileName).getParentFile().mkdirs();

        int seq = 0;
        String append = "";
        while (new File(fullFileName + append).exists()) {
            seq++;
            append = "" + seq;
        }

        if (attachment instanceof MimeBodyPart) {
            ((MimeBodyPart) attachment).saveFile(fullFileName);
        } else {
            // no reason to come here
            // but we can write file on our own code if needed
        }
    }

    /**
     * @return the mNMAttachments
     */
    public Map<String, DataHandler> getNMAttachments() {
        return mNMAttachments;
    }
}
