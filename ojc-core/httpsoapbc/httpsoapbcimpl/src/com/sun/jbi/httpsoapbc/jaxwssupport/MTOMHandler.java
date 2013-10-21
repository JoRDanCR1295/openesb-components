package com.sun.jbi.httpsoapbc.jaxwssupport;

import com.sun.jbi.common.util.Base64;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.wsdl11wrapper.util.WrapperUtil;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.Iterator;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.namespace.QName;
import javax.xml.soap.AttachmentPart;
import javax.xml.soap.MimeHeader;
import javax.xml.soap.MimeHeaders;
import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPEnvelope;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPMessage;
import javax.xml.ws.handler.MessageContext;
import javax.xml.ws.handler.soap.SOAPHandler;
import javax.xml.ws.handler.soap.SOAPMessageContext;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 *
 * @author David BRASSELY brasseld(at)gmail.com
 */
public class MTOMHandler implements SOAPHandler<SOAPMessageContext> {

    private static final Logger mLogger = Messages.getLogger(MTOMHandler.class);
    private static final String XMIME_NS = "http://www.w3.org/2005/05/xmlmime";
    private static final String XOP_INCLUDE_NS = "http://www.w3.org/2004/08/xop/include";
    private static final String CONTENT_TYPE_ATTR = "contentType";
    private static final String XOP_ELEMENT = "xop";
    private static final String XOP_INCLUDE = "Include";

    @Override
    public Set<QName> getHeaders() {
        return null;
    }

    @Override
    public boolean handleMessage(SOAPMessageContext context) {

        try {
            SOAPEnvelope envelope = context.getMessage().getSOAPPart().getEnvelope();
            resolveBase64Content(context.getMessage(), envelope.getBody());

            context.getMessage().saveChanges();
        } catch (Exception e) {
            e.printStackTrace();
        }
        //    }
        return true;
    }

    private void resolveBase64Content(SOAPMessage soapMessage, Node node) throws Exception {

        NodeList nl = node.getChildNodes();
        for (int i = 0; i < nl.getLength(); i++) {
            Node n = nl.item(i);

            if (n instanceof SOAPElement) {
                SOAPElement elt = (SOAPElement) n;

                // By default, JAXWS translate MTOM as inline base64
                if (Base64.isBase64(getFirstLevelTextContent(n))) {
                    addAttachmentPart(soapMessage, elt);
                } else if (getFirstLevelTextContent(n).startsWith("cid:")) {
                    // We are currently handling SwaRef
                    addAttachmentFromSwaRef(soapMessage, elt);
                } else {
                    resolveBase64Content(soapMessage, n);
                }
            } else {
                resolveBase64Content(soapMessage, n);
            }
        }
    }

    public static String getFirstLevelTextContent(Node node) {
        NodeList list = node.getChildNodes();
        StringBuilder textContent = new StringBuilder();
        for (int i = 0; i < list.getLength(); ++i) {
            Node child = list.item(i);
            if (child.getNodeType() == Node.TEXT_NODE) {
                textContent.append(child.getTextContent());
            }
        }
        return textContent.toString();
    }

    private void addAttachmentFromSwaRef(SOAPMessage soapMessage, SOAPElement elt) {
        try {
            // New attachment reference
            String newContentId = WrapperUtil.createXopCid();
            String contentId = getFirstLevelTextContent(elt);

            AttachmentPart oldAttachment = soapMessage.getAttachment(elt);

            if (oldAttachment == null) {
                // Retrieve the attachment from swaref
                for (Iterator attachments = soapMessage.getAttachments(); attachments.hasNext();) {
                    AttachmentPart part = (AttachmentPart) attachments.next();

                    if (part.getContentId().contains(contentId)) {
                        oldAttachment = part;
                        break;
                    }
                }
            }

            // Create a new attachment part.
            AttachmentPart attachment = soapMessage.createAttachmentPart();
            attachment.setContentId(newContentId);
            attachment.setContentType(oldAttachment.getContentType());
            attachment.setDataHandler(oldAttachment.getDataHandler());

            // Remove old attachment
            MimeHeaders headers = new MimeHeaders();
            for (Iterator mimeHeaders = oldAttachment.getAllMimeHeaders(); mimeHeaders.hasNext();) {
                MimeHeader mimeHeader = (MimeHeader) mimeHeaders.next();
                headers.addHeader(mimeHeader.getName(), mimeHeader.getValue());
            }
            soapMessage.removeAttachments(headers);

            // Copy mime headers from old attachment
            for (Iterator mimeHeaders = oldAttachment.getAllMimeHeaders(); mimeHeaders.hasNext();) {
                MimeHeader mimeHeader = (MimeHeader) mimeHeaders.next();
                attachment.addMimeHeader(mimeHeader.getName(), mimeHeader.getValue());
            }

            soapMessage.addAttachmentPart(attachment);

            // Remove inline content
            elt.setTextContent("");

            // And replace it with XOP:include
            SOAPElement cc = soapMessage.getSOAPBody().addChildElement(new QName(XOP_INCLUDE_NS, XOP_INCLUDE, XOP_ELEMENT));
            cc.addAttribute(new QName("href"), attachment.getContentId());
            elt.appendChild(cc);

        } catch (SOAPException ex) {
            ex.printStackTrace();
        }
    }

    private void addAttachmentPart(SOAPMessage soapMessage, Element elt) {
        try {
            // New attachment reference
            String contentId = WrapperUtil.createXopCid();

            // Create a new attachment part.
            AttachmentPart attachment = soapMessage.createAttachmentPart();
            attachment.setBase64Content(new ByteArrayInputStream(
                    elt.getTextContent().getBytes()), null);
            attachment.setContentId(contentId);

            AttachmentPart oldAttachment = resolveAttachment(soapMessage, attachment.getSize());

            if (oldAttachment != null) {

                attachment.setContentType(oldAttachment.getContentType());
                attachment.setDataHandler(oldAttachment.getDataHandler());
                // Remove old attachment
                MimeHeaders headers = new MimeHeaders();
                for (Iterator mimeHeaders = oldAttachment.getAllMimeHeaders(); mimeHeaders.hasNext();) {
                    MimeHeader mimeHeader = (MimeHeader) mimeHeaders.next();
                    headers.addHeader(mimeHeader.getName(), mimeHeader.getValue());
                }
                soapMessage.removeAttachments(headers);

                // Copy mime headers from old attachment
                for (Iterator mimeHeaders = oldAttachment.getAllMimeHeaders(); mimeHeaders.hasNext();) {
                    MimeHeader mimeHeader = (MimeHeader) mimeHeaders.next();
                    attachment.addMimeHeader(mimeHeader.getName(), mimeHeader.getValue());
                }

            } else {

                mLogger.log(Level.INFO, "Content type attribute from element : {0}", elt.getAttributeNS(XMIME_NS, CONTENT_TYPE_ATTR));

                String contentType = elt.getAttributeNS(XMIME_NS, CONTENT_TYPE_ATTR);

                //If soap response have xmlmime:contentType specified, set it accordingly
                if (contentType != null && contentType.trim().length() > 0) {
                    attachment.setContentType(contentType);
                } else { // other wise set octet-stream
                    attachment.setContentType("application/octet-stream");
                }
            }

            soapMessage.addAttachmentPart(attachment);
            // Remove inline content
            elt.setTextContent("");

            // And replace it with XOP:include
            SOAPElement cc = soapMessage.getSOAPBody().addChildElement(new QName(XOP_INCLUDE_NS, XOP_INCLUDE, XOP_ELEMENT));
            cc.addAttribute(new QName("href"), attachment.getContentId());
            elt.appendChild(cc);

        } catch (SOAPException ex) {
            ex.printStackTrace();
        }
    }

    private AttachmentPart resolveAttachment(SOAPMessage soapMessage, long size) throws SOAPException {
        for (Iterator attachments = soapMessage.getAttachments(); attachments.hasNext();) {
            AttachmentPart attachment = (AttachmentPart) attachments.next();

            if (attachment.getSize() == size) {
                return attachment;
            }
        }

        return null;
    }

    @Override
    public boolean handleFault(SOAPMessageContext context) {
        return true;
    }

    @Override
    public void close(MessageContext context) {
    }
}