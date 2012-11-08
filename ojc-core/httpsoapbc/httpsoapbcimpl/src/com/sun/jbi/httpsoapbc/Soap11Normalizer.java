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
 * @(#)SoapNormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.httpsoapbc;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.activation.DataHandler;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.servlet.http.HttpServletRequest;
import javax.wsdl.Message;
import javax.wsdl.Part;
import javax.wsdl.extensions.mime.MIMEContent;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.soap.AttachmentPart;
import javax.xml.soap.Detail;
import javax.xml.soap.DetailEntry;
import javax.xml.soap.SOAPBody;
import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPEnvelope;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFault;
import javax.xml.soap.SOAPHeader;
import javax.xml.soap.SOAPHeaderElement;
import javax.xml.soap.SOAPMessage;
import javax.xml.soap.SOAPPart;
import javax.xml.transform.dom.DOMSource;
import javax.xml.ws.handler.MessageContext;

import net.java.hulp.measure.Probe;

import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.jbi.httpsoapbc.util.DebugLog;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;
import com.sun.jbi.nms.wsdl11wrapper.util.WrapperUtil;

/**
 *
 */
public class Soap11Normalizer implements SoapNormalizer {

    private static Messages mMessages = Messages.getMessages(Soap11Normalizer.class);
    private static Logger mLog = Messages.getLogger(Soap11Normalizer.class);
    protected boolean mIsHttpHeaderCopyEnabled = true;
    protected boolean mIsSoapHeaderCopyEnabled = true;
    // measurements
    protected Probe mMeasurement = null;
    private static DocumentBuilder docBuilder;
    //private static DocumentBuilderFactory dbf;
    protected static Map<String, String> mStandardNamespaceURIs = new HashMap<String, String>();

    static {
        mStandardNamespaceURIs.put("http://schemas.xmlsoap.org/soap/envelope", "http://schemas.xmlsoap.org/soap/envelope");
        mStandardNamespaceURIs.put("http://www.w3.org/2003/05/soap-envelope", "http://www.w3.org/2003/05/soap-envelope");
        mStandardNamespaceURIs.put("http://www.w3.org/1999/XMLSchema", "http://www.w3.org/1999/XMLSchema");
        mStandardNamespaceURIs.put("http://www.w3.org/1999/XMLSchema-instance", "http://www.w3.org/1999/XMLSchema-instance");
        mStandardNamespaceURIs.put("http://www.w3.org/2001/XMLSchema", "http://www.w3.org/2001/XMLSchema");
        mStandardNamespaceURIs.put("http://www.w3.org/2001/XMLSchema-instance", "http://www.w3.org/2001/XMLSchema-instance");
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        try {
            docBuilder = dbf.newDocumentBuilder();
        } catch (ParserConfigurationException pce) {
            mLog.log(Level.SEVERE, mMessages.getString("HTTPBC-E01056.Failed_to_create_document_builder", pce.getLocalizedMessage()));
        }
    }

    ;

    /** Creates a new instance of SoapNormalizer */
    public Soap11Normalizer() {
    }

    public void setPropagateSoapHeader(boolean propagateSoapHeader) {
        mIsSoapHeaderCopyEnabled = propagateSoapHeader;
    }

    /*
     * (non-Javadoc) @see
     * com.sun.jbi.httpsoapbc.SoapNormalizerI#normalize(java.lang.Object,
     * javax.jbi.messaging.MessageExchange,
     * com.sun.jbi.httpsoapbc.OperationMetaData, boolean,
     * javax.xml.ws.handler.MessageContext)
     */
    public NormalizedMessage normalize(Object message, MessageExchange exchange, OperationMetaData meta, boolean inMsg, MessageContext context) throws MessagingException, SOAPException {
        NormalizedMessage normalMsg = exchange.createMessage();

        try {
            String topic = "normalize" + (inMsg ? "Request" : "Reply");
            mMeasurement = Probe.fine(getClass(), topic);

            WrapperBuilder wrapperBuilder = HelperFactory.createBuilder();
            SOAPMessage soapMessage = (SOAPMessage) message;

            // Get "global" namespace(s) from SOAP envelope
            Map soapEnvelopeNSs = new HashMap();
            SOAPPart soapPart = soapMessage.getSOAPPart();
            if (soapPart != null) {
                SOAPEnvelope soapEnvelope = soapMessage.getSOAPPart().getEnvelope();
                soapEnvelopeNSs = WrapperUtil.extractNamespaceDeclarations(soapEnvelope);
                filterStandardNS(soapEnvelopeNSs);
            }

            // Per namespace mapping rules, add any namespaces(s) from soap:envelope
            // as namespace(s) to the JBI message root element
            wrapperBuilder.declareGlobalNS(soapEnvelopeNSs);
            if (mLog.isLoggable(Level.FINE)) {
                DebugLog.debugLog(mLog, Level.FINE, "SOAP envelope namespaces", soapEnvelopeNSs);
            }

            // Don't create a new document; let the builder use the document 
            // reference from the first part that is added.
            QName type = null;
            String name = null;
            Message msg = null;
            if (inMsg) {
                name = meta.getInMsgName();
                msg = meta.getInputMessage();
            } else {
                name = meta.getOutMsgName();
                msg = meta.getOutputMessage();
            }


            /**
             * Process the SOAP body before the headers. This allows us to
             * re-use the (potentially heavyweight) document object from the
             * body instead of importing it into another document.
             */
            SOAPBody soapBody = soapMessage.getSOAPBody();
            if (mLog.isLoggable(Level.FINE)) {
                DebugLog.debugLog(mLog, Level.FINE, "SOAP body to normalize", soapBody);
            }

            wrapperBuilder.initialize(msg, name, soapBody.getOwnerDocument());

            //wrapperBuilder.initialize( msg, name);
            processSoapBody(wrapperBuilder, soapBody, meta, inMsg);




            SOAPHeader soapHeader = soapMessage.getSOAPHeader();
            if (mLog.isLoggable(Level.FINE)) {
                DebugLog.debugLog(mLog, Level.FINE, "SOAP header to normalize", soapHeader);
            }


            processSoapHeader(wrapperBuilder, normalMsg, soapHeader, meta, inMsg, exchange);


            if (mIsHttpHeaderCopyEnabled && context != null && inMsg) {
                processHTTPRequestHeaders(normalMsg, context);
            }


            processSoapAttachment(wrapperBuilder, soapMessage, meta, inMsg, normalMsg);


            Document normalDoc = wrapperBuilder.getResult();

            if (mLog.isLoggable(Level.FINE)) {
                DebugLog.debugLog(mLog, Level.FINE, "Normalized message", normalDoc);
            }

            normalMsg.setContent(new DOMSource(normalDoc));

        } catch (ParserConfigurationException tex) {
            throw new MessagingException(mMessages.getString("HTTPBC-E00798.Normalize_fail", tex.getLocalizedMessage()), tex);
        } catch (WrapperProcessingException ex) {
            throw new MessagingException(mMessages.getString("HTTPBC-E00798.Normalize_fail", ex.getLocalizedMessage()), ex);
        } catch (ClassCastException ex) {
            String msg = mMessages.getString("HTTPBC-E00797.Normalize_fail_wrong_type",
                    new Object[]{"SOAPMessage", message.getClass().toString()});
            throw new MessagingException(msg, ex);
        } catch (Exception ex) {
            throw new MessagingException(mMessages.getString("HTTPBC-E00798.Normalize_fail", ex.getLocalizedMessage()), ex);
        } finally {
            if (mMeasurement != null) {
                mMeasurement.end();
            }
        }

        return normalMsg;
    }

    public Fault normalizeFault(SOAPMessage soapMessage, MessageExchange exchange, OperationMetaData meta, boolean failFast) throws MessagingException, SOAPException {
        return normalizeFault(soapMessage.getSOAPBody().getFault(), exchange, meta, failFast);
    }

    public Fault normalizeFault(SOAPFault soapFault, MessageExchange exchange, OperationMetaData meta, boolean failFast) throws MessagingException, SOAPException {
        Fault normalFault = exchange.createFault();
        boolean hasMatchingDefinedFault = false;
        boolean hasFaultDetail = false;

        Document normalDoc = docBuilder.newDocument();

        try {

            normalFault.setContent(new DOMSource(normalDoc));

            Element normalRoot = null;

            // Identify fault by comparing the detail element in the
            // soap message to the wsdl fault messages defined
            Detail detail = soapFault.getDetail();
            hasFaultDetail = (detail != null);
            if (hasFaultDetail) {
                Map definedFaults = meta.getFaults();
                Message foundFault = null;
                String foundFaultName = null;
                Part foundPart = null;
                Iterator detailIterator = detail.getDetailEntries();
                while (detailIterator != null && detailIterator.hasNext() && foundFault == null) {
                    DetailEntry de = (DetailEntry) detailIterator.next();
                    String detailName = de.getLocalName();
                    String detailElemURI = de.getElementName().getURI();
                    String detailElemLocalName = de.getElementName().getLocalName();
                    Iterator faultsIter = definedFaults.entrySet().iterator();
                    while (faultsIter.hasNext() && foundFault == null) {
                        Map.Entry entry = (Map.Entry) faultsIter.next();
                        String faultName = (String) entry.getKey();
                        javax.wsdl.Fault wsdlFault = (javax.wsdl.Fault) entry.getValue();
                        Message faultMsg = wsdlFault.getMessage();
                        // Fault message can only have one part
                        Map partNamesToParts = faultMsg.getParts();
                        Iterator partsIter = partNamesToParts.values().iterator();
                        if (partsIter.hasNext()) {
                            Part part = (Part) partsIter.next();
                            QName partElemName = part.getElementName();
                            QName partTypeName = part.getTypeName();
                            // Match soap faults based on element name
                            if (partElemName != null) {
                                String partElemURI = partElemName.getNamespaceURI();
                                String partElemLocalName = partElemName.getLocalPart();
                                // if the namespace matches, and the local name of the detail element
                                // then this fault matches
                                if (((detailElemURI == null && partElemURI == null) || (detailElemURI != null && detailElemURI.equals(partElemURI))) && detailElemLocalName.equals(partElemLocalName)) {
                                    foundFault = faultMsg;
                                    foundFaultName = faultName;
                                    foundPart = part;
                                }
                                // Match soap faults based on type name
                            } else if (partTypeName != null) {
                                // Find an element with the same name as the part
                                if (detailElemLocalName.equals(part.getName())) {
                                    foundFault = faultMsg;
                                    foundFaultName = faultName;
                                    foundPart = part;
                                }
                            }
                        } else {
                            if (mLog.isLoggable(Level.WARNING)) {
                                mLog.log(Level.WARNING, "HTTPBC-W00710.Fault_message_has_no_parts", faultName);
                            }
                        }
                    }

                    hasMatchingDefinedFault = (foundFault != null);
                    if (hasMatchingDefinedFault) {
                        // super! we found the fault message associated with this fault
                        // create normalized message based on defined WSDL message

                        QName type = foundFault.getQName();
                        WrapperBuilder wrapperBuilder = HelperFactory.createBuilder();
                        wrapperBuilder.initialize(normalDoc, foundFault, foundFaultName);

                        if (mLog.isLoggable(Level.FINE)) {
                            mLog.log(Level.FINE, "Found matching WSDL message for detail entry: " + detailName);
                        }

                        // create jbi part wrapper and add to normal doc
                        wrapperBuilder.addPart(foundPart.getName(), de);
                        normalDoc = wrapperBuilder.getResult();
                    } else {
                        // message with this name is not found just get the DetailEntries
                        // and add them into the normalized document
                        if (mLog.isLoggable(Level.FINE)) {
                            mLog.log(Level.FINE, "Did NOT find matching WSDL message for detail entry " + detailName);
                        }

                        // @TODO: should it add wsdl 1.1 wrapper even though not defined in wsdl?
                        // If so, how could it fill in the mandatory type attribute?
                        if (!normalDoc.hasChildNodes()) {
                            normalRoot = normalDoc.createElement(meta.getOperationName());
                            normalDoc.appendChild(normalRoot);
                        }

                        Node n = normalDoc.importNode(de, true);
                        normalRoot.appendChild(n);
                    }
                }
            } else {
                // no details in the fault, its just a generic soap fault
                // create a generic normalized fault
                // @TODO: should it add wsdl 1.1 wrapper even though there is no detail content?
            }
            if (mLog.isLoggable(Level.FINE)) {
                DebugLog.debugLog(mLog, Level.FINE, "SOAP Fault to normalize", normalDoc);
            }
        } catch (WrapperProcessingException ex) {
            throw new MessagingException(mMessages.getString("HTTPBC-E00798.Normalize_fail", ex.getLocalizedMessage()), ex);
        }
        // failFast flag is intended to suppress "hiding" of a fault mismatch
        // (between what is defined in the WSDL, and what is in the actual message).
        // If message has no fault detail, or message has a fault not defined
        // in the web service operation, instead of returning a "fake" Fault,
        // return null pointer, when failFlag is set.
        if (failFast) {
            if (!hasMatchingDefinedFault || !hasFaultDetail) {
                return null;
            }
        }
        return normalFault;
    }

    protected void processSoapAttachment(WrapperBuilder wrapperBuilder, SOAPMessage soapMessage, OperationMetaData metaData, boolean inMsg, NormalizedMessage normalMsg)
            throws Exception {
        Map mimeParts = null;
        if (inMsg) {
            mimeParts = metaData.getInputSOAPMimeContents();
        } else {
            mimeParts = metaData.getOutputSOAPMimeContents();
        }

        //MimeHeaders mimeHeaders = soapMessage.getMimeHeaders();
        Iterator mimeContents = mimeParts.values().iterator();
        while (mimeContents.hasNext()) {
            MIMEContent mimeContent = (MIMEContent) mimeContents.next();
            String mimePart = mimeContent.getPart();
            String mimeType = mimeContent.getType();

            for (Iterator attachments = soapMessage.getAttachments(); attachments.hasNext();) {
                AttachmentPart attachment = (AttachmentPart) attachments.next();
                String contentId = attachment.getContentId();
                String contentType = attachment.getContentType();
                //if (contentId.indexOf(mimePart + "=") >= 0 && contentType.equalsIgnoreCase(mimeType)) {
                if (contentId.indexOf(mimePart + "=") >= 0 && contentType.indexOf(mimeType) >= 0) {
                    // found the mime attachment
                    DataHandler dataHandler = attachment.getDataHandler();
                    // add the message part
                    //wrapperBuilder.addPartWithAttachment(mimePart, contentId);
                    // rely on the WrapperBuilder API to create the cid per the URI scheme
                    String cid = wrapperBuilder.addPartWithAttachment(mimePart);
                    normalMsg.addAttachment(cid, dataHandler);
                }
            }
        }
    }

    protected void processSoapBody(WrapperBuilder wrapperBuilder, SOAPBody soapElement, OperationMetaData meta, boolean inMsg)
            throws WrapperProcessingException, Exception {
        // Per namespace mapping rules, collect all relevant namespaces from
        // soap:body and add them as namespaces to the JBI part
        Map nameSpaces = WrapperUtil.extractNamespaceDeclarations(soapElement);
        if (mLog.isLoggable(Level.FINE)) {
            DebugLog.debugLog(mLog, Level.FINE, "SOAP body namespaces", nameSpaces);
        }

        javax.wsdl.extensions.soap.SOAPBody soapBody = null;
        if (inMsg) {
            soapBody = meta.getInputSoapBody();
        } else {
            soapBody = meta.getOutputSoapBody();
        }
        Message msg = null;
        if (inMsg) {
            msg = meta.getInputMessage();
        } else {
            msg = meta.getOutputMessage();
        }
        String msgTNS = meta.getFullDefinition().getTargetNamespace();
        // IF the soap body is defined as empty, return
        if (soapBody == null) {
            return;
        }
        List bodyParts = soapBody.getParts();
        Iterator bodyIterator = null;
        if (bodyParts != null && bodyParts.size() > 0) {
            // since soap body binding specifies parts, we use only the parts that are in the binding
            bodyIterator = bodyParts.iterator();
        } else {
            // if no parts were specified in the SOAP binding section, we use all the message parts
            // currently doesn't care about order of parts
            if (msg == null) {
                // assert failed
                throw new IllegalStateException("Unexpected state, input WSDLMessage is not defined, however we are attempting to invoke the operation with non-empty input");
            }
            bodyIterator = msg.getParts().keySet().iterator();
            /*
             * For debugging String [] keyArr = (String[])
             * msg.getParts().keySet().toArray(new String[0]); Map.Entry[]
             * entryArr = (Map.Entry[]) msg.getParts().entrySet().toArray(new
             * Map.Entry[0]); for(int i=0;i<keyArr.length;++i) { String key =
             * keyArr[i]; Map.Entry entry = entryArr[i]; mLog.fine("####Debug
             * SoapNormalizer key ["+key+"] entry
             * ["+entry.getKey()+"]["+entry.getValue()+"]"); }
             */
        }
        String soapBodyURI = soapBody.getNamespaceURI();
        boolean isDocument = (meta.isDocumentMode() == true);
        boolean isRPC = (meta.isDocumentMode() == false);
        Element operElem = null;
        if (isRPC) {
            if (soapBodyURI != null) {
                if (mLog.isLoggable(Level.FINER)) {
                    mLog.log(Level.FINER, "SOAP body namespace in use: " + soapBodyURI);
                }
            }
            String operationTNS = soapBodyURI == null ? "*" : soapBodyURI;
            operElem = findResponsePart(soapElement, new QName(operationTNS, meta.getOperationName()), inMsg);
            // look for operation node in soap msg
            // should have only 1 element with operation's name under wrapper
            // get the part node under operation node
            // use namespace to find operation, parts do not have a namespace
            if (operElem == null) {
                mLog.log(Level.WARNING, "HTTPBC-W00711.Soap_message_has_no_match_for_operation", meta.getOperationName());
                throw new WrapperProcessingException(mMessages.getString("HTTPBC-W00711.Soap_message_has_no_match_for_operation",
                        meta.getOperationName()));
            } else {
                // Per namespace mapping rules, collect all relevant namespaces from
                // soap:body operation and add them as namespaces to the JBI part
                Map operElemNSs = WrapperUtil.extractNamespaceDeclarations(operElem);
                if (mLog.isLoggable(Level.FINE)) {
                    DebugLog.debugLog(mLog, Level.FINE, "Operation  " + operElem.getLocalName() + " namespaces", nameSpaces);
                }
                if (operElemNSs != null) {
                    nameSpaces.putAll(operElemNSs);
                }
            }
        }

        while (bodyIterator != null && bodyIterator.hasNext()) {
            Map partNameSpaces;
            Map partAttributes;
            if (nameSpaces != null) {
                partNameSpaces = new HashMap(nameSpaces);
            } else {
                partNameSpaces = new HashMap();
            }

            // initialize the part attributes map
            partAttributes = new HashMap();

            String bodyPart = (String) bodyIterator.next();

            // based on the part name, look up the element/type QName in the WSDLMessage
            if (msg == null) {
                // assert failed
                throw new IllegalStateException("Unexpected state, input WSDLMessage is not defined, however we are attempting to invoke the operation with non-empty input");
            }
            Part part = msg.getPart(bodyPart);
            if (part == null) {
                // assert failed
                String err = mMessages.getString("HTTPBC-E00702.Message_part_bad_reference", new Object[]{bodyPart, msg.getQName().toString()});
                throw new IllegalStateException(err);
            }
            QName typeQName = part.getTypeName();  //wsdl:part of type
            QName elemQName = part.getElementName();  //wsdl:part of element
            boolean isType = typeQName != null;
            boolean isElem = elemQName != null;
            boolean TYPE_DOC = (isType && isDocument);
            boolean TYPE_RPC = (isType && isRPC);
            boolean ELEM_DOC = (isElem && isDocument);
            boolean ELEM_RPC = (isElem && isRPC);
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, "TYPE_DOC: " + TYPE_DOC + " TYPE_RPC: " + TYPE_RPC + " ELEM_DOC: " + ELEM_DOC + " ELEM_RPC: " + ELEM_RPC);
            }

            Element e = null;
            Element msgElem = isDocument ? soapElement : operElem;
            String lookingMsg = isType ? "Looking for part in SOAP message" : "Looking for element in normalized message";
            String nullMsg = isType ? "HTTPBC-W00712.Soap_message_has_no_match_for_part" : "HTTPBC-W00713.Soap_message_has_no_match_for_element";
            String creatingMsg = null;
            String uri = null;
            String localName = null;
            if (TYPE_DOC) {
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, "Style is Document, part " + part.getName() + " is Type, QName " + typeQName.toString());
                }
                creatingMsg = "Creating normalized message for doc-literal mode with type";
                uri = msgTNS;
                localName = part.getName();
            } else if (TYPE_RPC) {
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, "Style is RPC, part " + part.getName() + " is Type, QName " + typeQName.toString());
                }
                creatingMsg = "Creating normalized message for rpc-literal mode with type";
                uri = "*";
                localName = part.getName();
            } else if (ELEM_DOC) {
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, "Style is Document, part " + part.getName() + " is Element, QName " + elemQName.toString());
                }
                creatingMsg = "Creating normalized message for doc-literal mode with element";
                uri = elemQName.getNamespaceURI();
                localName = elemQName.getLocalPart();
            } else if (ELEM_RPC) {
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, "Style is RPC, part " + part.getName() + " is Element, QName " + elemQName.toString());
                }
                creatingMsg = "Creating normalized message for rpc-literal mode with element";
                uri = "*";
                msgElem = findMsgElement(operElem, new QName(uri, bodyPart)); //elemQName.getLocalPart()
                if (msgElem == null) {
                    throw new Exception(mMessages.getString("HTTPBC-E00802.Missing_message_part_in_RPC_element", bodyPart));
                }
                localName = elemQName.getLocalPart();
            }
            QName partQName = new QName(uri, localName);

            if (mLog.isLoggable(Level.FINEST)) {
                mLog.log(Level.FINEST, lookingMsg);
            }
            e = findMsgElement(msgElem, partQName);
            if (e != null) {
                // Per namespace mapping rules, collect all relevant namespaces from
                // the part element of the soap message if the corresponding part is defined as a type
                // in the wsdl; these namespaces will be added to the JBI part
                if (isType) {
                    Map partNSs = WrapperUtil.extractNamespaceDeclarations(e);
                    if (mLog.isLoggable(Level.FINE)) {
                        DebugLog.debugLog(mLog, Level.FINE, "Part  '" + localName + "' namespaces", partNSs);
                    }
                    if (partNSs != null) {
                        partNameSpaces.putAll(partNSs);
                    }
                    // extract any attribute declarations on the part to propagate onto the corresponding jbi:part
                    partAttributes = WrapperUtil.extractAttributeDeclarations(e);
                    if (mLog.isLoggable(Level.FINE)) {
                        DebugLog.debugLog(mLog, Level.FINE, "Part  '" + localName + "' attributes", partAttributes);
                    }
                }
                if (mLog.isLoggable(Level.FINEST)) {
                    mLog.log(Level.FINEST, creatingMsg);
                }
                // add the part node to the normalized document
                if (TYPE_DOC) {
                    // document mode using type
                    // see if the part name can be found in the soap body element
                    // add the part node to the normalized document
                    wrapperBuilder.addPart(part.getName(), e.getChildNodes(), partNameSpaces, partAttributes);
                } else if (TYPE_RPC) {
                    // rpc mode using type
                    // look for operation node in soap msg
                    // should have only 1 element with operation's name under wrapper
                    // get the part node under operation node
                    wrapperBuilder.addPart(part.getName(), e.getChildNodes(), partNameSpaces, partAttributes);
                } else if (ELEM_DOC) {
                    // it is a element
                    // document mode using element
                    // see if the element name can be found in the soap body
                    wrapperBuilder.addPart(part.getName(), e, partNameSpaces);
                } else if (ELEM_RPC) {
                    // rpc mode using element
                    // look for operation node in soap msg
                    // get the part node under operation node
                    // Add the contents under the existing part node in the request to the JBI part wrapper
                    //mWrapperBuilder.addPart(part.getName(), e.getChildNodes());
                    wrapperBuilder.addPart(part.getName(), e, partNameSpaces);
                }
            } else {
                throw new WrapperProcessingException(mMessages.getString(nullMsg, partQName.toString()));
            }
        } // end while
    }

    protected void processSoapHeader(WrapperBuilder wrapperBuilder, NormalizedMessage normalMsg, SOAPHeader soapHeader, OperationMetaData meta, boolean inMsg, MessageExchange mex)
            throws WrapperProcessingException {
        List headerList = null;
        if (inMsg) {
            headerList = meta.getInputSoapHeaders();
        } else {
            headerList = meta.getOutputSoapHeaders();
        }

        // Handle custom reliability protocol.
        // If present, Extract MessageID and place it in the message exchange properties
        if (soapHeader != null) {
            Iterator headerIter = soapHeader.examineAllHeaderElements();
            while (headerIter.hasNext()) {
                SOAPHeaderElement he = (SOAPHeaderElement) headerIter.next();

                if (he.getLocalName().equals(WSRM_Sequence)
                        && (he.getNamespaceURI().equals(WSRM__1_0__NAMESPACE_URI) || he.getNamespaceURI().equals(WSRM__1_1__NAMESPACE_URI))) {
                    // handle ws-rm soap messages
                    propagateWSRMIdsToExchange(he, mex);
                }

                // 11-20-2007 - With the introduction of using WS-RM and the new JBI message properties for grouping/sequencing,
                // the following code in the "if" clause for handling custom reliability may be removed upon further review...
                if (he.getLocalName().equals(CUSTOM_RELIABILITY_HEADER_LOCAL_NAME) && CUSTOM_RELIABILITY_HEADER_NAMESPACE_URI.equals(he.getNamespaceURI())) {
                    String messageID = he.getTextContent();
                    normalMsg.setProperty(CUSTOM_RELIABILITY_MESSAGE_ID_PROPERTY, messageID);
                    if (mLog.isLoggable(Level.FINEST)) {
                        mLog.log(Level.FINEST, "Normalized Message ID set to: " + messageID);
                    }
                }
            }
        }
        // End handling of custom reliability protocol

        // The service does not define any headers.  All headers in the message
        // will be copied to the Normalized Message as properties.
        if (mIsSoapHeaderCopyEnabled && soapHeader != null && (headerList == null || headerList.size() < 1)) {
            for (Iterator headerIter = soapHeader.examineAllHeaderElements();
                    headerIter.hasNext();) {
                addSOAPHeader(normalMsg, (SOAPHeaderElement) headerIter.next());
            }
            return;
        }

        // assumes the message where the header part is defined is the same
        // as the in/out message of the operation.
        // @todo consider the case where header is not a part in the in/out message
        Message wsMessage = null;
        if (inMsg) {
            wsMessage = meta.getInputMessage();
        } else {
            wsMessage = meta.getOutputMessage();
        }
        if (soapHeader != null) {
            // Per namespace mapping rules, collect all relevant namespaces from
            // soap:header and add them as namespaces to the JBI part
            Map nameSpaces = WrapperUtil.extractNamespaceDeclarations(soapHeader);
            if (mLog.isLoggable(Level.FINE)) {
                DebugLog.debugLog(mLog, Level.FINE, "SOAP header namespaces", nameSpaces);
            }

            for (Iterator headerIter = soapHeader.examineAllHeaderElements();
                    headerIter.hasNext();) {

                SOAPHeaderElement elem = (SOAPHeaderElement) headerIter.next();
                boolean headerProcessed = false;

                for (int i = 0; i < headerList.size() && !headerProcessed; ++i) {
                    javax.wsdl.extensions.soap.SOAPHeader headerDef =
                            (javax.wsdl.extensions.soap.SOAPHeader) headerList.get(i);

                    if (headerDef.getMessage().equals(wsMessage.getQName())) {
                        Part part = wsMessage.getPart(headerDef.getPart());
                        if (part == null) {
                            // part not found in the in/out message, currently not supported
                            String msg = mMessages.getString("HTTPBC-W00714.Unsupported_feature_part_in_header_not_in_message", new Object[]{headerDef.getPart(), wsMessage.getQName()});
                            throw new IllegalArgumentException(msg);
                        }
                        QName partElemName = part.getElementName();
                        if (partElemName == null) {
                            // part needs to be defined as element, currently not supported
                            String msg = mMessages.getString("HTTPBC-W00715.Unsupported_feature_part_in_header_not_element", new Object[]{headerDef.getPart(), wsMessage.getQName()});
                            throw new IllegalArgumentException(msg);
                        }
                        if (partElemName.equals(elem.getElementQName())) {
                            Map partNameSpaces;
                            if (nameSpaces != null) {
                                partNameSpaces = new HashMap(nameSpaces);
                            } else {
                                partNameSpaces = new HashMap();
                            }

                            /*
                             * boolean isType = part.getTypeName() != null; if
                             * (isType) { // Per namespace mapping rules,
                             * collect all relevant namespaces from // the part
                             * element of the soap message if the corresponding
                             * part is defined as a type // in the wsdl; these
                             * namespaces will be added to the JBI part Map
                             * partNSs =
                             * WrapperUtil.extractNamespaceDeclarations(elem);
                             * if (partNSs != null) { if
                             * (mLog.isLoggable(Level.FINE)) {
                             * DebugLog.debugLog(mLog, Level.FINE, "SOAP header
                             * element (type) '" + elem.getLocalName() + "'
                             * namespaces", partNSs); }
                             * partNameSpaces.putAll(partNSs); } }
                             */
                            wrapperBuilder.addPart(part.getName(), elem, partNameSpaces);
                            headerProcessed = true;
                        }
                    }
                }
                if (mIsSoapHeaderCopyEnabled && !headerProcessed) {
                    addSOAPHeader(normalMsg, elem);
                }
            }
        }
    }

    protected void addSOAPHeader(NormalizedMessage msg, SOAPHeaderElement elem) throws WrapperProcessingException {
        Map<String, DocumentFragment> propertyMap = null;
        propertyMap = (Map<String, DocumentFragment>) msg.getProperty(NormalizedMessageProperties.SOAP_HEADER_PROPERTY);
        if (propertyMap == null) {
            propertyMap = new HashMap<String, DocumentFragment>();
            msg.setProperty(NormalizedMessageProperties.SOAP_HEADER_PROPERTY, propertyMap);
        }




        Document document = null;
        DocumentFragment documentFragment = null;

        String name = elem.getElementQName().toString();
        if (propertyMap.get(name) != null && propertyMap.get(name) instanceof DocumentFragment) {
            documentFragment = (DocumentFragment) propertyMap.get(name);
        } else {

            document = docBuilder.newDocument();
            documentFragment = document.createDocumentFragment();

        }

        Node imported = documentFragment.getOwnerDocument().importNode(elem, true);
        documentFragment.appendChild(imported);
        propertyMap.put(name, documentFragment);

    }

    protected Element findResponsePart(Element soapElement, QName operationQName, boolean inMsg) throws Exception {
        Element e = null;
        //Util.toXml(soapElement.getOwnerDocument(),"UTF-8", false);
        // The BP 1.0 clarifies in R2729 that for RPC literal, the response wrapper should be operation name + "Response"
        // Therefore for operation output messages check for an element matching this pattern first.
        if (!inMsg) {
            e = findMsgElement(soapElement, new QName(operationQName.getNamespaceURI(),
                    operationQName.getLocalPart() + SoapDenormalizer.RPC_RESPONSE_SUFFIX));
        }

        // For operation input messages, or if the output message wrapper is not named in a BP compliant manner
        // look for the wrapper named according to the operation name only
        if (e == null) {
            e = findMsgElement(soapElement, operationQName);
        }

        return e;
    }

    protected Element findMsgElement(Element root, QName elemQName) throws Exception {
        if (mLog.isLoggable(Level.FINER)) {
            mLog.log(Level.FINER, "Finding part for root", elemQName.toString());
            DebugLog.debugLog(mLog, Level.FINER, "", root);
        }
        boolean ignoreNS = elemQName.getNamespaceURI().equals("*") ? true : false;

        // We only care about the next level.
        NodeList nl = root.getChildNodes();
        int nodeLen = nl.getLength();
        if (mLog.isLoggable(Level.FINEST)) {
            mLog.log(Level.FINEST, "Found " + nl.getLength() + " nodes");
        }
        if (nl != null && nl.getLength() > 0) {
            for (int i = 0; i < nodeLen; i++) {
                Object obj = nl.item(i);
                if (!(obj instanceof Element)) {
                    continue;
                }
                Element e2 = (Element) obj;
                if (mLog.isLoggable(Level.FINEST)) {
                    mLog.log(Level.FINEST, "Looking for element namespace [" + e2.getNamespaceURI() + "] localname [" + e2.getLocalName() + "]");
                    //DebugLog.debugLog(mLog, Level.FINEST, "", e2);
                }
                if (e2.getLocalName().equals(elemQName.getLocalPart())) {
                    String e2NS = e2.getNamespaceURI() == null ? "" : e2.getNamespaceURI();
                    if (ignoreNS || e2NS.equals(elemQName.getNamespaceURI())) {
                        if (mLog.isLoggable(Level.FINER)) {
                            mLog.log(Level.FINEST, "Found element with namespace [" + e2.getNamespaceURI() + "] localname [" + e2.getLocalName() + "]");
                            DebugLog.debugLog(mLog, Level.FINER, "Returning element", e2);
                        }
                        return e2;
                    }
                }
            }
        }
        return null;
    }

    protected void propagateWSRMIdsToExchange(SOAPHeaderElement wsrmSequenceElem, MessageExchange mex) {
        boolean sequenceIdentifierFound = false;
        // First try locating the sequence identifier from the wsrm 1.0 namespace
        Iterator identifierIter = wsrmSequenceElem.getChildElements(QNAME_WSRM__1_0__Sequence_Identifier);
        while (identifierIter.hasNext()) {
            SOAPElement identifierElem = (SOAPElement) identifierIter.next();
            String sequenceIdentifier = identifierElem.getValue();
            mex.setProperty(CRMP_GROUP_ID, sequenceIdentifier);
            sequenceIdentifierFound = true;
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, "The soap message is ws-rm 1.0 enabled message; populated "
                        + SoapNormalizer.CRMP_GROUP_ID + " with value ["
                        + sequenceIdentifier + "] for message exchange with ID [" + mex.getExchangeId() + "]");
            }
            break;
        }

        // If sequence identifier not found, try the wsrm 1.1 namespace
        if (!sequenceIdentifierFound) {
            identifierIter = wsrmSequenceElem.getChildElements(this.QNAME_WSRM__1_1__Sequence_Identifier);
            while (identifierIter.hasNext()) {
                SOAPElement identifierElem = (SOAPElement) identifierIter.next();
                String sequenceIdentifier = identifierElem.getValue();
                mex.setProperty(CRMP_GROUP_ID, sequenceIdentifier);
                sequenceIdentifierFound = true;
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, "The soap message is ws-rm 1.1 enabled message; populated "
                            + SoapNormalizer.CRMP_GROUP_ID + " with value ["
                            + sequenceIdentifier + "] for message exchange with ID [" + mex.getExchangeId() + "]");
                }
                break;
            }
        }

        boolean sequenceMessageNumberFound = false;
        // First try locating the sequence messagenumber from the wsrm 1.0 namespace
        Iterator messageNumberIter = wsrmSequenceElem.getChildElements(QNAME_WSRM__1_0__Sequence_MessageNumber);
        while (messageNumberIter.hasNext()) {
            SOAPElement messageNumberElem = (SOAPElement) messageNumberIter.next();
            String sequenceMessageNumber = messageNumberElem.getValue();
            mex.setProperty(CRMP_MESSAGE_ID, sequenceMessageNumber);
            sequenceMessageNumberFound = true;
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, "The soap message is ws-rm 1.0 enabled message; populated "
                        + SoapNormalizer.CRMP_MESSAGE_ID + " with value ["
                        + sequenceMessageNumber + "] for message exchange with ID [" + mex.getExchangeId() + "]");
            }
            break;
        }

        // If sequence messagenumber not found, try the wsrm 1.1 namespace
        if (!sequenceMessageNumberFound) {
            messageNumberIter = wsrmSequenceElem.getChildElements(QNAME_WSRM__1_1__Sequence_MessageNumber);
            while (messageNumberIter.hasNext()) {
                SOAPElement messageNumberElem = (SOAPElement) messageNumberIter.next();
                String sequenceMessageNumber = messageNumberElem.getValue();
                mex.setProperty(CRMP_MESSAGE_ID, sequenceMessageNumber);
                sequenceMessageNumberFound = true;
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, "The soap message is ws-rm 1.1 enabled message; populated "
                            + SoapNormalizer.CRMP_MESSAGE_ID + " with value ["
                            + sequenceMessageNumber + "] for message exchange with ID [" + mex.getExchangeId() + "]");
                }
                break;
            }
        }
    }

    private void copyNode(Node src, Node dest) {
        NodeList nl = src.getChildNodes();
        for (int i = 0; i < nl.getLength(); i++) {
            Node n = nl.item(i);
            n = dest.getOwnerDocument().importNode(n, true);
            dest.appendChild(n);
        }

        NamedNodeMap attrs = src.getAttributes();
        NamedNodeMap destAttrs = dest.getAttributes();
        if (attrs != null) {
            for (int i = 0; i < attrs.getLength(); i++) {
                Node attr = attrs.item(i);
                Node n = dest.getOwnerDocument().importNode(attr, true);
                if (destAttrs != null) {
                    destAttrs.setNamedItemNS(n);
                }
            }
        }
    }

    // Copies HTTP request headers from the context, into the normalized
    // message.
    private void processHTTPRequestHeaders(NormalizedMessage normalMsg, MessageContext context) {
        Map httpHeadersProperty = (Map) normalMsg.getProperty(NormalizedMessageProperties.INBOUND_HTTP_HEADERS_PROPERTY);
        if (httpHeadersProperty == null) {
            httpHeadersProperty = new HashMap<String, String>();
            normalMsg.setProperty(NormalizedMessageProperties.INBOUND_HTTP_HEADERS_PROPERTY, httpHeadersProperty);
        }
        Map requestHeaders = (Map) context.get(MessageContext.HTTP_REQUEST_HEADERS);
        if (requestHeaders != null) {
            for (Iterator iter = requestHeaders.entrySet().iterator(); iter.hasNext();) {
                Map.Entry entry = (Map.Entry) iter.next();
                String key = (String) entry.getKey();
                // JAX-WS Packet implementation populates each header's value
                // as a single value contained in a singleton list.
                String value = (String) ((List) entry.getValue()).get(0);
                httpHeadersProperty.put(key, value);
            }
        }

        // let's also populate the client host information in this property
        if (context.get(MessageContext.SERVLET_REQUEST) != null) {
            HttpServletRequest servletRequest = (HttpServletRequest) context.get(MessageContext.SERVLET_REQUEST);
            httpHeadersProperty.put(NormalizedMessageProperties.HTTP_HEADER_ClIENT_HOST_NAME, servletRequest.getRemoteAddr());
            httpHeadersProperty.put(NormalizedMessageProperties.HTTP_HEADER_CLIENT_PORT_NUMBER, Integer.valueOf(servletRequest.getRemotePort()).toString());
        }

    }

    private void filterStandardNS(Map namespaces) {
        List standardNS = new ArrayList();

        for (Iterator entries = namespaces.keySet().iterator(); entries.hasNext();) {
            String prefix = (String) entries.next();
            String nsURI = ((String) namespaces.get(prefix)).trim();
            nsURI = nsURI.endsWith("/") ? new StringBuffer(nsURI).deleteCharAt(nsURI.length() - 1).toString() : nsURI;
            if (mStandardNamespaceURIs.containsKey(nsURI)) {
                standardNS.add(prefix);
            }
        }

        if (standardNS.size() > 0) {
            for (int ii = 0; ii < standardNS.size(); ii++) {
                namespaces.remove(standardNS.get(ii));
            }
        }
    }
}
