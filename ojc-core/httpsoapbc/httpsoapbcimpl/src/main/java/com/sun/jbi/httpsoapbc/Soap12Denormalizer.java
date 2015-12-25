package com.sun.jbi.httpsoapbc;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.Message;
import javax.wsdl.Part;
import javax.xml.namespace.QName;
import javax.xml.soap.Detail;
import javax.xml.soap.DetailEntry;
import javax.xml.soap.Name;
import javax.xml.soap.SOAPBody;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFault;
import javax.xml.soap.SOAPHeader;
import javax.xml.soap.SOAPHeaderElement;
import javax.xml.soap.SOAPMessage;

import net.java.hulp.measure.Probe;

import org.w3c.dom.Attr;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.jbi.httpsoapbc.util.DebugLog;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.wsdl11wrapper.WrapperParser;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;
import com.sun.jbi.nms.wsdl11wrapper.util.WrapperUtil;

public class Soap12Denormalizer extends Soap11Denormalizer {

    private static Messages mMessages = Messages.getMessages(Soap12Denormalizer.class);
    private static Logger mLog = Messages.getLogger(Soap12Denormalizer.class);

    @Override
    protected void processSoapFault(NormalizedMessage nm, Element normalRoot, SOAPMessage outSoapMessage, OperationMetaData meta, boolean inMsg, WrapperParser wrapperParser) throws MessagingException,
	    SOAPException, WrapperProcessingException {
	// TODO Auto-generated method stub
	processSoap12Fault(nm, normalRoot, outSoapMessage, meta, inMsg, wrapperParser);
    }

    @Override
    protected void processSoapHeader(MessageExchange exchange, SOAPMessage outSoapMessage, OperationMetaData meta, boolean inMsg, WrapperParser wrapperParser, Map jbiMessageNSs)
	    throws MessagingException, SOAPException, WrapperProcessingException {
	// TODO Auto-generated method stub
	processSoap12Header(exchange, outSoapMessage, meta, inMsg, wrapperParser, jbiMessageNSs);
    }

    private void processSoap12Header(MessageExchange exchange, SOAPMessage outSoapMessage, OperationMetaData meta, boolean inMsg, WrapperParser wrapperParser, Map jbiMessageNSs)
	    throws MessagingException, SOAPException, WrapperProcessingException {
	List headerList = null;
	if (inMsg) {
	    headerList = meta.getInputSoapHeaders();
	} else {
	    headerList = meta.getOutputSoapHeaders();
	}

	// TODO: this is just a work-around for testing
	if (outSoapMessage.getSOAPHeader() == null) {
	    outSoapMessage.getSOAPPart().getEnvelope().addHeader();
	}

	SOAPHeader soapHeader = outSoapMessage.getSOAPHeader();

	// Handle custom reliability protocol.
	// If a message ID is present on the "in" message message exchange
	// properties, populate a header with it
	// By always looking in the "in" message, it will result in the header
	// getting added to replies in request/reply scenarios as well
	NormalizedMessage inNormalMsg = exchange.getMessage(IN_MSG);
	if (inNormalMsg != null) {
	    String messageID = (String) inNormalMsg.getProperty(SoapNormalizer.CUSTOM_RELIABILITY_MESSAGE_ID_PROPERTY);
	    if (messageID != null) {
		Name msgIDHeaderName = mSoap12Factory.createName(SoapNormalizer.CUSTOM_RELIABILITY_HEADER_LOCAL_NAME, null, SoapNormalizer.CUSTOM_RELIABILITY_HEADER_NAMESPACE_URI);
		SOAPHeaderElement headerElem = soapHeader.addHeaderElement(msgIDHeaderName);
		headerElem.setTextContent(messageID);
	    }
	}
	// End handling of custom reliability protocol

	// Create header elements for every SOAP header stored in the normalized
	// message
	if (mIsHeaderCopyEnabled) {
	    if (inMsg) { // inbound
		extractSOAPHeadersProperty(soapHeader, inNormalMsg);
	    } else {
		NormalizedMessage outNormalMsg = exchange.getMessage(OUT_MSG);
		if (outNormalMsg != null) { // got a "regular" response
		    // propagate custom headers to the response envelope
		    extractSOAPHeadersProperty(soapHeader, outNormalMsg); 
		}
	    }
	}

	if ((headerList == null || headerList.size() < 1) && inNormalMsg.getProperty(SoapNormalizer.CUSTOM_RELIABILITY_MESSAGE_ID_PROPERTY) == null) {
	    // there is nothing to be done here because no headers are defined
	    return;
	}

	// assumes the message where the header part is defined is the same
	// as the in/out message of the operation.
	// @todo consider the case where header is not a part in the in/out
	// message
	Message wsMessage = null;
	if (inMsg) {
	    wsMessage = meta.getInputMessage();
	} else {
	    wsMessage = meta.getOutputMessage();
	}

	// for each one of the declared headers, see if we can find the
	// corresponding part
	// in the normalized message
	for (int i = 0; i < headerList.size(); i++) {
	    javax.wsdl.extensions.soap12.SOAP12Header headerDef = (javax.wsdl.extensions.soap12.SOAP12Header) headerList.get(i);
	    String partName = headerDef.getPart();

	    if (wrapperParser.hasPart(partName)) {
		// look up the part in the msg
		Part wsdlPart = wsMessage.getPart(partName);

		if (wsdlPart == null) {
		    String msg = mMessages.getString("HTTPBC-W00714.Unsupported_feature_part_in_header_not_in_message", new Object[] { partName, wsMessage.getQName() });
		    throw new IllegalStateException(msg);
		}
		QName partElemQName = wsdlPart.getElementName();
		if (partElemQName == null) {
		    String msg = mMessages.getString("HTTPBC-W00715.Unsupported_feature_part_in_header_not_element", new Object[] { partName, wsMessage.getQName() });
		    mLog.log(Level.WARNING, msg);
		}
		Name name = mSoap12Factory.createName(partElemQName.getLocalPart(), partElemQName.getPrefix(), partElemQName.getNamespaceURI());
		SOAPHeaderElement headerElem = soapHeader.addHeaderElement(name);
		// it is document literal mode with elements
		// after we have the part node, it should contain one child with
		// the element QName

		Node e2 = null;
		// remove the JBI wrapper and check that it contains the
		// expected element
		NodeList unwrappedList = wrapperParser.getPartNodes(partName);
		if (unwrappedList != null) {
		    for (int j = 0; j < unwrappedList.getLength(); j++) {
			Node unwrapped = (Node) unwrappedList.item(j);
			if (unwrapped.getLocalName() != null && unwrapped.getLocalName().equals(partElemQName.getLocalPart())) {
			    e2 = unwrapped;
			    break;
			}
		    }

		    if (e2 != null) {
			copyNode(e2, headerElem);

			// Per namespace mapping rules, collect all relevant
			// namespaces from
			// the JBI part wrapper element and copy these
			// namespaces to the soap message part element
			Map partElemNSs;
			if (jbiMessageNSs != null) {
			    partElemNSs = new HashMap(jbiMessageNSs);
			} else {
			    partElemNSs = new HashMap();
			}
			Element jbiWrappedPart = wrapperParser.getWrappedPart(partName);
			if (jbiWrappedPart != null) {
			    Map jbiWrappedPartNSs = WrapperUtil.extractNamespaceDeclarations(jbiWrappedPart);
			    if (jbiWrappedPartNSs != null) {
				if (mLog.isLoggable(Level.FINE)) {
				    DebugLog.debugLog(mLog, Level.FINE, "JBI part '" + partName + "' namespaces", jbiWrappedPartNSs);
				}
				partElemNSs.putAll(jbiWrappedPartNSs);
			    }
			}
			copyNameSpaces(headerElem, partElemNSs);
		    } else {
			String err = mMessages.getString("HTTPBC-W00703.Part_non_element_type_not_supported", new Object[] { partElemQName.getLocalPart(),
				wsMessage.getQName().toString() });
			throw new IllegalArgumentException(err);
		    }
		}
	    } else {
		String msg = mMessages.getString("HTTPBC-W00701.Message_has_no_match_for_part", partName);
		mLog.warning(msg);
	    }
	}

	if (mLog.isLoggable(Level.FINE)) {
	    DebugLog.debugLog(mLog, Level.FINE, "Denormalized SOAP header", soapHeader);
	}
    }

    @Override
    protected void processSoapBody(SOAPMessage outSoapMessage, OperationMetaData meta, boolean inMsg, WrapperParser wrapperParser, Map jbiMessageNSs, NormalizedMessage normalizedMessage) 
         throws MessagingException, SOAPException, WrapperProcessingException, Exception {
	// TODO Auto-generated method stub
	processSoap12Body(outSoapMessage, meta, inMsg, wrapperParser, jbiMessageNSs, normalizedMessage);
    }

    private void processSoap12Body(SOAPMessage outSoapMessage, OperationMetaData meta, boolean inMsg, WrapperParser wrapperParser, Map jbiMessageNSs, NormalizedMessage normalizedMessage) 
        throws MessagingException, SOAPException, WrapperProcessingException, Exception {
	if (meta.isDocumentMode()) {
	    processDoc12Body(outSoapMessage, meta, inMsg, wrapperParser, jbiMessageNSs, normalizedMessage);
	} else {
	    processRpc12Body(outSoapMessage, meta, inMsg, wrapperParser, jbiMessageNSs, normalizedMessage);
	}
    }

    private void processDoc12Body(SOAPMessage outSoapMessage, OperationMetaData meta, boolean inMsg, WrapperParser wrapperParser, Map jbiMessageNSs, NormalizedMessage normalizedMessage) 
        throws MessagingException, SOAPException, WrapperProcessingException, Exception {

	SOAPBody bodyElement = outSoapMessage.getSOAPBody();
	// based on the part name, look up the element/type QName in the
	// WSDLMessage
	Message msg = null;
	if (inMsg) {
	    msg = meta.getInputMessage();
	} else {
	    msg = meta.getOutputMessage();
	}

	// look up the part used as soap body from the operation meta-data.
	javax.wsdl.extensions.soap12.SOAP12Body soapBody = null;
	if (inMsg) {
	    soapBody = meta.getInputSoap12Body();
	} else {
	    soapBody = meta.getOutputSoap12Body();
	}

	List bodyParts = soapBody.getParts();
	Iterator bodyIterator = null;
	if (bodyParts != null && bodyParts.size() > 0) {
	    // since soap body binding specifies parts, we use only the parts
	    // that are in the binding
	    bodyIterator = bodyParts.iterator();
	} else {
	    bodyIterator = msg.getOrderedParts(null).iterator();
	}
	while (bodyIterator != null && bodyIterator.hasNext()) {
	    Part part = null;
	    Object value = bodyIterator.next();
	    String partName;
	    if (value instanceof String) {
		String bodyPart = partName = (String) value;
		part = msg.getPart(bodyPart);
	    } else {
		part = (Part) value;
		partName = (part == null ? null : part.toString());
	    }

	    if (msg == null) {
		// assert failed
		throw new IllegalStateException("Unexpected state, input WSDLMessage is not defined, however we are attempting to invoke the operation with non-empty input");
	    }

	    if (part == null) {
		// assert failed
		String err = mMessages.getString("HTTPBC-W00703.Part_non_element_type_not_supported", new Object[] { partName, msg.getQName().toString() });
		throw new IllegalStateException(err);
	    }
	    QName typeQName = part.getTypeName();
	    QName elemQName = part.getElementName();
	    if (typeQName != null) {
		// means its a type
		// see if the part name can be found in the normalized message
		if (mLog.isLoggable(Level.FINEST)) {
		    mLog.log(Level.FINEST, "Looking for part " + part.getName() + " in normalized message");
		}

		if (wrapperParser.hasPart(part.getName())) {
		    // it is document literal mode with types
		    if (mLog.isLoggable(Level.FINEST)) {
			mLog.log(Level.FINEST, "Creating SOAP node in document-literal mode with types");
		    }
		    // Add a part element, remove the JBI wrapper and add
		    // content to part element
		    Element srcPart = wrapperParser.getWrappedPart(part.getName());
		    Element partElem = bodyElement.getOwnerDocument().createElement(part.getName());
		    copyNode(srcPart, partElem);
		    bodyElement.appendChild(partElem);

		    // Per namespace mapping rules, collect all relevant
		    // namespaces from
		    // the JBI part wrapper element and copy these namespaces to
		    // the soap message part element
		    Map partElemNSs;
		    if (jbiMessageNSs != null) {
			partElemNSs = new HashMap(jbiMessageNSs);
		    } else {
			partElemNSs = new HashMap();
		    }
		    if (srcPart != null) {
			Map jbiWrappedPartNSs = WrapperUtil.extractNamespaceDeclarations(srcPart);
			if (jbiWrappedPartNSs != null) {
			    if (mLog.isLoggable(Level.FINE)) {
				DebugLog.debugLog(mLog, Level.FINE, "JBI part '" + part.getName() + "' namespaces", jbiWrappedPartNSs);
			    }
			    partElemNSs.putAll(jbiWrappedPartNSs);
			}
		    }
		    copyNameSpaces(partElem, partElemNSs);
		} else {
		    mLog.log(Level.WARNING, "HTTPBC-W00701.Message_has_no_match_for_part", part.getName());
		}
	    } else if (elemQName != null) {
		// it is of element type
		// see if the part name can be found in the normalized message
		if (mLog.isLoggable(Level.FINEST)) {
		    mLog.log(Level.FINEST, "Looking for part " + part.getName() + " in normalized message");
		}

		if (wrapperParser.hasPart(part.getName())) {
		    // it is document literal mode with elements
		    // after we have the part node, it should contain one child
		    // with the element QName

		    Node e2 = null;
		    // remove the JBI wrapper and check that it contains the
		    // expected element
		    NodeList unwrappedList = wrapperParser.getPartNodes(part.getName());
		    if (unwrappedList != null) {
			Probe m = Probe.fine(getClass(), "findDocumentElement");
			for (int i = 0; i < unwrappedList.getLength(); i++) {
			    Node unwrapped = (Node) unwrappedList.item(i);
			    if (WrapperUtil.isNodeXopInclude(unwrapped)) {
                            	e2 = processNodesAsAttachment(elemQName, unwrapped, normalizedMessage, bodyElement);
                            } else if (elemQName.getLocalPart().equals(unwrapped.getLocalName())) {
                                e2 = unwrapped;
                            }
                            
			    if (e2 != null) {
				if (mLog.isLoggable(Level.FINEST)) {
				    mLog.log(Level.FINEST, "Creating SOAP message for document-literal mode with elements");
				}
				Node n = bodyElement.getOwnerDocument().importNode(e2, true);
				bodyElement.appendChild(n);
				if (n instanceof Element) {
				    // Per namespace mapping rules, collect all
				    // relevant namespaces from
				    // the JBI part wrapper element and copy
				    // these namespaces to the soap message part
				    // element
				    Map partElemNSs;
				    if (jbiMessageNSs != null) {
					partElemNSs = new HashMap(jbiMessageNSs);
				    } else {
					partElemNSs = new HashMap();
				    }
				    Element jbiWrappedPart = wrapperParser.getWrappedPart(part.getName());
				    if (jbiWrappedPart != null) {
					Map jbiWrappedPartNSs = WrapperUtil.extractNamespaceDeclarations(jbiWrappedPart);
					if (jbiWrappedPartNSs != null) {
					    if (mLog.isLoggable(Level.FINE)) {
						DebugLog.debugLog(mLog, Level.FINE, "JBI part '" + part.getName() + "' namespaces", jbiWrappedPartNSs);
					    }
					    partElemNSs.putAll(jbiWrappedPartNSs);
					}
				    }
				    copyNameSpaces((Element) n, partElemNSs);
				}
				break;
			    }
			}
			if (m != null) {
			    m.end();
			}
		    }

		    if (e2 == null) {
			String err = mMessages.getString("HTTPBC-W00703.Part_non_element_type_not_supported", new Object[] { elemQName.getLocalPart(), msg.getQName().toString() });
			throw new IllegalArgumentException(err);
		    }
		} else {
		    mLog.log(Level.WARNING, "HTTPBC-W00701.Message_has_no_match_for_part", part.getName());
		}
	    } else {
		// both are null? unexpected
		throw new IllegalArgumentException(mMessages.getString("HTTPBC-E00783.Part_not_element_nor_type", part.getName()));
	    }
	} // end while
	if (mLog.isLoggable(Level.FINEST)) {
	    DebugLog.debugLog(mLog, Level.FINEST, "Document - Denormalized SOAP Body", outSoapMessage.getSOAPBody());
	}
    }

    private void processRpc12Body(SOAPMessage outSoapMessage, OperationMetaData meta, boolean inMsg, WrapperParser wrapperParser, Map jbiMessageNSs, NormalizedMessage normalizedMessage) 
        throws MessagingException, SOAPException, WrapperProcessingException, Exception {
	SOAPBody bodyElement = outSoapMessage.getSOAPBody();
	// based on the part name, look up the element/type QName in the
	// WSDLMessage
	Message msg = null;
	if (inMsg) {
	    msg = meta.getInputMessage();
	} else {
	    msg = meta.getOutputMessage();
	}

	// look up the part used as soap body from the operation meta-data.
	javax.wsdl.extensions.soap12.SOAP12Body soapBody = null;
	if (inMsg) {
	    soapBody = meta.getInputSoap12Body();
	} else {
	    soapBody = meta.getOutputSoap12Body();
	}
	
	// set the encodingStyle attribute on the soap:body 
        // although this is optional per SOAP specification, it needs to be set for certain webservice providers, e.g. Apache Axis
        if("encoded".equalsIgnoreCase(soapBody.getUse())) {
            bodyElement.setEncodingStyle(SOAP_RPC_ENCODING_STYLE);
        }
        
	List bodyParts = soapBody.getParts();
	Iterator bodyIterator = null;

	if (bodyParts != null && bodyParts.size() > 0) {
	    // since soap body binding specifies parts, we use only the parts
	    // that are in the binding
	    bodyIterator = bodyParts.iterator();
	} else {
	    if (msg == null) {
		// assert failed
		throw new IllegalStateException("Unexpected state, input WSDLMessage is not defined, however we are attempting to invoke the operation with non-empty input");
	    }
	    // Since no parts are defined, just add the operation wrapper
	    // element in the soap body and return
	    List msgParts = msg.getOrderedParts(null);
	    if (msgParts != null && msgParts.size() > 0) {
		bodyIterator = msgParts.iterator();
	    } else {
		// Since no parts are defined, just add the operation wrapper
		// element in soap body and return
		Element wrapper;
		String wrapperName = meta.getOperationName();
		String wrapperNS = soapBody.getNamespaceURI();
		if (wrapperNS != null && wrapperNS.length() > 0) {
		    // Add prefix to for body namespace
		    wrapperName = "m:" + wrapperName; // NOI18N
		}
		if (!inMsg) {
		    wrapperName += RPC_RESPONSE_SUFFIX;
		}
		wrapper = bodyElement.getOwnerDocument().createElementNS(wrapperNS, wrapperName);
		bodyElement.appendChild(wrapper);
		if (mLog.isLoggable(Level.FINE)) {
		    DebugLog.debugLog(mLog, Level.FINE, "RPC - Denormalized SOAP body", bodyElement.getOwnerDocument());
		}
		return;
	    }
	}

	while (bodyIterator != null && bodyIterator.hasNext()) {
	    Part part = null;
	    int nsCount = 0;
	    Object value = bodyIterator.next();
	    String partName;
	    if (value instanceof String) {
		if (msg == null) {
		    // assert failed
		    throw new IllegalStateException("Unexpected state, input WSDLMessage is not defined, however we are attempting to invoke the operation with non-empty input");
		}
		String bodyPart = partName = (String) value;
		part = msg.getPart(bodyPart);
	    } else {
		part = (Part) value;
		partName = (part == null ? null : part.toString());
	    }

	    if (part == null) {
		// assert failed
		String err = mMessages.getString("HTTPBC-E00702.Message_part_bad_reference", new Object[] { partName, msg.getQName().toString() });
		throw new IllegalStateException(err);
	    }
	    QName typeQName = part.getTypeName();
	    QName elemQName = part.getElementName();
	    if (typeQName != null) {
		// means its a type
		// see if the part name can be found in the normalized message
		if (mLog.isLoggable(Level.FINEST)) {
		    mLog.log(Level.FINEST, "Looking for part " + part.getName() + " in normalized message");
		}
		
		if (wrapperParser.hasPart(part.getName())) {
		    // it is rpc literal mode with types
		    // create a new operation node and append the part node to
		    // it.
		    if (mLog.isLoggable(Level.FINEST)) {
			mLog.log(Level.FINEST, "Creating SOAP node in rpc-literal mode with types");
		    }

		    Map<String, Attr> attrMap=null;
		    Element wrapper;
		    // Element bodyElementRoot =
		    // bodyElement.getOwnerDocument().getDocumentElement();
		    Element bodyElementRoot = (Element) bodyElement.getFirstChild();
		    if (bodyElementRoot == null) {
			String wrapperName = meta.getOperationName();
			String wrapperNS = soapBody.getNamespaceURI();
			if (wrapperNS != null && wrapperNS.length() > 0) {
			    // Add prefix to for body namespace
			    wrapperName = "m:" + wrapperName; // NOI18N
			}
			if (!inMsg) {
			    wrapperName += RPC_RESPONSE_SUFFIX;
			}
			wrapper = bodyElement.getOwnerDocument().createElementNS(wrapperNS, wrapperName);
			bodyElement.appendChild(wrapper);
		    } else {
			wrapper = bodyElementRoot;
		    }
		    bodyElement.getOwnerDocument().importNode(wrapper, true);
		    bodyElement.appendChild(wrapper);
		    // Add a part element, remove the JBI wrapper and add
		    // content to part element
		    Element partElem = bodyElement.getOwnerDocument().createElement(part.getName());
		    // handle rpc/encoded case by adding xsi:type for all
		    // elements
		    if ("encoded".equalsIgnoreCase(soapBody.getUse())) {
			// check if the namespace exists in the envelope, if not
			// add it.
			String namespace = outSoapMessage.getSOAPPart().getEnvelope().lookupNamespaceURI(part.getTypeName().getNamespaceURI());
			String prefix = meta.getFullDefinition().getPrefix(part.getTypeName().getNamespaceURI());
			if ((prefix == null) || (prefix.equals(""))) {
			    if (namespace == null) {
				outSoapMessage.getSOAPPart().getEnvelope().setAttribute("xmlns:ns" + nsCount++, part.getTypeName().getNamespaceURI());
				part.getTypeName().getNamespaceURI();
			    }
			    partElem.setAttributeNS(SCHEMA_XSI_URI_2001, "xsi:type", part.getTypeName().getLocalPart());
			} else {
			    if (namespace == null) {
				outSoapMessage.getSOAPPart().getEnvelope().setAttribute("xmlns:" + meta.getFullDefinition().getPrefix(part.getTypeName().getNamespaceURI()),
					part.getTypeName().getNamespaceURI());
			    }
			    partElem.setAttributeNS(SCHEMA_XSI_URI_2001, "xsi:type", prefix + ":" + part.getTypeName().getLocalPart());
			}

		    }
		    wrapper.appendChild(partElem);
		    NodeList unwrappedList = wrapperParser.getPartNodes(part.getName());
		    if (unwrappedList != null) {
			for (int i = 0; i < unwrappedList.getLength(); i++) {
			    Node unwrapped = (Node) unwrappedList.item(i);
			    if (WrapperUtil.isNodeXopInclude(unwrapped)) {
                            	Node n = processNodesAsAttachment(typeQName, unwrapped, normalizedMessage, bodyElement);
                            	partElem.appendChild(n);
                            } else {
                                Node n = bodyElement.getOwnerDocument().importNode(unwrapped, true);
                                partElem.appendChild(n);
                            }
			}
		    }

		    // Per namespace mapping rules, collect all relevant
		    // namespaces from
		    // the JBI part wrapper element and copy these namespaces to
		    // the soap message part element
		    Map partElemNSs;
		    if (jbiMessageNSs != null) {
			partElemNSs = new HashMap(jbiMessageNSs);
		    } else {
			partElemNSs = new HashMap();
		    }
		    Element jbiWrappedPart = wrapperParser.getWrappedPart(part.getName());
		    if (jbiWrappedPart != null) {
			Map jbiWrappedPartNSs = WrapperUtil.extractNamespaceDeclarations(jbiWrappedPart);
			if (jbiWrappedPartNSs != null) {
			    if (mLog.isLoggable(Level.FINE)) {
				DebugLog.debugLog(mLog, Level.FINE, "JBI part '" + part.getName() + "' namespaces", jbiWrappedPartNSs);
			    }
			    partElemNSs.putAll(jbiWrappedPartNSs);
			}
			attrMap = WrapperUtil.extractAttributeDeclarations(jbiWrappedPart);
                        if (attrMap != null) {
                            if (mLog.isLoggable(Level.FINE)) {
                                DebugLog.debugLog(mLog, Level.FINE, "JBI part '" + part.getName() + "' attributes", attrMap);
                            }
                            
                        }
		    }
		    copyNameSpaces(partElem, partElemNSs);
		    copyAttributes(partElem, attrMap);
		} else {
		    mLog.log(Level.WARNING, "HTTPBC-W00701.Message_has_no_match_for_part", part.getName());
		}
	    } else if (elemQName != null) {
		// it is of element type
		// see if the part name can be found in the normalized message
		if (mLog.isLoggable(Level.FINEST)) {
		    mLog.log(Level.FINEST, "Looking for part " + part.getName() + " in normalized message");
		}

		if (wrapperParser.hasPart(part.getName())) {
		    // it is rpc literal mode with elements
		    // look for the operation node wrapper, create a new one if
		    // necessary
		    // and append the part node to it.
		    if (mLog.isLoggable(Level.FINEST)) {
			mLog.log(Level.FINEST, "Creating SOAP node for rpc-literal mode with elements");
		    }
		    Element wrapper;
		    // Element bodyElementRoot =
		    // bodyElement.getOwnerDocument().getDocumentElement();
		    Element bodyElementRoot = (Element) bodyElement.getFirstChild();
		    if (bodyElementRoot == null) {
			String wrapperName = meta.getOperationName();
			String wrapperNS = soapBody.getNamespaceURI();
			if (wrapperNS != null && wrapperNS.length() > 0) {
			    // Add prefix to for body namespace
			    wrapperName = "m:" + wrapperName; // NOI18N
			}
			if (!inMsg) {
			    wrapperName += RPC_RESPONSE_SUFFIX;
			}
			wrapper = bodyElement.getOwnerDocument().createElementNS(wrapperNS, wrapperName);
			bodyElement.appendChild(wrapper);
		    } else {
			wrapper = bodyElementRoot;
		    }

		    // @TODO: is this correct? Should rpc/literal with element
		    // have the part name in there???
		    // ...WSDL spec seems to say it should have part name
		    // (without differentiating type/element parts)
		    // Add a part element, remove the JBI wrapper and add
		    // content to part element
		    Element partElem = bodyElement.getOwnerDocument().createElement(part.getName());
		    // handle rpc/encoded case by adding xsi:type for all
		    // elements
		    if ("encoded".equalsIgnoreCase(soapBody.getUse())) {
			// check if the namespace exists in the envelope, if not
			// add it.
			String namespace = outSoapMessage.getSOAPPart().getEnvelope().lookupNamespaceURI(part.getElementName().getNamespaceURI());
			String prefix = meta.getFullDefinition().getPrefix(part.getElementName().getNamespaceURI());
			if ((prefix == null) || (prefix.equals(""))) {
			    if (namespace == null) {
				outSoapMessage.getSOAPPart().getEnvelope().setAttribute("xmlns:ns" + nsCount++, part.getElementName().getNamespaceURI());
			    }
			    partElem.setAttributeNS(SCHEMA_XSI_URI_2001, "xsi:type", part.getElementName().getLocalPart());
			} else {
			    if (namespace == null) {
				outSoapMessage.getSOAPPart().getEnvelope().setAttribute("xmlns:" + meta.getFullDefinition().getPrefix(part.getElementName().getNamespaceURI()),
					part.getElementName().getNamespaceURI());

			    }
			    partElem.setAttributeNS(SCHEMA_XSI_URI_2001, "xsi:type", prefix + ":" + part.getElementName().getLocalPart());
			}

		    }
		    wrapper.appendChild(partElem);
		    NodeList unwrappedList = wrapperParser.getPartNodes(part.getName());
		    if (unwrappedList != null) {
			for (int i = 0; i < unwrappedList.getLength(); i++) {
			    Node unwrapped = (Node) unwrappedList.item(i);
			    // the part node may be an xop:Include element
                            // we need to handle it if that's the case
                            if (WrapperUtil.isNodeXopInclude(unwrapped)) {
                            	Node n = processNodesAsAttachment(elemQName, unwrapped, normalizedMessage, bodyElement);
                            	bodyElement.getOwnerDocument().importNode(n, true);
                                partElem.appendChild(n);
                            } else {
                                Node n = bodyElement.getOwnerDocument().importNode(unwrapped, true);
                                partElem.appendChild(n);
                            }
			}
		    }

		    // Per namespace mapping rules, collect all relevant
		    // namespaces from
		    // the JBI part wrapper element and copy these namespaces to
		    // the soap message part element
		    Map partElemNSs;
		    if (jbiMessageNSs != null) {
			partElemNSs = new HashMap(jbiMessageNSs);
		    } else {
			partElemNSs = new HashMap();
		    }
		    Element jbiWrappedPart = wrapperParser.getWrappedPart(part.getName());
		    if (jbiWrappedPart != null) {
			Map jbiWrappedPartNSs = WrapperUtil.extractNamespaceDeclarations(jbiWrappedPart);
			if (jbiWrappedPartNSs != null) {
			    if (mLog.isLoggable(Level.FINE)) {
				DebugLog.debugLog(mLog, Level.FINE, "JBI part '" + part.getName() + "' namespaces", jbiWrappedPartNSs);
			    }
			    partElemNSs.putAll(jbiWrappedPartNSs);
			}
		    }
		    copyNameSpaces(partElem, partElemNSs);
		} else {
		    mLog.log(Level.WARNING, "HTTPBC-W00701.Message_has_no_match_for_part", part.getName());
		}
	    } else {
		// both are null? unexpected
		mLog.log(Level.SEVERE, "HTTPBC-E00783.Part_not_element_nor_type", part.getName());
	    }
	}// end while
	if (mLog.isLoggable(Level.FINEST)) {
	    DebugLog.debugLog(mLog, Level.FINEST, "RPC - Denormalized SOAP body", bodyElement.getOwnerDocument());
	}

    }

    private void processSoap12Fault(NormalizedMessage nm, Element normalRoot, SOAPMessage outSoapMessage, OperationMetaData meta, boolean inMsg, WrapperParser wrapperParser) throws MessagingException,
	    SOAPException, WrapperProcessingException {
	String tagName = null;
	if (wrapperParser.isMessageWrapped()) {
	    // todo find a better way to map normalized fault to SOAP fault,
	    // perhaps by name
	    tagName = wrapperParser.getMessageType().getLocalPart();

	    // Process header faults

	    List headerList = null;
	    // header faults are defined in the corresponding "originating"
	    // header definition
	    if (inMsg) {
		headerList = meta.getInputSoapHeaders();
	    } else {
		headerList = meta.getOutputSoapHeaders();
	    }

	    if (headerList != null && headerList.size() > 0) {

		SOAPHeader soapHeader = outSoapMessage.getSOAPHeader();

		// for each one of the declared headers, see if we can find the
		// corresponding part
		// in the normalized message
		for (int i = 0; i < headerList.size(); i++) {
		    javax.wsdl.extensions.soap12.SOAP12Header headerDef = (javax.wsdl.extensions.soap12.SOAP12Header) headerList.get(i);

		    List soapHeaderFaults = headerDef.getSOAP12HeaderFaults();
		    if (soapHeaderFaults != null && soapHeaderFaults.size() > 0) {
			for (int hfCount = 0; hfCount < soapHeaderFaults.size(); hfCount++) {
			    javax.wsdl.extensions.soap12.SOAP12HeaderFault hf = (javax.wsdl.extensions.soap12.SOAP12HeaderFault) soapHeaderFaults.get(hfCount);
			    QName hfMsgName = hf.getMessage();
			    String hfPartName = hf.getPart();

			    if (wrapperParser.hasPart(hfPartName)) {

				// Add the fault in the soap body (w/o detail as
				// it is a headerfault)
				SOAPFault fault = outSoapMessage.getSOAPBody().addFault();
				String faultCode = outSoapMessage.getSOAPPart().getEnvelope().getPrefix() + ":" + (inMsg ? "Sender" : "Receiver"); // NOI18N
				fault.setFaultCode(faultCode);
				fault.setFaultString("headerfault");

				// For header fault, add the 'details' in the
				// header
				Message wsMessage = meta.getFullDefinition().getMessage(hfMsgName);

				if (wsMessage == null) {
				    mLog.log(Level.WARNING, "HTTPBC-W00704.Headerfault_bad_message_reference", hfMsgName);
				} else {

				    // look up the part in the msg
				    Part wsdlPart = wsMessage.getPart(hfPartName);

				    if (wsdlPart == null) {
					mLog.log(Level.WARNING, "HTTPBC-W00702.Headerfault_bad_message_part_reference", new Object[] { hfMsgName, hfPartName });
				    } else {
					QName partElemQName = wsdlPart.getElementName();
					if (partElemQName == null) {
					    String msg = mMessages
						    .getString("HTTPBC-W00703.Part_non_element_type_not_supported", new Object[] { hfPartName, wsMessage.getQName() });
					    throw new IllegalStateException(msg);
					}
					Name name = mSoap12Factory.createName(partElemQName.getLocalPart(), partElemQName.getPrefix(), partElemQName.getNamespaceURI());
					SOAPHeaderElement headerElem = soapHeader.addHeaderElement(name);

					boolean found = false;
					NodeList unwrappedList = wrapperParser.getPartNodes(hfPartName);
					for (int j = 0; j < unwrappedList.getLength(); j++) {
					    Node unwrapped = (Node) unwrappedList.item(j);
					    if (unwrapped.getLocalName() != null && unwrapped.getLocalName().equals(partElemQName.getLocalPart())) {
						copyNode(unwrapped, headerElem);
						found = true;
						break;
					    }
					}
					if (!found) {
					    String msg = mMessages.getString("HTTPBC-W00705.Message_missing_element", new Object[] { hfPartName, partElemQName.getLocalPart() });
					    throw new IllegalStateException(msg);
					}
				    }
				}
				if (mLog.isLoggable(Level.FINE)) {
				    DebugLog.debugLog(mLog, Level.FINE, "Denormalized SOAP headerfault", outSoapMessage.getSOAPPart());
				}
				return;
			    }
			}
		    }
		}
	    }

	    // Process Faults

	    Map definedFaults = meta.getFaults();
	    if (definedFaults != null && definedFaults.size() > 0) {
		// faults defined
		Iterator iter = definedFaults.values().iterator();
		while (iter.hasNext()) {
		    javax.wsdl.Fault f = (javax.wsdl.Fault) iter.next();
		    Message msg = f.getMessage();
		    QName faultMsgName = msg.getQName();

		    if (tagName.equals(faultMsgName.getLocalPart())) {
			// found the msg that defines this fault
			// create soap fault
			SOAPFault fault = outSoapMessage.getSOAPBody().addFault();
                        final String faultCodeProp = (String)nm.getProperty(NormalizedMessageProperties.SOAP_FAULTCODE_PROPERTY);

                        // Sorry for this "WAT?!"
			final String faultCode = outSoapMessage.getSOAPPart().getEnvelope().getPrefix() + ":" +
                                ((faultCodeProp != null) ? faultCodeProp :
                                 (inMsg ? "Sender" : "Receiver")); // NOI18N
			fault.setFaultCode(faultCode);

                        final String faultActorProp = (String)nm.getProperty(NormalizedMessageProperties.SOAP_FAULTACTOR_PROPERTY);
                        if (faultActorProp != null)
                            fault.setFaultActor(faultActorProp);

                        final String faultStringProp = (String)nm.getProperty(NormalizedMessageProperties.SOAP_FAULTSTRING_PROPERTY);
			final String faultName = (faultStringProp != null) ? faultStringProp : f.getName();
			fault.setFaultString(faultName);
			Detail detail = fault.addDetail();

			// extract part from normalized message
			List l = msg.getOrderedParts(null);
			for (int i = 0; l != null && i < l.size(); i++) {
			    Part p = (Part) l.get(i);
			    tagName = p.getName();

			    String partName = tagName;
			    if (wrapperParser.hasPart(partName)) {
				NodeList detailList = wrapperParser.getPartNodes(partName);
				for (int j = 0; j < detailList.getLength(); j++) {
				    Node detailnode = (Node) detailList.item(j);
				    if (detailnode.getNodeType() == Node.TEXT_NODE) {
					// Wrap the text node using the part
					// name. This is the
					// way we're doing it when we have
					// document style with
					// parts that have type attributes.
					Name name = mSoap12Factory.createName(tagName, "", "");
					DetailEntry de = detail.addDetailEntry(name);
					de.addTextNode(detailnode.getNodeValue());
					// detail.addTextNode(detailnode.getNodeValue());
				    } else {
					Name name = mSoap12Factory.createName(detailnode.getLocalName(), "", detailnode.getNamespaceURI()); // NOI18N
					DetailEntry de = detail.addDetailEntry(name);
					NodeList nl = detailnode.getChildNodes();
					for (int k = 0; k < nl.getLength(); k++) {
					    Node n2 = nl.item(k);
					    n2 = outSoapMessage.getSOAPBody().getOwnerDocument().importNode(n2, true);
					    de.appendChild(n2);
					}
					addFaultDetailNodeAttrributes(outSoapMessage, detailnode, de);
				    }
				}
			    }
			}

			if (mLog.isLoggable(Level.FINE)) {
			    DebugLog.debugLog(mLog, Level.FINE, "Denormalized SOAP fault", outSoapMessage.getSOAPPart());
			}
			return;
		    }
		}
		String msg = mMessages.getString("HTTPBC-W00706.Fault_not_for_operation", meta.getOperationName());
		mLog.log(Level.WARNING, msg);
	    } else {
		// no fault was defined for this operation
		String msg = mMessages.getString("HTTPBC-W00707.Operation_no_faults", meta.getOperationName());
		mLog.log(Level.WARNING, msg);
	    }
	}

	// unknown fault, we simply create a SERVER fault and set the normalized
	// message in the fault detail
	SOAPFault fault = outSoapMessage.getSOAPBody().addFault();
	// Ensure we use QName to set the fault code in the envelope name space
	String faultCode = outSoapMessage.getSOAPPart().getEnvelope().getPrefix() + ":" + "Receiver"; // NOI18N
	fault.setFaultCode(faultCode);
	fault.setFaultString(""); // NOI18N

	if (tagName != null) {
	    Detail detail = fault.addDetail();
	    Name name = mSoap12Factory.createName(tagName);
	    DetailEntry de = detail.addDetailEntry(name);
	    // maybe we want to consider not copying from the root of normalized
	    // doc, should
	    // revisit this later
	    if (normalRoot != null) {
		copyNode(normalRoot, de);
	    }
	} else if (normalRoot != null) {
	    Detail detail = fault.addDetail();
	    Name name = mSoap12Factory.createName(normalRoot.getLocalName());
	    DetailEntry de = detail.addDetailEntry(name);
	    copyNode(normalRoot, de);
	}

	if (mLog.isLoggable(Level.FINE)) {
	    DebugLog.debugLog(mLog, Level.FINE, "Denormalized SOAP fault", outSoapMessage.getSOAPPart());
	}
    }

}
