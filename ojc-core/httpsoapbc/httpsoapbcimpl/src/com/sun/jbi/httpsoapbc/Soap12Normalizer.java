package com.sun.jbi.httpsoapbc;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.Message;
import javax.wsdl.Part;
import javax.xml.namespace.QName;
import javax.xml.soap.SOAPBody;
import javax.xml.soap.SOAPHeader;
import javax.xml.soap.SOAPHeaderElement;
import javax.xml.soap.SOAPMessage;

import org.w3c.dom.Attr;
import org.w3c.dom.Element;

import com.sun.jbi.httpsoapbc.util.DebugLog;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;
import com.sun.jbi.nms.wsdl11wrapper.util.WrapperUtil;

public class Soap12Normalizer extends Soap11Normalizer {

    private static Messages mMessages = Messages.getMessages(Soap12Normalizer.class);
    private static Logger mLog = Messages.getLogger(Soap12Normalizer.class);

    @Override
    protected void processSoapBody(WrapperBuilder wrapperBuilder, SOAPBody soapElement, OperationMetaData meta, boolean inMsg) throws WrapperProcessingException, Exception {
	// TODO Auto-generated method stub
	processSoap12Body(wrapperBuilder, soapElement, meta, inMsg);
    }

    @Override
    protected void processSoapHeader(WrapperBuilder wrapperBuilder, NormalizedMessage normalMsg, SOAPHeader soapHeader, OperationMetaData meta, boolean inMsg, MessageExchange mex)
	    throws WrapperProcessingException {
	// TODO Auto-generated method stub
	processSoap12Header(wrapperBuilder, normalMsg, soapHeader, meta, inMsg, mex);
    }

    private void processSoap12Header(WrapperBuilder wrapperBuilder, NormalizedMessage normalMsg, SOAPHeader soapHeader, OperationMetaData meta, boolean inMsg, MessageExchange mex)
	    throws WrapperProcessingException {
	List headerList = null;
	if (inMsg) {
	    headerList = meta.getInputSoapHeaders();
	} else {
	    headerList = meta.getOutputSoapHeaders();
	}

	// Handle custom reliability protocol.
	// If present, Extract MessageID and place it in the message exchange
	// properties
	if (soapHeader != null) {
	    Iterator headerIter = soapHeader.examineAllHeaderElements();
	    while (headerIter.hasNext()) {
		SOAPHeaderElement he = (SOAPHeaderElement) headerIter.next();

		if (he.getLocalName().equals(WSRM_Sequence) && (he.getNamespaceURI().equals(WSRM__1_0__NAMESPACE_URI) || he.getNamespaceURI().equals(WSRM__1_1__NAMESPACE_URI))) {
		    // handle ws-rm soap messages
		    propagateWSRMIdsToExchange(he, mex);
		}

		// 11-20-2007 - With the introduction of using WS-RM and the new
		// JBI message properties for grouping/sequencing,
		// the following code in the "if" clause for handling custom
		// reliability may be removed upon further review...
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

	// The service does not define any headers. All headers in the message
	// will be copied to the Normalized Message as properties.
	if (mIsSoapHeaderCopyEnabled && soapHeader != null && (headerList == null || headerList.size() < 1)) {
	    for (Iterator headerIter = soapHeader.examineAllHeaderElements(); headerIter.hasNext();) {
		addSOAPHeader(normalMsg, (SOAPHeaderElement) headerIter.next());
	    }
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
	if (soapHeader != null) {
	    // Per namespace mapping rules, collect all relevant namespaces from
	    // soap:header and add them as namespaces to the JBI part
	    Map nameSpaces = WrapperUtil.extractNamespaceDeclarations(soapHeader);
	    if (mLog.isLoggable(Level.FINE)) {
		DebugLog.debugLog(mLog, Level.FINE, "SOAP header namespaces", nameSpaces);
	    }

	    for (Iterator headerIter = soapHeader.examineAllHeaderElements(); headerIter.hasNext();) {

		SOAPHeaderElement elem = (SOAPHeaderElement) headerIter.next();
		boolean headerProcessed = false;

		for (int i = 0; i < headerList.size() && !headerProcessed; ++i) {
		    javax.wsdl.extensions.soap12.SOAP12Header headerDef = (javax.wsdl.extensions.soap12.SOAP12Header) headerList.get(i);

		    if (headerDef.getMessage().equals(wsMessage.getQName())) {
			Part part = wsMessage.getPart(headerDef.getPart());
			if (part == null) {
			    // part not found in the in/out message, currently
			    // not supported
			    String msg = mMessages.getString("HTTPBC-W00714.Unsupported_feature_part_in_header_not_in_message", new Object[] { headerDef.getPart(),
				    wsMessage.getQName() });
			    throw new IllegalArgumentException(msg);
			}
			QName partElemName = part.getElementName();
			if (partElemName == null) {
			    // part needs to be defined as element, currently
			    // not supported
			    String msg = mMessages.getString("HTTPBC-W00715.Unsupported_feature_part_in_header_not_element", new Object[] { headerDef.getPart(),
				    wsMessage.getQName() });
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

    private void processSoap12Body(WrapperBuilder wrapperBuilder, SOAPBody soapElement, OperationMetaData meta, boolean inMsg) throws WrapperProcessingException, Exception {
	// Per namespace mapping rules, collect all relevant namespaces from
	// soap:body and add them as namespaces to the JBI part
	Map nameSpaces = WrapperUtil.extractNamespaceDeclarations(soapElement);
	if (mLog.isLoggable(Level.FINE)) {
	    DebugLog.debugLog(mLog, Level.FINE, "SOAP body namespaces", nameSpaces);
	}

	javax.wsdl.extensions.soap12.SOAP12Body soapBody = null;
	if (inMsg) {
	    soapBody = meta.getInputSoap12Body();
	} else {
	    soapBody = meta.getOutputSoap12Body();
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
	    // since soap body binding specifies parts, we use only the parts
	    // that are in the binding
	    bodyIterator = bodyParts.iterator();
	} else {
	    // if no parts were specified in the SOAP binding section, we use
	    // all the message parts
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
	    if (soapBodyURI != null)
		if (mLog.isLoggable(Level.FINER)) {
		    mLog.log(Level.FINER, "SOAP body namespace in use: " + soapBodyURI);
		}
	    String operationTNS = soapBodyURI == null ? "*" : soapBodyURI;
	    operElem = findResponsePart(soapElement, new QName(operationTNS, meta.getOperationName()), inMsg);
	    // look for operation node in soap msg
	    // should have only 1 element with operation's name under wrapper
	    // get the part node under operation node
	    // use namespace to find operation, parts do not have a namespace
	    if (operElem == null) {
		mLog.log(Level.WARNING, "HTTPBC-W00711.Soap_message_has_no_match_for_operation", meta.getOperationName());
		throw new WrapperProcessingException(mMessages.getString("HTTPBC-W00711.Soap_message_has_no_match_for_operation", meta.getOperationName()));
	    } else {
		// Per namespace mapping rules, collect all relevant namespaces
		// from
		// soap:body operation and add them as namespaces to the JBI
		// part
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
	    if (nameSpaces != null) {
		partNameSpaces = new HashMap(nameSpaces);
	    } else {
		partNameSpaces = new HashMap();
	    }

	    String bodyPart = (String) bodyIterator.next();

	    // based on the part name, look up the element/type QName in the
	    // WSDLMessage
	    if (msg == null) {
		// assert failed
		throw new IllegalStateException("Unexpected state, input WSDLMessage is not defined, however we are attempting to invoke the operation with non-empty input");
	    }
	    Part part = msg.getPart(bodyPart);
	    if (part == null) {
		// assert failed
		String err = mMessages.getString("HTTPBC-E00702.Message_part_bad_reference", new Object[] { bodyPart, msg.getQName().toString() });
		throw new IllegalStateException(err);
	    }
	    QName typeQName = part.getTypeName(); // wsdl:part of type
	    QName elemQName = part.getElementName(); // wsdl:part of element
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
		msgElem = findMsgElement(operElem, new QName(uri, bodyPart)); // elemQName.getLocalPart()
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
	    Map<String, Attr> partAttributes=new HashMap<String, Attr>();
	    if (e != null) {
		// Per namespace mapping rules, collect all relevant namespaces
		// from
		// the part element of the soap message if the corresponding
		// part is defined as a type
		// in the wsdl; these namespaces will be added to the JBI part
		if (isType) {
		    Map partNSs = WrapperUtil.extractNamespaceDeclarations(e);
		    if (mLog.isLoggable(Level.FINE)) {
			DebugLog.debugLog(mLog, Level.FINE, "Part  '" + localName + "' namespaces", partNSs);
		    }
		    if (partNSs != null) {
			partNameSpaces.putAll(partNSs);
		    }
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
		    // see if the part name can be found in the soap body
		    // element
		    // add the part node to the normalized document
		    wrapperBuilder.addPart(part.getName(), e.getChildNodes(), partNameSpaces,partAttributes);
		} else if (TYPE_RPC) {
		    // rpc mode using type
		    // look for operation node in soap msg
		    // should have only 1 element with operation's name under
		    // wrapper
		    // get the part node under operation node
		    wrapperBuilder.addPart(part.getName(), e.getChildNodes(), partNameSpaces,partAttributes);
		} else if (ELEM_DOC) {
		    // it is a element
		    // document mode using element
		    // see if the element name can be found in the soap body
		    wrapperBuilder.addPart(part.getName(), e, partNameSpaces);
		} else if (ELEM_RPC) {
		    // rpc mode using element
		    // look for operation node in soap msg
		    // get the part node under operation node
		    // Add the contents under the existing part node in the
		    // request to the JBI part wrapper
		    // mWrapperBuilder.addPart(part.getName(),
		    // e.getChildNodes());
		    wrapperBuilder.addPart(part.getName(), e, partNameSpaces);
		}
	    } else {
		throw new WrapperProcessingException(mMessages.getString(nullMsg, partQName.toString()));
	    }
	} // end while
    }

}
