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
 * @(#)OperationResolver.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import com.sun.jbi.internationalization.Messages;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Map;
import java.util.Set;

import javax.xml.namespace.QName;
import javax.wsdl.Message;
import javax.wsdl.Part;
import javax.xml.soap.SOAPBody;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPMessage;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;

/**
 * Looks at the soap message content to decide which operation is being invoked/to be invoked.
 */
public class OperationResolver {
    
    private static final String SOAP_ACTION_HEADER = "SOAPAction"; // NOI18N
    private static Messages mMessages = Messages.getMessages(OperationResolver.class);
    private static Logger mLogger = Messages.getLogger(OperationResolver.class);
    
    /** Creates a new instance of MessageRouter */
    public OperationResolver() {
    }
    
    /**
     * first look at the soap action to see if any defined operation match.
     * second look at the soap body to see if the names of the root nodes matches
     * any of the defined Input messages or operation.
     *
     * Note:
     * In doc-lit mode, if all messages have the same number of parts, all parts use type, and all parts have the same name.
     * There is no way to differentiate between operations based on operation because
     * all nodes of soap body would be the same part name.
     *
     * @param soapMsg Soap message
     * @param metaDataMap map of OperationMetaData objects keyed by operation name
     * @return
     */
    public static OperationMetaData resolveOperation(SOAPMessage soapMsg, Map metaDataMap) {
	
	String[] tempS = soapMsg.getMimeHeaders().getHeader(SOAP_ACTION_HEADER);
	String rawSoapAction = (tempS != null && tempS.length > 0) ? tempS[0] : null;

	//use this once the metro fix for the content-type 
	//action parameter is propagated 
	//if( (rawSoapAction!=null) && isSoap12(metaDataMap)){
	//    rawSoapAction = getSoap12Action(soapMsg);
	//}

	// Allow for SOAPAction with or without surrounding quotes
        String soapAction = stripQuotes(rawSoapAction);
        
        if (soapAction != null && !soapAction.equals("")) { // NOI18N
            // see if the soap action can be used to find the mapping
            Iterator iter = metaDataMap.values().iterator();
            while (iter != null && iter.hasNext()) {
                OperationMetaData meta = (OperationMetaData) iter.next();
                if (meta.useSoapAction() && soapAction.equals(meta.getSoapActionURL())) {
                    return meta;
                }
            }
            // if soap action is not null and we don't find a matching operation, the routing hint wasn't defined correctly'
            mLogger.log(Level.WARNING, "HTTPBC-W00760.Soap_action_cant_resolve_to_operation", soapAction);
        }
        
        try {
            SOAPBody soapBody = soapMsg.getSOAPBody();
            NodeList nl = soapBody.getChildNodes();
            
            List nodeNames = new ArrayList();
            List nodeLocalNames = new ArrayList();
            for (int i = 0; i < nl.getLength(); i++) {
                Node n = nl.item(i);
                
                if (Node.ELEMENT_NODE == n.getNodeType()) {
                    String ns = n.lookupNamespaceURI(n.getPrefix());
                    QName nodeQName = new QName(ns, ((Element)n).getLocalName());
                    QName unqualifiedNodeQName = new QName(((Element)n).getLocalName());
                    nodeNames.add(nodeQName);
                    nodeLocalNames.add(unqualifiedNodeQName);
                }
            }
            
            // now we have a list of node names from the SOAPBody, we see if these match
            // any of the messages in the list of possible operations
            Iterator iter = metaDataMap.values().iterator();
            while (iter != null && iter.hasNext()) {
                OperationMetaData meta = (OperationMetaData) iter.next();
                if (meta.isDocumentMode()) {
                    if (meta.useMsgAsID()) {
                        Set set = meta.getCachedInputPartNames();
                        // see if the list of input node names matches the list of expected names in the Input WSDL message
                        if (set.size() == nodeNames.size() && set.containsAll(nodeNames)) {
                            // it matches, so this operation is the one we want
                            return meta;
                        }
                    } else {
                        mLogger.log(Level.WARNING, "HTTPBC-W00761.Soap_action_cant_resolve_by_message", soapAction);
                    }
                } else {
                    // for rpc mode, the root node name (wrapper) would equal the operation name
                    if (nodeNames.size() > 0) {                        
                        QName wrapperName = (QName)nodeNames.get(0);
                        if (meta.getOperationName().equals(wrapperName.getLocalPart())) {
                            String metaWrapperNS = getNameSpaceURIFrom(meta);
                            // if both have no namespace, it's a match'
                            if (metaWrapperNS == null || metaWrapperNS.length() == 0) {
                                if (wrapperName.getNamespaceURI() == null) {
                                    return meta;
                                }
                            } else {
                                // if there is a namespace, it's a match if it's the same'
                                if (metaWrapperNS.equals(wrapperName.getNamespaceURI())) {
                                    return meta;
                                }
                            }
                        }
                    } else {
                        mLogger.warning("HTTPBC-W00763.Soap_body_empty");
                        return null;
                    }                            
                }
            }
                
            // If no strict match is found, fall back onto more lenient matching
            // It may be desirable to make this leniency optional 
            Iterator lenientIter = metaDataMap.values().iterator();
            while (lenientIter != null && lenientIter.hasNext()) {
                OperationMetaData meta = (OperationMetaData) lenientIter.next();
                if (meta.isDocumentMode()) {
                    if (meta.useMsgAsID()) {
                        Set set = meta.getCachedInputPartNames();
                        // see if the list of input node names matches the list of expected names in the Input WSDL message
                        // More lenient match, look at local name of nodes
                        if (set.size() == nodeNames.size() && set.containsAll(nodeLocalNames)) {
                            return meta;
                        }
                    }
                } else {
                    // for rpc mode, the root node name (wrapper) would equal the operation name
                    if (nodeNames.size() > 0) {
                        QName wrapperName = (QName)nodeNames.get(0);
                        // Just match on operation name, ignore namespace to be lenient
                        if (meta.getOperationName().equals(wrapperName.getLocalPart())) {
                            return meta;
                        }
                    }
                }
            }
        } catch (SOAPException e) {
            if (mLogger.isLoggable(Level.WARNING)) {
                String text = mMessages.getString("HTTPBC-W00762.Soap_action_cant_resolve_due_access_error", e.getLocalizedMessage());
                mLogger.log(Level.WARNING, text, e);
            }
        }
        return null;
    }

    private static String getNameSpaceURIFrom(OperationMetaData meta) {
	if (meta.isSoap12())
	    return meta.getInputSoap12Body().getNamespaceURI();
	else
	    return meta.getInputSoapBody().getNamespaceURI();
    }

    private static String getSoap12Action(SOAPMessage soapMsg) {
	
	String soapAction = null;
	String[] values = soapMsg.getMimeHeaders().getHeader("Content-Type");
	for (int i = 0; i < values.length; i++) {
	    String s= values[i];
	    
	    if( s.startsWith("action=")){
		soapAction = s.substring(s.lastIndexOf("action="));
		break;
	    }
	    
	}
	
	return soapAction;
    }

    private static boolean isSoap12(Map metaDataMap) {
	Object key = metaDataMap.keySet().iterator().next();
	
	OperationMetaData meta = (OperationMetaData) metaDataMap.get(key);
	return meta.isSoap12();
    }

    /**
     * Remove quotes surrounding a string if present
     * @param rawString string which might be surrounded by quotes
     * @return string without surrounding quotes
     */
    static String stripQuotes(String rawString) {
        String result = rawString;
        if (rawString != null && rawString.length() > 1 && rawString.startsWith("\"") && rawString.endsWith("\"")) { // NOI18N
            result = rawString.substring(1, rawString.length() - 1);
        }
        return result;
    }
}
