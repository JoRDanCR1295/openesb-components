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
 * @(#)ToExecutorFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.util;

import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.namespace.QName;

import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.JXPathException;
import org.apache.commons.jxpath.Pointer;
import org.apache.commons.jxpath.ri.model.NodePointer;
import org.apache.commons.jxpath.ri.model.VariablePointer;
import org.apache.commons.jxpath.ri.model.beans.BeanPointer;
import org.apache.commons.jxpath.ri.model.dom.DOMAttributePointer;
import org.w3c.dom.Attr;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.bpel.model.Copy;
import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.To;
import com.sun.bpel.model.meta.RExpressionElement;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.model.meta.RVariableElement;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.exception.StandardException;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.xpath.dom.BPELSEDOMNodePointer;
import com.sun.jbi.engine.bpel.core.bpel.xpath.functions.BPWSFunctions;
import com.sun.wsdl4j.ext.bpel.MessagePropertyAlias;
import org.w3c.dom.Document;

/**
 * 
 *
 *
 * @author Sun Microsystems
 */
public class ToExecutorFactory {

	/* */
	private static final String TEXT_NODE = "text()";

	/* */
	private static final char PREDICATE_END = ']';

	/* */
	private static final char PREDICATE_BEGIN = '[';
	
	/* */
	private static final String PREFIX = "sx";

	/* */
	private static DOMFactory domFactory = new DOMFactory();

	/**
	 * 
	 * @param to
	 * @return
	 */
	public static ToExecutor getToExecutor(To to) {
        if (to.getBPELPartnerLink() != null) {
            return new ToPartnerLinkExecutor();
        } else { //else if (to.getBPELVariable() != null) {
//          <to variable="NCName"/>
//          <to variable="NCName" part="NCName"/>
            return new ToVariableExecutor();
        }
	}

	/*
	 * 
	 */
    private static void updateXSDVariable(RuntimeVariable varForUpdate, Object fromVal) {
        fromVal = getFromVal(fromVal);
        varForUpdate.setXSDVariableData(fromVal);
    }

    private static Object getFromVal(Object fromVal) {
        if (fromVal instanceof Iterator) {
            Pointer sourcePtr = (Pointer) ((Iterator) fromVal).next();
            if (sourcePtr instanceof BeanPointer || sourcePtr instanceof DOMAttributePointer) {
                fromVal = sourcePtr.getValue();
            } else {
                fromVal = sourcePtr;
            }
        } else if (fromVal instanceof Document) {
            fromVal = ((Document) fromVal).getDocumentElement();
        } else if (fromVal instanceof DocumentFragment) {
            if (((DocumentFragment) fromVal).getChildNodes().getLength() == 1) {
                fromVal = ((DocumentFragment) fromVal).getFirstChild();
            }
            //TODOvb should we care of DocumentFragment with multiple nodes?
        }
        return fromVal;
    }
    
    public static Object processValForNMProperty(Object fromVal) {
        fromVal = getFromVal(fromVal);
        fromVal = BPWSFunctions.convertParam(fromVal);
        //WSDL message part assigned to NM property, remove the wrapper 
        fromVal = processNodeIfMessagePart(fromVal);
        return fromVal;
    }

    private static Object processNodeIfMessagePart(Object fromVal) {
        if (fromVal instanceof Node) {
            Node node = (Node) fromVal;
            if ("http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper".equals(
                    node.getNamespaceURI())
                    && "part".equals(node.getLocalName())) {
                return node.getTextContent();
            }
            return node;
        }
        return fromVal;
    }
    
    /**
     * 
     *
     *
     * @author Sun Microsystems
     */
    public interface ToExecutor {
        void executeTo(To to, Context ctx, Object fromVal, ICallFrame frame) throws Exception;
    }

	/**
	 * 
	 *
	 *
	 * @author Sun Microsystems
	 */
    private static class ToVariableExecutor implements ToExecutor {

        public void executeTo(To to, Context ctx, Object fromVal, ICallFrame frame) throws Exception {


            RVariable variable = ((RVariableElement) to).getRVariable();
            if (variable == null) {
                throw new Exception(I18n.loc("BPCOR-6188: Invalid variable name. " + 
                        "Process id {0}. Line no. {1}", frame.getProcess().getBPELId(), 
                        new Integer(to.getLocator().getLineNumber())));
            }
            RuntimeVariable varForUpdate = (RuntimeVariable) ctx.getRuntimeVariable(variable);
            String toQuery = null;
            String toPartName = null;
            if (!Utility.isEmpty(to.getNMProperty())) {
                String nmProperty = to.getNMProperty();
                WSMessage wsMessage = varForUpdate.getWSMessage();
                if (wsMessage == null) {
                    Utility.initializeVariableValue(varForUpdate);
                } else {
                    Utility.createCopyIfRequired(varForUpdate);
                }
                wsMessage = varForUpdate.getWSMessage();
                fromVal = processValForNMProperty(fromVal);
                wsMessage.setNMProperty(nmProperty, fromVal);
                return;
            }

            if (!Utility.isEmpty(to.getProperty())) {
                String propName = to.getProperty();
                // variable cannot be null if property is present.
                String varName = to.getVariable(); 
                String key = varName + "#" + propName; //$NON-NLS-1$
                MessagePropertyAlias propAlias = 
                    ((RExpressionElement) to).getPropertyAliasForVariableProperty(key);
                String nmProperty = propAlias.getNMProperty();
                //propertyAlias defined as
                //<bpws:propertyAlias propertyName="tns:swa_ReplyTo_Addr" nmProperty="org.glassfish.openesb.headers.soap">
                if (nmProperty != null) {
                    //process the value to be compatible for NM property
                    fromVal = processValForNMProperty(fromVal);

                    WSMessage wsMessage = varForUpdate.getWSMessage();
                    if (wsMessage == null) {
                        Utility.initializeVariableValue(varForUpdate);
                    } else {
                        Utility.createCopyIfRequired(varForUpdate);
                    }
                    wsMessage = varForUpdate.getWSMessage();
                    if(propAlias.getQuery() == null){
                        //if it is a node, wrap it in document fragment
                        if(fromVal instanceof Node){
                            fromVal = Utility.wrapInDocumentFragment((Node)fromVal);
                        }
                        wsMessage.setNMProperty(nmProperty, fromVal);
                        return;
                    }
                    //when query != null, property is a Map
                    Map nmPropertyMap = (Map)wsMessage.getNMProperty(nmProperty);
                    if(nmPropertyMap == null){
                        nmPropertyMap = new HashMap();
                        wsMessage.setNMProperty(nmProperty, nmPropertyMap);
                    }

                    String query =  propAlias.getQuery().getQueryString();
                    String retVal[] = Utility.getKeyForNMProperty(propAlias.getQuery(), query);
                    query = retVal[0];
                    String keyForNMProperty = retVal[1];
                    //it is simple query with one step
                    if(query == null){
                        //if it is a node, wrap it in document fragment
                        if(fromVal instanceof Node){
                            fromVal = Utility.wrapInDocumentFragment((Node)fromVal);
                        }
                        nmPropertyMap.put(keyForNMProperty, fromVal);
                        return;
                    }
                    Node propVal = (Node)nmPropertyMap.get(keyForNMProperty);
                    if(propVal == null){
                        propVal = Utility.constructElementWrapinDF(QName.valueOf(keyForNMProperty));
                        nmPropertyMap.put(keyForNMProperty, propVal);
                    }
                    propVal = propVal.getFirstChild();
                    JXPathContext jxpathContext = Utility.createJXPathContextOnObject(propAlias, propVal, domFactory);
                    Pointer targetPtr = jxpathContext.createPath(query);
                    updateTargetPtr(targetPtr, fromVal, jxpathContext, query);
                    return;
                } else {
                    //ws-bpel  <to variable="NCName" property="QName"/>

                    toPartName = propAlias.getPartName();
                    String query = propAlias.getQuery() == null ? null
                            : propAlias.getQuery().getQueryString();
                    //when query is defined on propertyAlias
                    if (!Utility.isEmpty(query)) {
                        Object obj = null;
                        if (propAlias.getMessageType() != null) {
                            WSMessage wsMessage = varForUpdate.getWSMessage();
                            if (wsMessage == null) {
                                Utility.initializeVariableValue(varForUpdate);
                            } else {
                                Utility.createCopyIfRequired(varForUpdate);
                            }
                            wsMessage = varForUpdate.getWSMessage();
                            obj = wsMessage.getPart(toPartName);
                            if (obj == null) {
                                obj = wsMessage.createPart(toPartName);
                            }
                        } else {
                            obj = varForUpdate.getXSDVariableData();
                            if (obj == null) {
                                Utility.initializeVariableValue(varForUpdate);
                                obj = varForUpdate.getXSDVariableData();
                            }
                        }
                        JXPathContext jxpathContext = Utility.createJXPathContextOnObject(propAlias, obj, domFactory);
                        query = Utility.adjustXpathQuery(query);
                        Pointer targetPtr = jxpathContext.createPath(query);
                        updateTargetPtr(targetPtr, fromVal, jxpathContext, query);

                        //when propertyAlias is associated with message
                    } else if (toPartName != null) {
                        updateMessagePart(fromVal, varForUpdate, toPartName);

                        //when it is on variable
                    } else {
                        updateVariable(to, fromVal, frame, variable, varForUpdate);
                    }
                }
                return;
            }

            toQuery = to.getQuery();
            toPartName = to.getPart();
            if (toQuery != null) {
                //<to expressionLanguage="anyURI"?>expression</to>
                executeExpression(to, ctx, fromVal, frame, varForUpdate, toQuery);
            } else if (toPartName != null) {
                // <to variable="NCName" part="NCName"/>
                updateMessagePart(fromVal, varForUpdate, toPartName);
            } else {
                // <to variable="NCName"/>
                updateVariable(to, fromVal, frame, variable, varForUpdate);
            }

        }

        private void updateVariable(To to, Object fromVal, ICallFrame frame,
                RVariable variable, RuntimeVariable varForUpdate) {
            if (varForUpdate.getVariableDef().getMessageType() == null) {
                // Variable is of XSD type
                if (fromVal instanceof WSMessage) {                    	
                    throw new StandardException(StandardException.Fault.MismatchedAssignmentFailure,
                    		I18n.loc("BPCOR-6103: Missmatched assign from message " + 
                            "to non message. Process id {0}. Line no. {1}", frame.getProcess().getBPELId(), 
                            new Integer(to.getLocator().getLineNumber())));
                }
                updateXSDVariable(varForUpdate, fromVal);
            } else {
                QName fromMsgType = ((WSMessage) fromVal).getWSDLMessage().getQName();
                QName toMsgType = varForUpdate.getVariableDef().getWSDLMessageType().getQName();
                if (!fromMsgType.equals(toMsgType)) {
                	throw new StandardException(StandardException.Fault.MismatchedAssignmentFailure, 
                			I18n.loc("BPCOR-6137: Missmatched message types." + 
                            "Process id {0}. Line no. {1}", frame.getProcess().getBPELId(), 
                            new Integer(to.getLocator().getLineNumber())));
                }
                // since we are copying by reference, add this to variable to the 
                // list of references maintained by this ws message
                ((WSMessage) fromVal).addInternalReference(variable);
                // Variable is defined using WSDL Message
                varForUpdate.setWSMessage((WSMessage) fromVal);
            }
        }

        private void updateMessagePart(Object fromVal,
                RuntimeVariable varForUpdate, String toPartName) {
            WSMessage message = varForUpdate.getWSMessage();
            if (message == null) {
                /* Variable may not have been initialized, ensure document structure */
                Utility.initializeVariableValue(varForUpdate);
            } else {
                Utility.createCopyIfRequired(varForUpdate);
            }
            message = varForUpdate.getWSMessage();
            Node partDocumentElementNode = message.getPart(toPartName);
            if (partDocumentElementNode == null) {
                partDocumentElementNode = message.createPart(toPartName);
            }
            Utility.updateNode(partDocumentElementNode, fromVal);
            if (fromVal instanceof Utility.AttachmentWrapper) {
                Utility.AttachmentWrapper fVal = (Utility.AttachmentWrapper) fromVal;
                if (fVal.mBinaryCopy.equals(Copy.BINARY_COPY_ENUM_VALS[1])) {
                    // Copy.BINARY_COPY_ENUM_VALS[1] = "attachment". Binary is set as attachment on message.
                       message.setAttachment(fVal.mAttchName, fVal.mDHdlr);
                   }
            }
        }

        private void executeExpression(To to, Context ctx, Object fromVal,
                ICallFrame frame, RuntimeVariable varForUpdate, String toQuery) {
            
            Utility.createCopyIfRequired(varForUpdate);

            JXPathContext jxpathContext = Utility.createJXPathContext(to, 
                    ctx, varForUpdate, domFactory);            

            String targetQuery = toQuery;
            if (toQuery.endsWith(TEXT_NODE)) {
                int position = toQuery.indexOf(TEXT_NODE);
                targetQuery = toQuery.substring(0, position - 1);
            }
            Pointer targetPtr = null;
            try {
                targetPtr = jxpathContext.createPath(targetQuery);
            } catch (JXPathException jxpathExe) {
                // has to be treated as bpws:SelectionFailure Exception
                // refer to INF 112133
                throw new StandardException(StandardException.Fault.SelectionFailure, 
                        I18n.loc("BPCOR-6174: Selection Failure occurred in BPEL({0}) at line {1}", 
                                frame.getProcess().getBPELId(), to.getLocator().getLineNumber()),
                                jxpathExe);
            }

            if (targetPtr instanceof VariablePointer) {
                // <to queryLanguage="anyURI"?>$var.part</to>
                // This is an alternate syntax for <to variable="NCName"
                // part="NCName"/>
                if (varForUpdate.getVariableDef().getMessageType() == null) {
                    // Variable is of XSD type
                    updateXSDVariable(varForUpdate, fromVal);
                } else {
                    Utility.updateNode((Node)targetPtr.getNode(), fromVal);
                    if (fromVal instanceof Utility.AttachmentWrapper) {
                        Utility.AttachmentWrapper fVal = (Utility.AttachmentWrapper) fromVal;
                        if (fVal.mBinaryCopy.equals(Copy.BINARY_COPY_ENUM_VALS[1])) {
                         // Copy.BINARY_COPY_ENUM_VALS[1] = "attachment". Binary is set as attachment on message.
                            varForUpdate.getWSMessage().setAttachment(fVal.mAttchName, fVal.mDHdlr);
                        }
                    }
                }
            } else if (fromVal instanceof Utility.AttachmentWrapper) {
                Utility.updateNode((Node)targetPtr.getNode(), fromVal);
                Utility.AttachmentWrapper fVal = (Utility.AttachmentWrapper) fromVal;
                if (fVal.mBinaryCopy.equals(Copy.BINARY_COPY_ENUM_VALS[1])) {
                 // Copy.BINARY_COPY_ENUM_VALS[1] = "attachment". Binary is set as attachment on message.
                    varForUpdate.getWSMessage().setAttachment(fVal.mAttchName, fVal.mDHdlr);
                }
        	
            } else {
                updateTargetPtr(targetPtr, fromVal, jxpathContext, targetQuery);
            }
        }

        private void updateTargetPtr(Pointer targetPtr,
                Object fromVal, JXPathContext jxpathContext,
                String targetQuery) {
        	if (fromVal instanceof QName) {
        		QName qname = (QName) fromVal;
        		String prefix = qname.getPrefix();
        		if ((targetPtr instanceof NodePointer)) {
        			NodePointer parentNodePointer = ((NodePointer) targetPtr).getParent();
        			Object parent = parentNodePointer.getNode();
        			if (parent instanceof Element) {
        				Element parentElement = (Element) parent;
        				String existingPrefix = parentElement.lookupPrefix(qname.getNamespaceURI());
        				if (existingPrefix == null) {
        					while (parentElement.lookupNamespaceURI(prefix) != null) {
        						prefix = PREFIX + prefix; 
        					}
        					parentElement.setAttributeNS(DOMHelper.XMLNS_URI, DOMHelper.XMLNS_PREFIX +":"+ prefix, 
        							qname.getNamespaceURI());
        				} else {
        					prefix = existingPrefix;
        				}
        			}
        		}
        		targetPtr.setValue(prefix + ":" + qname.getLocalPart());
        		
        	} else if (fromVal instanceof Node) { // <to queryLanguage="anyURI"?>$var.part/query</to>
                /*
                 * This happens for the form <from variable="NCName
                 * "part="NCName"/> in case of WSDL Messages and for the
                 * form <from variable="NCName"/> in case of variables
                 * defined using XSD types
                 */
                Utility.updateNode((Node) targetPtr.getNode(), fromVal);

            } else if ((fromVal instanceof String) || (fromVal instanceof Number)
                    || (fromVal instanceof Boolean) || (fromVal instanceof Date)) {
                // Used for the form using literals -
                // <from><literal>literal value</literal></from>

                // Also when the "From" value is evaluated from a simple type variable and the 
                // "To" expresion has a query, as shown here,
                // <from variable="Variable1"/>
                // <to>$DateTimeSubBPOperationOut.part1/ns0:userString</to>

                // QName and Double value are accounted for in the setValue
                targetPtr.setValue(fromVal);

            } else if ((targetPtr instanceof DOMAttributePointer)) {
                /*
                 * When target is an attribute. Only one attribute can be
                 * selected by using the form <to
                 * queryLanguage="anyURI"?>$var.part/query</to>, so we get
                 * only the first value from the fromVal and set it to
                 * the target attribute.
                 */
                Pointer sourcePtr = (Pointer) ((Iterator) fromVal).next();
                /*
                 * The source can be an DOMAttributePointer, Object (string,
                 * etc), or a Node represent an element with simple context.
                 * sourcePtr.getValue().toString()) will work for all three
                 * cases.
                 */
                if (BPELSEDOMNodePointer.isAttrNodeQNameType((Node) targetPtr.getNode())) {
                    NamedNodeMap attrMap;
                    Attr attrNode;                            
                    Node tmpNode = null;
                    if (sourcePtr instanceof DOMAttributePointer) {
                        tmpNode = (Node) ((DOMAttributePointer) sourcePtr).getParent().getNode();
                    } else if (sourcePtr instanceof BPELSEDOMNodePointer) {
                        tmpNode = (Node) ((BPELSEDOMNodePointer) sourcePtr).getNode();
                    }
                    String prefixVal = DOMHelper.getPrefix(sourcePtr.getValue().toString());
                    Map<String, String> nsMap = new HashMap<String, String>();
                    Element tgtElem = (Element) ((DOMAttributePointer) targetPtr).getParent().getNode();
                    String attrName;

                    nodeLoop: while (tmpNode != null) {
                        attrMap = tmpNode.getAttributes();
                        if (attrMap != null) {
                            attrLoop: for (int i = 0, size = attrMap.getLength();
                            i < size; i++) {
                                attrNode = (Attr) attrMap.item(i);
                                attrName = attrNode.getName();
                                String prefix;
                                if ("xmlns".equals(attrName)) {
                                    prefix = "";
                                } else if (DOMHelper.XMLNS_URI.equals(
                                        attrNode.getNamespaceURI())) {
                                    prefix = DOMHelper.getLocalName(attrName);
                                } else {
                                    continue attrLoop;
                                }
                                if (prefix.equals(prefixVal)) {
                                    nsMap.put(prefix, attrNode.getValue());
                                    break nodeLoop;
                                }
                            }
                        }
                        tmpNode = tmpNode.getParentNode();
                    }
                    int index[] = new int[1];
                    index[0] = 0;
                    for (Entry<String, String> entry: nsMap.entrySet()) {
                        DOMHelper.declareNamespace(entry.getKey(),
                                entry.getValue(), tgtElem, index);
                    }
                    targetPtr.setValue(sourcePtr.getValue());
                } else {
                    targetPtr.setValue(sourcePtr.getValue());
                }

            } else {
                /*
                 * For the variants of the form <from expressionLanguage="anyURI"?>expression</from>.
                 * This expression could result in multiple nodes, so we iterator and set the
                 * values on the target multiple times (repeating nodes)
                 */
                assert fromVal instanceof Iterator;
                Iterator sourcePtrIterator = (Iterator) fromVal;

                for (int predicatePos = 1; sourcePtrIterator.hasNext(); predicatePos++) {
                    Pointer currentSourcePtr = (Pointer) sourcePtrIterator.next();

                    if(predicatePos > 1){
                        /*
                        char[] charArray = toQuery.toCharArray();
                        if (charArray[charArray.length - 1] == PREDICATE_END) {
                            int position = charArray.length - 2;
                            while (charArray[position] != PREDICATE_BEGIN) {
                                position--;
                            }
                            targetQuery = toQuery.substring(0, position);
                        }

                         */
                        String targetQueryWithPred = 
                            targetQuery + PREDICATE_BEGIN + predicatePos + PREDICATE_END;
                        targetPtr = jxpathContext.createPath(targetQueryWithPred);
                    }

                    // Source is an attribute or a result of an expression like
                    // concat('var1.part' + 'some string') or
                    // var1.part/query + some number
                    if (currentSourcePtr instanceof BeanPointer 
                            || currentSourcePtr instanceof DOMAttributePointer) {
                        targetPtr.setValue(currentSourcePtr.getValue());

                    } else {
                        // when ptr is variable pointer and it is simple type this would be Node, so do not typecast to Node
                        Utility.updateNode((Node) targetPtr.getNode(), currentSourcePtr.getNode());
                    }
                }
            }
        }
    }
    
    /**
     * 
     *
     *
     * @author Sun Microsystems
     */
    private static class ToPartnerLinkExecutor implements ToExecutor {

        public void executeTo(To to, Context ctx, Object fromVal, ICallFrame frame) throws Exception {
            PartnerLinkScope partnerLinkScope = (PartnerLinkScope) ctx;
            PartnerLink toPLink = to.getBPELPartnerLink();
            Object fromValue = null;
            if (fromVal instanceof Element) {
                fromValue = constructDocFrag((Element) fromVal); 
            } else if (fromVal instanceof Iterator) {
// TODOvb check the cases when Iterator can be returned
                Pointer sourcePtr = (Pointer) ((Iterator) fromVal).next();
                Element node = (Element) sourcePtr.getNode();
                fromValue = constructDocFrag(node);
            } else {
                // fromVal instanceof DocumentFragment
//TODOvb add as min assertion for that case
                fromValue = fromVal;
            }
            
            RuntimePartnerLink rToPLink = partnerLinkScope.getRuntimePartnerLink(toPLink);
            if (rToPLink == null) {
                rToPLink = partnerLinkScope.createRuntimePartnerLink(toPLink);
            }
            rToPLink.setServiceRef(fromValue);
            partnerLinkScope.setRuntimePartnerLink(toPLink, rToPLink);
        }
        
        private static DocumentFragment constructDocFrag(Element fromValue) throws Exception {
            //TODOvb fromVal of Element type is not neccessary to be a service ref
            // since this should we use additional check to be sure that it is service ref?

            // parse the from Value to obtain the sref document fragment.
            Element serRef = (Element) fromValue;
            // Since this is a service-ref copy, we can safely assume the following 
            // because of the current model parsing.
            NodeList serRefChildNodes = serRef.getChildNodes();
            Node endPointRef = null;
            int j = 0;
            for (; j < serRefChildNodes.getLength(); j++) {
                if (serRefChildNodes.item(j) instanceof Element) {
                    endPointRef = serRefChildNodes.item(j);
                    break;
                }
            }
            DocumentFragment frag = Utility.wrapInDocumentFragment(endPointRef);
            //copy NS declarations from Service-Ref
            NamedNodeMap attrMap = serRef.getAttributes();
            Element newNode = (Element)frag.getFirstChild();
            Attr attrNode = null;
            Attr newAttrNode = null;
            for (int i = 0, size = attrMap.getLength(); i < size; i++) {
                attrNode = (Attr) attrMap.item(i);
                newAttrNode = (Attr) frag.getOwnerDocument().importNode(attrNode, true);
                newNode.setAttributeNode(newAttrNode);
            }
            
            return frag;
        }
    }
}
