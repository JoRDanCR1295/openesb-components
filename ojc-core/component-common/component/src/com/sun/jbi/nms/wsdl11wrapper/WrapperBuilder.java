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
 * @(#)WrapperBuilder.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.nms.wsdl11wrapper;

import java.util.Map;
import javax.wsdl.Message;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.sun.jbi.nms.wsdl11wrapper.util.WrapperUtil;

/**
 * Assist in processing normalized messages with JBI WSDL 1.1 wrappers
 *
 * Usage sequence, for each message to normalize:
 * initialize()
 * addParts() or multiple addPart() calls
 * getResult()
 *
 * The same instance should not be used by multiple threads concurrently as it is
 * not guaranteed to be thread safe - and maintains state
 *
 * The re-use of the same instance however is encouraged.
 *
 * @author Sun Microsystems
 */
public interface WrapperBuilder {

    /**
     * Constant Status
     */
    static public final String STATUS_TAG = "Status";

    /**
     * Constant RESULT
     */
    static public final String RESULT_TAG = "Result";
    
    
    static public final String XOP_NAMESPACE = "http://www.w3.org/2004/08/xop/include";

    /**
     * Initialize the builder to start a build sequence.
     *
     * Also re-sets a result document if it already exists.
     *
     * @pram docToPopulate Provide an empty document to popluate, or null if the
     * builder should create a new document itself
     * @param wsdlMessageDefinition sets the WSDL message definition of the message to normalize
     * @param operationBindingMessageName The name defined in the WSDL operation binding for the message to normalize to.
     *
     * @throws WrapperProcessingException if the builder could not be initialized
     */
    void initialize(Document docToPopulate, Message wsdlMessageDefinition, String operationBindingMessageName) throws WrapperProcessingException;
    
    
    /**
     * 
     * the message wrapper is created at this point , does not wait till
     * the first part that is added to the builder.
     *
     *
     * @param wsdlMessageDefinition sets the WSDL message definition of the message to normalize
     * @param operationBindingMessageName The name defined in the WSDL operation binding for the message to normalize to.
     * @param docToUse , document to use
     *
     * @throws WrapperProcessingException if the builder could not be initialized
     */
    void initialize( Message wsdlMessageDefinition, String operationBindingMessageName,Document docToUse) throws WrapperProcessingException;
    
    
    
    /**
     * Initialize the builder to start a build sequence.
     *
     * This overloaded version uses the DOM document from the first part added
     * instead of using a new document which requires nodes to be imported.
     *
     * @pram docToPopulate Provide an empty document to popluate, or null if the
     * builder should create a new document itself
     * @param wsdlMessageDefinition sets the WSDL message definition of the message to normalize
     * @param operationBindingMessageName The name defined in the WSDL operation binding for the message to normalize to.
     *
     * @throws WrapperProcessingException if the builder could not be initialized
     */
    void initialize(Message wsdlMessageDefinition, String operationBindingMessageName) throws WrapperProcessingException;

    /**
     * Declares namespace attributes on the JBI message wrapper element.
     * 
     * @param namespaceMap the map of namespace declarations
     * @see WrapperUtil#extractNamespaceDeclarations(Element)
     */
    void declareGlobalNS(Map namespaceMap);
    
    
    /**
     * Add attributes (e.g. xsi:type) on the JBI message wrapper element.
     * 
     * @param attributeMap the map of attribute declarations
     * @see WrapperUtil#extractAttributeDeclarations(Element)
     */
    void declareGlobalAttributes(Map attrMap);
    
    /**
     * Add a part in the right position (wrapped in a JBI part wrapper) to the JBI message wrapper element
     * @param partName the name of the message part
     * @param partNode the part node (payload)
     * The part node does not have to be associated with the normalDoc yet, it will be imported
     * @throws WrapperProcessingException if the part could not be added
     */
    void addPart(String partName, Element partNode) throws WrapperProcessingException;

    /**
     * Add a part in the right position (wrapped in a JBI part wrapper) to the JBI message wrapper element
     * @param partName the name of the message part
     * @param partNodes the part node(s) (payload)
     * The part node does not have to be associated with the normalDoc yet, it will be imported
     * @throws WrapperProcessingException if the part could not be added
     */
    void addPart(String partName, NodeList partNodes) throws WrapperProcessingException;

    /**
     * Add parts in the right order (each wrapped in a JBI part wrapper) to the passed in JBI message wrapper element
     * The jbiMessageWrapper must be a node of the normalDoc.
     * @param normalDoc The target document of the normalization
     * @param jbiMessageWrapper The message wrapper element to add the jbi part wrappers and part payload to
     * @param messageDefinintion the WSDL message definition
     * @param partNameToPartNodes a mapping from the part name to the part node(s) of type NodeList (payload),
     * The part node does not have to be associated with the normalDoc yet, it will be imported
     * @throws WrapperProcessingException if the parts could not be added
     */
    void addParts(Map partNameToPartNodes) throws WrapperProcessingException;

    
    /**
     * Overloaded version of {@link #addPart(java.lang.String, org.w3c.dom.Element)} that also
     * accepts a map of namespace attributes to be declared on the added JBI part.
     * 
     * @param partName the name of the message part
     * @param partNode the part node (payload)
     * @param namespaceMap the map of namespace declarations
     * @throws WrapperProcessingException if the part could not be added
     */
    void addPart(String partName, Element partNode, Map namespaceMap) throws WrapperProcessingException;
    
    
    /**
     * Overloaded version of {@link #addPart(java.lang.String, org.w3c.dom.Element)} that also
     * accepts a map of namespace definitions and a map of attribute declarations to be added on the added JBI part.
     * 
     * @param partName the name of the message part
     * @param partNode the part node (payload)
     * @param namespaceMap the map of namespace declarations
     * @param attributesMap the map of attribute declarations
     * @throws WrapperProcessingException if the part could not be added
     */
    void addPart(String partName, Element partNode, Map namespaceMap, Map attributesMap) throws WrapperProcessingException;
    

    /**
     * Overloaded version of {@link #addPart(java.lang.String, org.w3c.dom.Element)} that also
     * accepts a map of namespace attributes to be declared on the added JBI part.
     * 
     * @param partName the name of the message part
     * @param partNodes the part node(s) (payload)
     * @param namespaceMap the map of namespace declarations
     * @throws WrapperProcessingException if the part could not be added
     */
    void addPart(String partName, NodeList partNodes, Map namespaceMap) throws WrapperProcessingException;
    
    /**
     * Overloaded version of {@link #addPart(java.lang.String, org.w3c.dom.Element)} that also
     * accepts a map of namespace definitions and a map of attribute declarations to be added on the added JBI part.
     * 
     * @param partName the name of the message part
     * @param partNodes the part node(s) (payload)
     * @param namespaceMap the map of namespace declarations
     * @param attributesMap the map of attribute declarations
     * @throws WrapperProcessingException if the part could not be added
     */
    void addPart(String partName, NodeList partNodes, Map namespaceMap, Map attributesMap) throws WrapperProcessingException;
    
    
    /**
     * Add a part which contains an xop:Include reference. The xop:Include reference should point
     * to a JBI attachment (javax.activation.DataHandler object)
     * 
     * @param partName the name of the message part
     * @return the content ID of the attachment. The content ID is also "href" attribute in the xop:Include element
     * @throws WrapperProcessingException if the part could not be added
     */
    String addPartWithAttachment(String partName) throws WrapperProcessingException;
    
    /**
     * Add a part which contains an xop:Include reference. The xop:Include reference should point
     * to a JBI attachment (javax.activation.DataHandler object)
     * 
     * @param partName the name of the message part
     * @param content ID matching the ID of the DataHandler encapsulating the attachment content
     * @throws WrapperProcessingException if the part could not be added
     */
    void addPartWithAttachment(String partName, String contentId) throws WrapperProcessingException;
    
    /**
     * Overloaded version of {@link #addPart(java.lang.String, org.w3c.dom.Element)} that also
     * accepts a map of namespace attributes to be declared on <b>ALL</b> of the added JBI parts.
     * 
     * @param partNameToPartNodes a mapping from the part name to the part node(s) of type NodeList (payload),
     * @param namespaceMap the map of namespace declarations
     * @throws WrapperProcessingException if the parts could not be added
     */
    void addParts(Map partNameToPartNodes, Map namespaceMap) throws WrapperProcessingException;

    /**
     * Obtain the result document, i.e. the Normalized Message payload in
     * jbi wsdl 1.1 wrapper format
     * @return the normalized message payload document
     */
    Document getResult() throws WrapperProcessingException;

    /**
     * Obtain the status document, i.e. the Normalized Message payload in
     * jbi wsdl 1.1 wrapper format that represents a status (succeed or error)
     * @return the normalized message payload document
     */
    Document getStatusDocument(String statusMessage) throws WrapperProcessingException;

    /**
     * Obtain the status Message definition
     * @return the normalized message payload document
     */
    Message getStatusMessage() throws WrapperProcessingException;
}
