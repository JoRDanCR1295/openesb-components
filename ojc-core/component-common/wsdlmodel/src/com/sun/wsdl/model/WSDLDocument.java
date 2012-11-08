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
 * @(#)WSDLDocument.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model;

import java.io.Reader;
import java.io.Writer;
import java.util.Collection;
import java.util.Map;

import com.sun.wsdl.model.bpel.Property;
import com.sun.wsdl.model.bpel.PropertyAlias;
import com.sun.wsdl.model.bpel.Query;
import com.sun.wsdl.model.common.model.Documentation;
import com.sun.wsdl.model.common.model.PrivateExtensionMapModel;
import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.model.XMLElement;
import com.sun.wsdl.model.common.model.XMLNode;
import com.sun.wsdl.model.common.visitor.VisitorService;
import com.sun.wsdl.model.extensions.ExtensibilityElement;
import com.sun.wsdl.model.extensions.soap.SOAPAddress;
import com.sun.wsdl.model.extensions.soap.SOAPBinding;
import com.sun.wsdl.model.extensions.soap.SOAPBody;
import com.sun.wsdl.model.extensions.soap.SOAPFault;
import com.sun.wsdl.model.extensions.soap.SOAPHeader;
import com.sun.wsdl.model.extensions.soap.SOAPHeaderFault;
import com.sun.wsdl.model.extensions.soap.SOAPOperation;

/**
 * Describes a WSDL document.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface WSDLDocument extends XMLDocument, XMLElement {
    
    /** Preferred prefix for WSDL */
    public static final String WSDL_PREFIX = "wsdl";
    
    /** Namespace for WSDL */
    public static final String WSDL_NAMESPACE =
        "http://schemas.xmlsoap.org/wsdl/";
    
    /** Prefix for BPEL document */
    public static final String BPEL_PREFIX = "bpws";
    
    /** Namespace for BPEL document */
    public static final String BPEL_NAMESPACE =
        "http://docs.oasis-open.org/wsbpel/2.0/process/executable";
    
    /** Namespace for Property and PropertyAlias */
    public static final String BPEL_PROP_NAMESPACE =
        "http://docs.oasis-open.org/wsbpel/2.0/varprop";
    
    /** Preferred prefix for WSDL Service Link Type */
    public static final String WSDL_SLNK_PREFIX = "slnk";
    
    /** Namespace for WSDL Service Link Type */
    public static final String WSDL_SLNK_NAMESPACE =
        "http://docs.oasis-open.org/wsbpel/2.0/plnktype";
    
    /** Preferred prefix for WSDL Service Reference */
    public static final String WSDL_SREF_PREFIX = "sref";
    
    /** Namespace for WSDL Service Reference */
    public static final String WSDL_SREF_NAMESPACE =
        "http://schemas.xmlsoap.org/ws/2002/07/service-reference/";
    
    
    /**
     * Loads the WSDL document from the reader.
     * @param reader the reader
     */
    void load(Reader reader, String baseURI);
    
    /**
     * Loads the WSDL document from the reader.
     * @param reader the reader
     * @param parseSchemas whether or not parse imported and inline schemas
     */
    void load(Reader reader, WSDLParseContext context, String baseURI);
    
    /**
     * Serializes the WSDL document to the writer.
     * @param writer the writer
     */
    void serialize(Writer writer);
    
    /**
     * Duplicates (deep cloning) an element.
     * @param   original    Original element to be cloned.
     * @param   destination Destination document.
     * @return  Duplicated element.
     */
    XMLNode duplicate(XMLNode original, WSDLDocument destination);
    
    /**
     * Validates the BPEL document.
     * @param   todoListeners   Collection of <code>ToDoListener</code> for <code>ToDoEvent</code>
     */
    void validate(Collection todoListeners);
    
    /** Gets an imported document according to namespace prefix.
     * @param   elem    Element from which imported document is referenced.
     * @param   prefix  Namespace prefix.
     * @return  Imported document.
     */
    XMLDocument getImportedDocument(XMLElement elem, String prefix);
    
    /** Creates a XML node for this document.
     * @param   t   Type of node to create.
     *  <table border="1">
     *  <th>Type</th><th>XML Node created</th>
     *  <tr><td>"part"</td><td>PartImpl</td></tr>
     *  <tr><td>"#processingInstruction"</td><td>XMLProcessingInstructionImpl
     *  </td></tr>
     *  <tr><td>"message"</td><td>WSDLMessageImpl</td></tr>
     *  <tr><td>"#comment"</td><td>XMLCommentImpl</td></tr>
     *  <tr><td>"#text"</td><td>XMLTextImpl</td></tr>
     *  </table>
     * @return  New XML node.
     */
    XMLNode createXmlNode(String t);
    
    /** Creates a part element.
     * @return  new part element.
     */
    Part createPart();
    
    /** Creates a wsdl:messsage element.
     * @return  new wsdl:message element.
     */
    WSDLMessage createWSDLMessage();
    
    /**
     * Creates a &lt;definitions&gt; element.
     * @return new definitions element
     */
    WSDLDefinitions createDefinitions();
    
    /**
     * Sets the root &lt;definitions&gt; element for the WSDL document.
     * @param definitions the root element
     */
    void setDocumentDefinitions(WSDLDefinitions definitions);
    
    /**
     * Gets the root &lt;definitions&gt; element of the WSDL document.
     * @return the root element
     */
    WSDLDefinitions getDocumentDefinitions();
    
    /**
     * Creates a &lt;portType&gt; element.
     * @return new portType element
     */
    PortType createPortType();
    
    /**
     * Creates a &lt;operation&gt; element.
     * @return new operation element
     */
    Operation createOperation();
    
    /**
     * Creates an operation &lt;input&gt; element.
     * @return new input element
     */
    OperationInput createOperationInput();
    
    /**
     * Creates an operation &lt;output&gt; element.
     * @return new output element
     */
    OperationOutput createOperationOutput();
    
    /**
     * Creates an operation &lt;fault&gt; element.
     * @return new fault element
     */
    OperationFault createOperationFault();
    
    /**
     * Creates a &lt;service&gt; element.
     * @return new service element
     */
    Service createService();
    
    /**
     * Creates a &lt;port&gt; element.
     * @return new port element
     */
    ServicePort createServicePort();
    
    /**
     * Creates a &lt;partnerLinkType&gt; element.
     * @return new partnerLinkType element
     */
    PartnerLinkType createPartnerLinkType();
    
    /**
     * Creates a partnerLinkType &lt;role&gt; element.
     * @return new role element
     */
    PartnerLinkRole createPartnerLinkRole();
    
    /**
     * Creates an &lt;import&gt; element.
     * @return new import element
     */
    Import createImport();
    
    /**
     * Creates a &lt;binding&gt; element.
     * @return new binding element.
     */
    Binding createBinding();
    
    /**
     * Creates a SOAP &lt;binding&gt; element.
     * @return new SOAP binding element.
     */
    SOAPBinding createSOAPBinding();
    
    /**
     * Creates a binding &lt;operation&gt; element.
     * @return new binding operation element.
     */
    BindingOperation createBindingOperation();
    
    /**
     * Creates a SOAP &lt;operation&gt; element.
     * @return new SOAP operation element.
     */
    SOAPOperation createSOAPOperation();
    
    /**
     * Creates a binding &lt;input&gt; element.
     * @return new binding input element.
     */
    BindingInput createBindingInput();
    
    /**
     * Creates a binding &lt;output&gt; element.
     * @return new binding output element.
     */
    BindingOutput createBindingOutput();
    
    /**
     * Creates a binding &lt;fault&gt; element.
     * @return new binding fault element.
     */
    BindingFault createBindingFault();
    
    /**
     * Creates a SOAP &lt;body&gt; element.
     * @return new SOAP body element.
     */
    SOAPBody createSOAPBody();
    
    /**
     * Creates a SOAP &lt;header&gt; element.
     * @return new SOAP header element.
     */
    SOAPHeader createSOAPHeader();
    
    /**
     * Creates a SOAP &lt;headerfault&gt; element.
     * @return new SOAP headerfault element.
     */
    SOAPHeaderFault createSOAPHeaderFault();
    
    /**
     * Creates a SOAP &lt;fault&gt; element.
     * @return new SOAP fault element.
     */
    SOAPFault createSOAPFault();
    
    /**
     * Creates a SOAP &lt;address&gt; element.
     * @return new SOAP address element.
     */
    SOAPAddress createSOAPAddress();
    
    /**
     * Creates a &lt;documenation&gt; element.
     * @return new documentation element.
     */
    Documentation createDocumentation();
    
    
    WSDLDocumentation createWSDLDocumentation();
    	
    
    /**
     * Creates a &lt;types&gt; element.
     * @return new types element.
     */
    Types createTypes();
    
    /**
     * Creates a generic extensibility element.
     * @return new extensibility element.
     */
    ExtensibilityElement createExtensibilityElement();
    
    /**
     * Creates a BPEL &lt;bpws:property&gt; element.
     * @return new bpws:property element.
     */
    Property createBPWSProperty();
    
    
    /**
     * Creates a BPEL &lt;bpws:propertyAlias&gt; element.
     * @return new bpws:propertyAlias element.
     */
    PropertyAlias createBPWSPropertyAlias();
    
    
    /**
     * Creates a BPEL &lt;bpws:query&gt; element.
     * @return new bpws:query element.
     */
    Query createBPWSQuery();
    
    /**
     * Traverses the document to perform some work by the visitor
     * that is provided by the service. 
     *
     * @param   s   Visitor service provider.
     * @param   v   Values to prepare the persistor.
     */
    void traverse(VisitorService s, Object[] v);    
     
    
    /** Gets the inline Schema's imported XMLSchema to Schema correlation map.
     * @return  Correlation map.
     */
    Map getInlineXMLSchemaSchemaMap();
    
    /** Gets the private extension map model associated with this document's associated
     *  repository object (if any).
     *
     *  @return Private extension map model for this document.
     *  @since  5.1.0
     */
    PrivateExtensionMapModel getPrivateExtensionMapModel();
  
    /**
     * get the latest wsdl parse context to be used for parsing wsdls
     * @return WSDLParseContext
     */
    WSDLParseContext getWSDLParseContext();
}
