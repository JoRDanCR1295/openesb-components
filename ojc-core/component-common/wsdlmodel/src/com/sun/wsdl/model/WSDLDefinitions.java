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
 * @(#)WSDLDefinitions.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model;

import java.util.Collection;

import org.exolab.castor.xml.schema.ElementDecl;
import org.exolab.castor.xml.schema.XMLType;

import com.sun.wsdl.model.bpel.Property;
import com.sun.wsdl.model.bpel.PropertyAlias;
import javax.xml.namespace.QName;
import com.sun.wsdl.model.common.model.XMLDocumentElement;

/**
 * Describes the WSDL &lt;definitions&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface WSDLDefinitions
    extends WSDLExtensibleElement, XMLDocumentElement {
        
    /** Tag for this element */
    public static final String TAG = "definitions";
    
    /** Describes the attributes for this element */
    public interface ATTR extends WellKnownAttr {
        
        /** "targetNamespace" attribute token */
        public static final String TARGET_NAMESPACE = "targetNamespace";
        
        /** "name" attribute token */
        public static final String NAME = "name";
        
        /** "uri" attribute token -- eGate extension */
        public static final String URI = "uri";
    }
    
    /** Ordinal position of targetNamespace attribute. */
    public static final int TARGET_NAMESPACE = 0;
    
    /** Ordinal position of name attribute. */
    public static final int NAME = 1;
    
    /** Ordinal position of the uri attribute. */
    public static final int URI = 2;
    
    /**
     * Gets the list of all imported WSDL documents.
     * @return a read-only collection of WSDLDocuments
     */
    Collection getImportedDocuments();
    
    /**
     * Recursively gets the list of all imported WSDL documents including this one.
     * @return  A unmodifiable collection of all imported (including child imports) WSDL's
     */
    Collection getAllDocuments();
    
    /**
     * Gets the list of all imported XML Schemas.
     * @return a read-only collection of XML Schemas.
     */
    Collection getImportedXMLSchemas();
    
    /**
     * Setter for property targetNamespace.
     * @param qName             New qName of property targetNamespace.
     * @param targetNamespace   New value of property targetNamespace.
     */
    void setTargetNamespace(String qName, String targetNamespace);
    
    /**
     * Getter for property name.
     * @return Value of property name.
     */
    String getName();
    
    /**
     * Setter for property name.
     * @param name   New value of property name.
     */
    void setName(String name);
    
    /**
     * Getter for property uri.
     * @return Value of property uri.
     */
    String getURI();
    
    /**
     * Setter for property uri.
     * @param uri   New value of property uri.
     */
    void setURI(String uri);
    
    /**
     * Getter for property message.
     * @param messageLocalName name of the message, this should not localName.
     * @return Value of property message.
     *
     */
    WSDLMessage getMessage(String messageLocalName);
    
    /**
     * get WSDLMessage given the QName. Also look into imported document.
     * @param name QName of the message.
     * @return WSDLMessage
     *
     */
    WSDLMessage getMessage(QName name);
    
    
    /**
     * Adds a new message.
     * @param message the message to add
     */
    void addMessage(WSDLMessage message);
    
    /**
     * remove all messages.
     *
     */
    void removeAllMessages();
    
    /**
     * Removes the given message.
     * @param message to be removed
     */
    void removeMessage(WSDLMessage message);
    
    
    /**
     * Gets the list of all messages.
     * @return a read-only collection of WSDLMessages.
     */
    Collection getMessages();
    
    /**
     * Getter for property portType.
     * @param name name of the PortType
     * @return portType.
     *
     */
    PortType getPortType(String name);
    
    /**
     * Get the PortType given the QName. 
     * Also look into the imported document.
     * @param name name of the PortType
     * @return portType.
     *
     */
    PortType getPortType(QName name);
    
    /**
     * Getter for property portType.
     * @param index index of the property to get
     * @return Value of property portType.
     *
     */
    PortType getPortType(int index);
    
    
    /**
     * Adds a new portType.
     * @param portType the portType to add
     */
    void addPortType(PortType portType);
    
    
    /**
     * Removes the given porType.
     * @param portType to be removed
     */
    void removePortType(PortType portType);
    
    /**
     * Gets the number of portTypes.
     * @return the number of portTypes
     */
    int countPortTypes();
    
    /**
     * Gets the list of all portTypes
     * @return a read-only collection of PortTypes.
     */
    Collection getPortTypes();
    
    /**
     * Getter for property serviceLinkType.
     * @param name name of serviceLinkType
     * @return serviceLinkType.
     *
     */
    PartnerLinkType getServiceLinkType(String name);
    
    
    /**
     * Get PartnerLinkType for given QName. Also look into imported wsdls.
     * @param name Qname of serviceLinkType
     * @return serviceLinkType.
     *
     */
    PartnerLinkType getServiceLinkType(QName name);
    
    
    
    /**
     * Adds a new serviceLinkType.
     * @param serviceLinkType the serviceLinkType to add
     */
    void addServiceLinkType(PartnerLinkType serviceLinkType);
    
    
    /**
     * Removes the given serviceLinkType.
     * @param serviceLinkType to be removed
     */
    void removeServiceLinkType(PartnerLinkType serviceLinkType);
    
    
    /**
     * Gets the list of all serviceLinkTypes
     * @return a read-only collection of Services.
     */
    Collection getServiceLinkTypes();
    
    /**
     * Getter for property service.Finds first matching ServicePort in 
     * first matching Service.
     * @param portType PortType whose Service we are looking.
     * @return Collection of ServicePorts.
     *
     */
    Collection getServicePort(PortType portType);
    
    
    /**
     * Get the Service given QName. Also look into imported documents.
     * @param name name of the service
     * @return Value of property service.
     *
     */
    Service getService(QName name);
    
    /**
     * Getter for property service.
     * @param name name of the service
     * @return Value of property service.
     *
     */
    Service getService(String name);
    
    
    /**
     * Adds a new service.
     * @param service the service to add
     */
    void addService(Service service);
    
    
    /**
     * Removes the given service.
     * @param service to be removed
     */
    void removeService(Service service);
    
    
    /**
     * Gets the list of all services
     * @return a read-only collection of Services.
     */
    Collection getServices();
    
    
    /**
     * get the import for a namespace
     * @param namespace
     * @return Import
     */
    Import getImport(String namespace);
    
    /**
     * Adds a new import.
     * @param wsdlImport the import to add
     */
    void addImport(Import wsdlImport);
    
    
    /**
     * Removes the given import.
     * @param wsdlImport to be removed
     */
    void removeImport(Import wsdlImport);
    
   
    /**
     * Gets the list of all imports
     * @return a read-only collection of Imports.
     */
    Collection getImports();
    
    /**
     * get Binding given the QName. Also look into imported document.
     * @param name QName of the message.
     * @return Binding
     *
     */
    Binding getBinding(QName name);
    
    /**
     * Getter for property binding.
     * @param name name of the binding
     * @return Binding.
     *
     */
    Binding getBinding(String name);
    
    
    
    /**
     * Adds a new binding.
     * @param binding the binding to add
     */
    void addBinding(Binding binding);
    
    
    /**
     * Removes the given binding.
     * @param binding to be removed
     */
    void removeBinding(Binding binding);
    
    
    /**
     * Gets the list of all bindings
     * @return a read-only collection of bindings.
     */
    Collection getBindings();
    
    /**
     * Gets the types sub-element.
     * @return  types sub-element.
     */
    Types getTypes();
    
    /**
     * Sets the types sub-element.
     * @param   types   types sub-element.
     */
    void setTypes(Types types);
    /** Getter for the bpws:property
     * @param index Index of the property.
     * @return property instance at <CODE>index</CODE>.
     *
     */
    Property getBPWSProperty(int index);
    
    /** Number of bpws:property elements present.
     * @return  Number of property elements.
     */
    int countBPWSProperties();
    
    /** Add a new bpws:property to the list.
     * @param   prop    New property instance.
     */
    void addBPWSProperty(Property prop);
    
    /** Removes a bpws:property element from the list.
     * @param   i   Index to the bpws:property element.
     */
    void removeBPWSProperty(int i);
    
    /** Removes a bpws:property element from the list.
     * @param   prop    bpws:property element to remove.
     */
    void removeBPWSProperty(Property prop);
    
    /** Gets a non-modifiable collection of all bpws:property elements.
     * @return Collection of property elements.
     */
    Collection getBPWSProperties();
    
    /** Gets a bpws:property element matching name
     * @return Property element.
     */
    Property getBPWSProperty(String propertyName);
    
    /** Gets a bpws:property element matching QName
     * @return Property element.
     */
    Property getBPWSProperty(QName propertyQName);
    
    /** Getter for the bpws:propertyAlias
     * @param index Index of the propertyAlias.
     * @return propertyAlias instance at <CODE>index</CODE>.
     *
     */
    PropertyAlias getBPWSPropertyAlias(int index);
    
    
    /** Number of bpws:propertyAlias elements present.
     * @return  Number of propertyAlias elements.
     */
    int countBPWSPropertyAliases();
    
    /** Add a new bpws:propertyAlias to the list.
     * @param   propAlias   New propertyAlias instance.
     */
    void addBPWSPropertyAlias(PropertyAlias propAlias);
    
    /** Removes a bpws:propertyAlias element from the list.
     * @param   i   Index to the bpws:propertyAlias element.
     */
    void removeBPWSPropertyAlias(int i);
    
    /** Removes a bpws:propertyAlias element from the list.
     * @param   propAlias   bpws:propertyAlias element to remove.
     */
    void removeBPWSPropertyAlias(PropertyAlias propAlias);
    
    /** Gets a non-modifiable collection of all bpws:propertyAlias elements.
     * @return Collection of propertyAlias elements.
     */
    Collection getBPWSPropertyAliases();

    /**
     * Returns a collection of all complex types in the model recursively.
     * @return the collection of all complex types
     */
    Collection getSchemaTypes();
    
    /**
     * Gets the complex types with the given name in the given target namespace.
     * @param name the name of the type
     * @param targetNamespace the target namespace
     * @return the type or null if not found
     */
    Object getSchemaType(String name, String targetNamespace);
    
    /**
     * Returns a collection of all XML Schema elements in the model.
     * @return the collection of all XML Schema elements
     */
    Collection getSchemaElements();
    
    /**
     * Gets the XML Schema with the given name in the given target namespace.
     * @param name the name of the type
     * @param targetNamespace the target namespace
     * @return the element or null if not found
     */
    Object getSchemaElement(String name, String targetNamespace);
    
 
    /**
     * Get ElementDecl object given its QName.
     * First looks into imported wsdls.
     * Then looks into imported xsds.
     * @param elementQName
     * @return
     */
    ElementDecl getXSDElement(QName elementQName);
    
    /**
     * Get XMLType  (Simple or Complex) object given its QName.
     * First looks into imported wsdls.
     * Then looks into imported xsds.
     * @param elementQName
     * @return
     */
    XMLType getXSDType(QName typeQName);
    
    /**
     * Returns recursively a collection of all XML Schemas in the given WSDL document.
     * @return the collection of all XML Schemas in the WSDL document.
     * element
     */
    public Collection getXMLSchemas();

    /**
     *  Gets the XML Schemas available here at this WSDL Document level.
     * @return  Collection of XML Schemas available here.
     */
    public Collection getXMLSchemasHere();
    
    void addImportAtTail(Import wsdlImport);
    
    void setTypesAtTail(Types types);
    
    void addMessageAtTail(WSDLMessage message);
    
    void addPortTypeAtTail(PortType portType);
    
    void addBindingAtTail(Binding binding);
    
    void addServiceAtTail(Service service); 
	
    void addServiceLinkTypeAtTail(PartnerLinkType serviceLinkType); 
	
    void addBPWSPropertyAtTail(Property prop);
    
    void addBPWSPropertyAliasAtTail(PropertyAlias propAlias);

    /**
     * Validates the BPEL document.
     * @param   todoListeners   Collection of <code>ToDoListener</code> for <code>ToDoEvent</code>
     */
    void validate(Collection todoListeners);
}
