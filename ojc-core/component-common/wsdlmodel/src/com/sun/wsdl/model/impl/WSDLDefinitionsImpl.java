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
 * @(#)WSDLDefinitionsImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.impl;


import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;
import javax.xml.namespace.QName;

import org.exolab.castor.xml.schema.ElementDecl;
import org.exolab.castor.xml.schema.Schema;
import org.exolab.castor.xml.schema.XMLType;

import com.sun.wsdl.model.Binding;
import com.sun.wsdl.model.BindingOperation;
import com.sun.wsdl.model.Import;
import com.sun.wsdl.model.NamespaceUtility;
import com.sun.wsdl.model.Part;
import com.sun.wsdl.model.PortType;
import com.sun.wsdl.model.Service;
import com.sun.wsdl.model.PartnerLinkType;
import com.sun.wsdl.model.ServicePort;
import com.sun.wsdl.model.Types;
import com.sun.wsdl.model.WSDLDefinitions;
import com.sun.wsdl.model.WSDLDocument;
import com.sun.wsdl.model.WSDLExtensibleElement;
import com.sun.wsdl.model.WSDLMessage;
import com.sun.wsdl.model.bpel.Property;
import com.sun.wsdl.model.bpel.PropertyAlias;
import com.sun.wsdl.model.common.model.XMLAttribute;
import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.model.XMLNode;
import com.sun.wsdl.model.common.model.impl.XMLAttributeImpl;
import com.sun.wsdl.model.common.todotask.ToDoListener;
import com.sun.wsdl.model.common.util.ArtifactDictionary;
import com.sun.wsdl.model.common.util.ArtifactDictionaryFactory;
import com.sun.wsdl.model.common.visitor.Visitor;
import com.sun.wsdl.model.common.visitor.VisitorService;
import com.sun.wsdl.model.common.visitor.XMLParseVisitorException;
import com.sun.wsdl.model.extensions.ExtensibilityElement;
import com.sun.wsdl.model.extensions.soap.SOAPBinding;
import com.sun.wsdl.model.extensions.soap.SOAPConstants;
import com.sun.wsdl.model.extensions.soap.SOAPHeader;
import com.sun.wsdl.model.extensions.soap.SOAPOperation;
import com.sun.wsdl.model.visitor.ValidateVisitor;
import com.sun.wsdl.model.visitor.ValidateVisitorService;
import com.sun.wsdl.model.visitor.WSDLVisitor;
import com.sun.wsdl.model.xsd.CastorSupport;
import com.sun.wsdl.model.xsd.XMLSchema;
import com.sun.wsdl.model.xsd.impl.CastorSupportImpl;
 
/**
 * Implements the WSDL &lt;definitions&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class WSDLDefinitionsImpl extends WSDLExtensibleElementImpl implements WSDLDefinitions {
        
    /**
     * The logger.
     */
    private static Logger mLogger =
        Logger.getLogger(WSDLDefinitionsImpl.class.getName());
        
    /**
     * List of messages.
     */
    private List mMessages;
    
    /**
     * List of port types.
     */
    private List mPortTypes;
    
    /**
     * List of service link types.
     */
    private List mServiceLinkTypes;
    
    /**
     * List of services.
     */
    private List mServices;
    
    /**
     * List of import elements.
     */
    private List mImportElements;
    
    /**
     * List of binding elements.
     */
    private List mBindings;
    
    /**
     * Holds the types element.
     */
    private Types mTypes;
    
    /**
     * Holds the list of bpws:property elements.
     */
    private List mProperties;
    
    /**
     * Holds the list of bpws:propertyAlias elements.
     */
    private List mPropertyAliases;
    
    /**
     * Holds the CastorSupport implementing object.
     */
    private CastorSupport mACS = null;
    
    /** Holds cache of artifact dictionary */
    protected ArtifactDictionary mArtDict = null;
    
    /** Holds selector for artifact dictionary */
    protected Set mSelector = null;
    
    
    /**
     * Creates a new instance of WSDLDefinitionsImpl.
     */
    public WSDLDefinitionsImpl() {
        super();
        initWSDLDefinitions();
    }
    
    /**
     * Creates a new instance of WSDLDefinitionsImpl.
     * @param   d   Owner document.
     */
    public WSDLDefinitionsImpl(XMLDocument d) {
        super(d);
        initWSDLDefinitions();
    }
    
    /**
     * Initializes this class.
     */
    private void initWSDLDefinitions() {
        mMessages = new ArrayList();
        mPortTypes = new ArrayList();
        mServiceLinkTypes = new ArrayList();
        mServices = new ArrayList();
        mImportElements = new ArrayList();
        mBindings = new ArrayList();
        mProperties = new ArrayList();
        mPropertyAliases = new ArrayList();
        
        setLocalName(WSDLDefinitions.TAG);
        setDefaultNamespace(WSDLDocument.WSDL_NAMESPACE);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(WSDLDefinitions.ATTR.TARGET_NAMESPACE, String.class, true,
                                 null),
            new XMLAttributeImpl(WSDLDefinitions.ATTR.NAME, String.class, true, null),
            new XMLAttributeImpl(WSDLDefinitions.ATTR.URI, String.class, true, null)
        };
        childrenTags = new String[] {
            Import.TAG,
            WSDLMessage.TAG,
            PortType.TAG,
            Binding.TAG,
            Service.TAG,
            PartnerLinkType.TAG,
            "", //BPELDocument.BPEL_PREFIX + ":" + Property.TAG,
            "" //BPELDocument.BPEL_PREFIX + ":" + PropertyAlias.TAG
        };
    }
    
    
   

    
    /** @see WSDLDefinitions#getImportedDocuments()
     */
    public Collection getImportedDocuments() {
    	ArrayList wsdls = new ArrayList();
    	Iterator it = this.getImports().iterator();
    	while(it.hasNext()) {
    		Import imp = (Import) it.next();
    		Object importedObject = imp.getImportedObject();
    		
    		if(importedObject instanceof WSDLDocument) {
    			wsdls.add(importedObject);
    		}
    	}
    	
        return Collections.unmodifiableCollection(wsdls);
    }
    
    /*
     * get all imported documents. Find recursively all the imported documents.
     */
    public Collection getAllImportedDocuments() {
    	HashSet hs = new HashSet();
        rgetImportedDocuments(hs);
        return Collections.unmodifiableCollection(hs);
    }
    
    /** @see WSDLDefinitions#getAllDocuments
     */
    public Collection getAllDocuments() {
        HashSet hs = new HashSet();
        hs.add(getOwnerDocument());
        rgetImportedDocuments(hs);
        return Collections.unmodifiableCollection(hs);
    }
    
    /** Helper function for recursive getting of imported WSDL documents.
     * @param   hs      A <code>HashSet</code> object to contain results.
     */
    private void rgetImportedDocuments(HashSet hs) {
        Collection impColl = getImportedDocuments();
        if (impColl.size() > 0) {
            Iterator iter = impColl.iterator();
            while (iter.hasNext()) {
                WSDLDocument impDoc = (WSDLDocument) iter.next();
                
                // Defend against cyclic dependencies
                if (!hs.contains(impDoc)) {
                    hs.add(impDoc);
                    WSDLDefinitionsImpl wsdlDefsImpl = (WSDLDefinitionsImpl) impDoc.getDocumentDefinitions();
                    if (wsdlDefsImpl != null) {
                        wsdlDefsImpl.rgetImportedDocuments(hs);
                    }
                }
            }
        }
    }
    
    /**
     * @see WSDLDefinitions#getImportedXMLSchemas
     */
    public Collection getImportedXMLSchemas() {
    	ArrayList schemas = new ArrayList();
    	Iterator it = this.getImports().iterator();
    	while(it.hasNext()) {
    		Import imp = (Import) it.next();
    		Object importedObject = imp.getImportedObject();
    		
    		if(importedObject instanceof XMLSchema) {
    			schemas.add(importedObject);
    		}
    	}
    	
        return Collections.unmodifiableCollection(schemas);
    }
    
    /**
     * Returns targetNamespace attribute of this document.
     * If targetNamespace of this document is not defined
     * then it will return its owner documents targetNamespace.
     * @return Value of property targetNamespace.
     */
    public String getTargetNamespace() {
        String tNs = xmlAttrs[TARGET_NAMESPACE].getValue();
        return tNs;
    }
    
    /**
     * Setter for property targetNamespace.
     * @param targetNamespace   New value of property targetNamespace.
     */
    public void setTargetNamespace(String targetNamespace) {
        setAttribute(TARGET_NAMESPACE, targetNamespace);
    }
    
    /**
     * Setter for property targetNamespace.
     * @param qName             New qName of property targetNamespace.
     * @param targetNamespace   New value of property targetNamespace.
     */
    public void setTargetNamespace(String qName, String targetNamespace) {
        setAttribute(TARGET_NAMESPACE, qName, targetNamespace);
    }
    
    /**
     * Getter for property name.
     * @return Value of property name.
     */
    public String getName() {
        return xmlAttrs[NAME].getValue();
    }
    
    /**
     * Setter for property name.
     * @param name   New value of property name.
     */
    public void setName(String name) {
        setAttribute(NAME, name);
    }
    
    /**
     * Getter for property uri.
     * @return Value of property uri.
     */
    public String getURI() {
        return xmlAttrs[URI].getValue();
    }
    
    /**
     * Setter for property uri.
     * @param uri   New value of property uri.
     */
    public void setURI(String uri) {
        setAttribute(URI, uri);
    }
    
    /**
	 * add a node at the end of a collection.
	 * This will be called while parsing this XMLNode.
	 * This method should always add this child at the end of 
	 * the collection because we want to preserve the order
	 * in which this node occured in the xml.
	 * This method should also set the sibliing sequence order
	 * before adding to collection if it different than the
	 * default Interger.MAX_VALUE
	 * @param c
	 */
	public void addChildAtTail(XMLNode c) {
		if (c instanceof Import) {
            addImportAtTail((Import) c);
        } else if (c instanceof Types) {
            setTypesAtTail((Types) c);
        } else if (c instanceof WSDLMessage) {
            addMessageAtTail((WSDLMessage) c);
        } else if (c instanceof PortType) {
            addPortTypeAtTail((PortType) c);
        } else if (c instanceof Binding) {
            addBindingAtTail((Binding) c);
        } else if (c instanceof Service) {
            addServiceAtTail((Service) c);
        } else if (c instanceof PartnerLinkType) {
            addServiceLinkTypeAtTail((PartnerLinkType) c);
        } else if (c instanceof Property) {
            addBPWSPropertyAtTail((Property) c);
        } else if (c instanceof PropertyAlias) {
            addBPWSPropertyAliasAtTail((PropertyAlias) c);
        } else {
            super.addChildAtTail(c);
        }
	}
	
    
    /**
     * Adds a child of the element.
     * @param   c   Child.
     */
    public void addChild(XMLNode c) {
        if (c instanceof Import) {
            addImport((Import) c);
        } else if (c instanceof Types) {
            setTypes((Types) c);
        } else if (c instanceof WSDLMessage) {
            addMessage((WSDLMessage) c);
        } else if (c instanceof PortType) {
            addPortType((PortType) c);
        } else if (c instanceof Binding) {
            addBinding((Binding) c);
        } else if (c instanceof Service) {
            addService((Service) c);
        } else if (c instanceof PartnerLinkType) {
            addServiceLinkType((PartnerLinkType) c);
        } else if (c instanceof Property) {
            addBPWSProperty((Property) c);
        } else if (c instanceof PropertyAlias) {
            addBPWSPropertyAlias((PropertyAlias) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** @see XMLNode#addChild(int, XMLNode)
     * @param index index of node in xml
     */
    public void addChild(int index, XMLNode c) {
        if (c instanceof Import) {
            addImport(index, (Import) c);
        } else if (c instanceof Types) {
            setTypes(index, (Types) c);
        } else if (c instanceof WSDLMessage) {
            addMessage(index, (WSDLMessage) c);
        } else if (c instanceof PortType) {
            addPortType(index, (PortType) c);
        } else if (c instanceof Binding) {
            addBinding(index, (Binding) c);
        } else if (c instanceof Service) {
            addService(index, (Service) c);
        } else if (c instanceof PartnerLinkType) {
            addServiceLinkType(index, (PartnerLinkType) c);
        } else if (c instanceof Property) {
            addBPWSProperty(index, (Property) c);
        } else if (c instanceof PropertyAlias) {
            addBPWSPropertyAlias(index, (PropertyAlias) c);
        } else {
            super.addChild(index, c);
        }
    }
    
    
    /**
     * Removes a child of the element
     * @param   c   Child.
     */
    public void removeChild(XMLNode c) {
        if (c instanceof Import) {
            removeImport((Import) c);
        } else if (c instanceof Types) {
            setTypes(null);
        } else if (c instanceof WSDLMessage) {
            removeMessage((WSDLMessage) c);
        } else if (c instanceof PortType) {
            removePortType((PortType) c);
        } else if (c instanceof Binding) {
            removeBinding((Binding) c);
        } else if (c instanceof Service) {
            removeService((Service) c);
        } else if (c instanceof PartnerLinkType) {
            removeServiceLinkType((PartnerLinkType) c);
        } else if (c instanceof Property) {
            removeBPWSProperty((Property) c);
        } else if (c instanceof PropertyAlias) {
            removeBPWSPropertyAlias((PropertyAlias) c);
        } else {
            super.removeChild(c);
        }
    }
    
    /**
     * get the import for a namespace
     * @param namespace
     * @return Import
     */
    public Import getImport(String namespace) {
    	if(namespace == null) {
    		return null;
    	}
    	
    	Import imp =  null;
    	Iterator it = mImportElements.iterator();
    	while(it.hasNext()) {
    		Import im = (Import) it.next();
    		if(namespace.equals(im.getNamespaceAttr())) {
    			imp = im;
    			break;
    		}
    	}
    	
    	return imp;
    }
   
    
    /**
     * Adds a new import.
     * @param wsdlImport the import to add
     */
    public void addImport(Import wsdlImport) {
        mImportElements.add(wsdlImport);
        super.addChild(2, wsdlImport);
    }
    
    /**
     * Adds a new import.
     * @param wsdlImport the import to add
     */
    private void addImport(int index, Import wsdlImport) {
    	mImportElements.add(wsdlImport);
        super.addChild(index, wsdlImport);
    }
    
    
    /**
     * Removes the given import.
     * @param wsdlImport to be removed
     */
    public void removeImport(Import wsdlImport) {
    	mImportElements.remove(wsdlImport);
    	super.removeChild(wsdlImport);
    }
   

    /**
     * Gets the list of all imports
     * @return a read-only collection of Imports.
     */
    public Collection getImports() {
        return Collections.unmodifiableCollection(mImportElements);
    }
    
    /**
     * @see WSDLDefinitions#getTypes
     */
    public Types getTypes() {
        return mTypes;
    }
    
    /**
     * @see WSDLDefinitions#setTypes
     */
    public void setTypes(Types types) {
    	Types oldTypes = mTypes;
    	mTypes = types;
    	replaceChild(2, oldTypes, types);
        
    }
    
    /**
     * @see WSDLDefinitions#setTypes
     */
    private void setTypes(int index, Types types) {
    	Types oldTypes = mTypes;
    	mTypes = types;
    	replaceChild(index, oldTypes, types);
        
    }
    
    /**
     * get WSDLMessage given the QName. Also look into imported document.
     * @param name QName of the message.
     * @return WSDLMessage
     *
     */
    public WSDLMessage getMessage(QName name) {
    	WSDLMessage message = null;
    	if(name == null) {
    		return null;
    	}
    	String namespace = name.getNamespaceURI();
    	if(namespace == null) {
    		return null;
    	}
    	Iterator it = this.getAllDocuments().iterator();
    	while(it.hasNext()) {
    		WSDLDocument doc = (WSDLDocument) it.next();
    		if(namespace.equals(doc.getTargetNamespace())) {
    			message = doc.getDocumentDefinitions().getMessage(name.getLocalPart());
    			if(message != null) {
    				break;
    			}
    		}
    	}
    	return message;
    }
    
    /**
     * Getter for property message.
     * @param messageLocalName name of the message, this should not localName.
     * @return Value of property message.
     *
     */
    public WSDLMessage getMessage(String messageLocalName) {
    	if(messageLocalName == null) {
    		return null;
    	}
    	
    	WSDLMessage msg = null;
    	Iterator it = this.mMessages.iterator();
    	while(it.hasNext()) {
    		WSDLMessage message = (WSDLMessage) it.next();
    		if(messageLocalName.equals(message.getName())) {
    			msg = message;
    			break;
    		}
    	}
    	
    	return msg;
    }
    
    
    /**
     * Adds a new message. If a message with the same name and target namespace
     * already exists, then the message is not added. Duplicate messages may
     * appear when other WSDL documents are imported.
     * @param message the message to add
     */
    public void addMessage(WSDLMessage message) {
        mMessages.add(message);
        super.addChild(2, message);
    }
    
    /**
     * Adds a new message. If a message with the same name and target namespace
     * already exists, then the message is not added. Duplicate messages may
     * appear when other WSDL documents are imported.
     * @param message the message to add
     */
    private void addMessage(int index, WSDLMessage message) {
        mMessages.add(message);
        super.addChild(index, message);
    }
    
    /**
     * remove all messages.
     *
     */
    public void removeAllMessages() {
    	Iterator it = this.mMessages.iterator();
    	while(it.hasNext()) {
    		WSDLMessage msg = (WSDLMessage) it.next();
    		removeMessage(msg);
    	}
    	
    	mMessages.clear();
    }
    
    /**
     * Removes the given message.
     * @param message to be removed
     */
    public void removeMessage(WSDLMessage message) {
    	mMessages.remove(message);
    	super.removeChild(message);
        
    }
    
    /**
     * Gets the list of all messages.
     * @return a read-only collection of WSDLMessages.
     */
    public Collection getMessages() {
        return Collections.unmodifiableCollection(mMessages);
    }
    
    /**
     * Getter for property portType.
     * @param name name of the PortType
     * @return portType.
     *
     */
    public PortType getPortType(String name) {
    	PortType portType = null;
    	Iterator it = mPortTypes.iterator();
    	while(it.hasNext()) {
    		PortType pType = (PortType) it.next();
    		
    		if(name != null && name.equals(pType.getName())) {
    			portType = pType;
    			break;
    		}
    	}
    	
    	return portType;
    }
    
    /**
     * Get the PortType given the QName. 
     * Also look into the imported document.
     * @param name name of the PortType
     * @return portType.
     *
     */
    public PortType getPortType(QName name) {
    	PortType portType = null;
    	if(name == null) {
    		return null;
    	}
    	String namespace = name.getNamespaceURI();
    	if(namespace == null) {
    		return null;
    	}
    	Iterator it = this.getAllDocuments().iterator();
    	while(it.hasNext()) {
    		WSDLDocument doc = (WSDLDocument) it.next();
    		if(namespace.equals(doc.getTargetNamespace())) {
    			portType = doc.getDocumentDefinitions().getPortType(name.getLocalPart());
    			if(portType != null) {
    				break;
    			}
    		}
    	}
    	
    	return portType;
    }
    
    /**
     * Getter for property portType.
     * @param index index of the property to get
     * @return Value of property portType.
     *
     */
    public PortType getPortType(int index) {
        return (PortType) mPortTypes.get(index);
    }
    
    
    /**
     * Adds a new portType.
     * @param portType the portType to add
     */
    public void addPortType(PortType portType) {
        mPortTypes.add(portType);
        super.addChild(2, portType);
    }
   
    /**
     * Adds a new portType.
     * @param portType the portType to add
     */
    private void addPortType(int index, PortType portType) {
        mPortTypes.add(portType);
        super.addChild(index, portType);
    }
    
    
    /**
     * Removes the given porType.
     * @param portType to be removed
     */
    public void removePortType(PortType portType) {
    	mPortTypes.remove(portType);
    	super.removeChild(portType);
    }
    
    /**
     * Gets the number of portTypes.
     * @return the number of portTypes
     */
    public int countPortTypes() {
        return mPortTypes.size();
    }
   
    
    /**
     * Gets the list of all portTypes
     * @return a read-only collection of PortTypes.
     */
    public Collection getPortTypes() {
        return Collections.unmodifiableCollection(mPortTypes);
    }
    
    /**
     * get Binding given the QName. Also look into imported document.
     * @param name QName of the message.
     * @return Binding
     *
     */
    public Binding getBinding(QName name) {
    	Binding binding = null;
    	if(name == null) {
    		return null;
    	}
    	String namespace = name.getNamespaceURI();
    	if(namespace == null) {
    		return null;
    	}
    	Iterator it = this.getAllDocuments().iterator();
    	while(it.hasNext()) {
    		WSDLDocument doc = (WSDLDocument) it.next();
    		if(namespace.equals(doc.getTargetNamespace())) {
    			binding = doc.getDocumentDefinitions().getBinding(name.getLocalPart());
    			if(binding != null) {
    				break;
    			}
    		}
    	}
    	return binding;
    }
    
    /**
     * Getter for property binding.
     * @param name name of the binding
     * @return Binding.
     *
     */
    public Binding getBinding(String name) {
    	if(name == null) {
    		return null;
    	}
    	
    	Binding binding = null;
    	Iterator it = this.mBindings.iterator();
    	
    	while(it.hasNext()) {
    		Binding bnd = (Binding) it.next();
    		
    		if(name != null && name.equals(bnd.getName())) {
    			binding = bnd;
    			break;
    		}
    	}
    	
    	return binding;
    }
    
    
    /**
     * @see WSDLDefinitions#addBinding
     */
    public void addBinding(Binding binding) {
        mBindings.add(binding);
        super.addChild(2, binding);
    }
   
    /**
     * @see WSDLDefinitions#addBinding
     */
    private void addBinding(int index, Binding binding) {
        mBindings.add(binding);
        super.addChild(index, binding);
    }
    
    
    /**
     * @see WSDLDefinitions#removeBinding(Binding)
     */
    public void removeBinding(Binding binding) {
    	mBindings.remove(binding);
    	super.removeChild(binding);
    }
    

    /**
     * @see WSDLDefinitions#getBindings
     */
    public Collection getBindings() {
        return Collections.unmodifiableCollection(mBindings);
    }
    
    /**
     * Getter for property service. Finds first matching ServicePort in 
     * first matching Service.
     * @param portType PortType whose Service we are looking.
     * @return Collection of ServicePorts.
     *
     */
    public Collection getServicePort(PortType portType) {
    	ArrayList servicePorts = new ArrayList();
    	Iterator it = this.mServices.iterator();
    	while(it.hasNext()) {
    		Service srv = (Service) it.next();
    		Iterator pIterator = srv.getPorts().iterator();
    		while(pIterator.hasNext()) {
    			ServicePort port = (ServicePort) pIterator.next();
    			QName bindingQName = port.getBinding();
    			Binding binding = this.getBinding( bindingQName);
    			if(binding != null) {
    				//get the portType of the binding
    				PortType bindingPortType = binding.getWSDLPortType();
    				
    				//if binding port type points to passed port type then add the ServicePort
    				if(bindingPortType != null 
    				  && bindingPortType.getQName().equals(portType.getQName())) {
    					servicePorts.add(port);
    				}
    				
    			}
    		}
    	}
    	
    	return servicePorts;
    }
    
    /**
     * Get the Service given QName. Also look into imported documents.
     * @param name name of the service
     * @return Value of property service.
     *
     */
    public Service getService(QName name) {
    	Service service = null;
    	if(name == null) {
    		return null;
    	}
    	String namespace = name.getNamespaceURI();
    	if(namespace == null) {
    		return null;
    	}
    	Iterator it = this.getAllDocuments().iterator();
    	while(it.hasNext()) {
    		WSDLDocument doc = (WSDLDocument) it.next();
    		if(namespace.equals(doc.getTargetNamespace())) {
    			service = doc.getDocumentDefinitions().getService(name.getLocalPart());
    			if(service != null) {
    				break;
    			}
    		}
    	}
    	return service;
    }
    
    /**
     * Getter for property service.
     * @param name name of the service
     * @return Value of property service.
     *
     */
    public Service getService(String name) {
        Service service = null;
        if(name == null) {
        	return null;
        }
        
        Iterator it = mServices.iterator();
        while(it.hasNext()) {
        	Service serv = (Service) it.next();
        	if(name.equals(serv.getName())) {
        		service = serv;
        		break;
        	}
        }
        
        return service;
    }
    
    /**
     * Adds a new service.
     * @param service the service to add
     */
    public void addService(Service service) {
        mServices.add(service);
        super.addChild(2, service);
    }
    
    /**
     * Adds a new service.
     * @param service the service to add
     */
    private void addService(int index, Service service) {
        mServices.add(service);
        super.addChild(index, service);
    }
    
    
    /**
     * Removes the given service.
     * @param service to be removed
     */
    public void removeService(Service service) {
    	mServices.remove(service);
    	super.removeChild(service);
    }
   
   
    /**
     * Gets the list of all services
     * @return a read-only collection of Services.
     */
    public Collection getServices() {
        return Collections.unmodifiableCollection(mServices);
    }
    
    /**
     * Get PartnerLinkType for given QName. Also look into imported wsdls.
     * @param name Qname of serviceLinkType
     * @return serviceLinkType.
     *
     */
    public PartnerLinkType getServiceLinkType(QName name) {
    	PartnerLinkType serviceLinkType = null;
    	if(name == null) {
    		return null;
    	}
    	String namespace = name.getNamespaceURI();
    	if(namespace == null) {
    		return null;
    	}
    	Iterator it = this.getAllDocuments().iterator();
    	while(it.hasNext()) {
    		WSDLDocument doc = (WSDLDocument) it.next();
    		if(namespace.equals(doc.getTargetNamespace())) {
    			serviceLinkType = doc.getDocumentDefinitions().getServiceLinkType(name.getLocalPart());
    			if(serviceLinkType != null) {
    				break;
    			}
    		}
    	}
    	
    	return serviceLinkType;
    }
    
    /**
     * Getter for property serviceLinkType.
     * @param name name of serviceLinkType
     * @return serviceLinkType.
     *
     */
    public PartnerLinkType getServiceLinkType(String name) {
    	PartnerLinkType serviceLinkType = null;
    	
    	Iterator it = mServiceLinkTypes.iterator();
    	while(it.hasNext()) {
    		PartnerLinkType sLinkType = (PartnerLinkType) it.next();
    		if(name != null && name.equals(sLinkType.getName())) {
    			serviceLinkType = sLinkType;
    			break;
    		}
    	}
    	
    	return serviceLinkType;
    }
    
    /**
     * Getter for property serviceLinkType.
     * @param index index of the property to get
     * @return Value of property serviceLinkType.
     *
     */
    public PartnerLinkType getServiceLinkType(int index) {
        return (PartnerLinkType) mServiceLinkTypes.get(index);
    }
    
    /**
     * Adds a new serviceLinkType.
     * @param serviceLinkType the serviceLinkType to add
     */
    public void addServiceLinkType(PartnerLinkType serviceLinkType) {
        mServiceLinkTypes.add(serviceLinkType);
        super.addChild(1, serviceLinkType);
    }
   
    /**
     * Adds a new serviceLinkType.
     * @param serviceLinkType the serviceLinkType to add
     */
    private void addServiceLinkType(int index, PartnerLinkType serviceLinkType) {
        mServiceLinkTypes.add(serviceLinkType);
        super.addChild(index, serviceLinkType);
    }
    
    
    /**
     * Removes the given serviceLinkType.
     * @param serviceLinkType to be removed
     */
    public void removeServiceLinkType(PartnerLinkType serviceLinkType) {
        mServiceLinkTypes.remove(serviceLinkType);
        super.removeChild(serviceLinkType);
    }
    
   
    /**
     * Gets the list of all serviceLinkTypes
     * @return a read-only collection of ServiceLinkTypes.
     */
    public Collection getServiceLinkTypes() {
        return Collections.unmodifiableCollection(mServiceLinkTypes);
    }
    
    /**
     * @see WSDLDefinitions#getBPWSProperty
     */
    public Property getBPWSProperty(int index) {
        return (Property) mProperties.get(index);
    }
    
    
    /**
     * @see WSDLDefinitions#countBPWSProperties
     */
    public int countBPWSProperties() {
        return mProperties.size();
    }
    
    /**
     * @see WSDLDefinitions#addBPWSProperty
     */
    public void addBPWSProperty(Property prop) {
        mProperties.add(prop);
        super.addChild(1, prop);
    }
    
    /**
     * @see WSDLDefinitions#addBPWSProperty
     */
    private void addBPWSProperty(int index, Property prop) {
        mProperties.add(prop);
        super.addChild(index, prop);
    }
    
    
    /**
     * @see WSDLDefinitions#removeBPWSProperty(int)
     */
    public void removeBPWSProperty(int i) {
    	Property oldProperty = getBPWSProperty(i); 
        mProperties.remove(i);
        super.removeChild(oldProperty);
    }
        
    
    /**
     * @see WSDLDefinitions#removeBPWSProperty(Property)
     */
    public void removeBPWSProperty(Property prop) {
        mProperties.remove(prop);
        super.removeChild(prop);
    }
   
    
    /**
     * @see WSDLDefinitions#getBPWSProperties
     */
    public Collection getBPWSProperties() {
        return Collections.unmodifiableList(mProperties);
    }
    
    
    public Property getBPWSProperty(QName propertyQName) {
    	Property property = null;
    	if(propertyQName == null) {
    		return null;
    	}
    	String namespace = propertyQName.getNamespaceURI();
    	if(namespace == null) {
    		return null;
    	}
    	Iterator it = this.getAllDocuments().iterator();
    	while(it.hasNext()) {
    		WSDLDocument doc = (WSDLDocument) it.next();
    		if(namespace.equals(doc.getTargetNamespace())) {
    			property = doc.getDocumentDefinitions().getBPWSProperty(propertyQName.getLocalPart());
    			if(property != null) {
    				break;
    			}
    		}
    	}
		return property;
	}

    
    public Property getBPWSProperty(String name) {
    	Property property = null;
    	if(name == null) {
        	return null;
        }
        
        Iterator it = mProperties.iterator();
        while(it.hasNext()) {
        	Property prop = (Property) it.next();
        	if(name.equals(prop.getName())) {
        		property = prop;
        		break;
        	}
        }
        
        return property;
    }
    
	/**
     * @see WSDLDefinitions#getBPWSPropertyAlias
     */
    public PropertyAlias getBPWSPropertyAlias(int index) {
        return (PropertyAlias) mPropertyAliases.get(index);
    }
    
    /**
     * @see WSDLDefinitions#setBPWSPropertyAlias
     */
    public void setBPWSPropertyAlias(int index, PropertyAlias propAlias) {
        if (mPropertyAliases.size() == index) {
            addBPWSPropertyAlias(propAlias);
        } else {
        	PropertyAlias oldPropertyAlias = getBPWSPropertyAlias(index); 
            mPropertyAliases.set(index, propAlias);
            replaceChild(oldPropertyAlias, propAlias);
        }
    }
    
    /**
     * @see WSDLDefinitions#countBPWSPropertyAliases
     */
    public int countBPWSPropertyAliases() {
        return mPropertyAliases.size();
    }
    
    /**
     * @see WSDLDefinitions#addBPWSPropertyAlias
     */
    public void addBPWSPropertyAlias(PropertyAlias propAlias) {
        mPropertyAliases.add(propAlias);
        super.addChild(1, propAlias);
    }
   
    /**
     * @see WSDLDefinitions#addBPWSPropertyAlias
     */
    private void addBPWSPropertyAlias(int index, PropertyAlias propAlias) {
        mPropertyAliases.add(propAlias);
        super.addChild(index, propAlias);
    }
    
    /**
     * @see WSDLDefinitions#removeBPWSPropertyAlias(int)
     */
    public void removeBPWSPropertyAlias(int i) {
    	PropertyAlias oldPropertyAlias = getBPWSPropertyAlias(i); 
        mPropertyAliases.remove(i);
        super.removeChild(oldPropertyAlias);
    }
    
    /**
     * @see WSDLDefinitions#removeBPWSPropertyAlias(PropertyAlias)
     */
    public void removeBPWSPropertyAlias(PropertyAlias propAlias) {
        mPropertyAliases.remove(propAlias);
        super.removeChild(propAlias);
    }
    
    
    /**
     * @see WSDLDefinitions#getBPWSPropertyAliases
     */
    public Collection getBPWSPropertyAliases() {
        return Collections.unmodifiableList(mPropertyAliases);
    }
    
    /**
     * Accepts a visitor to perform some work on the element.
     * @param   w   The working visitor
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean accept(Visitor w) {
        WSDLVisitor v = morphVisitor(w);
        if (traverseParentFirst(v)) {
            if (!v.visit(this)) {
                return false;
            }
        }
        
        if (!super.accept(v)) {
            return false;
        }

        if (traverseParentLast(v)) {
            if (!v.visit(this)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Returns recursively a collection of all XML Schemas in the given WSDL document.
     * @return the collection of all XML Schemas in the WSDL document.
     * element
     */
    public Collection getXMLSchemas() {
        Collection wsdls = getAllDocuments();
        Collection result = new ArrayList();
        if (wsdls != null) {
            for (Iterator iter = wsdls.iterator(); iter.hasNext();) {
                WSDLDefinitionsImpl wsdlDefsImpl =
                    (WSDLDefinitionsImpl) ((WSDLDocument) iter.next()).getDocumentDefinitions();
                Collection schs = wsdlDefsImpl.getXMLSchemasHere();
                if (schs != null) {
                    result.addAll(schs);
                }
            }
        }
        return result;
    }
    
    /**
     *  Gets the XML Schemas available here at this WSDL Document level.
     * @return  Collection of XML Schemas available here.
     */
    public Collection getXMLSchemasHere() {
        List result = null;
        Types wsdlTypes = getTypes();
        Collection schs = null;
        
        // Add in inlined XML Schemas from this WSDL document level
        if ((wsdlTypes != null) && ((schs = wsdlTypes.getSchemas()) != null)) {
            result = new ArrayList();
            result.addAll(schs);
        }
        
        // Add in XML Schemas explicitly imported at this WSDL document level
        Collection impSchs = getImportedXMLSchemas();
        if (impSchs != null) {
            if (null == result) {
                result = new ArrayList();
            }
            result.addAll(impSchs);
        }
        
        return result;
    }
    
    /** Gets the CastorSupport object.
     * @return  CastorSupport object to use.
     */
    private CastorSupport getCastorSupport() {
        if (null == mACS) {
            
            mACS = getInstanceOfCastorSupport(null);
            if (null == mACS) {
                throw new XMLParseVisitorException("Cannot find CastorSupport implementation class--"
                    + "MUST use EInsightManager to create WSDLRepositoryObject or WSDLDocument!");
            }
        }

    	return mACS;
    }

    /**
     * Returns a collection of all complex types in the given WSDL document.
     * @return the collection of all complex types in the WSDL document's types
     * element
     */
    public Collection getSchemaTypes() {
        Collection schs = getXMLSchemas();
        ArtifactDictionary artDict = ArtifactDictionaryFactory.getInstance().newArtifactDictionary();
            
        if (schs != null) {
            for (Iterator iter = schs.iterator(); iter.hasNext();) {
                XMLSchema xmlSchema = (XMLSchema) iter.next();
                Collection complexTypes = getCastorSupport().getComplexTypes(xmlSchema);
                
                if (complexTypes != null) {
                    for (Iterator typeIter = complexTypes.iterator(); typeIter.hasNext();) {
                        Object type = typeIter.next();
                        artDict.put(getCastorSupport().getTargetNamespace(type),
                                    type.getClass(),
                                    getCastorSupport().getName(type),
                                    type);
                    }
                }
            }
        }
        return artDict.get();
    }
 
    
    /**
     * Gets the complex types with the given name in the given target namespace.
     * @param name the name of the type
     * @param targetNamespace the target namespace
     * @return the type or null if not found
     */
    public Object getSchemaType(String name, String targetNamespace) {
        Collection schemaTypes = getSchemaTypes();
        for (Iterator iter = schemaTypes.iterator(); iter.hasNext();) {
            Object schemaType = iter.next();
            if (name.equals(getCastorSupport().getName(schemaType))
                    && targetNamespace.equals(getCastorSupport().getTargetNamespace(schemaType))) {
                return schemaType;
            }
        }
        
        return null;
    }

   /**
     * Returns recursively a collection of all elements in the given WSDL document.
     * @return the collection of all elements in the WSDL document's types
     * element
     */
    public Collection getSchemaElements() {
        Collection schs = getXMLSchemas();
        ArtifactDictionary artDict = ArtifactDictionaryFactory.getInstance().newArtifactDictionary();
        
        if (schs != null) {
            for (Iterator iter = schs.iterator(); iter.hasNext();) {
                XMLSchema xmlSchema = (XMLSchema) iter.next();
                Collection elementDecls = getCastorSupport().getElementDecls(xmlSchema);
                if (elementDecls != null) {
                    for (Iterator elemIter = elementDecls.iterator(); elemIter.hasNext();) {
                        Object elem = elemIter.next();
                        artDict.put(getCastorSupport().getTargetNamespace(elem),
                                    elem.getClass(),
                                    getCastorSupport().getName(elem),
                                    elem);
                    }
                }
            }
        }
        return artDict.get();

    }

    /**
     * Gets the XML Schema with the given name in the given target namespace.
     * @param name the name of the type
     * @param targetNamespace the target namespace
     * @return the element or null if not found
     */
    public Object getSchemaElement(String name, String targetNamespace) {
        Collection schemaElements = getSchemaElements();
        for (Iterator iter = schemaElements.iterator(); iter.hasNext();) {
            Object elementDecl = iter.next();
            if (name.equals(getCastorSupport().getName(elementDecl))
                    && targetNamespace.equals(getCastorSupport().getTargetNamespace(elementDecl))) {
                return elementDecl;
            }
        }
        
        return null;
    }

    public ElementDecl getXSDElement(QName elementQName) {
    	ElementDecl element = null;
    	if(elementQName == null) {
    		return null;
    	}
    	String namespace = elementQName.getNamespaceURI();
    	if(namespace == null) {
    		return null;
    	}
    	Iterator it = this.getAllDocuments().iterator();
    	while(it.hasNext()) {
    		WSDLDocument doc = (WSDLDocument) it.next();
			element = (ElementDecl) doc.getDocumentDefinitions().getSchemaElement(elementQName.getLocalPart(), namespace);
			if(element != null) {
				break;
			}
    	}
    	
    	return element;
	}

	public XMLType getXSDType(QName typeQName) {
		XMLType type = null;
    	if(typeQName == null) {
    		return null;
    	}
    	String namespace = typeQName.getNamespaceURI();
    	if(namespace == null) {
    		return null;
    	}
    	//first check if it is built in schema type
    	if(namespace.equals(Schema.DEFAULT_SCHEMA_NS)) {
    		return XMLSchema.getBuiltInType(typeQName);
    	}
    	Iterator it = this.getAllDocuments().iterator();
    	while(it.hasNext()) {
    		WSDLDocument doc = (WSDLDocument) it.next();
			type = (XMLType) doc.getDocumentDefinitions().getSchemaType(typeQName.getLocalPart(), namespace);
			if(type != null) {
				break;
			}
    		
    	}
    	
    	return type;
	}

    /*************************************************/
    /* EVERYTHING YOU SEE BELOW THIS LINE IS A HACK. */
    /*************************************************/
        
    
	/**
     * Determines if the binding is SOAP.
     * @param binding the binding
     * @return boolean flag
     */
    boolean isSOAPBinding(Binding binding) {
        int num = binding.getExtensibilityElementsSize();
        
        for (int i = 0; i < num; i++) {
            ExtensibilityElement elem = binding.getExtensibilityElement(i);
            String qName = elem.getQualifiedName();
            //String localName = QName.getLocalName(qName);
            //String prefix = QName.getPrefix(qName);
            String localName = NamespaceUtility.getLocalName(qName);
            String prefix = NamespaceUtility.getPrefix(qName);
            if (null == prefix) {
                prefix = "";
            }
            if (localName.equals(SOAPBinding.TAG)) {
                if (getNamespace(prefix).equals(SOAPConstants.NS_URI)) {
                    return true;
                }
            }
        }
        
        return false;
    }
    
        
    /**
     * Determines if the operation is SOAP.
     * @param op the operation
     * @return boolean flag
     */
    boolean isSOAPOperation(BindingOperation op) {
        int num = op.getExtensibilityElementsSize();
        
        for (int i = 0; i < num; i++) {
            ExtensibilityElement elem = op.getExtensibilityElement(i);
            String qName = elem.getQualifiedName();
            //String localName = QName.getLocalName(qName);
            //String prefix = QName.getPrefix(qName);
            String localName = NamespaceUtility.getLocalName(qName);
            String prefix = NamespaceUtility.getPrefix(qName);
            if (null == prefix) {
                prefix = "";
            }
            if (localName.equals(SOAPOperation.TAG)) {
                if (getNamespace(prefix).equals(SOAPConstants.NS_URI)) {
                    return true;
                }
            }
        }
        
        return false;
    }
    
    
    
    /**
     * Gets the SOAP header element.
     * @param elem the element
     * @return the SOAP header or null if not found
     */
    SOAPHeader getSOAPHeader(WSDLExtensibleElement elem) {
        int num = elem.getExtensibilityElementsSize();
        
        for (int i = 0; i < num; i++) {
            ExtensibilityElement ee = elem.getExtensibilityElement(i);
            String qName = ee.getQualifiedName();
            //String localName = QName.getLocalName(qName);
            //String prefix = QName.getPrefix(qName);
            String localName = NamespaceUtility.getLocalName(qName);
            String prefix = NamespaceUtility.getPrefix(qName);
            if (null == prefix) {
                prefix = "";
            }
            if (localName.equals(SOAPHeader.TAG)) {
                if (getNamespace(prefix).equals(SOAPConstants.NS_URI)) {
                    return (SOAPHeader) ee;
                }
            }
        }
        
        return null;
    }
    
    /**
     * Gets the message part.
     * @param msg the message
     * @param name the name of the part
     * @return the message part or null if not found
     */
    Part getPart(WSDLMessage msg, String name) {
        int numParts = msg.getPartSize();
        
        for (int i = 0; i < numParts; i++) {
            Part part = msg.getPart(i);
            if (part.getName().equals(name)) {
                return part;
            }
        }
        
        return null;
    }
    
    
    /**
     * Convenience method to provide the <code>CastorSupport</code> object since eWay installation SARs may carry
     * their own old version of com.stc.einsightintegrationapi.jar which used to contain the CastorSupport abstract
     * class (it's now in com.stc.einsightintegrationprivapi.jar), we cannot access the fixed version of CastorSupport.
     *
     * @param   clsLdr      ClassLoader to use.
     * @return  A CastorSupport object.
     */
    protected CastorSupport getInstanceOfCastorSupport(ClassLoader clsLdr) {
        CastorSupport retCastorSupport = null;
        boolean oldAPIJar = true;
        try {
            // getComplexType() was added in 5.0.4
            Method getComplexType = CastorSupport.class.getDeclaredMethod("getComplexType", new Class[0]);
            oldAPIJar = (getComplexType == null);
        } catch (Exception e) {
            oldAPIJar = true;
        }
        
        if (oldAPIJar) {
            retCastorSupport = new CastorSupportImpl();
        } else {
            retCastorSupport = CastorSupport.getInstance(clsLdr);
        }
        
        return retCastorSupport;
    }
    
    //get the namespace to use for a given QName
    private String getNamespace(QName name) {
    	String prefix = name.getPrefix();
    	String ns = name.getNamespaceURI();
    	//String localName = name.getLocalName();
    	String localName = name.getLocalPart();
    	
    	String namespace = null;
    	//if prefix is defined then get the corresponding namespace
    	if(prefix != null && !prefix.trim().equals("")) {
    		namespace = this.getNamespace(prefix);
    	} else if(ns != null && !ns.trim().equals("")) {
    	//if namespace is defined then this is the namespace to use	
    		namespace = ns;
    	} else {
    		//TODO: this is wrong need to fix it
    		//see the schema on QName interpretation
    		namespace = this.getTargetNamespace();
    		
    	}
    	//uncomment me this is correct way
    	//if there is default namespace on this or any other ancestor then use that
    	// if default is also missing then namespace is absent
//    	else if(this.getDefaultNamespace() != null) {
//    		//else use the default namespace
//    		namespace = this.getDefaultNamespace();
//    	} 
    	
    	
////    	if prefix is defined then get the corresponding namespace
//    	if(prefix != null && !prefix.trim().equals("")) {
//    		namespace = this.getNamespace(prefix);
//    	} else if(ns != null && !ns.trim().equals("")) {
//    	//if namespace is defined then this is the namespace to use	
//    		namespace = ns;
//    	} 
//    	else if(this.getTargetNamespace() != null 
//    			&& this.getTargetNamespace().equals(this.getDefaultNamespace()))  {
//    		//if targetNamespace is not null and it is same as default name space
//    		//then we use target namespace
//    		namespace = this.getTargetNamespace();
//    	} else {
//    		//otherwise only local name specfied and
//    		//target namespace  does not equal default namespace
//    		//so we can not search for this QName which has only local name
//    		namespace = null;
//    	}
    	
    	return namespace;
    }
     
  
    public void addImportAtTail(Import wsdlImport) {
        mImportElements.add(wsdlImport);
        super.addChildAtTheEnd(wsdlImport, 2);
    }
    
    public void setTypesAtTail(Types types) {
    	Types oldTypes = mTypes;
    	mTypes = types;
    	super.addChildAtTheEnd(types, 2);
    }
    
    public void addMessageAtTail(WSDLMessage message) {
        mMessages.add(message);
        super.addChildAtTheEnd(message, 2);
    }
    
    public void addPortTypeAtTail(PortType portType) {
        mPortTypes.add(portType);
        super.addChildAtTheEnd(portType, 2);
    }
    
    public void addBindingAtTail(Binding binding) {
        mBindings.add(binding);
        super.addChildAtTheEnd(binding, 2);
    }
    
    public void addServiceAtTail(Service service) {
        mServices.add(service);
        super.addChildAtTheEnd(service, 2);
    }
    
    public void addServiceLinkTypeAtTail(PartnerLinkType serviceLinkType) {
        mServiceLinkTypes.add(serviceLinkType);
        super.addChildAtTheEnd(serviceLinkType, 1);
    }
    
    public void addBPWSPropertyAtTail(Property prop) {
        mProperties.add(prop);
        super.addChildAtTheEnd(prop, 1);
    }
    
    public void addBPWSPropertyAliasAtTail(PropertyAlias propAlias) {
        mPropertyAliases.add(propAlias);
        super.addChildAtTheEnd(propAlias, 1);
    }
    
    public void validate(Collection todoListeners) {
        VisitorService visitorService = new ValidateVisitorService();
        Object[] params = new Object[] {todoListeners};
        ValidateVisitor visitor = (ValidateVisitor) visitorService.fetch(WSDLVisitor.class, null);
        
        ensureInlineSchemasMatchExtensElems();
        
        visitor.prepare(params);
        accept(visitor);
        
        //TODO: validate imported documents.
        
        // remove ToDoListeners
        Iterator iter = todoListeners.iterator();
        while (iter.hasNext()) {
            visitor.getValidateSupport().removeToDoListener((ToDoListener) iter.next());
        }
    }
    
    
    private void ensureInlineSchemasMatchExtensElems() {
        if (getTypes() != null) {
            CastorSupport.getInstance(getClass().getClassLoader())
                .convertSchemaToExtensibilityElement(getTypes());
        }
    }
    
}
