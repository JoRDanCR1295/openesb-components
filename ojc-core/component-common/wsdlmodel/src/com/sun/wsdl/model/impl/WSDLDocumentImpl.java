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
 * @(#)WSDLDocumentImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.impl;

import java.beans.PropertyChangeEvent;
import java.io.Reader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import javax.xml.namespace.QName;

import org.exolab.castor.xml.schema.ComplexType;
import org.exolab.castor.xml.schema.ElementDecl;
import org.exolab.castor.xml.schema.Schema;
import org.exolab.castor.xml.schema.SimpleType;

import com.sun.wsdl.model.Binding;
import com.sun.wsdl.model.BindingFault;
import com.sun.wsdl.model.BindingInput;
import com.sun.wsdl.model.BindingOperation;
import com.sun.wsdl.model.BindingOutput;
import com.sun.wsdl.model.Import;
import com.sun.wsdl.model.Operation;
import com.sun.wsdl.model.OperationFault;
import com.sun.wsdl.model.OperationInput;
import com.sun.wsdl.model.OperationOutput;
import com.sun.wsdl.model.Part;
import com.sun.wsdl.model.PortType;
import com.sun.wsdl.model.Service;
import com.sun.wsdl.model.PartnerLinkRole;
import com.sun.wsdl.model.PartnerLinkType;
import com.sun.wsdl.model.ServicePort;
import com.sun.wsdl.model.Types;
import com.sun.wsdl.model.WSDLDefinitions;
import com.sun.wsdl.model.WSDLDocument;
import com.sun.wsdl.model.WSDLDocumentParserFactory;
import com.sun.wsdl.model.WSDLDocumentation;
import com.sun.wsdl.model.WSDLMessage;
import com.sun.wsdl.model.WSDLParseContext;
import com.sun.wsdl.model.bpel.Property;
import com.sun.wsdl.model.bpel.PropertyAlias;
import com.sun.wsdl.model.bpel.Query;
import com.sun.wsdl.model.bpel.impl.PropertyAliasImpl;
import com.sun.wsdl.model.bpel.impl.PropertyImpl;
import com.sun.wsdl.model.bpel.impl.QueryImpl;
import com.sun.wsdl.model.common.model.Documentation;
import com.sun.wsdl.model.common.model.PrivateExtensionMapModel;
import com.sun.wsdl.model.common.model.XMLComment;
import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.model.XMLElement;
import com.sun.wsdl.model.common.model.XMLNode;
import com.sun.wsdl.model.common.model.XMLProcessingInstruction;
import com.sun.wsdl.model.common.model.XMLText;
import com.sun.wsdl.model.common.model.impl.DocumentationImpl;
import com.sun.wsdl.model.common.model.impl.PrivateExtensionMapModelImpl;
import com.sun.wsdl.model.common.model.impl.XMLCommentImpl;
import com.sun.wsdl.model.common.model.impl.XMLDocumentImpl;
import com.sun.wsdl.model.common.model.impl.XMLProcessingInstructionImpl;
import com.sun.wsdl.model.common.model.impl.XMLTextImpl;
import com.sun.wsdl.model.common.todotask.ToDoListener;
import com.sun.wsdl.model.common.visitor.CloneSupport;
import com.sun.wsdl.model.common.visitor.Visitor;
import com.sun.wsdl.model.common.visitor.VisitorService;
import com.sun.wsdl.model.common.visitor.XMLParseVisitorException;
import com.sun.wsdl.model.extensions.ExtensibilityElement;
import com.sun.wsdl.model.extensions.impl.ExtensibilityElementImpl;
import com.sun.wsdl.model.extensions.soap.SOAPAddress;
import com.sun.wsdl.model.extensions.soap.SOAPBinding;
import com.sun.wsdl.model.extensions.soap.SOAPBody;
import com.sun.wsdl.model.extensions.soap.SOAPFault;
import com.sun.wsdl.model.extensions.soap.SOAPHeader;
import com.sun.wsdl.model.extensions.soap.SOAPHeaderFault;
import com.sun.wsdl.model.extensions.soap.SOAPOperation;
import com.sun.wsdl.model.extensions.soap.impl.SOAPAddressImpl;
import com.sun.wsdl.model.extensions.soap.impl.SOAPBindingImpl;
import com.sun.wsdl.model.extensions.soap.impl.SOAPBodyImpl;
import com.sun.wsdl.model.extensions.soap.impl.SOAPFaultImpl;
import com.sun.wsdl.model.extensions.soap.impl.SOAPHeaderFaultImpl;
import com.sun.wsdl.model.extensions.soap.impl.SOAPHeaderImpl;
import com.sun.wsdl.model.extensions.soap.impl.SOAPOperationImpl;
import com.sun.wsdl.model.visitor.CloneVisitorService;
import com.sun.wsdl.model.visitor.SAXWriteVisitorService;
import com.sun.wsdl.model.visitor.ValidateVisitor;
import com.sun.wsdl.model.visitor.ValidateVisitorService;
import com.sun.wsdl.model.visitor.WSDLVisitor;
import com.sun.wsdl.model.xsd.CastorSupport;
import com.sun.wsdl.model.xsd.XMLSchema;

/**
 * Implements a WSDL document.
 *
 * @author Sun Microsystems
 * TODO: extension creation should be done by using some generic
 * factory which maps extension to implementation class. similar to wsdl4j
 * @version 
 */
public class WSDLDocumentImpl extends XMLDocumentImpl
    implements WSDLDocument {    


    /** serialVersionUID for this class */
    static final long serialVersionUID = 4946080299337401274L;
    
    /** Persisting WSDL repository object */

    
    /** WSDL provider repository object */

    
    /** Map for holding inline schema's imported XMLSchema to Schema correlation */
    protected Map inlineXMLSchemaSchemaMap = null;
    
    /** Holds the private extension map model associated with this document.
     *  @since  5.1.0
     */
    protected PrivateExtensionMapModel mPrivateExtMapModel = null;
    
    private WSDLParseContext mWSDLParseContext = null;
    
    /** Mapping of document component tags to their constructors. */
    private static transient Map typeMap = new HashMap();
    static {
        typeMap.put(WSDLDefinitions.TAG, WSDLDefinitionsImpl.class);
        typeMap.put(WSDLMessage.TAG, WSDLMessageImpl.class);
        typeMap.put(Part.TAG, PartImpl.class);
        typeMap.put(PortType.TAG, PortTypeImpl.class);
        typeMap.put(Operation.TAG, OperationImpl.class);
        typeMap.put(OperationInput.TAG, OperationInputImpl.class);
        typeMap.put(OperationOutput.TAG, OperationOutputImpl.class);
        typeMap.put(OperationFault.TAG, OperationFaultImpl.class);
        typeMap.put(PartnerLinkType.TAG, PartnerLinkTypeImpl.class);
        typeMap.put(PartnerLinkRole.TAG, PartnerLinkRoleImpl.class);
        typeMap.put(Service.TAG, ServiceImpl.class);
        typeMap.put(ServicePort.TAG, ServicePortImpl.class);
        typeMap.put(Import.TAG, ImportImpl.class);
        typeMap.put(XMLProcessingInstruction.TAG,
                    XMLProcessingInstructionImpl.class);
        typeMap.put(XMLComment.TAG, XMLCommentImpl.class);
        typeMap.put(XMLText.TAG, XMLTextImpl.class);
        typeMap.put(Binding.TAG, BindingImpl.class);
        typeMap.put(SOAPBinding.TAG, SOAPBindingImpl.class);
        typeMap.put(BindingOperation.TAG, BindingOperationImpl.class);
        typeMap.put(SOAPOperation.TAG, SOAPOperationImpl.class);
        typeMap.put(BindingInput.TAG, BindingInputImpl.class);
        typeMap.put(BindingOutput.TAG, BindingOutputImpl.class);
        typeMap.put(BindingFault.TAG, BindingFaultImpl.class);
        typeMap.put(SOAPBody.TAG, SOAPBodyImpl.class);
        typeMap.put(SOAPHeader.TAG, SOAPHeaderImpl.class);
        typeMap.put(SOAPHeaderFault.TAG, SOAPHeaderFaultImpl.class);
        typeMap.put(SOAPFault.TAG, SOAPFaultImpl.class);
        typeMap.put(SOAPAddress.TAG, SOAPAddressImpl.class);
        typeMap.put(Documentation.TAG, DocumentationImpl.class);
        typeMap.put(ExtensibilityElement.TAG, ExtensibilityElementImpl.class);
//        typeMap.put(Property.TAG, PropertyImpl.class);
//        typeMap.put(PropertyAlias.TAG, PropertyAliasImpl.class);
    }
    
    /** Creates a new instance of WSDLDocumentImpl */
    public WSDLDocumentImpl() {
        super();
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#load(java.io.Reader)
     */
    public void load(Reader reader, String baseURI) {
    	load(reader, new WSDLParseContext.DefaultParseContext(), baseURI);
    }

    /**
     * @see com.sun.wsdl.model.WSDLDocument#load(java.io.Reader)
     */
    public void load(Reader reader, WSDLParseContext context, String baseURI ) {
    	this.mWSDLParseContext = context;
        reset();
        WSDLDocumentParserFactory fac = WSDLDocumentParserFactory.getInstance();
        fac.load(this, reader, context);
        setBaseURI(baseURI);
        setDataChanged(true);
    }
    
    
    /** Ensure the types element have extensibility elements corresponding to inline schemas.
     *  @since  5.1.0
     */
    protected void ensureInlineSchemasMatchExtensElems() {
        if ((getDocumentDefinitions() != null) && (getDocumentDefinitions().getTypes() != null)) {
            CastorSupport.getInstance(getClass().getClassLoader())
                .convertSchemaToExtensibilityElement(getDocumentDefinitions().getTypes());
        }
    }
    
    /**
     * Serializes the WSDL document to the writer.
     * @param writer the writer
     */
    public void serialize(Writer writer) {
        VisitorService visitorService = new SAXWriteVisitorService();
        Object[] params = new Object[] {writer, Boolean.TRUE, Boolean.FALSE};
        
        ensureInlineSchemasMatchExtensElems();
        
        traverse(visitorService, params);
    }
    
    /** @see com.sun.wsdl.model.WSDLDocument#duplicate(com.sun.wsdl.model.common.model.XMLNode,
     *  com.sun.wsdl.model.WSDLDocument)
     */
    public XMLNode duplicate(XMLNode original, WSDLDocument destination) {
        VisitorService visitorService = new CloneVisitorService();
        XMLNode[] result = new XMLNode[1];
        Object[] params = new Object[] {destination, result};
        WSDLVisitor visitor = (WSDLVisitor) visitorService.fetch(WSDLVisitor.class, null);
        
        ensureInlineSchemasMatchExtensElems();
        
        visitor.prepare(params);
        original.accept(visitor);
        XMLNode cloned = result[0];
        CloneSupport.propagateNamespaces(original, cloned);
        return cloned;
    }
    
    /** @see com.sun.wsdl.model.WSDLDocument#validate(java.util.Collection)
     */
    public void validate(Collection todoListeners) {
        VisitorService visitorService = new ValidateVisitorService();
        Object[] params = new Object[] {todoListeners};
        ValidateVisitor visitor = (ValidateVisitor) visitorService.fetch(WSDLVisitor.class, null);
        
        ensureInlineSchemasMatchExtensElems();
        
        visitor.prepare(params);
        accept(visitor);
        
        // remove ToDoListeners
        Iterator iter = todoListeners.iterator();
        while (iter.hasNext()) {
            visitor.getValidateSupport().removeToDoListener((ToDoListener) iter.next());
        }
    }
    
//    /** @see com.sun.wsdl.model.WSDLDocument#createBPELDocument()
//     */
//    public BPELDocument createBPELDocument() throws EInsightModelException {
//        BPELDocument b = new BPELDocumentImpl();
//        b.setOwnerDocument(this);
//        return b;
//    }
    
    /** Gets an imported document according to namespace prefix.
     * @param   elem    Element from which imported document is referenced.
     * @param   prefix  Namespace prefix.
     * @return  Imported document.
     */
    public XMLDocument getImportedDocument(XMLElement elem, String prefix) {
        XMLDocument impDoc = null;
        String ns = null;
        
        if (elem != null) {
            ns = elem.getNamespace(prefix);
        } else {
            ns = getNamespace(prefix);
        }
//        
//        if (null == ns) {
//            if (prefix.equals(BPELDocument.BPEL_PREFIX)) {
//                ns = BPELDocument.BPEL_NAMESPACE;
//            }
//        }
//        
//        if (BPELDocument.BPEL_NAMESPACE.equals(ns)
//                && ((impDoc = getImportedDocument(ns)) == null)) {
//            impDoc = createBPELDocument();
//            setImportedDocument(ns, impDoc);
//        }
        
        return impDoc;
    }
    
    /** @see com.sun.wsdl.model.WSDLDocument#getWSDLRepositoryObject()
     */
    
    /** @see com.sun.wsdl.model.WSDLDocument#setWSDLRepositoryObject()
     */
    
    /** @see com.sun.wsdl.model.WSDLDocument#getWSDLProvider()
     */
    
    /** @see com.sun.wsdl.model.WSDLDocument#setWSDLProvider()
     */
    
    /** Accepts a visitor to perform some work on the element.
     * @param   w   The working visitor
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean accept(Visitor w) {
        WSDLVisitor v = (WSDLVisitor) w;
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
     * Traverses the document to perform some work by the visitor
     * that is provided by the service. 
     *
     * @param   s   Visitor service provider.
     * @param   v   Values to prepare the persistor.
     */
    public void traverse(VisitorService s, Object[] v) {
        WSDLVisitor visitor = (WSDLVisitor) s.fetch(WSDLVisitor.class, null);
        visitor.prepare(v);
        accept(visitor);
    }

    /**
     * @see com.sun.wsdl.model.WSDLDocument#createXmlNode()
     */
    public XMLNode createXmlNode(String t) {
        XMLNode n = null;
        Class tc = (Class) typeMap.get(t);
        if (tc != null) {
            try {
                n = (XMLNode) tc.newInstance();
                if (n instanceof XMLElement) {
                    XMLElement e = (XMLElement) n;
                    e.setOwnerDocument(this);
                }
            } catch (Throwable tro) {
                throw new XMLParseVisitorException(
                    "Cannot create XMLNode object", tro);
            }
        }
        return n;
    }
    
    /**
     * @see com.sun.wsdl.model.common.model.XMLNode#addChild(com.sun.wsdl.model.common.model.XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof XMLProcessingInstruction) {
            addProcessingInstruction((XMLProcessingInstruction) c);
        } else if (c instanceof WSDLDefinitions) {
            setDocumentDefinitions((WSDLDefinitions) c);
        } else {
            super.addChild(c);
        }
    }
    
    /**
     * @see com.sun.wsdl.model.common.model.XMLNode#removeChild()
     */
    public void removeChild(XMLNode c) {
        if (c instanceof XMLProcessingInstruction) {
            removeProcessingInstruction((XMLProcessingInstruction) c);
        } else if (c instanceof WSDLDefinitions) {
            setDocumentDefinitions(null);
        } else {
            super.removeChild(c);
        }
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createDefinitions()
     */
    public WSDLDefinitions createDefinitions() {
        return new WSDLDefinitionsImpl(this);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#setDocumentDefinitions()
     */
    public void setDocumentDefinitions(WSDLDefinitions definitions) {
        setDocumentElement(definitions);
        setDataChanged(true);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#getDocumentDefinitions()
     */
    public WSDLDefinitions getDocumentDefinitions() {
        return (WSDLDefinitions) rootElement;
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createPortType()
     */
    public PortType createPortType() {
        return new PortTypeImpl(this);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createOperation()
     */
    public Operation createOperation() {
        return new OperationImpl(this);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createOperationInput()
     */
    public OperationInput createOperationInput() {
        return new OperationInputImpl(this);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createOperationOutput()
     */
    public OperationOutput createOperationOutput() {
        return new OperationOutputImpl(this);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createOperationFault()
     */
    public OperationFault createOperationFault() {
        return new OperationFaultImpl(this);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createService()
     */
    public Service createService() {
        return new ServiceImpl(this);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createServicePort()
     */
    public ServicePort createServicePort() {
        return new ServicePortImpl(this);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createPartnerLinkType()
     */
    public PartnerLinkType createPartnerLinkType() {
        return new PartnerLinkTypeImpl(this);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createPartnerLinkRole()
     */
    public PartnerLinkRole createPartnerLinkRole() {
        return new PartnerLinkRoleImpl(this);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createPart()
     */
    public Part createPart() {
        Part part = new PartImpl(this);
        if ((getOwnerDocument() != null) && !this.equals(getOwnerDocument())) {
            part.setQualifiedName(WSDL_PREFIX + ":" + part.getLocalName());
            getOwnerDocument().getDocumentElement().setNamespace(WSDL_PREFIX, WSDL_NAMESPACE);
        }
        return part;
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createWSDLMessage()
     */
    public WSDLMessage createWSDLMessage() {
        WSDLMessage m = new WSDLMessageImpl(this);
        if ((getOwnerDocument() != null) && !this.equals(getOwnerDocument())) {
            m.setQualifiedName(WSDL_PREFIX + ":" + m.getLocalName());
            getOwnerDocument().getDocumentElement().setNamespace(WSDL_PREFIX, WSDL_NAMESPACE);
        }
        return m;
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createImport()
     */
    public Import createImport() {
        return new ImportImpl(this);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createBinding()
     */
    public Binding createBinding() {
        return new BindingImpl(this);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createSOAPBinding()
     */
    public SOAPBinding createSOAPBinding() {
        return new SOAPBindingImpl(this);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createBindingOperation()
     */
    public BindingOperation createBindingOperation() {
        return new BindingOperationImpl(this);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createSOAPOperation()
     */
    public SOAPOperation createSOAPOperation() {
        return new SOAPOperationImpl(this);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createBindingInput()
     */
    public BindingInput createBindingInput() {
        return new BindingInputImpl(this);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createBindingOutput()
     */
    public BindingOutput createBindingOutput() {
        return new BindingOutputImpl(this);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createBindingFault()
     */
    public BindingFault createBindingFault() {
        return new BindingFaultImpl(this);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createSOAPBody()
     */
    public SOAPBody createSOAPBody() {
        return new SOAPBodyImpl(this);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createSOAPHeader()
     */
    public SOAPHeader createSOAPHeader() {
        return new SOAPHeaderImpl(this);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createSOAPHeaderFault()
     */
    public SOAPHeaderFault createSOAPHeaderFault() {
        return new SOAPHeaderFaultImpl(this);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createSOAPFault()
     */
    public SOAPFault createSOAPFault() {
        return new SOAPFaultImpl(this);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createSOAPAddress()
     */
    public SOAPAddress createSOAPAddress() {
        return new SOAPAddressImpl(this);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createDocumentation()
     */
    public Documentation createDocumentation() {
        return new DocumentationImpl(this);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createDocumentation()
     */
    public WSDLDocumentation createWSDLDocumentation() {
        return new WSDLDocumentationImpl(this);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createTypes()
     */
    public Types createTypes() {
        return new TypesImpl(this);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createExtensibilityElement()
     */
    public ExtensibilityElement createExtensibilityElement() {
        return new ExtensibilityElementImpl(this);
    }
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createBPWSProperty()
     */
    public Property createBPWSProperty() {
//        if (getOwnerDocument() == null) {
//            throw new EInsightModelException("Associate element with owner XML document first!");
//        } else if (getOwnerDocument().getDocumentElement() == null) {
//            throw new EInsightModelException("Add root element to owning XML document first!");
//        }
//        
//        Property p = new PropertyImpl(this);
//        if (!this.equals(getOwnerDocument())) {
//            p.setQualifiedName(BPEL_PREFIX + ":" + p.getLocalName());
//            getOwnerDocument().getDocumentElement().setNamespace(BPEL_PREFIX, BPEL_NAMESPACE);
//        }
        
        Property p = new PropertyImpl(this);
        p.setQualifiedName(BPEL_PREFIX + ":" + p.getLocalName());
        return p;
    }
    
    
    
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createBPWSPropertyAlias()
     */
    public PropertyAlias createBPWSPropertyAlias() {
//        if (getOwnerDocument() == null) {
//            throw new EInsightModelException("Associate element with owner XML document first!");
//        } else if (getOwnerDocument().getDocumentElement() == null) {
//            throw new EInsightModelException("Add root element to owning XML document first!");
//        }
//        
//        PropertyAlias p = new PropertyAliasImpl(this);
//        if (!this.equals(getOwnerDocument())) {
//            p.setQualifiedName(BPEL_PREFIX + ":" + p.getLocalName());
//            getOwnerDocument().getDocumentElement().setNamespace(BPEL_PREFIX, BPEL_NAMESPACE);
//        }
//        
        PropertyAlias p = new PropertyAliasImpl(this);
        p.setQualifiedName(BPEL_PREFIX + ":" + p.getLocalName());
        return p;
    }
   
    /**
     * @see com.sun.wsdl.model.WSDLDocument#createBPWSPropertyAlias()
     */
    public Query createBPWSQuery() {
    	Query q = new QueryImpl(this);
        q.setQualifiedName(BPEL_PREFIX + ":" + q.getLocalName());
        return q;
    }
        
    /** @see com.sun.wsdl.model.WSDLDocument#getInlineXMLSchemaSchemaMap()
     */
    public Map getInlineXMLSchemaSchemaMap() {
        if (null == inlineXMLSchemaSchemaMap) {
            inlineXMLSchemaSchemaMap = new HashMap();
        }
        return inlineXMLSchemaSchemaMap;
    }

    /** @see com.sun.wsdl.model.WSDLDocument#getPrivateExtensionMapModel()
     */
    public PrivateExtensionMapModel getPrivateExtensionMapModel() {
        if (null == mPrivateExtMapModel) {
            mPrivateExtMapModel = new PrivateExtensionMapModelImpl();
        }
        return mPrivateExtMapModel;
    }
    
    /**
     * get an named Object that has a name attribute whose value is same
     * as local name of the given qName and the object is defined
     * in the namespace given by the namespace in the qName.
     * 
     * localName must be present in the qName.
     * Either prefix or namespaceURI can be present but not both.
     * 
     * If namespace is missing in qName then default namespace of the
     * document is used.
     * 
     * A null is returned if no object is found.
     *  
     * @param qName QName of the object we are searching.
     * @return Object object matching the qName. caller should
     * check for the type of object he is looking for.
     */
    public Object getElementByQName(QName qName) {
    	if(qName == null) {
    		return null;
    	}
    	
    	//String localName = qName.getLocalName();
    	String localName = qName.getLocalPart();
    	String prefix = qName.getPrefix();
    	String ns = qName.getNamespaceURI();
    	
    	if(prefix != null && ns != null) {
    		throw new IllegalArgumentException("Qname "+ qName + "should not have both prefix and namespace at the same time.");
    	}
    	
    	//if local name is null we can not locate the named object.
    	if(localName == null || localName.trim().equals("")) {
    		return null;
    	}
    	
    	//if prefix is present then try to use it
    	if(prefix != null) {
    		ns = this.getNamespace(prefix);
    	} 
    	
    	//if namespace is null then use the default namespace
    	//of the document
    	if(ns == null) {
    		ns = this.getDefaultNamespace();
    	}
    	
    	//TODO:look into wsdls
    	
    	//look into schemas
    	Collection schemas = this.getDocumentDefinitions().getXMLSchemas();
    	Iterator it = schemas.iterator();
    	
    	while(it.hasNext()) {
    		XMLSchema xmlSchema = (XMLSchema) it.next();
    		Schema schema = (Schema) xmlSchema.getSchema();
    		
    		if(schema != null && ns != null && ns.equals(schema.getTargetNamespace())) {
    			
    			//look for element
    			ElementDecl element = schema.getElementDecl(localName);
    			if(element != null) {
    				return element;
    			}
    			
    			//look for simple type
    			SimpleType simpleType = schema.getSimpleType(localName);
    			if(simpleType != null) {
    				return simpleType;
    			}
    			
    			//look for complex type
    			ComplexType complexType = schema.getComplexType(localName);
    			if(complexType != null) {
    				return complexType;
    			}
    		}
    		
    	}
		
    	return null;
    }
    
    /**
     * get the latest wsdl parse context to be used for parsing wsdls
     * @return WSDLParseContext
     */
    public WSDLParseContext getWSDLParseContext() {
    	return this.mWSDLParseContext;
    }
    
    /*
     * (non-Javadoc)
     * @see java.lang.Object#equals(java.lang.Object)
     */
    public boolean equals(Object obj) {
        // TODO Auto-generated method stub
        if (! (obj instanceof WSDLDocumentImpl)) {
            return false;
        }
        WSDLDocumentImpl toCompare = (WSDLDocumentImpl) obj;
        if (getBaseURI().equals(toCompare.getBaseURI())) {
            return true;
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * @see java.lang.Object#hashCode()
     */
    public int hashCode() {
        // TODO Auto-generated method stub
        return getBaseURI().hashCode();
    }    
}
