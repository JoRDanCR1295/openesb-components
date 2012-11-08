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
 * @(#)SAXParseVisitor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.visitor;

import com.sun.jbi.internationalization.Messages;
import java.io.InputStream;
import java.io.Reader;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import java.util.logging.Level;
import javax.xml.namespace.QName;

import org.xml.sax.Attributes;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;

import com.sun.wsdl.model.Binding;
import com.sun.wsdl.model.BindingFault;
import com.sun.wsdl.model.BindingInput;
import com.sun.wsdl.model.BindingOperation;
import com.sun.wsdl.model.BindingOutput;
import com.sun.wsdl.model.Import;
import com.sun.wsdl.model.NamespaceUtility;
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
import com.sun.wsdl.model.WSDLMessage;
import com.sun.wsdl.model.bpel.Property;
import com.sun.wsdl.model.bpel.PropertyAlias;
import com.sun.wsdl.model.bpel.Query;
import com.sun.wsdl.model.common.MessageManager;
import com.sun.wsdl.model.common.model.Documentation;
import com.sun.wsdl.model.common.model.EInsightModelException;
//import com.sun.wsdl.model.common.model.QName;
import com.sun.wsdl.model.common.model.XMLComment;
import com.sun.wsdl.model.common.model.XMLElement;
import com.sun.wsdl.model.common.model.XMLNode;
import com.sun.wsdl.model.common.model.XMLProcessingInstruction;
import com.sun.wsdl.model.common.util.Namespaces;
import com.sun.wsdl.model.common.visitor.AutonomousVisitor;
import com.sun.wsdl.model.common.visitor.DocumentationVisitor;
import com.sun.wsdl.model.common.visitor.SAXParserSupport;
import com.sun.wsdl.model.common.visitor.XMLParseVisitorException;
import com.sun.wsdl.model.common.visitor.SAXParserSupport.XmlParser;
import com.sun.wsdl.model.extensions.ExtensibilityElement;
import com.sun.wsdl.model.extensions.soap.SOAPAddress;
import com.sun.wsdl.model.extensions.soap.SOAPBinding;
import com.sun.wsdl.model.extensions.soap.SOAPBody;
import com.sun.wsdl.model.extensions.soap.SOAPFault;
import com.sun.wsdl.model.extensions.soap.SOAPHeader;
import com.sun.wsdl.model.extensions.soap.SOAPHeaderFault;
import com.sun.wsdl.model.extensions.soap.SOAPOperation;
import com.sun.wsdl.model.uri.BaseURIResolver;
import com.sun.wsdl.model.xsd.CastorSupport;
import com.sun.wsdl.model.xsd.XMLSchema;
import com.sun.wsdl.model.xsd.impl.CastorSupportImpl;




/**
 * Uses the tokens provided by a SAX parser to visit and set the appropriate
 * nodes of a WSDL model.
 *
 * @author Sun Microsystems
 * @version 
 */
public class SAXParseVisitor extends AbstractVisitor implements AutonomousVisitor {
    
    /** Message key for MUST_SPECIFY_PARENT_PROJECT_ELEM */
    private static final String MUST_SPECIFY_PARENT_PROJECT_ELEM = "MUST_SPECIFY_PARENT_PROJECT_ELEM";  // Not I18N
    
    /** Message key for MUST_SPECIFY_URI_RESOLVER_FACTORY */
    private static final String MUST_SPECIFY_URI_RESOLVER_FACTORY = "MUST_SPECIFY_URI_RESOLVER_FACTORY";  // Not I18N
    
    /** Message key for THRW_UNRECOGNIZED_START_ELEMENT */
    private static final String THRW_UNRECOGNIZED_START_ELEMENT = "THRW_UNRECOGNIZED_START_ELEMENT";  // Not I18N
    
    /** Message key for THRW_UNRECOGNIZED_END_ELEMENT */
    private static final String THRW_UNRECOGNIZED_END_ELEMENT = "THRW_UNRECOGNIZED_END_ELEMENT";  // Not I18N
    
    /** Message key for ILLEGAL_ELEMENT_ENCOUNTERED */
    private static final String ILLEGAL_ELEMENT_ENCOUNTERED = "ILLEGAL_ELEMENT_ENCOUNTERED";  // Not I18N
    
    /** Message key for THRW_MISSING_REQUIRED_ATTRIBUTE */
    private static final String THRW_MISSING_REQUIRED_ATTRIBUTE = "THRW_MISSING_REQUIRED_ATTRIBUTE";  // Not I18N
    
    /** Message key for THRW_IMPORT_PROBLEM */
    private static final String THRW_IMPORT_PROBLEM = "THRW_IMPORT_PROBLEM";  // Not I18N
    
    /** The logger. */
    private static final Messages MESSAGES = 
            Messages.getMessages(SAXParseVisitor.class);
    private static final Logger LOGGER = 
            Messages.getLogger(SAXParseVisitor.class);
    
    /** MessageManager for localized strings. */    
    private MessageManager mMsg = MessageManager.getManager(getClass());
    
    /** Holds Reader for input */
    private Reader reader = null;
    
    /**
     * URI Resolver to resolve  imported documents
     */
    private BaseURIResolver mUriResolver = null;
    
    /** Holds parent project element repository object which creates imported objects */

    
    /** Holds current XML element being processed */
    private XMLElement curXmlElement = null;

    /** Holds the XML parser for the WSDL document. */
    private WSDLDocumentXmlParser mWSDLDocumentXmlParser = null;
    
    /** Holds the XML parser for the definitions element. */
    private DefinitionsXmlParser mDefinitionsXmlParser = null;
    
    /** Holds the XML parser for the Import element. */
    private ImportXmlParser mImportXmlParser = null;
    
    /** Holds the XML parser for the portType element. */
    private PortTypeXmlParser mPortTypeXmlParser = null;
    
    /** Holds the XML parser for the operation element. */
    private OperationXmlParser mOperationXmlParser = null;

    /** Holds the XML parser for the operation input element. */
    private OperationInputXmlParser mOperationInputXmlParser = null;

    /** Holds the XML parser for the operation output element. */
    private OperationOutputXmlParser mOperationOutputXmlParser = null;

    /** Holds the XML parser for the operation fault element. */
    private OperationFaultXmlParser mOperationFaultXmlParser = null;
    
    /** Holds the XML parser for each element */
    private WSDLMessageXmlParser wsdlMessageXmlParser = null;
    
    /** Holds the XML parser for each element */
    private PartXmlParser mPartXmlParser = null;
    
    /** Holds the XML parser for the service link type element. */
    private SLTypeXmlParser mSLTypeXmlParser = null;
    
    /** Holds the XML parser for the service link role element. */
    private SLTypeRoleXmlParser mSLTypeRoleXmlParser = null;
    
    /** Holds the XML parser for the service element. */
    private ServiceXmlParser mServiceXmlParser = null;
    
    /** Holds the XML parser for the binding element. */
    private BindingXmlParser mBindingXmlParser = null;
    
    /** Holds the XML parser for the binding operation element. */
    private BindingOperationXmlParser mBindingOperationXmlParser = null;
    
    /** Holds the XML parser for the binding input element. */
    private BindingInputXmlParser mBindingInputXmlParser = null;
    
    /** Holds the XML parser for the SOAP header element. */
    private SOAPHeaderXmlParser mSOAPHeaderXmlParser = null;
    
    /** Holds the XML parser for the binding output element. */
    private BindingOutputXmlParser mBindingOutputXmlParser = null;
    
    /** Holds the XML parser for the binding fault element. */
    private BindingFaultXmlParser mBindingFaultXmlParser = null;
    
    /** Holds the XML parser for the service port element. */
    private ServicePortXmlParser mServicePortXmlParser = null;
    
    /** Holds the XML parser for the documentation element. */
    private DocumentationXmlParser mDocumentationXmlParser = null;
    
    /** Holds the XML parser for the types element. */
    private TypesXmlParser mTypesXmlParser = null;
    
    /** Holds the XML parser for an extensibility element. */
    private ExtensibilityElementXmlParser mExtensibilityElementXmlParser = null;
    
    /** Holds the XML parser for an PropertyAlias element. */
    private PropertyAliasXmlParser mPropertyAliasXmlParser = null;
    
    /** Holds the XML parser for an Query element. */
    private QueryXmlParser mQueryXmlParser = null;
    
    private XMLReader mXMLReader = null;
    
    /** Creates a new instance of SAXParseVisitor */
    public SAXParseVisitor() {
        super();
    }
    
    /** Getter for property reader.
     * @return Value of property reader.
     *
     */
    public Reader getReader() {
        return reader;
    }
    
    /** Setter for property reader.
     * @param reader New value of property reader.
     *
     */
    public void setReader(Reader reader) {
        this.reader = reader;
    }
    
    /** Getter for property uriResolver.
     * @return  Value of property uriResolver.
     */
    public BaseURIResolver getBaseURIResolver() {
        return mUriResolver;
    }
    
    /** Setter for property uriResolver.
     * @param   uriResolverFac  Value of property uriResolver;
     */
    public void setBaseURIResolver(BaseURIResolver uriResolver) {
        this.mUriResolver = uriResolver;
    }
    
    
    /** Getter for property parentProjectElement.
     * @return  Value of property parentProjectElement.
     */
    
    /** Setter for property parentProjectElement.
     * @param   parentProjectElement  Value of property parentProjectElement.
     */

    
    /** Getter for the outermost XML document.
     * @return  XML document.
     */
    public WSDLDocument getXmlDocument() {
        WSDLDocument doc = (WSDLDocument) curXmlElement.getOwnerDocument();
        if (null == doc) {
            doc = (WSDLDocument) curXmlElement;
        }
        return doc;
    }

    /** Getter for the XML parser support.
     * @return  XML parser support.
     */
    public SAXParserSupport getParserSupport() {
        if (null == getVisitorSupport()) {
            setVisitorSupport(new SAXParserSupport());
        }
        return (SAXParserSupport) getVisitorSupport();
    }
    
    /** Prepares the visitor for use.
     * @param   v   Values to use.
     *              <ol start="0">
     *              <li><code>Reader</code> implementing object.</li>
     *              <li><code>BaseURIResolverFactory</code> implementing object to provide resolution
     *                  for imported namespace object</li>
     *              <li><code>ProjectElement</code> implementing object to create imported objects.</li>
     *              <li><code>Map</code> of imported document registry.</li>
     *              <li><code>Boolean</code> object specifying whether imports should be followed deeply</li>
     *              <li><code>URI</code> object specifying mount point.</li>
     *              <li><code>Project</code> object specifying the base project for imports.</li>
     *              </ol>
     */
    public void prepare(Object[] v) {
        if (v.length > 0 && (v[0] != null) && (v[0] instanceof Reader)) {
            setReader((Reader) v[0]);
        }
        if ((v.length > 1) && (v[1] != null) && (v[1] instanceof BaseURIResolver)) {
            setBaseURIResolver((BaseURIResolver) v[1]);
        }
//        if ((v.length > 2) && (v[2] != null) && (v[2] instanceof ProjectElement)) {
//            setParentProjectElement((ProjectElement) v[2]);
//        }
        if ((v.length > 3) && (v[3] != null) && (v[3] instanceof Map)) {
            getParserSupport().setProcessedImportDocumentRegistry((Map) v[3]);
        } else {
            getParserSupport().clearProcessedImportDocumentRegistry();
        }
//        if ((v.length > 4) && (v[4] != null) && (v[4] instanceof Boolean)) {
//            getParserSupport().setDeepImport((Boolean) v[4]);
//        }
//        if ((v.length > 5) && (v[5] != null) && (v[5] instanceof URI)) {
//            getParserSupport().setMountPoint((URI) v[5]);
//        }
//        if ((v.length > 6) && (v[6] != null) && (v[6] instanceof Project)) {
//            getParserSupport().setBaseProject((Project) v[6]);
//        }

    }
    
    /** Handle illegal elements found.
     */
    private void handleIllegalElement(String foundInElem, String uri, String localName,
                                      String qName, Attributes attributes) {
        LOGGER.log(Level.SEVERE,
                MESSAGES.getString(
                "SAXParseVisitor.ILLEGAL_ELEMENT_ENCOUNTERED",
                new Object[] {uri, localName, foundInElem} ));
    }
    
    /**
     * Adds a WSDL extensibility element under the current WSDL element.
     *
     * @param   uri         The governing URI namespace.
     * @param   localName   Local name of the element.
     * @param   qName       Qualified name of the element.
     * @param   attributes  Attributes of the element.
     */
    private void addExtensibilityElement(String uri, String localName, String qName,
                                         Attributes attributes) {
        LOGGER.log(Level.FINE,
                MESSAGES.getString(
                "SAXParseVisitor.THRW_UNRECOGNIZED_START_ELEMENT",
                new Object[] {qName, uri} ));
        
        ExtensibilityElement exten = getXmlDocument().createExtensibilityElement();
        exten.setElementType(NamespaceUtility.getQNameFromURIAndString(uri, qName));
        exten.setLocator(getParserSupport().getLocator());
        SAXParserSupport.setAttributes(exten, attributes);
        curXmlElement.addChildAtTail(exten);
        exten.accept(SAXParseVisitor.this);
    }
    
    /** Visits a WSDL document.
     * @param   d   a WSDL document.
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(WSDLDocument d) {
        try {
        	createXMLReader();
            
            if (null == mWSDLDocumentXmlParser) {
                mWSDLDocumentXmlParser = new WSDLDocumentXmlParser();
            }
            
            curXmlElement = getParserSupport()
                .pushXmlParser(mWSDLDocumentXmlParser, d);
            
        } catch (Throwable trw) {
            throw new XMLParseVisitorException(
                MESSAGES.getString("SAXParseVisitor.CANNOT_PARSE_WSDL"), trw);
        }
        return false;
    }
    
    public void startParsing(String baseURI) throws XMLParseVisitorException {
    	try {
    	if(this.mXMLReader == null) {        
    		throw new XMLParseVisitorException(
                        MESSAGES.getString("SAXParseVisitor.XML_RDR_IS_NULL"));
    	}
	        Reader reader = getReader();
                InputSource in = new InputSource(reader);
	        in.setSystemId(baseURI);
	        
	        mXMLReader.parse(in);
                reader.close();
    	} catch (Throwable trw) {
            throw new XMLParseVisitorException(
                "Cannot parse WSDL", trw);
        }
    }
    
    public void createXMLReader() throws Exception {
    	SAXParseVisitorService vService = (SAXParseVisitorService) this.getVisitorService();
		ErrorHandler errorHandler = vService.getWSDLParseContext().getErrorHandler();
		boolean validate = errorHandler != null ? true : false;
		
		InputStream[] wsdlExtensionSchemas = vService.getWSDLParseContext().getWSDLExtensionSchemas();
		
		mXMLReader = 
        	getParserSupport().createXmlReader(validate, wsdlExtensionSchemas);
		
		
		
		mXMLReader.setErrorHandler(errorHandler);
    }
    
    /**
     * Implements XML parser for WSDL document.
     */
    protected class WSDLDocumentXmlParser extends XmlParser {
        
        /** Constructs a WSDLDocumentXmlParser.
         */
        public WSDLDocumentXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                                 Attributes attributes) throws SAXException {
            if (Namespaces.isWSDLElement(WSDLDefinitions.TAG, uri, localName, qName)) {
                WSDLDefinitions definitions =
                    getXmlDocument().createDefinitions();
                definitions.setQualifiedName(qName);
                definitions.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(definitions, attributes);
                getXmlDocument().setDocumentDefinitions(definitions);
                
                // Set the processed import document to prevent cyclic dependencies
                getParserSupport().registerProcessedImportDocument(definitions.getTargetNamespace(),
                                                                   getXmlDocument().getBaseURI(),
                                                                   getXmlDocument());
                
                definitions.accept(SAXParseVisitor.this);
            } else if (!Namespaces.isWSDLElement(localName, uri, localName, qName)
                       && !Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(uri, localName, qName, attributes);
            } else {
                handleIllegalElement(MESSAGES.getString("SaxParseVisitor.WSDL_DOC"), uri, localName,
                                     qName, attributes);
            }
        }
        
        /** Called when a processing instruction is encountered.
         * @param   target  Target of processing instruction.
         * @param   data    Data of processing instruction.
         * @throws  SAXException    When SAX problems occur.
         */
        public void processingInstruction(String target, String data)
            throws SAXException {
            XMLProcessingInstruction tProcessingInstruction =
                getXmlDocument().createProcessingInstruction();
            tProcessingInstruction.setTarget(target);
            tProcessingInstruction.setData(data);
            getXmlDocument().addProcessingInstruction(tProcessingInstruction);
            tProcessingInstruction.accept(SAXParseVisitor.this);
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
            throws SAXException {
            if (Namespaces.isWSDLElement(WSDLDefinitions.TAG, uri, localName, qName)) {
                curXmlElement = getParserSupport().popXmlParser();
            }
        }
        
    }
    
    /** Visits a XML processing instruction section.
     * @param   p   XML processing instruction.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(XMLProcessingInstruction p) {
        getXmlDocument().addProcessingInstruction(p);
        return false;
    }
    
    /** Visits a comment section.
     * @param   c   XML comment section
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(XMLComment c) {
        return false;
    }
    
    /**
     * Visits a WSDL definitions.
     * @param w a WSDL definitions element
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(WSDLDefinitions w) {
        if (null == mDefinitionsXmlParser) {
            mDefinitionsXmlParser = new DefinitionsXmlParser();
        }
        curXmlElement = getParserSupport().pushXmlParser(mDefinitionsXmlParser, w);
        return false;
    }
    
    /**
     * Implements XML parser for the &lt;definitions&gt; element.
     */
    protected class DefinitionsXmlParser extends XmlParser {
        
        /** Constructs a DefinitionsXmlParser.
         */
        public DefinitionsXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                                 Attributes attributes) throws SAXException {
            if (Namespaces.isWSDLElement(Import.TAG, uri, localName, qName)) {
                Import wsdlImport = getXmlDocument().createImport();
                wsdlImport.setQualifiedName(qName);
                wsdlImport.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(wsdlImport, attributes);
                ((WSDLDefinitions) curXmlElement).addImportAtTail(wsdlImport);
                wsdlImport.accept(SAXParseVisitor.this);
                // import other WSDL documents
                //importDocument(wsdlImport);
            } else if (Namespaces.isWSDLElement(Types.TAG, uri, localName, qName)) {
                Types types = getXmlDocument().createTypes();
                types.setQualifiedName(qName);
                types.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(types, attributes);
                ((WSDLDefinitions) curXmlElement).setTypesAtTail(types);
                types.accept(SAXParseVisitor.this);
            } else if (Namespaces.isWSDLElement(WSDLMessage.TAG, uri, localName, qName)) {
                WSDLMessage message = getXmlDocument().createWSDLMessage();
                message.setQualifiedName(qName);
                message.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(message, attributes);
                ((WSDLDefinitions) curXmlElement).addMessageAtTail(message);
                message.accept(SAXParseVisitor.this);
            } else if (Namespaces.isWSDLElement(PortType.TAG, uri, localName, qName)) {
                PortType portType = getXmlDocument().createPortType();
                portType.setQualifiedName(qName);
                portType.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(portType, attributes);
                ((WSDLDefinitions) curXmlElement).addPortTypeAtTail(portType);
                portType.accept(SAXParseVisitor.this);
            } else if (Namespaces.isWSDLElement(Binding.TAG, uri, localName, qName)) {
                Binding binding = getXmlDocument().createBinding();
                binding.setQualifiedName(qName);
                binding.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(binding, attributes);
                ((WSDLDefinitions) curXmlElement).addBindingAtTail(binding);
                binding.accept(SAXParseVisitor.this);
            } else if (Namespaces.isWSDLElement(Service.TAG, uri, localName, qName)) {
                Service service = getXmlDocument().createService();
                service.setQualifiedName(qName);
                service.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(service, attributes);
                ((WSDLDefinitions) curXmlElement).addServiceAtTail(service);
                service.accept(SAXParseVisitor.this);
            } else if (Namespaces.isWSDLServiceLinkTypeElement(PartnerLinkType.TAG, uri, localName, qName)) {
                PartnerLinkType serviceLinkType =
                    getXmlDocument().createPartnerLinkType();
                serviceLinkType.setQualifiedName(qName);
                serviceLinkType.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(serviceLinkType, attributes);
                ((WSDLDefinitions) curXmlElement).addServiceLinkTypeAtTail(
                                                            serviceLinkType);
                serviceLinkType.accept(SAXParseVisitor.this);
            } else if (addDocumentationElement(qName, attributes, getParserSupport().getLocator())) {
                // captured
            } else if (Namespaces.isBPELElement(Property.TAG, uri, localName, qName)) {
                Property prop = getXmlDocument().createBPWSProperty();
                prop.setQualifiedName(qName);
                prop.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(prop, attributes);
                ((WSDLDefinitions) curXmlElement).addBPWSPropertyAtTail(prop);
            } else if (Namespaces.isBPELElement(PropertyAlias.TAG, uri, localName, qName)) {
                PropertyAlias propAlias = getXmlDocument().createBPWSPropertyAlias();
                propAlias.setQualifiedName(qName);
                propAlias.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(propAlias, attributes);
                ((WSDLDefinitions) curXmlElement).addBPWSPropertyAliasAtTail(propAlias);
                propAlias.accept(SAXParseVisitor.this);
            } else if (!Namespaces.isWSDLElement(localName, uri, localName, qName)
                       && !Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(uri, localName, qName, attributes);
            } else {
                handleIllegalElement(curXmlElement.getLocalName(), uri, localName,
                                     qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
            throws SAXException {
            if (Namespaces.isWSDLElement(WSDLDefinitions.TAG, uri, localName, qName)) {
                curXmlElement = getParserSupport().popXmlParser();
            } else if (Namespaces.isWSDLElement(Service.TAG, uri, localName, qName)) {
                // do nothing
            } else if (Namespaces.isWSDLElement(Import.TAG, uri, localName, qName)) {
                // do nothing
            } else if (Namespaces.isBPELElement(Property.TAG, uri, localName, qName)) {
                // do nothing
            } else if (Namespaces.isBPELElement(PropertyAlias.TAG, uri, localName, qName)) {
                // do nothing
            } else {
                LOGGER.log(Level.FINE, 
                        MESSAGES.getString("SAXParseVisitor.THRW_UNRECOGNIZED_END_ELEMENT"),
                        new Object[] {qName, uri} );
            }
        }
        
    }
    
    /**
     * Visits a portType element.
     * @param portType a portType element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(PortType portType) {
        if (null == mPortTypeXmlParser) {
            mPortTypeXmlParser = new PortTypeXmlParser();
        }
        curXmlElement = getParserSupport().pushXmlParser(mPortTypeXmlParser, portType);
        return false;
    }
    
    /**
     * Implements XML parser for the &lt;portType&gt; element.
     */
    protected class PortTypeXmlParser extends XmlParser {
        
        /** Constructs a PortTypeXmlParser.
         */
        public PortTypeXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                                 Attributes attributes) throws SAXException {
            if (Namespaces.isWSDLElement(Operation.TAG, uri, localName, qName)) {
                Operation operation = getXmlDocument().createOperation();
                operation.setQualifiedName(qName);
                operation.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(operation, attributes);
                ((PortType) curXmlElement).addOperationAtTail(operation);
                operation.accept(SAXParseVisitor.this);
            } else if (addDocumentationElement(qName, attributes, getParserSupport().getLocator())) {
                // captured
            } else if (!Namespaces.isWSDLElement(localName, uri, localName, qName)
                       && !Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(uri, localName, qName, attributes);
            } else {
                handleIllegalElement(curXmlElement.getLocalName(), uri, localName,
                                     qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
            throws SAXException {
            if (Namespaces.isWSDLElement(PortType.TAG, uri, localName, qName)) {
                curXmlElement = getParserSupport().popXmlParser();
            } else {
                LOGGER.log(Level.FINE, 
                        MESSAGES.getString("SAXParseVisitor.PORTTYPEXMLPARSER_ENDELEM") +
                        MESSAGES.getString("SAXParseVisitor.THRW_UNRECOGNIZED_END_ELEMENT", 
                        new Object[] {qName, uri}));
            }
        }
    }
    
    /**
     * Visits a portType operation element.
     * @param operation a portType operation element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Operation operation) {
        if (null == mOperationXmlParser) {
            mOperationXmlParser = new OperationXmlParser();
        }
        curXmlElement = getParserSupport().pushXmlParser(mOperationXmlParser, operation);
        return false;
    }
    
    /**
     * Implements XML parser for the &lt;operations&gt; element.
     */
    protected class OperationXmlParser extends XmlParser {
        
        /** Constructs a OperationXmlParser.
         */
        public OperationXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                                 Attributes attributes) throws SAXException {
            if (Namespaces.isWSDLElement(OperationInput.TAG, uri, localName, qName)) {
                OperationInput input = getXmlDocument().createOperationInput();
                input.setQualifiedName(qName);
                input.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(input, attributes);
                Operation operation = (Operation) curXmlElement;
                if (operation.getOperationType() == Operation.UNKNOWN_OPERATION) {
                    operation.setOperationType(Operation.REQUEST_RESPONSE_ONE_WAY_OPERATION);
                }
                operation.setInputAtTail(input);
                input.accept(SAXParseVisitor.this);
            } else if (Namespaces.isWSDLElement(OperationOutput.TAG, uri, localName, qName)) {
                OperationOutput output = getXmlDocument().createOperationOutput();
                output.setQualifiedName(qName);
                output.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(output, attributes);
                Operation operation = (Operation) curXmlElement;
                if (operation.getOperationType() == Operation.UNKNOWN_OPERATION) {
                    operation.setOperationType(Operation.SOLICIT_RESPONSE_NOTIFICATION_OPERATION);
                }
                operation.setOutputAtTail(output);
                output.accept(SAXParseVisitor.this);
            } else if (Namespaces.isWSDLElement(OperationFault.TAG, uri, localName, qName)) {
                OperationFault fault = getXmlDocument().createOperationFault();
                fault.setQualifiedName(qName);
                fault.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(fault, attributes);
                ((Operation) curXmlElement).addFaultAtTail(fault);
                
                fault.accept(SAXParseVisitor.this);
            } else if (addDocumentationElement(qName, attributes, getParserSupport().getLocator())) {
                // captured
            } else if (!Namespaces.isWSDLElement(localName, uri, localName, qName)
                       && !Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(uri, localName, qName, attributes);
            } else {
                handleIllegalElement(curXmlElement.getLocalName(), uri, localName,
                                     qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
            throws SAXException {
            if (Namespaces.isWSDLElement(Operation.TAG, uri, localName, qName)) {
                curXmlElement = getParserSupport().popXmlParser();
            } else if (Namespaces.isWSDLElement(OperationInput.TAG, uri, localName, qName)) {
                // do nothing
            } else if (Namespaces.isWSDLElement(OperationOutput.TAG, uri, localName, qName)) {
                // do nothing
            } else if (Namespaces.isWSDLElement(OperationFault.TAG, uri, localName, qName)) {
                // do nothing
            } else {
                LOGGER.log(Level.FINE, 
                        MESSAGES.getString("SAXParseVisitor.OPERXMLPARSER_ENDELEM") +
                        MESSAGES.getString("SAXParseVisitor.THRW_UNRECOGNIZED_END_ELEMENT", 
                        new Object[] {qName, uri} ));  
            }
        }
    }
    
    /**
     * Visits an operation input element.
     * @param input an operation input element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(OperationInput input) {
    	if (null == mOperationInputXmlParser) {
    		mOperationInputXmlParser = new OperationInputXmlParser();
        }
        curXmlElement = getParserSupport().pushXmlParser(mOperationInputXmlParser, input);
        return false;
    }
    
    /**
     * Implements XML parser for the &lt;binding&gt; element.
     */
    protected class OperationInputXmlParser extends XmlParser {
        
        /** Constructs a BindingXmlParser.
         */
        public OperationInputXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                                 Attributes attributes) throws SAXException {
            if (addDocumentationElement(qName, attributes, getParserSupport().getLocator())) {
                // captured
            }  else {
                handleIllegalElement(curXmlElement.getLocalName(), uri, localName,
                                     qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
            throws SAXException {
        	if (Namespaces.isWSDLElement(OperationInput.TAG, uri, localName, qName)) {
                curXmlElement = getParserSupport().popXmlParser();
            }else if (qName.endsWith(Documentation.TAG)) {
            	//this is fine
            } else {
                LOGGER.log(Level.FINE, 
                        MESSAGES.getString("SAXParseVisitor.IMPORTXMLPARSER_ENDELEM") +
                        MESSAGES.getString("SAXParseVisitor.THRW_UNRECOGNIZED_END_ELEMENT", 
                        new Object[] {qName, uri} ));  
            }
        }
    }
    
    /**
     * Visits an operation output element.
     * @param output an operation output element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(OperationOutput output) {
    	if (null == mOperationOutputXmlParser) {
            mOperationOutputXmlParser = new OperationOutputXmlParser();
        }
        curXmlElement = getParserSupport().pushXmlParser(mOperationOutputXmlParser, output);
        return false;
    }
    
    /**
     * Implements XML parser for the &lt;binding&gt; element.
     */
    protected class OperationOutputXmlParser extends XmlParser {
        
        /** Constructs a BindingXmlParser.
         */
        public OperationOutputXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                                 Attributes attributes) throws SAXException {
            if (addDocumentationElement(qName, attributes, getParserSupport().getLocator())) {
                // captured
            }  else {
                handleIllegalElement(curXmlElement.getLocalName(), uri, localName,
                                     qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
            throws SAXException {
        	if (Namespaces.isWSDLElement(OperationOutput.TAG, uri, localName, qName)) {
                curXmlElement = getParserSupport().popXmlParser();
            }else if (qName.endsWith(Documentation.TAG)) {
            	//this is fine
            } else {
                LOGGER.log(Level.FINE, 
                        MESSAGES.getString("SAXParseVisitor.IMPORTXMLPARSER_ENDELEM") +
                        MESSAGES.getString("SAXParseVisitor.THRW_UNRECOGNIZED_END_ELEMENT", 
                        new Object[] {qName, uri} ));
            }
        }
    }
    
    /**
     * Visits an operation fault element.
     * @param fault an operation fault element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(OperationFault fault) {
    	if (null == mOperationFaultXmlParser) {
            mOperationFaultXmlParser = new OperationFaultXmlParser();
        }
        curXmlElement = getParserSupport().pushXmlParser(mOperationFaultXmlParser, fault);
        return false;
    }
    
    /**
     * Implements XML parser for the &lt;binding&gt; element.
     */
    protected class OperationFaultXmlParser extends XmlParser {
        
        /** Constructs a BindingXmlParser.
         */
        public OperationFaultXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                                 Attributes attributes) throws SAXException {
            if (addDocumentationElement(qName, attributes, getParserSupport().getLocator())) {
                // captured
            }  else {
                handleIllegalElement(curXmlElement.getLocalName(), uri, localName,
                                     qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
            throws SAXException {
        	if (Namespaces.isWSDLElement(OperationFault.TAG, uri, localName, qName)) {
                curXmlElement = getParserSupport().popXmlParser();
            }else if (qName.endsWith(Documentation.TAG)) {
            	//this is fine
            } else {
                LOGGER.log(Level.FINE, 
                        MESSAGES.getString("SAXParseVisitor.IMPORTXMLPARSER_ENDELEM") +
                        MESSAGES.getString("SAXParseVisitor.THRW_UNRECOGNIZED_END_ELEMENT", 
                        new Object[] {qName, uri} ));
            }
        }
    }
    
    /** Visits a wsdl:message element.
     * @param   w   a wsdl:message element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(WSDLMessage w) {
        if (null == wsdlMessageXmlParser) {
            wsdlMessageXmlParser = new WSDLMessageXmlParser();
        }
        curXmlElement = getParserSupport().pushXmlParser(wsdlMessageXmlParser, w);
        return false;
    }
    
    /** Implements XML parser for the message element.
     */
    protected class WSDLMessageXmlParser extends XmlParser {
        
        /** Constructs a WSDLMessageXmlParser.
         */
        public WSDLMessageXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when start of a element is detected.
         * @param   uri         URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of the element.
         * @throws  SAXException    When SAX problems occur.
         */
        public void startElement(String uri, String localName, String qName,
                                 Attributes attributes) throws SAXException {
            if (Namespaces.isWSDLElement(Part.TAG, uri, localName, qName)) {
                Part tPart = getXmlDocument().createPart();
                tPart.setQualifiedName(qName);
                tPart.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tPart, attributes);
                ((WSDLMessage) curXmlElement).addPartAtTail(tPart);
                tPart.accept(SAXParseVisitor.this);
            } else if (addDocumentationElement(qName, attributes, getParserSupport().getLocator())) {
                // captured
            } else if (!Namespaces.isWSDLElement(localName, uri, localName, qName)
                       && !Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(uri, localName, qName, attributes);
            } else {
                handleIllegalElement(curXmlElement.getLocalName(), uri, localName,
                                     qName, attributes);
            }
        }
        
        /** Called when end of a element is detected.
         * @param   uri         URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    When SAX problems occur.
         */
        public void endElement(String uri, String localName, String qName)
            throws SAXException {
            if (Namespaces.isWSDLElement(Part.TAG, uri, localName, qName)) {
                // Action already taken in above test
            } else if (Namespaces.isWSDLElement(WSDLMessage.TAG, uri, localName, qName)) {
                curXmlElement = getParserSupport().popXmlParser();
            } else {
                LOGGER.log(Level.FINE, 
                        MESSAGES.getString("SAXParseVisitor.WSDLMSGXMLPARSER_ENDELEM") +
                        MESSAGES.getString("SAXParseVisitor.THRW_UNRECOGNIZED_END_ELEMENT", 
                        new Object[] {qName, uri} ));
            }
        }
    }
    
    /** Visits a part element.
     * @param   p   a part element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(Part p) {
    	 if (null == mPartXmlParser) {
    	 	mPartXmlParser = new PartXmlParser();
        }
        curXmlElement = getParserSupport().pushXmlParser(mPartXmlParser, p);
        return false;
    }
    
    /**
     * Implements XML parser for the &lt;binding&gt; element.
     */
    protected class PartXmlParser extends XmlParser {
        
        /** Constructs a BindingXmlParser.
         */
        public PartXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                                 Attributes attributes) throws SAXException {
            if (addDocumentationElement(qName, attributes, getParserSupport().getLocator())) {
                // captured
            }  else {
                handleIllegalElement(curXmlElement.getLocalName(), uri, localName,
                                     qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
            throws SAXException {
        	if (Namespaces.isWSDLElement(Part.TAG, uri, localName, qName)) {
                curXmlElement = getParserSupport().popXmlParser();
            }else if (qName.endsWith(Documentation.TAG)) {
            	//this is fine
            } else {
                LOGGER.log(Level.FINE, 
                        MESSAGES.getString("SAXParseVisitor.IMPORTXMLPARSER_ENDELEM") +
                        MESSAGES.getString("SAXParseVisitor.THRW_UNRECOGNIZED_END_ELEMENT", 
                        new Object[] {qName, uri} ));
            }
        }
    }
    
    /**
     * Visits a service element.
     * @param service a service element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Service service) {
        if (null == mServiceXmlParser) {
            mServiceXmlParser = new ServiceXmlParser();
        }
        curXmlElement = getParserSupport().pushXmlParser(mServiceXmlParser, service);
        return false;
    }
    
    /**
     * Implements XML parser for the &lt;service&gt; element.
     */
    protected class ServiceXmlParser extends XmlParser {
        
        /** Constructs a ServiceXmlParser.
         */
        public ServiceXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                                 Attributes attributes) throws SAXException {
            if (Namespaces.isWSDLElement(ServicePort.TAG, uri, localName, qName)) {
                ServicePort port = getXmlDocument().createServicePort();
                port.setQualifiedName(qName);
                port.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(port, attributes);
                ((Service) curXmlElement).addPortAtTail(port);
                port.accept(SAXParseVisitor.this);
            } else if (addDocumentationElement(qName, attributes, getParserSupport().getLocator())) {
                // captured
            } else if (!Namespaces.isWSDLElement(localName, uri, localName, qName)
                       && !Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(uri, localName, qName, attributes);
            } else {
                handleIllegalElement(curXmlElement.getLocalName(), uri, localName,
                                     qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
            throws SAXException {
            if (Namespaces.isWSDLElement(Service.TAG, uri, localName, qName)) {
                curXmlElement = getParserSupport().popXmlParser();
            } else if (Namespaces.isWSDLElement(ServicePort.TAG, uri, localName, qName)) {
                // do nothing
            } else if (qName.endsWith("documentation")) {
                // do nothing
            } else {
                LOGGER.log(Level.FINE, 
                        MESSAGES.getString("SAXParseVisitor.SERVICEXMLPARSER_ENDELEM") +
                        MESSAGES.getString("SAXParseVisitor.THRW_UNRECOGNIZED_END_ELEMENT", 
                        new Object[] {qName, uri} ));
            }
        }
    }
    
    /**
     * Visits a service port element.
     * @param port a service port element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(ServicePort port) {
        if (null == mServicePortXmlParser) {
            mServicePortXmlParser = new ServicePortXmlParser();
        }
        curXmlElement = getParserSupport().pushXmlParser(mServicePortXmlParser, port);
        return false;
    }
    
    /**
     * Implements XML parser for the service &lt;port&gt; element.
     */
    protected class ServicePortXmlParser extends XmlParser {
        
        /** Constructs a ServicePortXmlParser.
         */
        public ServicePortXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                                 Attributes attributes) throws SAXException {
            if (Namespaces.isSOAPElement(SOAPAddress.TAG, uri, localName, qName)) {
                SOAPAddress soapAddress = getXmlDocument().createSOAPAddress();
                soapAddress.setQualifiedName(qName);
                soapAddress.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(soapAddress, attributes);
                ((ServicePort) curXmlElement).addExtensibilityElementAtTail(soapAddress);
            } else if (addDocumentationElement(qName, attributes, getParserSupport().getLocator())) {
                // captured
            } else if (!Namespaces.isWSDLElement(localName, uri, localName, qName)
                       && !Namespaces.isSOAPElement(localName, uri, localName, qName)
                       && !Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(uri, localName, qName, attributes);
            } else {
                handleIllegalElement(curXmlElement.getLocalName(), uri, localName,
                                     qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
            throws SAXException {
            if (Namespaces.isSOAPElement(SOAPAddress.TAG, uri, localName, qName)) {
                // do nothing
            } else if (Namespaces.isWSDLElement(ServicePort.TAG, uri, localName, qName)) {
                curXmlElement = getParserSupport().popXmlParser();
            } else {
                LOGGER.log(Level.FINE, 
                        MESSAGES.getString("SAXParseVisitor.SERVICEXMLPARSER_ENDELEM") +
                        MESSAGES.getString("SAXParseVisitor.THRW_UNRECOGNIZED_END_ELEMENT", 
                        new Object[] {qName, uri} ));
            }
        }
    }
    
    /**
     * Visits a service link type element.
     * @param type a service link type element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(PartnerLinkType type) {
        if (null == mSLTypeXmlParser) {
            mSLTypeXmlParser = new SLTypeXmlParser();
        }
        curXmlElement = getParserSupport().pushXmlParser(mSLTypeXmlParser, type);
        return false;
    }
    
    /**
     * Implements XML parser for the &lt;serviceLinkType&gt; element.
     */
    protected class SLTypeXmlParser extends XmlParser {
        
        /** Constructs a SLTypeXmlParser.
         */
        public SLTypeXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                                 Attributes attributes) throws SAXException {
            if (Namespaces.isWSDLServiceLinkTypeElement(PartnerLinkRole.TAG, uri, localName, qName)) {
                PartnerLinkRole role = getXmlDocument().createPartnerLinkRole();
                role.setQualifiedName(qName);
                role.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(role, attributes);
                ((PartnerLinkType) curXmlElement).addRole(role);
                role.accept(SAXParseVisitor.this);
            } else if (addDocumentationElement(qName, attributes, getParserSupport().getLocator())) {
                // captured
            } else if (!Namespaces.isWSDLElement(localName, uri, localName, qName)
                       && !Namespaces.isWSDLServiceLinkTypeElement(localName, uri, localName, qName)
                       && !Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(uri, localName, qName, attributes);
            } else {
                handleIllegalElement(curXmlElement.getLocalName(), uri, localName,
                                     qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
            throws SAXException {
            if (Namespaces.isWSDLServiceLinkTypeElement(PartnerLinkType.TAG, uri, localName, qName)) {
                curXmlElement = getParserSupport().popXmlParser();
            } else {
                LOGGER.log(Level.FINE, 
                        MESSAGES.getString("SAXParseVisitor.SLTYPEXMLPARSER_ENDELEM") +
                        MESSAGES.getString("SAXParseVisitor.THRW_UNRECOGNIZED_END_ELEMENT", 
                        new Object[] {qName, uri} ));
            }
        }
    }
    
    /**
     * Visits a service link role element.
     * @param role a service link role element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(PartnerLinkRole role) {
        if (null == mSLTypeRoleXmlParser) {
            mSLTypeRoleXmlParser = new SLTypeRoleXmlParser();
        }
        curXmlElement = getParserSupport().pushXmlParser(mSLTypeRoleXmlParser, role);
        return false;
    }
    
    /**
     * Implements XML parser for the SLT &lt;role&gt; element.
     */
    protected class SLTypeRoleXmlParser extends XmlParser {
        
        /** Constructs a SLTypeRoleXmlParser.
         */
        public SLTypeRoleXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                                 Attributes attributes) throws SAXException {
            if (addDocumentationElement(qName, attributes, getParserSupport().getLocator())) {
                // captured
            } else if (!Namespaces.isWSDLElement(localName, uri, localName, qName)
                       && !Namespaces.isWSDLServiceLinkTypeElement(localName, uri, localName, qName)
                       && !Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(uri, localName, qName, attributes);
            } else {
                handleIllegalElement(curXmlElement.getLocalName(), uri, localName,
                                     qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
            throws SAXException {
            if (Namespaces.isWSDLServiceLinkTypeElement(PartnerLinkRole.TAG, uri, localName, qName)) {
                curXmlElement = getParserSupport().popXmlParser();
            } else {
                LOGGER.log(Level.FINE, 
                        MESSAGES.getString("SAXParseVisitor.SLTYPEROLEXMLPARSER_ENDELEM") +
                        MESSAGES.getString("SAXParseVisitor.THRW_UNRECOGNIZED_END_ELEMENT", 
                        new Object[] {qName, uri} ));
            }
        }
    }
    
    /**
     * Visits an import element.
     * @param wsdlImport an import element
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Import wsdlImport) {
    	 if (null == mImportXmlParser) {
    	 	mImportXmlParser = new ImportXmlParser();
        }
        curXmlElement = getParserSupport().pushXmlParser(mImportXmlParser, wsdlImport);
        return false;
    }
    
    /**
     * Implements XML parser for the &lt;binding&gt; element.
     */
    protected class ImportXmlParser extends XmlParser {
        
        /** Constructs a BindingXmlParser.
         */
        public ImportXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                                 Attributes attributes) throws SAXException {
            if (addDocumentationElement(qName, attributes, getParserSupport().getLocator())) {
                // captured
            }  else {
                handleIllegalElement(curXmlElement.getLocalName(), uri, localName,
                                     qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
            throws SAXException {
        	if (Namespaces.isWSDLElement(Import.TAG, uri, localName, qName)) {
                curXmlElement = getParserSupport().popXmlParser();
            }else if (qName.endsWith(Documentation.TAG)) {
            	//this is fine
            } else {
                LOGGER.log(Level.FINE, 
                        MESSAGES.getString("SAXParseVisitor.IMPORTXMLPARSER_ENDELEM") +
                        MESSAGES.getString("SAXParseVisitor.THRW_UNRECOGNIZED_END_ELEMENT", 
                        new Object[] {qName, uri} ));
            }
        }
    }
    
    /**
     * @see WSDLVisitor#visit(Binding)
     */
    public boolean visit(Binding binding) {
        if (null == mBindingXmlParser) {
            mBindingXmlParser = new BindingXmlParser();
        }
        curXmlElement = getParserSupport().pushXmlParser(mBindingXmlParser, binding);
        return false;
    }
    
    /**
     * Implements XML parser for the &lt;binding&gt; element.
     */
    protected class BindingXmlParser extends XmlParser {
        
        /** Constructs a BindingXmlParser.
         */
        public BindingXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                                 Attributes attributes) throws SAXException {
            if (Namespaces.isSOAPElement(SOAPBinding.TAG, uri, localName, qName)) {
                SOAPBinding soapBinding = getXmlDocument().createSOAPBinding();
                soapBinding.setQualifiedName(qName);
                soapBinding.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(soapBinding, attributes);
                ((Binding) curXmlElement).addExtensibilityElementAtTail(soapBinding);
            } else if (Namespaces.isWSDLElement(BindingOperation.TAG, uri, localName, qName)) {
                BindingOperation bindingOp = getXmlDocument().createBindingOperation();
                bindingOp.setQualifiedName(qName);
                bindingOp.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(bindingOp, attributes);
                ((Binding) curXmlElement).addBindingOperationAtTail(bindingOp);
                bindingOp.accept(SAXParseVisitor.this);
            } else if (addDocumentationElement(qName, attributes, getParserSupport().getLocator())) {
                // captured
            } else if (!Namespaces.isWSDLElement(localName, uri, localName, qName)
                       && !Namespaces.isSOAPElement(localName, uri, localName, qName)
                       && !Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(uri, localName, qName, attributes);
            } else {
                handleIllegalElement(curXmlElement.getLocalName(), uri, localName,
                                     qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
            throws SAXException {
            if (Namespaces.isSOAPElement(SOAPBinding.TAG, uri, localName, qName)) {
                // do nothing
            } else if (Namespaces.isWSDLElement(Binding.TAG, uri, localName, qName)) {
                curXmlElement = getParserSupport().popXmlParser();
            } else {
                LOGGER.log(Level.FINE, 
                        MESSAGES.getString("SAXParseVisitor.BINDINGXMLPARSER_ENDELEM") +
                        MESSAGES.getString("SAXParseVisitor.THRW_UNRECOGNIZED_END_ELEMENT", 
                        new Object[] {qName, uri} ));
            }
        }
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPBinding)
     */
    public boolean visit(SOAPBinding sBinding) {
        return false;
    }
    
    /**
     * @see WSDLVisitor#visit(BindingOperation)
     */
    public boolean visit(BindingOperation bindingOp) {
        if (null == mBindingOperationXmlParser) {
            mBindingOperationXmlParser = new BindingOperationXmlParser();
        }
        curXmlElement = getParserSupport().pushXmlParser(mBindingOperationXmlParser, bindingOp);
        return false;
    }
    
    /**
     * Implements XML parser for the binding &lt;operation&gt; element.
     */
    protected class BindingOperationXmlParser extends XmlParser {
        
        /** Constructs a BindingOperationXmlParser.
         */
        public BindingOperationXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                                 Attributes attributes) throws SAXException {
            if (Namespaces.isSOAPElement(SOAPOperation.TAG, uri, localName, qName)) {
                SOAPOperation soapOperation = getXmlDocument().createSOAPOperation();
                soapOperation.setQualifiedName(qName);
                soapOperation.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(soapOperation, attributes);
                ((BindingOperation) curXmlElement).addExtensibilityElementAtTail(soapOperation);
            } else if (Namespaces.isWSDLElement(BindingInput.TAG, uri, localName, qName)) {
                BindingInput bindingIn = getXmlDocument().createBindingInput();
                bindingIn.setQualifiedName(qName);
                bindingIn.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(bindingIn, attributes);
                ((BindingOperation) curXmlElement).setBindingInputAtTail(bindingIn);
                bindingIn.accept(SAXParseVisitor.this);
            } else if (Namespaces.isWSDLElement(BindingOutput.TAG, uri, localName, qName)) {
                BindingOutput bindingOut = getXmlDocument().createBindingOutput();
                bindingOut.setQualifiedName(qName);
                bindingOut.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(bindingOut, attributes);
                ((BindingOperation) curXmlElement).setBindingOutputAtTail(bindingOut);
                bindingOut.accept(SAXParseVisitor.this);
            } else if (Namespaces.isWSDLElement(BindingFault.TAG, uri, localName, qName)) {
                BindingFault bindingFault = getXmlDocument().createBindingFault();
                bindingFault.setQualifiedName(qName);
                bindingFault.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(bindingFault, attributes);
                ((BindingOperation) curXmlElement).addBindingFaultAtTail(bindingFault);
                bindingFault.accept(SAXParseVisitor.this);
            } else if (addDocumentationElement(qName, attributes, getParserSupport().getLocator())) {
                // captured
            } else if (!Namespaces.isWSDLElement(localName, uri, localName, qName)
                       && !Namespaces.isSOAPElement(localName, uri, localName, qName)
                       && !Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(uri, localName, qName, attributes);
            } else {
                handleIllegalElement(curXmlElement.getLocalName(), uri, localName,
                                     qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
            throws SAXException {
            if (Namespaces.isSOAPElement(SOAPOperation.TAG, uri, localName, qName)) {
                // do nothing
            } else if (Namespaces.isWSDLElement(BindingOperation.TAG, uri, localName, qName)) {
                curXmlElement = getParserSupport().popXmlParser();
            } else {
                LOGGER.log(Level.FINE, 
                        MESSAGES.getString("SAXParseVisitor.BINDINGOPERATIONXMLPARSER_ENDELEM") +
                        MESSAGES.getString("SAXParseVisitor.THRW_UNRECOGNIZED_END_ELEMENT", 
                        new Object[] {qName, uri} ));   
            }
        }
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPOperation)
     */
    public boolean visit(SOAPOperation sOperation) {
        return false;
    }
    
    /**
     * @see WSDLVisitor#visit(BindingInput)
     */
    public boolean visit(BindingInput bindingIn) {
        if (null == mBindingInputXmlParser) {
            mBindingInputXmlParser = new BindingInputXmlParser();
        }
        curXmlElement = getParserSupport().pushXmlParser(mBindingInputXmlParser, bindingIn);
        return false;
    }
    
    /**
     * Implements XML parser for the binding &lt;input&gt; element.
     */
    protected class BindingInputXmlParser extends XmlParser {
        
        /** Constructs a BindingInputXmlParser.
         */
        public BindingInputXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                                 Attributes attributes) throws SAXException {
            if (Namespaces.isSOAPElement(SOAPBody.TAG, uri, localName, qName)) {
                SOAPBody soapBody = getXmlDocument().createSOAPBody();
                soapBody.setQualifiedName(qName);
                soapBody.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(soapBody, attributes);
                ((BindingInput) curXmlElement).addExtensibilityElementAtTail(soapBody);
            } else if (Namespaces.isSOAPElement(SOAPHeader.TAG, uri, localName, qName)) {
                SOAPHeader soapHeader = getXmlDocument().createSOAPHeader();
                soapHeader.setQualifiedName(qName);
                soapHeader.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(soapHeader, attributes);
                ((BindingInput) curXmlElement).addExtensibilityElementAtTail(soapHeader);
                soapHeader.accept(SAXParseVisitor.this);
            } else if (addDocumentationElement(qName, attributes, getParserSupport().getLocator())) {
                // captured
            } else if (!Namespaces.isWSDLElement(localName, uri, localName, qName)
                       && !Namespaces.isSOAPElement(localName, uri, localName, qName)
                       && !Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(uri, localName, qName, attributes);
            } else {
                handleIllegalElement(curXmlElement.getLocalName(), uri, localName,
                                     qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
            throws SAXException {
            if (Namespaces.isSOAPElement(SOAPBody.TAG, uri, localName, qName)) {
                // do nothing
            } else if (Namespaces.isWSDLElement(BindingInput.TAG, uri, localName, qName)) {
                curXmlElement = getParserSupport().popXmlParser();
            } else {
                LOGGER.log(Level.FINE, 
                        MESSAGES.getString("SAXParseVisitor.BINDINGINPUTXMLPARSER_ENDELEM") +
                        MESSAGES.getString("SAXParseVisitor.THRW_UNRECOGNIZED_END_ELEMENT", 
                        new Object[] {qName, uri} ));   
            }
        }
    }
    
    /**
     * @see WSDLVisitor#visit(BindingOutput)
     */
    public boolean visit(BindingOutput bindingOut) {
        if (null == mBindingOutputXmlParser) {
            mBindingOutputXmlParser = new BindingOutputXmlParser();
        }
        curXmlElement = getParserSupport().pushXmlParser(mBindingOutputXmlParser, bindingOut);
        return false;
    }
    
    /**
     * Implements XML parser for the binding &lt;output&gt; element.
     */
    protected class BindingOutputXmlParser extends XmlParser {
        
        /** Constructs a BindingOutputXmlParser.
         */
        public BindingOutputXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                                 Attributes attributes) throws SAXException {
            if (Namespaces.isSOAPElement(SOAPBody.TAG, uri, localName, qName)) {
                SOAPBody soapBody = getXmlDocument().createSOAPBody();
                soapBody.setQualifiedName(qName);
                soapBody.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(soapBody, attributes);
                ((BindingOutput) curXmlElement).addExtensibilityElementAtTail(soapBody);
            } else if (Namespaces.isSOAPElement(SOAPHeader.TAG, uri, localName, qName)) {
                SOAPHeader soapHeader = getXmlDocument().createSOAPHeader();
                soapHeader.setQualifiedName(qName);
                soapHeader.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(soapHeader, attributes);
                ((BindingOutput) curXmlElement).addExtensibilityElementAtTail(soapHeader);
                soapHeader.accept(SAXParseVisitor.this);
            } else if (addDocumentationElement(qName, attributes, getParserSupport().getLocator())) {
                // captured
            } else if (!Namespaces.isWSDLElement(localName, uri, localName, qName)
                       && !Namespaces.isSOAPElement(localName, uri, localName, qName)
                       && !Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(uri, localName, qName, attributes);
            } else {
                handleIllegalElement(curXmlElement.getLocalName(), uri, localName,
                                     qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
            throws SAXException {
            if (Namespaces.isSOAPElement(SOAPBody.TAG, uri, localName, qName)) {
                // do nothing
            } else if (Namespaces.isWSDLElement(BindingOutput.TAG, uri, localName, qName)) {
                curXmlElement = getParserSupport().popXmlParser();
            } else {
                LOGGER.log(Level.FINE, 
                        MESSAGES.getString("SAXParseVisitor.BINDINGOUTPUTXMLPARSER_ENDELEM") +
                        MESSAGES.getString("SAXParseVisitor.THRW_UNRECOGNIZED_END_ELEMENT", 
                        new Object[] {qName, uri} ));  
            }
        }
    }
    
    /**
     * @see WSDLVisitor#visit(BindingFault)
     */
    public boolean visit(BindingFault bindingFault) {
        if (null == mBindingFaultXmlParser) {
            mBindingFaultXmlParser = new BindingFaultXmlParser();
        }
        curXmlElement = getParserSupport().pushXmlParser(mBindingFaultXmlParser, bindingFault);
        return false;
    }
    
    /**
     * Implements XML parser for the binding &lt;fault&gt; element.
     */
    protected class BindingFaultXmlParser extends XmlParser {
        
        /** Constructs a BindingFaultXmlParser.
         */
        public BindingFaultXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                                 Attributes attributes) throws SAXException {
            if (Namespaces.isSOAPElement(SOAPFault.TAG, uri, localName, qName)) {
                SOAPFault soapFault = getXmlDocument().createSOAPFault();
                soapFault.setQualifiedName(qName);
                soapFault.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(soapFault, attributes);
                ((BindingFault) curXmlElement).addExtensibilityElementAtTail(soapFault);
            } else if (addDocumentationElement(qName, attributes, getParserSupport().getLocator())) {
                // captured
            } else if (!Namespaces.isWSDLElement(localName, uri, localName, qName)
                       && !Namespaces.isSOAPElement(localName, uri, localName, qName)
                       && !Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(uri, localName, qName, attributes);
            } else {
                handleIllegalElement(curXmlElement.getLocalName(), uri, localName,
                                     qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
            throws SAXException {
            if (Namespaces.isSOAPElement(SOAPFault.TAG, uri, localName, qName)) {
                // do nothing
            } else if (Namespaces.isWSDLElement(BindingFault.TAG, uri, localName, qName)) {
                curXmlElement = getParserSupport().popXmlParser();
            } else {
                LOGGER.log(Level.FINE, 
                        MESSAGES.getString("SAXParseVisitor.BINDINGFAULTXMLPARSER_ENDELEM") +
                        MESSAGES.getString("SAXParseVisitor.THRW_UNRECOGNIZED_END_ELEMENT", 
                        new Object[] {qName, uri} )); 
            }
        }
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPBody)
     */
    public boolean visit(SOAPBody sBody) {
        return false;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPHeader)
     */
    public boolean visit(SOAPHeader sHeader) {
        if (null == mSOAPHeaderXmlParser) {
            mSOAPHeaderXmlParser = new SOAPHeaderXmlParser();
        }
        curXmlElement = getParserSupport().pushXmlParser(mSOAPHeaderXmlParser, sHeader);
        return false;
    }
    
    /**
     * Implements XML parser for the SOAP &lt;header&gt; element.
     */
    protected class SOAPHeaderXmlParser extends XmlParser {
        
        /** Constructs a SOAPHeaderXmlParser.
         */
        public SOAPHeaderXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                                 Attributes attributes) throws SAXException {
            if (Namespaces.isSOAPElement(SOAPHeaderFault.TAG, uri, localName, qName)) {
                SOAPHeaderFault soapHeaderFault = getXmlDocument().createSOAPHeaderFault();
                soapHeaderFault.setQualifiedName(qName);
                soapHeaderFault.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(soapHeaderFault, attributes);
                ((SOAPHeader) curXmlElement).addSOAPHeaderFault(soapHeaderFault);
            } else if (!Namespaces.isWSDLElement(localName, uri, localName, qName)
                       && !Namespaces.isSOAPElement(localName, uri, localName, qName)
                       && !Namespaces.isBPELElement(localName, uri, localName, qName)) {
                addExtensibilityElement(uri, localName, qName, attributes);
            } else {
                handleIllegalElement(curXmlElement.getLocalName(), uri, localName,
                                     qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
            throws SAXException {
            if (Namespaces.isSOAPElement(SOAPHeaderFault.TAG, uri, localName, qName)) {
                // do nothing
            } else if (Namespaces.isSOAPElement(SOAPHeader.TAG, uri, localName, qName)) {
                curXmlElement = getParserSupport().popXmlParser();
            } else {
                LOGGER.log(Level.FINE, 
                        MESSAGES.getString("SAXParseVisitor.SOAPHDRXMLPARSER_ENDELEM") +
                        MESSAGES.getString("SAXParseVisitor.THRW_UNRECOGNIZED_END_ELEMENT", 
                        new Object[] {qName, uri} )); 
            }
        }
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPHeaderFault)
     */
    public boolean visit(SOAPHeaderFault sHeaderFault) {
        return false;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPAddress)
     */
    public boolean visit(SOAPAddress sAddress) {
        return false;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPFault)
     */
    public boolean visit(SOAPFault sFault) {
        return false;
    }
    
    /** Add a potential documentation element.
     * @param   qName   Qualified name of potential element.
     * @param   attrs   Attributes of potential element.
     * @param   locator Locator for potential element.
     * @return  <tt>true</tt> if a documentation element was added.
     */
    protected boolean addDocumentationElement(String qName, Attributes attrs, Locator locator) {
        boolean added = false;
        if (qName.endsWith(Documentation.TAG)) {
            Documentation doc = getXmlDocument().createWSDLDocumentation();
            doc.setQualifiedName(qName);
            doc.setLocator(locator);
            SAXParserSupport.setAttributes(doc, attrs);
            curXmlElement.setDocumentationAtTail(doc);
            doc.accept(SAXParseVisitor.this);
            added = true;
        }
        return added;
    }
    
    /**
     * @see DocumentationVisitor#visit(Documentation)
     */
    public boolean visit(Documentation doc) {
        if (null == mDocumentationXmlParser) {
            mDocumentationXmlParser = new DocumentationXmlParser();
        }
        curXmlElement = getParserSupport().pushXmlParser(mDocumentationXmlParser, doc);
        return false;
    }
    
    /** Implements XML parser for documentation element.
     */
    protected class DocumentationXmlParser extends XmlParser {
        
        /** Constructs a DocumentationXmlParser.
         */
        public DocumentationXmlParser() {
            super(getParserSupport());
        }

        /** Called when character data (CDATA) is detected.
         * @param   ch      Characters encountered.
         * @param   start   Starting offset in array.
         * @param   length  Number of characters.
         * @throws  SAXException    If SAX parse problems encountered.
         */
        public void characters(char[] ch, int start, int length)
                throws SAXException {
            String val = ((Documentation) curXmlElement).getValue();
            if (null == val) {
                ((Documentation) curXmlElement).setValue(new String(ch, start, length));
            } else {
                ((Documentation) curXmlElement).setValue(val.concat(new String(ch, start, length)));
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
            throws SAXException {
            if (qName.endsWith(Documentation.TAG)) {
                curXmlElement = getParserSupport().popXmlParser();
            } else {
                LOGGER.log(Level.FINE, 
                        MESSAGES.getString("SAXParseVisitor.DOCUMENTATIONXMLPARSER_ENDELEM") +
                        MESSAGES.getString("SAXParseVisitor.THRW_UNRECOGNIZED_END_ELEMENT", 
                        new Object[] {qName, uri} )); 
            }
        }
    }
    
    /**
     * @see WSDLVisitor#visit(Types)
     */
    public boolean visit(Types types) {
        if (null == mTypesXmlParser) {
            mTypesXmlParser = new TypesXmlParser();
        }
        curXmlElement = getParserSupport().pushXmlParser(mTypesXmlParser, types);
//        mTypesXmlParser.reset();
        return false;
    }
    
    /**
     * Implements XML parser for the &lt;types&gt; element.
     */
    protected class TypesXmlParser extends XmlParser {
//        
//        /**
//         * The current schema being parsed.
//         */
//        ExtensibilityElement mCurSchema;
//
//        /**
//         * Resets the parser for the next schema.
//         */
//        public void reset() {
//            mCurSchema = null;
//        }
        
        /** Constructs a TypesXmlParser.
         */
        public TypesXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                                 Attributes attributes) throws SAXException {
        	SAXParseVisitorService vService = (SAXParseVisitorService) getVisitorService();
        	//should we parse inline schema
        	if(!vService.getWSDLParseContext().isParseInlineSchema()) {
        		return;
        	}
        	
        	ExtensibilityElement exten = getXmlDocument().createExtensibilityElement();
            exten.setElementType(NamespaceUtility.getQNameFromURIAndString(uri, qName));
            exten.setLocator(getParserSupport().getLocator());
            SAXParserSupport.setAttributes(exten, attributes);
            ((Types) curXmlElement).addExtensibilityElementAtTail(exten);
            exten.accept(SAXParseVisitor.this);
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
            throws SAXException {
        	if (Namespaces.isWSDLElement(Types.TAG, uri, localName, qName)) {
                processSchemas((Types) curXmlElement);
                curXmlElement = getParserSupport().popXmlParser();
            } else {
                // do nothing
            }
        }
        
        /**
         * Processes all children of the &lt;types&gt; element.
         * For each element representing an XML Schema, parse it into Castor's
         * XML Schema object model and add to the list of XML Schemas in the
         * types element.
         * @param types the types element
         * @throws SAXException errors parsing the XML Schemas
         */
        void processSchemas(Types types) throws SAXException {
        	SAXParseVisitorService vService = (SAXParseVisitorService) getVisitorService();
        	//should we parse inline schema
        	if(!vService.getWSDLParseContext().isParseInlineSchema()) {
        		return;
        	}
        	
        	BaseURIResolver resolver = null;
            if (getBaseURIResolver() != null) {
                resolver = getBaseURIResolver();
            } else {
                throw new EInsightModelException(mMsg.getString(MUST_SPECIFY_URI_RESOLVER_FACTORY));
            }

            
            int count = types.getExtensibilityElementsSize();
            Map map = new HashMap();
            
            for (int i = 0; i < count; i++) {
                ExtensibilityElement elem = types.getExtensibilityElement(i);
                if (elem.getLocalName().equals("schema")) {
                    Map attrs = elem.getOtherAttributes();
                    String targetNamespace = null;
                    if (attrs != null) {
                        targetNamespace = (String) attrs.get("targetNamespace");
                    }
                    if (null == targetNamespace) {
                        targetNamespace = elem.getOwnerDocument().getTargetNamespace();
                    }
                    if (null == targetNamespace) {
                        targetNamespace = "";
                    }
                    if (map.containsKey(targetNamespace)) {
                        mergSchema ((ExtensibilityElement)map.get(targetNamespace), elem);
                    } else {
                        map.put(targetNamespace, elem);
                    }
                }
            }
            
            Collection schemas = map.values();
            resolver.setMap(map);
            
            for (Iterator iter = schemas.iterator(); iter.hasNext();) {
                ExtensibilityElement elem = (ExtensibilityElement) iter.next();
                ClassLoader clsLdr = null;
                clsLdr = SAXParseVisitor.this.getClass().getClassLoader();
                boolean validateSchema = vService.getWSDLParseContext().isValidateSchema();
                XMLSchema xmlSchema = getInstanceOfCastorSupport(clsLdr).parseSchema(elem, resolver, validateSchema);
                xmlSchema.setBaseURI(types.getOwnerDocument().getBaseURI());
                if (xmlSchema != null) {
                    types.addSchema(xmlSchema);
                }
            }
            
            if (resolver != null) {

            }
        }

        private void mergSchema(ExtensibilityElement element, ExtensibilityElement elem) {
            // TODO Auto-generated method stub
            List children = elem.getChildren();
            for (int i= 0; i<children.size(); i++) {
                element.addChildAtTail((XMLNode)children.get(i));
            }            
        }
    }
    
    /**
     * @see WSDLVisitor#visit(ExtensibilityElement)
     */
    public boolean visit(ExtensibilityElement ext) {
        if (null == mExtensibilityElementXmlParser) {
            mExtensibilityElementXmlParser = new ExtensibilityElementXmlParser();
        }
        curXmlElement = getParserSupport().pushXmlParser(mExtensibilityElementXmlParser, ext);
        return false;
    }
    
    /**
     * Convenience method to provide the <code>CastorSupport</code> object since eWay installation SARs may carry
     * their own old version of com.stc.einsightintegrationapi.jar which used to contain the CastorSupport abstract
     * class (it's now in com.stc.einsightintegrationprivapi.jar), we cannot access the fixed version of CastorSupport.
     *
     * @param   clsLdr      ClassLoader to use.
     * @return  A CastorSupport object.
     */
    private CastorSupport getInstanceOfCastorSupport(ClassLoader clsLdr) {
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
    
    /**
     * Implements XML parser for the &lt;types&gt; element.
     */
    protected class ExtensibilityElementXmlParser extends XmlParser {
        
        /** Constructs a ExtensibilityElementXmlParser.
         */
        public ExtensibilityElementXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                                 Attributes attributes) throws SAXException {
        	if (qName.endsWith(Documentation.TAG)) {
        		addDocumentationElement(qName, attributes, getParserSupport().getLocator());
        	} else {
	            ExtensibilityElement exten = getXmlDocument().createExtensibilityElement();
	            exten.setElementType(NamespaceUtility.getQNameFromURIAndString(uri, qName));
	            exten.setLocator(getParserSupport().getLocator());
	            SAXParserSupport.setAttributes(exten, attributes);
	            ((ExtensibilityElement) curXmlElement).addExtensibilityElementAtTail(exten);
	            exten.accept(SAXParseVisitor.this);
        	}
        }
        
        /** Called when character data (CDATA) is detected.
         * @param   ch      Characters encountered.
         * @param   start   Starting offset in array.
         * @param   length  Number of characters.
         * @throws  SAXException    If SAX parse problems encountered.
         */
        public void characters(char[] ch, int start, int length)
                throws SAXException {
            String val = ((ExtensibilityElement) curXmlElement).getValue();
            if (null == val) {
                ((ExtensibilityElement) curXmlElement).setValue(new String(ch, start, length));
            } else {
                ((ExtensibilityElement) curXmlElement).setValue(val.concat(new String(ch, start, length)));
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
            throws SAXException {
            curXmlElement = getParserSupport().popXmlParser();
        }
    }
    
    /** Visits a property element.
     * @param   p   a property element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Property p) {
        return false;
    }
    
    /** Visits a propertyAlias element.
     * @param   p   a propertyAlias element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(PropertyAlias p) {
    	if (null == mPropertyAliasXmlParser) {
    		mPropertyAliasXmlParser = new PropertyAliasXmlParser();
        }
        curXmlElement = getParserSupport().pushXmlParser(mPropertyAliasXmlParser, p);
        return false;
    }
    
    /** Implements XML parser for PropertyAlias element.
     */
    protected class PropertyAliasXmlParser extends XmlParser {
        
        /** Constructs a PropertyAlias.
         */
        public PropertyAliasXmlParser() {
            super(getParserSupport());
        }
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                                 Attributes attributes) throws SAXException {
            if (Namespaces.isBPELElement(Query.TAG, uri, localName, qName)) {
            	Query tQuery = getXmlDocument().createBPWSQuery();
                ((PropertyAlias) curXmlElement).setQueryObject(tQuery);
                tQuery.setQualifiedName(qName);
                tQuery.setLocator(getParserSupport().getLocator());
                SAXParserSupport.setAttributes(tQuery, attributes);
                tQuery.accept(SAXParseVisitor.this);
            } else {
                handleIllegalElement(curXmlElement.getLocalName(), uri, localName,
                                     qName, attributes);
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
            throws SAXException {
            if (qName.endsWith(PropertyAlias.TAG)) {
                curXmlElement = getParserSupport().popXmlParser();
            } else {
                LOGGER.log(Level.FINE, 
                        MESSAGES.getString("SAXParseVisitor.PROPALIASXMLPARSER_ENDELEM") +
                        MESSAGES.getString("SAXParseVisitor.THRW_UNRECOGNIZED_END_ELEMENT", 
                        new Object[] {qName, uri} )); 
            }
        }
    }

    /** Visits a Query element.
     * @param   q   a Query element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Query q) {
    	if (null == mQueryXmlParser) {
    		mQueryXmlParser = new QueryXmlParser();
        }
        curXmlElement = getParserSupport().pushXmlParser(mQueryXmlParser, q);
        return false;
    }
   
    
    /** Implements XML parser for Query element.
     */
    protected class QueryXmlParser extends XmlParser {
        
        /** Constructs a QueryXmlParser.
         */
        public QueryXmlParser() {
            super(getParserSupport());
        }

        /** Called when character data (CDATA) is detected.
         * @param   ch      Characters encountered.
         * @param   start   Starting offset in array.
         * @param   length  Number of characters.
         * @throws  SAXException    If SAX parse problems encountered.
         */
        public void characters(char[] ch, int start, int length)
                throws SAXException {
            String val = ((Query) curXmlElement).getValue();
            if (null == val) {
                ((Query) curXmlElement).setValue(new String(ch, start, length));
            } else {
                ((Query) curXmlElement).setValue(val.concat(new String(ch, start, length)));
            }
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
            throws SAXException {
            if (qName.endsWith(Query.TAG)) {
                String val = ((Query) curXmlElement).getValue();
                if (val != null) {
                    ((Query) curXmlElement).setValue(val.trim());
                }
                curXmlElement = getParserSupport().popXmlParser();
            } else {
                LOGGER.log(Level.FINE, 
                        MESSAGES.getString("SAXParseVisitor.QUERYXMLPARSER_ENDELEM") +
                        MESSAGES.getString("SAXParseVisitor.THRW_UNRECOGNIZED_END_ELEMENT", 
                        new Object[] {qName, uri} ));
            }
        }
    }
    

}
