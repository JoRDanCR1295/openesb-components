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
 * @(#)JCODOMTransformer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc;

import com.sap.mw.jco.JCO;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.sapbc.util.ApproximateName;
import com.sun.jbi.sapbc.util.SAPWSDLUtilities;
import com.sun.wsdl.model.WSDLDefinitions;
import com.sun.wsdl.model.WSDLMessage;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.exolab.castor.xml.schema.ComplexType;
import org.exolab.castor.xml.schema.ElementDecl;
import org.exolab.castor.xml.schema.Group;
import org.exolab.castor.xml.schema.Particle;
import org.exolab.castor.xml.schema.Structure;
import org.exolab.castor.xml.schema.XMLType;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Generates XML data from the response data in a JCO Function object.
 *
 * @author Noel Ang (noel.ang@sun.com)
 */
public class JCODOMTransformer {
    
    public static final String SAP_NAMESPACE_PREFIX = "sapns:";
    public static final String XML_NAMESPACE_PREFIX = "xmlns:";
    public static final String XML_ATTRIBUTE_TYPE = "type";
    public static final String XML_ATTRIBUTE_NAME = "name";
    
    // NOTE: The current implementation ignores namespaces correctly.
    // A rework that uses the castor lib will probably be necessary to
    // be able to accomplish namespace calculations.  The current implementation
    // uses the org.w3c.dom API, which presents the XSD elements as general
    // document elements.
    
    public JCODOMTransformer(WSDLDefinitions def) throws ParserConfigurationException {
        mDefinition = def;
        mDocBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
        mTargetNamespace = def.getTargetNamespace();
        mNamespaces.putAll(def.getNamespaces());
        
        // Collect simpleType, complexType and element definitions
        // for look-up purposes later.
        //readXsdSchemas(def);
    }
    
    public Document transform(WSDLMessage msgdef, JCO.Response response) {
        String msgName = msgdef.getName();
        /**
         *  For SAP there is only one part per message type
         */
        ElementDecl defElem = msgdef.getPart(0).getXSDElement();
        String defElementName = defElem.getName();
        
        // Compose the document
        Document document = mDocBuilder.newDocument();
        document.setDocumentURI(mTargetNamespace);
        
        /*
        for (Element defElem: elemDefs) {
            //QName cmnElemName = defElem.get;
            String tagName = getAttributeValue(defElem, XML_ATTRIBUTE_NAME);
            if (tagName != null && !"".equals(tagName)) {
                mLogger.fine("Creating element ["+tagName+"] with namespace ["+mTargetNamespace+"]");
                Element docElem = document.createElementNS(mTargetNamespace,SAP_NAMESPACE_PREFIX+tagName);
                docElem.setAttribute(XML_NAMESPACE_PREFIX + SAP_NAMESPACE_PREFIX, mTargetNamespace);
                buildElement(docElem, defElem, document, response);
                document.appendChild(docElem);
            }
        }
         */
        
        Element docElem = document.createElementNS(mTargetNamespace,SAP_NAMESPACE_PREFIX + defElementName);
        docElem.setAttribute(XML_NAMESPACE_PREFIX + SAP_NAMESPACE_PREFIX, mTargetNamespace);
        buildElement(docElem, defElem, document, response);
        document.appendChild(docElem);
        
        SAPWSDLUtilities.doctostring(document);
        
        return document;
    }
/*
                DOC XML CONTENT
                <?xml version="1.0" encoding="ISO-8859-1" standalone="yes"?>
                <FlightGetDetailResponse>
                    <AdditionalInfo>
                        <Flighttime>30240</Flighttime>
                        <Distance>7865.0000</Distance>
                        <Unit>KM</Unit>
                        <Unitiso>KMT</Unitiso>
                        <Planetype>A319</Planetype>
                        <Flighttype/>
                    </AdditionalInfo>
                    <Availibility>
                        <Economax>350</Economax>
                        <Econofree>347</Econofree>
                        <Businmax>0</Businmax>
                        <Businfree>0</Businfree>
                        <Firstmax>0</Firstmax>
                        <Firstfree>0</Firstfree>
                    </Availibility>
                    <ExtensionOut/>
                    <FlightData>
                        <Airlineid>LH</Airlineid>
                        <Airline>Lufthansa</Airline>
                        <Connectid>0400</Connectid>
                        <Flightdate>1995-02-28</Flightdate>
                        <Airportfr>FRA</Airportfr>
                        <Cityfrom>FRANKFURT</Cityfrom>
                        <Airportto>JFK</Airportto>
                        <Cityto>NEW YORK</Cityto>
                        <Deptime>10:10:00</Deptime>
                        <Arrtime>11:34:00</Arrtime>
                        <Arrdate>1995-02-28</Arrdate>
                        <Price>899.0000</Price>
                        <Curr>DEM</Curr>
                        <Curriso>DEM</Curriso>
                    </FlightData>
                    <Return>
                        <item>
                            <Type>S</Type>
                            <Id>BC_IBF</Id>
                            <Number>000</Number>
                            <Message>Method was executed successfully</Message>
                            <Logno/>
                            <Logmsgno>000000</Logmsgno>
                            <Messagev1/>
                            <Messagev2/>
                            <Messagev3/>
                            <Messagev4/>
                            <Parameter/>
                            <Row>0</Row>
                            <Field/>
                            <System>LSYS800</System>
                        </item>
                    </Return>
                </FlightGetDetailResponse>
 */
    private final void readXsdSchemas(WSDLDefinitions def) {
        mElements.clear();
        mSimpleTypes.clear();
        mComplexTypes.clear();
        
        /*
        Types types = def.getTypes();
        List<ExtensibilityElement> elements = types.getExtensibilityElements();
        for (ExtensibilityElement element: elements) {
            QName typeName = element.getElementType();
            if (typeName != null) {
                if (XMLConstants.W3C_XML_SCHEMA_NS_URI.equals(typeName.getNamespaceURI())) {
                    Element elem = ((UnknownExtensibilityElement) element).getElement();
                    readXsdElement(elem);
                }
            }
        }
         **/
    }
    
    private final void readXsdElement(Element elem) {
        NodeList children = elem.getChildNodes();
        
        // simpleType, complexType, and element definitions are stored in
        // seperate maps
        for (int i = 0; i < children.getLength(); ++i) {
            Node child = children.item(i);
            String name = child.getLocalName();
            if (name != null && !"".equals(name)) {
                if ("simpleType".equals(name)) {
                    addSimpleType(child);
                } else if ("complexType".equals(name)) {
                    addComplexType(child);
                } else if ("element".equals(name)) {
                    addElement(child);
                }
            }
        }
    }
    
    // Returns the value of a document node's attribute.
    private String getAttributeValue(Node node, String attributeName) {
        String value = null;
        NamedNodeMap attrMap = node.getAttributes();
        if (attrMap != null) {
            node = attrMap.getNamedItem(attributeName);
            if (node != null) {
                value = node.getNodeValue();
            }
        }
        return stripNS(value);
    }
    
    private void addSimpleType(Node node) {
        // I don't care about the details of simple types, because
        // this map will simply be used to identify whether or not a given
        // type is a simple type.
        String name = getAttributeValue(node, XML_ATTRIBUTE_NAME);
        mSimpleTypes.add(name);
    }
    
    private void addComplexType(Node node) {
        // The actual structures of complexTypes are stored for traversal later.
        String name = getAttributeValue(node, XML_ATTRIBUTE_NAME);
        mComplexTypes.put(name, node);
    }
    
    private void addElement(Node node) {
        // The actual structure of elements are stored for traversal later.
        String name = getAttributeValue(node, XML_ATTRIBUTE_NAME);
        mElements.put(name, node);
    }
    
    // Render an element structure into the document.
    private void buildElement(Element docElem, ElementDecl defElem, Document doc, JCO.Response response) {
        XMLType elemType = defElem.getType();
        if(elemType.getStructureType() == Structure.COMPLEX_TYPE) {
            ComplexType ct = (ComplexType) elemType;
            buildFromComplexType(docElem, ct, doc, response);
            
        }
        
        
        /*
        Node node = defElem.getFirstChild();
        while (node != null) {
            String nodeName = node.getLocalName();
            if ("complexType".equals(nodeName)) {
                buildFromComplexType(docElem, node, doc, response);
            }
            node = node.getNextSibling();
        }
         */
    }
    
    // Render a complexType structure into the document.
    // docElem:  start point in the document to populate.
    // defNode:  the root node of the complexType structure that describes the document's format.
    // response: the JCO.Record supplies the document's data.
    // doc:      the entire document; acts as element factory.
    private void buildFromComplexType(Element docElem, ComplexType complexType, Document doc, JCO.Record response) {
        Enumeration particleEnum = complexType.enumerate();
        
        while (particleEnum.hasMoreElements()) {
            Particle particle = (Particle) particleEnum.nextElement();
            if (particle.getStructureType() == Structure.GROUP){
                // sequence element
                Group group = (Group) particle;
                buildFromSequence(docElem, group, doc, response);
            } else if (particle.getStructureType() == Structure.ELEMENT) {
                // TODO: Decide what to do here
                
            } else if (particle.getStructureType() == Structure.WILDCARD) {
                // TODO: Decide what to do here
                
            }
        }
        /*
        Node node = defNode.getFirstChild();
        while (node != null) {
            String nodeName = (node != null ? node.getLocalName() : null);
            if ("sequence".equals(nodeName)) {
                buildFromSequence(docElem, node, doc, response);
            }
            node = node.getNextSibling();
        }
         */
    }
    
    // Render a sequence structure into the document.
    // docElem:  start point in the document to populate.
    // defNode:  the root node of the complexType structure that describes the document's format.
    // response: the JCO.Record supplies the document's data.
    // doc:      the entire document; acts as element factory.
    private void buildFromSequence(Element docElem, Group group, Document doc, JCO.Record response) {
        Enumeration groupEnum = group.enumerate();
        
        while (groupEnum.hasMoreElements()) {
            Particle groupParticle = (Particle) groupEnum.nextElement();
            
            if (groupParticle.getStructureType() == Structure.ELEMENT) {
                ElementDecl elem = (ElementDecl) groupParticle;
                String tagName = elem.getName();
                XMLType type = elem.getType();
                
                if (tagName != null) {
                    ApproximateName name = new ApproximateName(tagName);
                    
                    // element is a simpleType.
                    // Generate: <elementName>textdata</elementName>
                    if (type.isSimpleType()) {
                        String value = getField(response, name);
                        boolean exclude = (value == null || "".equals(value)) && elem.isNillable();
                        if (!exclude) {
                            value = (value == null ? "" : value);
                            Element newElem = doc.createElementNS(mTargetNamespace,SAP_NAMESPACE_PREFIX+tagName);
                            newElem.appendChild(doc.createTextNode(value));
                            docElem.appendChild(newElem);
                        }
                    } //End simple type
                    
                    // element is a complexType.
                    // Generate: <elementName></elementName>,
                    //           populate it with the complexType's elements...
                    else {
                        ComplexType complexType = (ComplexType) type;
                        String typeName = type.getName();
                        if (typeName != null) {
                            Enumeration complexTypeEnum = complexType.enumerate();
                            
                            while (complexTypeEnum.hasMoreElements()) {
                                Particle complexTypeParticle = (Particle) complexTypeEnum.nextElement();
                                if (complexTypeParticle.getStructureType() == Structure.GROUP){
                                    // This is a sequence
                                    Group complexTypeGroup = (Group) complexTypeParticle;
                                    
                                    Element newElem = doc.createElementNS(mTargetNamespace,SAP_NAMESPACE_PREFIX+tagName);
                                    
                                    // complexType describing a table
                                    // prepare for a repeating group of elements...
                                    if (isTableComplexType(complexType.getName())) {
                                        JCO.Table table = findTable(response, name);
                                        buildFromTableSequence(newElem, complexTypeGroup, doc, table);
                                    }
                                    
                                    // non-table complexType
                                    // non-repeating group of elements...
                                    else {
                                        JCO.Structure struct = findStructure(response, name);
                                        buildFromComplexType(newElem, complexType, doc, struct);
                                    }
                                    docElem.appendChild(newElem);
                                }
                            }
                        }
                    } // End of complex type
                }
            }// End of Element
            else if (groupParticle.getStructureType() == Structure.GROUP) {
                //Should not occur
            }// End of Group
            else if (groupParticle.getStructureType() == Structure.WILDCARD) {
                //Should not occur
            }// End of Wildcard
        }
        /*
        Node node = defNode.getFirstChild();
        do {
            String nodeName = node.getLocalName();
         
            if ("element".equals(nodeName)) {
                String tagName = getAttributeValue(node, XML_ATTRIBUTE_NAME);
         
                if (tagName != null) {
                    ApproximateName name = new ApproximateName(tagName);
         
                    // element is a simpleType.
                    // Generate: <elementName>textdata</elementName>
                    if (isSimpleType(node)) {
                        String value = getField(response, name);
                        boolean exclude = (value == null || "".equals(value)) && isOptional(node);
                        if (!exclude) {
                            value = (value == null ? "" : value);
                            Element newElem = doc.createElementNS(mTargetNamespace,SAP_NAMESPACE_PREFIX+tagName);
                            newElem.appendChild(doc.createTextNode(value));
                            docElem.appendChild(newElem);
                        }
                    }
         
                    // element is a complexType.
                    // Generate: <elementName></elementName>,
                    //           populate it with the complexType's elements...
                    else {
                        String typeName = getAttributeValue(node, XML_ATTRIBUTE_TYPE);
                        if (typeName != null) {
                            Node complexNode = findComplexType(typeName);
                            if (complexNode !=  null) {
                                Element newElem = doc.createElementNS(mTargetNamespace,SAP_NAMESPACE_PREFIX+tagName);
         
                                // complexType describing a table
                                // prepare for a repeating group of elements...
                                if (isTableComplexType(node)) {
                                    JCO.Table table = findTable(response, name);
                                    buildFromTableSequence(newElem, complexNode, doc, table);
                                }
         
                                // non-table complexType
                                // non-repeating group of elements...
                                else {
                                    JCO.Structure struct = findStructure(response, name);
                                    buildFromComplexType(newElem, complexNode, doc, struct);
                                }
                                docElem.appendChild(newElem);
                            }
                        }
                    }
                }
            }
         
            node = node.getNextSibling();
         
        } while (node != null);
         */
    }
    
    // Render a repeating sequence structure into the document.
    // docElem: start point in the document to populate.
    // defNode: the root node of the complexType structure that describes the document's format.
    // table:   the JCO.Record supplies the document's data.
    // doc:     the entire document; acts as element factory.
    private void buildFromTableSequence(Element docElem, Group group, Document doc, JCO.Table table) {
        // <complexType>
        Enumeration groupEnum = group.enumerate();
        
        while (groupEnum.hasMoreElements()) {
            Particle groupParticle = (Particle) groupEnum.nextElement();
            // <element>
            if (groupParticle.getStructureType() == Structure.ELEMENT) {
                ElementDecl elem = (ElementDecl) groupParticle;
                String tagName = elem.getName();
                XMLType type = elem.getType();
                String typeName = type.getName();
                if (typeName != null && type.isComplexType()) {
                    ComplexType elementComplexNode = (ComplexType) type;
                    
                    // Only generate something if the table isn't empty.
                    if (!table.isEmpty()) {
                        int numRows = table.getNumRows();
                        table.firstRow();
                        do {
                            Element newElem = doc.createElementNS(mTargetNamespace,SAP_NAMESPACE_PREFIX+tagName);
                            
                            // Each row might be a complexType in itself
                            buildFromComplexType(newElem, elementComplexNode, doc, table);
                            docElem.appendChild(newElem);
                        } while (table.nextRow());
                    }
                }
            }
        }
    }
    
    
    private Node findComplexType(String name) {
        return mComplexTypes.get(name);
    }
    
    private String stripNS(String name) {
        if (name != null) {
            int pos = name.indexOf(":");
            if (pos != -1) {
                name = name.substring(pos + 1);
            }
        }
        return name;
    }
    
    private boolean isSimpleType(Node node) {
        String type = getAttributeValue(node, XML_ATTRIBUTE_TYPE);
        return mSimpleTypes.contains(type) || !mComplexTypes.containsKey(type);
    }
    
    private boolean isTableComplexType(String typeName) {
        // This assumption works only with SAP-generated WSDL,
        // but that's an understood constraint for this implementation.
        // SAP renders tables in WSDL as having a type name prefixed with "TableOf".
        return typeName.startsWith("TableOf");
    }
    
    private boolean isOptional(Node node) {
        String occurs = getAttributeValue(node, "minOccurs");
        
        if (occurs == null || "".equals(occurs)) {
            return false;
        }
        try {
            return Integer.parseInt(occurs) == 0;
        } catch (NumberFormatException e) {
            return false;
        }
    }
    
    private String getField(JCO.Record record, ApproximateName name) {
        boolean success = false;
        boolean foundStructureOrTable = false;
        String value = null;
        JCO.FieldIterator iter = record.fields();
        
        while (!success && iter.hasMoreFields()) {
            JCO.Field field = iter.nextField();
            String fieldName = field.getName();
            if (name.similar(fieldName)) {
                if (!field.isStructure() && !field.isTable()) {
                    value = field.getString();
                    success = true;
                } else {
                    foundStructureOrTable = true;
                }
            }
        }
        
        if (!success) {
            String msg = (foundStructureOrTable
                    ? mMessages.getString("JCODOMransformer.Unresolved_field_name_got_incompat_obj", name.toString())
                    : mMessages.getString("JCODOMTransformer.Unresolved_field_name", name.toString()));
            mLogger.log(Level.SEVERE, msg);
            throw new RuntimeException(msg);
        }
        
        return value;
    }
    
    private JCO.Table findTable(JCO.Record record, ApproximateName name) {
        boolean foundElementOrStructure = false;
        JCO.FieldIterator iter = record.fields();
        JCO.Table table = null;
        
        while (table == null && iter.hasMoreFields()) {
            JCO.Field field = iter.nextField();
            String fieldName = field.getName();
            if (name.similar(fieldName)) {
                if (field.isTable()) {
                    table = record.getTable(fieldName);
                } else {
                    foundElementOrStructure = true;
                }
            }
        }
        
        if (table == null) {
            String msg = (foundElementOrStructure
                    ? mMessages.getString("JCODOMTransformer.Unresolved_table_got_incompat_obj", name.toString())
                    : mMessages.getString("JCODOMTransformer.Unresolved_table", name.toString()));
            mLogger.log(Level.SEVERE, msg);
            throw new RuntimeException(msg);
        }
        
        return table;
    }
    
    private JCO.Structure findStructure(JCO.Record record, ApproximateName name) {
        boolean foundElementOrTable = false;
        JCO.FieldIterator iter = record.fields();
        JCO.Structure structure = null;
        
        while (structure == null && iter.hasMoreFields()) {
            JCO.Field field = iter.nextField();
            String fieldName = field.getName();
            if (name.similar(fieldName)) {
                if (field.isStructure()) {
                    structure = record.getStructure(fieldName);
                } else {
                    foundElementOrTable = true;
                }
            }
        }
        
        if (structure == null) {
            String msg = (foundElementOrTable
                    ? mMessages.getString("JCODOMTransformer.Unresolved_struct_got_incompat_obj", name.toString())
                    : mMessages.getString("JCODOMTransformer.Unresolved_struct", name.toString()));
            mLogger.log(Level.SEVERE, msg);
            throw new RuntimeException(msg);
        }
        
        return structure;
    }
    
    private final Logger mLogger = mMessages.getLogger(getClass());
    private final DocumentBuilder mDocBuilder;
    private final String mTargetNamespace;
    private final WSDLDefinitions mDefinition;
    private final Map<String, String> mNamespaces = new HashMap<String, String>();
    private final List<String> mSimpleTypes = new ArrayList<String>();
    private final Map<String, Node> mComplexTypes = new HashMap<String, Node>();
    private final Map<String, Node> mElements = new HashMap<String, Node>();
    
    private static final Messages mMessages = Messages.getMessages(JCODOMTransformer.class);
    
}
