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
 * @(#)CastorSupportImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.xsd.impl;


import com.sun.wsdl.model.NamespaceUtility;
import com.sun.wsdl.model.Types;
import com.sun.wsdl.model.WSDLDefinitions;
import com.sun.wsdl.model.WSDLDocument;
import com.sun.wsdl.model.common.CascadingSAXException;
import com.sun.wsdl.model.common.MessageManager;
//import com.sun.wsdl.model.common.model.QName;
import com.sun.wsdl.model.common.model.XMLComment;
import com.sun.wsdl.model.common.model.XMLElement;
import com.sun.wsdl.model.common.model.XMLNode;
import com.sun.wsdl.model.common.model.impl.XMLCommentImpl;
import com.sun.wsdl.model.common.util.Utility;
import com.sun.wsdl.model.common.visitor.SAXParserSupport;
import com.sun.wsdl.model.common.visitor.VisitorService;
import com.sun.wsdl.model.common.visitor.XMLParseVisitorException;
import com.sun.wsdl.model.extensions.ExtensibilityElement;
import com.sun.wsdl.model.uri.BaseURIResolver;
import com.sun.wsdl.model.uri.impl.BaseURIResolverImpl;
import com.sun.wsdl.model.visitor.SAXWriteVisitorService;
import com.sun.wsdl.model.visitor.WSDLVisitor;
import com.sun.wsdl.model.xsd.CastorSupport;
import com.sun.wsdl.model.xsd.XMLSchema;
import com.sun.jbi.internationalization.Messages;

import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.namespace.QName;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import org.exolab.castor.xml.schema.AttributeDecl;
import org.exolab.castor.xml.schema.AttributeGroup;
import org.exolab.castor.xml.schema.AttributeGroupReference;
import org.exolab.castor.xml.schema.ComplexType;
import org.exolab.castor.xml.schema.ElementDecl;
import org.exolab.castor.xml.schema.Facet;
import org.exolab.castor.xml.schema.Group;
import org.exolab.castor.xml.schema.Schema;
import org.exolab.castor.xml.schema.SimpleType;
import org.exolab.castor.xml.schema.Wildcard;
import org.exolab.castor.xml.schema.XMLType;
import org.exolab.castor.xml.schema.reader.SchemaReader;
import org.exolab.castor.xml.schema.writer.SchemaWriter;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.ext.LexicalHandler;
import org.xml.sax.helpers.DefaultHandler;



/**
 * Support for Castor XML Schema object model.
 *
 * @author Sun Microsystems
 * @version 
 */
public class CastorSupportImpl extends CastorSupport {
    
    private static final Messages MESSAGES = Messages.getMessages(CastorSupportImpl.class);
    private static final Logger LOGGER = Messages.getLogger(CastorSupportImpl.class);
    
    /** Holds the xsd:import QName */
    private static final QName XSD_IMPORT_QNAME = 
    	NamespaceUtility.getQName(XMLSchema.URI, MESSAGES.getString("CastorSupportImpl.IMPORT"));
  
    /** the following unsupportedNode is meant for the xsd any support. At this point we 
     donot know how to handle xsd:any and hence we resolved to this failsafe mechanism.
     */
//    private static final String ANY_TOOL_TIP = 
//            MessageManager.getManager(CastorSupportImpl.class).getString("ANY_TOOL_TIP");
    
    /** Holds the WSDL Document being parsed */
    protected WSDLDocument mWsdlDoc = null;
    
    /** Holds the XML Document parser */
    protected ContentHandler mXmlParser = null;
    
    /** Holds the ExtensibilityElement stack */
    protected Stack mExtensibilityElementStack = null;
    
    /** Holds the root ExtensibilityElement representing a Schema */
    protected ExtensibilityElement mRootExten = null;
    
    /** Holds the XML Lexer */
    protected LexicalHandler mXmlLexicalHandler = null;
  
    /** @see CastorSupport#parseSchema(ExtensibilityElement, Map)
     */
    public XMLSchema parseSchema(ExtensibilityElement elem, Map map)
            throws SAXException {
        return parseSchema(elem, new BaseURIResolverImpl(map));
    }
    
    /**
     * Parses an extensible element into an XMLSchema.
     * @param elem  the extensible element
     * @param res   URI resolver for imports.
     * @return the XMLSchema object instance or null if there are errors
     * @throws SAXException parsing errors
     */
    public XMLSchema parseSchema(ExtensibilityElement elem, BaseURIResolver resolver, boolean validateSchema)
            throws SAXException {
    	mExtensibilityElementStack = null;
        mRootExten = null;
        
        if ((elem.getOwnerDocument() != null) && (elem.getOwnerDocument() instanceof WSDLDocument)) {
            mWsdlDoc = (WSDLDocument) elem.getOwnerDocument();
        } else {
            throw new SAXException(
                    MESSAGES.getString("CastorSupportImpl.PARSE_SCHEMA_CAN_ONLY_BE_PARSED_FOR_INLINE_SCHEMA"));
        }
        
        try {
            Writer writer = new StringWriter();
            serializeElement(elem, writer);
            String schemaStr = writer.toString();
            
            Reader reader = new StringReader(schemaStr);
            InputSource inputSource = new InputSource(reader);
            inputSource.setSystemId(mWsdlDoc.getBaseURI());
            SchemaReader schemaReader = new SchemaReader(inputSource);
            schemaReader.setURIResolver(resolver);
            schemaReader.addPropertyChangeListener(resolver);
            schemaReader.setValidation(validateSchema);
            
            Schema schema = schemaReader.read();
            
            if (null == schema.getTargetNamespace()) {
                XMLElement top = elem.getOwnerDocument().getDocumentElement();
                String targetNS = "";
                if (top instanceof WSDLDefinitions) {
                    targetNS = ((WSDLDefinitions) top).getTargetNamespace();
                }
                schema.setTargetNamespace(targetNS);
            }
            
            convertSchemaToExtensibilityElement(elem, schema);
            XMLSchema xmlSchema = new XMLSchemaImpl();
            xmlSchema.setSchema(schema);
            return xmlSchema;
        } catch (Exception e) {
            String errMsg = e.getMessage();
            String msg = MESSAGES.getString("CastorSupportImpl.CANNOT_PARSE_INLINE_SCHEMA");
            if ((errMsg != null) && (errMsg.length() > 0)) {
                msg += 
                  MESSAGES.getString( "CastorSupportImpl.SEE_BELOW_x_____x",
                        new Object[]{ System.getProperty( MESSAGES.getString("CastorSupportImpl.LINE_SEPARATOR")),
                                        errMsg });   
            }
            throw new CascadingSAXException(msg, e);
        }
    }
    
    /** @see CastorSupport#parseSchema(ExtensibilityElement, BaseURIResolver)
     */
    public XMLSchema parseSchema(ExtensibilityElement elem, BaseURIResolver resolver)
            throws SAXException {
        return parseSchema(elem, resolver, true);
    }
    
    
    /** @see CastorSupport#serializeElement
     */
    public void serializeElement(ExtensibilityElement elem, Writer writer) {
        VisitorService visitorService = new SAXWriteVisitorService();
        Object[] params = new Object[] {writer, Boolean.TRUE, Boolean.FALSE};
        WSDLVisitor visitor = (WSDLVisitor) visitorService.fetch(WSDLVisitor.class, null);
        visitor.prepare(params);
        Map prevNamespaces = elem.getNamespaces();
        Map namesps = elem.getTotalNamespaces();
        /* CR6502230 WSDL parsing error in the BPEL SE causes deployment error
         * Internal error in depending upon a fixed namespace for extensibility
         * elements(ex: partnerlinktype) was leading to this map having prefixes
         * with empty namespaceuri, which leads to Schema validation to fail and 
         * hence the deployment.
         * Future plans to move towards WSDL4J, would make fixing the original
         * problem now redundant and expensive as this WSDL model would change.
         * 
         */
        Iterator iter = namesps.keySet().iterator();
        Map newmap = new HashMap();
        while (iter.hasNext()) {
        	Object key = iter.next();
        	Object value = namesps.get(key);
        	if (value != null && value instanceof String) {
        		String namespace = (String) value;
        		if (namespace.length() > 0) {
        			newmap.put(key, value);
        		}
        	}
        	
        }
        elem.setNamespaces(newmap);
        
        elem.accept(visitor);
        
        elem.setNamespaces(prevNamespaces);
    }
    
    /** @see CastorSupport#getSimpleTypes
     */
    public Collection getSimpleTypes(XMLSchema xmlSchema) {
        return getSimpleTypes((Schema) xmlSchema.getSchema(), new ArrayList(), new ArrayList());
    }
    
    protected Collection getSimpleTypes(Schema schema, List types, List visitedSchs) {
        if (null == schema) {
            return types;
        }
        
        // Defend against cyclic dependencies
        if (visitedSchs.contains(schema)) {
            return types;
        }
        visitedSchs.add(schema);
        
        // Grab the SimpleType's here
        for (Enumeration enumer = schema.getSimpleTypes(); enumer.hasMoreElements();) {
            types.add(enumer.nextElement());
        }
        
        // Recurse through the imported schemas
        for (Enumeration enumer = schema.getImportedSchema(); enumer.hasMoreElements();) {
            getSimpleTypes((Schema) enumer.nextElement(), types, visitedSchs);
        }
        
        return types;
    }
    
    /** @see CastorSupport#getComplexTypes
     */
    public Collection getComplexTypes(XMLSchema xmlSchema) {
        return getComplexTypes((Schema) xmlSchema.getSchema(), new ArrayList(), new ArrayList());
    }
    
    protected Collection getComplexTypes(Schema schema, List types, List visitedSchs) {
        if (null == schema) {
            return types;
        }
        
        // Defend against cyclic dependencies
        if (visitedSchs.contains(schema)) {
            return types;
        }
        visitedSchs.add(schema);
        
        // Grab the ComplexType's here
        for (Enumeration enumer = schema.getComplexTypes(); enumer.hasMoreElements();) {
            types.add(enumer.nextElement());
        }
        
        // Recurse through the imported schemas
        for (Enumeration enumer = schema.getImportedSchema(); enumer.hasMoreElements();) {
            getComplexTypes((Schema) enumer.nextElement(), types, visitedSchs);
        }
        
        return types;
    }
    
    /** @see CastorSupport#getElementDecls
     */
    public Collection getElementDecls(XMLSchema xmlSchema) {
        return getElementDecls((Schema) xmlSchema.getSchema(), new ArrayList(), new ArrayList());
    }
    
    protected Collection getElementDecls(Schema schema, List elements, List visitedSchs) {
        if (null == schema) {
            return elements;
        }
        
        // Defend against cyclic dependencies
        if (visitedSchs.contains(schema)) {
            return elements;
        }
        visitedSchs.add(schema);
        
        // Grab the ElementDecl's here
        for (Enumeration enumer = schema.getElementDecls(); enumer.hasMoreElements();) {
            elements.add(enumer.nextElement());
        }
        
        // Recurse through the imported schemas
        for (Enumeration enumer = schema.getImportedSchema(); enumer.hasMoreElements();) {
            getElementDecls((Schema) enumer.nextElement(), elements, visitedSchs);
        }
        
        return elements;
    }
    
    
    /** @see CastorSupport#getTargetNamespace
     */
    public String getTargetNamespace(Object obj) {
        if (obj instanceof Schema) {
            return ((Schema) obj).getTargetNamespace();
        } else if (obj instanceof XMLSchema) {
            return ((Schema) ((XMLSchema) obj).getSchema()).getTargetNamespace();
        } else if (obj instanceof SimpleType) {
            return ((SimpleType) obj).getSchema().getTargetNamespace();
        } else if (obj instanceof ComplexType) {
            return ((ComplexType) obj).getSchema().getTargetNamespace();
        } else if (obj instanceof ElementDecl) {
            return ((ElementDecl) obj).getSchema().getTargetNamespace();
        } else {
            return null;
        }
    }
    
    
    /** @see CastorSupport#getName
     */
    public String getName(Object obj) {
        if (obj instanceof SimpleType) {
            return ((SimpleType) obj).getName();
        } else if (obj instanceof ComplexType) {
            return ((ComplexType) obj).getName();
        } else if (obj instanceof ElementDecl) {
            ElementDecl elem = (ElementDecl) obj;
            String name = elem.getName();
            return name;
        } else if (obj instanceof Group) {
            return ((Group) obj).getName();
        } else if (obj instanceof AttributeDecl) {
            return ((AttributeDecl) obj).getName();
        } else if (obj instanceof Wildcard) {
        	Wildcard wc = (Wildcard) obj;
        	//wildcard can be <any> or <anyAttribute>
        	if(wc.isAttributeWildcard()) {
        		return ANY_ATTRIBUTE;
        	}
        	
        	return ANY_ELEMENT;
        } else {
            return null;
        }
    }
    
    /* 
     * @see com.sun.wsdl.model.common.model.xsd.CastorSupport#getType(java.lang.Object)
     */
     public QName getType(Object obj) {
        XMLType type = null;
        if (obj instanceof ComplexType) {
			type = ((ComplexType) obj).getBaseType();
            if (type == null) {
            	String strName = ((ComplexType) obj).getName();
            	String namespaceURI = ((ComplexType) obj).getSchema().getTargetNamespace();
            	QName qName = NamespaceUtility.getQNameFromURIAndString(
            					namespaceURI, strName);
            	return qName;

            }
        } else if (obj instanceof ElementDecl) {
            type = ((ElementDecl) obj).getType();
        } else if (obj instanceof Group) {
            return null;
        } else if (obj instanceof AttributeDecl) {
            type = ((AttributeDecl) obj).getSimpleType();
        } else if (obj instanceof Wildcard) {
        	Wildcard wc = (Wildcard) obj;
        	//wildcard can be <any> or <anyAttribute>
        	if(wc.isAttributeWildcard()) {
        		return NamespaceUtility.getQName(ANY_ATTRIBUTE, ANY_ATTRIBUTE);
        	}
        	
        	return NamespaceUtility.getQName(ANY_ELEMENT, ANY_ELEMENT);
        	
        } else if (obj instanceof SimpleType) {
            type = ((SimpleType) obj).getBaseType();
        } else {
            return null;
        }
        //TODO: type name can be null if we have inline type defined for 
        //an element.Check the useage of this method and fix it.
        if (type != null) {
            return NamespaceUtility.getQNameFromURIAndString(
            			type.getSchema().getTargetNamespace(), type.getName());
        } else {
            return null;
        }
    }
   
    /* 
     * @see com.sun.wsdl.model.common.model.xsd.CastorSupport#getXSDType(java.lang.Object)
     */
    public String getXSDType(Object obj) {
        XMLType type = null;
        Hashtable table;
        String elementName = null;
        
        if (obj instanceof ComplexType) {
            type = ((ComplexType) obj);
            table = ((ComplexType) obj).getSchema().getNamespaces();
        } else if (obj instanceof ElementDecl) {
            type = ((ElementDecl) obj).getType();
            table = ((ElementDecl) obj).getSchema().getNamespaces();
            elementName = ((ElementDecl) obj).getName();
        } else if (obj instanceof Group) {
            return null;
        } else if (obj instanceof AttributeDecl) {
            type = ((AttributeDecl) obj).getSimpleType();
            table = ((AttributeDecl) obj).getSchema().getNamespaces();
        } else if (obj instanceof Wildcard) {
        	Wildcard wc = (Wildcard) obj;
        	if(wc.isAttributeWildcard()) {
        		return ANY_ATTRIBUTE;
        	}
            return ANY_ELEMENT;
        } else if (obj instanceof SimpleType) {
            type = ((SimpleType) obj);
            table = ((SimpleType) obj).getSchema().getNamespaces();
        } else {
            return null;
        }
        if (type != null) {
            // for the simple types that are derived we show the exact type. 
            /* for example the following derived simple type will have an xsdType of tns:mySType 
             * From an MNode point of view, (who is the sole consumer of this API) this is not sufficient
             * to support the usage of derived types. Correlations are created using the simpletypes, pre-defineds
             * or dervided. Correlation implementaion as of now in the BPEL editor cannot support this type 
             * of Derived types.
             * 
             <xs:element name="sType" type="tns:mySType" minOccurs="0"></xs:element>            

             <xs:simpleType name="mySType">
                 <xs:restriction base="xs:string">
                     <xs:pattern value="[a-z]"/>
                     </xs:restriction>
             </xs:simpleType>

             * Both MNode and this implementaion and probably this 
             * interface needs to be enhanced to support more advanced features of XSD. 
             */
        	String prefix = getNamespacePrefix(type, table);
        	
        	if(type.isAnyType()) {
        		return MESSAGES.getString("CastorSupportImpl.ANYTYPE");
        	} if(type.isSimpleType()) {
        		SimpleType sType = (SimpleType) type;
        		String typeName = type.getName();
        		return processSimpleOrComplexType(typeName, sType.getBaseType(), elementName, prefix);
        		
        	} else if (type.isComplexType()) {
                ComplexType cType = (ComplexType) type;
                String typeName = type.getName();
                return processSimpleOrComplexType(typeName, cType.getBaseType(), elementName, prefix);
            } else {
                
                String typeName = type.getName();
                String localName = (typeName != null) ? typeName : elementName;
                return Utility.isEmpty(prefix) ? localName : prefix + ":" + localName;
            }
        } else {
            return null;
        }
    }

    private String processSimpleOrComplexType(String typeName,
    										  XMLType baseType,	
    										  String elementName, 
    										  String prefix) {
    	//if simple type has a name then we are refering a named type so use it
		String localName = null;
		if(typeName != null) {
			localName = typeName;
			return Utility.isEmpty(prefix) ? localName : prefix + ":" + localName;
		} else if(elementName != null) {
			localName = elementName;
			return Utility.isEmpty(prefix) ? localName : prefix + ":" + localName;
		} else  {
			//we need to use base type
			//we also need to find what is the prefix of base type in the 
			//xsd where our original SimpleType (ie. sType) is defined.
			//so if we recursively call getXSDType this takes care of it
			return getXSDType(baseType);
		}
    }
    
    public String getShortDescription(Object obj) {
    	XMLType type = null;
        Hashtable table;
        String elementName = null;
        
        if (obj instanceof ComplexType) {
            type = ((ComplexType) obj);
            table = ((ComplexType) obj).getSchema().getNamespaces();
        } else if (obj instanceof ElementDecl) {
            type = ((ElementDecl) obj).getType();
            table = ((ElementDecl) obj).getSchema().getNamespaces();
            elementName = ((ElementDecl) obj).getName();
        } else if (obj instanceof Group) {
            return null;
        } else if (obj instanceof AttributeDecl) {
            type = ((AttributeDecl) obj).getSimpleType();
            table = ((AttributeDecl) obj).getSchema().getNamespaces();
        } else if (obj instanceof Wildcard) {
        	Wildcard wc = (Wildcard) obj;
        	if(wc.isAttributeWildcard()) {
        		return ANY_ATTRIBUTE;
        	}
            return ANY_ELEMENT;
        } else if (obj instanceof SimpleType) {
            type = ((SimpleType) obj);
            table = ((SimpleType) obj).getSchema().getNamespaces();
        } else {
            return null;
        }
        if (type != null) {
        	
        	String prefix = getNamespacePrefix(type, table);
        	
        	if(type.isSimpleType()) {
        		SimpleType sType = (SimpleType) type;
        		return buildSimpleTypeDescription(sType);
        		
        	}
        }
        
    	return getXSDType(obj);
    }
    
    
    /** @see CastorSupport#isKnownSchemaObject
     */
    public boolean isKnownSchemaObject(Object obj) {
        return ((obj instanceof SimpleType)
                || (obj instanceof ComplexType)
                || (obj instanceof ElementDecl)
                || (obj instanceof Group)
                || (obj instanceof AttributeDecl)
                || (obj instanceof Wildcard));
    }
    
    /** @see CastorSupport#getChildCount
     */
    public int getChildCount(Object parent) {
        int result = 0;
        
        if (parent instanceof ElementDecl) {
            result = getElementDeclChildCount(parent);
        } else if (parent instanceof ComplexType) {
            result = getComplexTypeChildCount(parent);
        } else if (parent instanceof Group) {
            result = getGroupChildCount(parent);
        }
        
        return result;
    }
    
    /** @see CastorSupport#getChild
     */
    public Object getChild(Object parent, int index) {
        Object result = null;
        
        if (parent instanceof ElementDecl) {
            result = getElementDeclChild(parent, index);
        } else if (parent instanceof ComplexType) {
            result = getComplexTypeChild(parent, index);
        } else if (parent instanceof Group) {
            result = getGroupChild(parent, index);
        }
        
        return result;
    }
    
    /** @see CastorSupport#getComplexTypeChildCount
     */
    public int getComplexTypeChildCount(Object obj) {
        int result = 0;
        ComplexType complexType = (ComplexType) obj;
        
        if (complexType.getMaxOccurs() > 1) {
            return result;
        }
        
//        for (Enumeration enum = complexType.getAttributeDecls();
//                enum.hasMoreElements();) {
//            result++;
//            enum.nextElement();
//        }
        
        //first iterate through local attributes
        result += getCount(complexType.getLocalAttributeDecls());
        
        //then iterator through attribute group refs
        for (Enumeration enumum = complexType.getAttributeGroupReferences();
        enumum.hasMoreElements();) {
		    AttributeGroupReference agRef = (AttributeGroupReference) enumum.nextElement();
		    result += getCount(agRef.getAttributes());
		    //also check for <anyAttribute>
		    //TODO: we need to check for <anyAttribute> which 
		    //may be available in other AttributeGroupReference which
		    //this AttributeGroupReference refers.
		    //ex: agr1 ref to agr2.
		    //agr1  has one <anyAttribute>
		    //agr2 also has <anyAttribute>
		    //currently we are not checking <antAttribute> in agr2
		    //we may need to modify castor api.
		    //getAttributes() in AttributeGroup returns all
		    //the attribute in current AttributeGroup and all
		    //refered AttributeGroupRefernce but it does not return WildCard
		    if(agRef.getAnyAttribute() != null) {
		    	result++;
		    }
        }
        
        // get all the parent elements if this is an extended complex type
        if (complexType.getDerivationMethod() != null 
            && complexType.getDerivationMethod().equals(MESSAGES.getString("CastorSupportImpl.EXTENSION"))) {

            XMLType xmlType = complexType.getBaseType();
            result += getChildCount(xmlType);
        }

        int groupCount = 0;
        for (Enumeration enumum = complexType.enumerate();
        enumum.hasMoreElements();) {
            groupCount++;
            result = result + getGroupChildCount(enumum.nextElement());
        }

        return result;
    }
    
    private int getCount(Enumeration enumum) {
    	int count = 0;
    	while(enumum.hasMoreElements()) {
    		count++;
            enumum.nextElement();
    	}
    	
    	return count;
    }
    
    private Object getElement(Enumeration enumum, int offset, int index) {
    	Object result = null; 
    	int k = offset;
    	while(enumum.hasMoreElements() && (k < index)) {
    		result = enumum.nextElement();
    		k++;
    	 }
    	
    	return result;
    }
    
    /** @see CastorSupport#getComplexTypeChild
     */
    public Object getComplexTypeChild(Object obj, int index) {
        Object result = null;
        ComplexType complexType = (ComplexType) obj;
        int k = -1;
        
//        for (Enumeration enum = complexType.getAttributeDecls();
//                enum.hasMoreElements() && (k < index); k++) {
//            result = enum.nextElement();
//        }
        
//        IF (K == INDEX) {
//            RETURN RESULT;
//        }
        
//      first iterate through local attributes
        for (Enumeration enumum = complexType.getLocalAttributeDecls();
        enumum.hasMoreElements() && (k < index); k++) {
        	result = enumum.nextElement();
        }
        
        //found it
        if(k == index) {
        	return result;
        }
        
        //then iterator through attribute group refs
        for (Enumeration enumum = complexType.getAttributeGroupReferences();
        enumum.hasMoreElements();) {
		    
        	AttributeGroupReference agRef = (AttributeGroupReference) enumum.nextElement();
		    for(Enumeration subEnu = agRef.getAttributes();
		    subEnu.hasMoreElements() && (k < index); k++) {
		    	result = subEnu.nextElement();
		    }
		    
		    //found it
	        if(k == index) {
	        	return result;
	        }
	        
		    //also check for <anyAttribute>
		    if(agRef.getAnyAttribute() != null) {
		    	k++;
		    	result = agRef.getAnyAttribute();
		    	//found it
		        if(k == index) {
		        	return result;
		        }
		    }
        }
        
        
        Vector totalElements = new Vector();

        // get all the parent elements if this is an extended complex type
        if (complexType.getDerivationMethod() != null 
            && complexType.getDerivationMethod().equals(MESSAGES.getString("CastorSupportImpl.EXTENSION"))) {

            XMLType xmlType = complexType.getBaseType();
            int count = getChildCount(xmlType);
            for (int i = 0; i < count; i++) {
                totalElements.add(getChild(xmlType, i));
            }
        }

        for (Enumeration enumum = complexType.enumerate();
        enumum.hasMoreElements();) {
            
            Object grp = enumum.nextElement();
            int count = getGroupChildCount(grp);
            for (int j = 0; j < count; j++) {
                
                totalElements.add(getGroupChild(grp, j));
            }
        }
        return totalElements.elementAt(index - (k + 1));
    }
    
    /** Describes the ElementDecl.
     * @return  Description of ElementDecl
     */
    private static String describeElementDecl(ElementDecl elemDecl) {
        return MESSAGES.getString( "CastorSupportImpl.ELEMDECL_NAMED_x_OF_SCHEMA_x_SCHEMALOCATN_x", 
                new Object[]{  elemDecl.getName(),
                               elemDecl.getSchema().getTargetNamespace(), 
                               elemDecl.getSchema().getSchemaLocation()  } );
    }
    
    /** @see CastorSupport#getElementDeclChildCount
     */
    public int getElementDeclChildCount(Object obj) {
        int result = 0;
        ElementDecl elemDecl = (ElementDecl) obj;
        
        if (elemDecl.isReference()) {
            result = getElementDeclChildCount(elemDecl.getReference());
        } else {
            XMLType xmlType = elemDecl.getType();
            if (null == xmlType) {
                 LOGGER.log(Level.FINE,
                        MESSAGES.getString("CastorSupportImpl.TYPE_FOR_x_IS_MISSING_NULL", 
                            new Object[]{ describeElementDecl(elemDecl) } ));
            } else if (xmlType.isSimpleType()) {
                result = 0;
            } else if (xmlType.isAnyType()) {
                result = 0;
            } else if (xmlType.isComplexType()) {
                result = getComplexTypeChildCount(xmlType);
            } else {
                result = 0;
            }
        }
        
        return result;
    }
    
    /** @see CastorSupport#getElementDeclChild
     */
    public Object getElementDeclChild(Object obj, int index) {
        Object result = null;
        ElementDecl elemDecl = (ElementDecl) obj;
        
        if (elemDecl.isReference()) {
            result = getElementDeclChild(elemDecl.getReference(), index);
        } else {
            XMLType xmlType = elemDecl.getType();
            if (null == xmlType) {
                LOGGER.log(Level.FINE,
                        MESSAGES.getString("CastorSupportImpl.TYPE_FOR_x_IS_MISSING_NULL", 
                            new Object[]{ describeElementDecl(elemDecl) } ));
            } else if (xmlType.isSimpleType()) {
                result = null;
            } else if (xmlType.isAnyType()) {
                result = null;
            } else if (xmlType.isComplexType()) {
                result = getComplexTypeChild(xmlType, index);
            } else {
                result = null;
            }
        }
        
        return result;
    }
    
    /** @see CastorSupport#getGroupChildCount
     */
    public int getGroupChildCount(Object obj) {
        int result = 0;
        Group group = (Group) obj;
        
        result = group.getParticleCount();
        
        return result;
    }
    
    /** @see CastorSupport#getGroupChild
     */
    public Object getGroupChild(Object obj, int index) {
        Object result = null;
        Group group = (Group) obj;
        
        result = group.getParticle(index);
        
        return result;
    }
    
    /** @see CastorSupport#isSimpleType
     */
    public boolean isSimpleType(Object obj) {
        return obj instanceof SimpleType;
    }
    
    /** @see CastorSupport#isComplexType
     */
    public boolean isComplexType(Object obj) {
        return obj instanceof ComplexType;
    }
    
    /** @see CastorSupport#isGroupType
     */
    public boolean isGroupType(Object obj) {
        return obj instanceof Group;
    }
    
    /** @see CastorSupport#isElementType
     */
    public boolean isElementType(Object obj) {
        return obj instanceof ElementDecl;
    }
    
    /** @see CastorSupport#isAttributeType
     */
    public boolean isAttributeType(Object obj) {
    	boolean result = false;
    	if(obj instanceof AttributeDecl) {
    		result =  true;
    	} else if (obj instanceof Wildcard) {
    		Wildcard wc = (Wildcard) obj;
    		result = wc.isAttributeWildcard();
    	}
        
        return result;
    }

    /** @see CastorSupport#isOptionalAttribute
     */
    public boolean isOptionalAttribute(Object obj) {
        return (obj instanceof AttributeDecl) 
            && ((AttributeDecl) obj).isOptional();
    }

    /** @see CastorSupport#getMin
     */
    public int getMin(Object obj) {
        
        int result = 1;
        
        if (obj instanceof ElementDecl) {
            result = ((ElementDecl) obj).getMinOccurs();
        } else if (obj instanceof ComplexType) {
            result = ((ComplexType) obj).getMinOccurs();
        } else if (obj instanceof Group) {
            result = ((Group) obj).getMinOccurs();
        }
        
        return result;
    }
    
    /** @see CastorSupport#getMax
     */
    public int getMax(Object obj) {
        
        int result = 1;
        
        if (obj instanceof ElementDecl) {
            result = ((ElementDecl) obj).getMaxOccurs();
        } else if (obj instanceof ComplexType) {
            result = ((ComplexType) obj).getMaxOccurs();
        } else if (obj instanceof Group) {
            result = ((Group) obj).getMaxOccurs();
        }
        
        return result;
    }
    
    /** @see CastorSupport#getSimpleType
     */
    public Class getSimpleType() {
        return SimpleType.class;
    }
    
    /** @see CastorSupport#getComplexType
     */
    public Class getComplexType() {
        return ComplexType.class;
    }
    
    /** @see CastorSupport#getElementType
     */
    public Class getElementType() {
        return ElementDecl.class;
    }
    
    /** @see CastorSupport#getGroupType
     */
    public Class getGroupType() {
        return Group.class;
    }
    
    /** @see CastorSupport#getAttributeType
     */
    public Class getAttributeType() {
        return AttributeDecl.class;
    }
    
    /** Ignore exceptions if any from SAX XML Reader feature sets.
     * @param   xmlReader   XML Reader
     * @param   feature     Feature to set
     * @param   boolVal     Boolean value to use
     */
    private void setSAXXMLReaderFeature(XMLReader xr, String feature, boolean boolVal) {
        try {
            xr.setFeature(feature, boolVal);
        } catch (SAXException se) {
            LOGGER.log(Level.FINE,
                    (MESSAGES.getString("CastorSupportImpl.SAX_XML_RDR_FEATURE_x_NOT_SET_TO_x",
                            new Object[]{ feature, Boolean.toString(boolVal) } )));
        }
    }
    
    /** Getter for the XML document.
     * @return  XML document.
     */
    protected WSDLDocument getXmlDocument() {
        return mWsdlDoc;
    }
    
    /** Getter for the XML reader.
     * @return  XML reader.
     */
    private XMLReader getXmlReader() {
        XMLReader xmlReader = null;
        try {
            SAXParserFactory factory = SAXParserFactory.newInstance();
            SAXParser saxParser = factory.newSAXParser();
            xmlReader = saxParser.getXMLReader();

            // SAX2 core features
            setSAXXMLReaderFeature(xmlReader, "http://xml.org/sax/features/namespaces", true);
            setSAXXMLReaderFeature(xmlReader, "http://xml.org/sax/features/namespace-prefixes", true);
            setSAXXMLReaderFeature(xmlReader, "http://xml.org/sax/features/validation", false);
            setSAXXMLReaderFeature(xmlReader, "http://xml.org/sax/features/external-general-entities", false);
            setSAXXMLReaderFeature(xmlReader, "http://xml.org/sax/features/external-parameter-entities", false);
            xmlReader.setProperty("http://xml.org/sax/properties/lexical-handler", getXmlLexicalHandler());
        } catch (Throwable trw) {
            throw new XMLParseVisitorException(
                MESSAGES.getString("CastorSupportImpl.CANT_INST_XML_RDR",trw));
        }
        return xmlReader;
    }
    
    /** Gets a Schema document parser that converts all to Extensibility elements.
     * @return  A XML document parser.
     */
    protected ContentHandler getXmlParser() {
        if (null == mXmlParser) {
            mXmlParser = new DocumentXmlParser();
        }
        return mXmlParser;
    }
    
    /** Pushes the ExtensibilityElement onto a stack.
     * @param   top     ExtensibilityElement to push.
     */
    protected void pushCurrentXMLElement(ExtensibilityElement top) {
        if (null == mExtensibilityElementStack) {
            mExtensibilityElementStack = new Stack();
        }
        mExtensibilityElementStack.push(top);
    }
    
    /** Peeks at the top ExtensibilityElement on the stack.
     * @return  Top ExtensibilityElement.
     */
    protected ExtensibilityElement peekCurrentXMLElement() {
        return ((mExtensibilityElementStack != null) && !mExtensibilityElementStack.empty())
                    ? (ExtensibilityElement) mExtensibilityElementStack.peek() : null;
    }
    
    /** Pops off a ExtensibilityElement off the stack.
     * @return  Popped off ExtensibilityElement.
     */
    protected ExtensibilityElement popCurrentXMLElement() {
        return ((mExtensibilityElementStack != null) && !mExtensibilityElementStack.empty())
                    ? (ExtensibilityElement) mExtensibilityElementStack.pop() : null;
    }
    
    /** Getter for the root ExtensibilityElement.
     * @return  Current root ExtensibilityElement.
     */
    protected ExtensibilityElement getRootExtensibilityElement() {
        return mRootExten;
    }
    
    /** Setter for the root ExtensibilityElement.
     * @param   root    Current root ExtensibilityElement.
     */
    protected void setRootExtensibilityElement(ExtensibilityElement root) {
        if (root != null) {
            mRootExten = root;
        }
    }
    
    private String getXsdPrefix(Schema schema) {
    	String prefix = null;
    	
    	Hashtable prefixToNSTable = schema.getNamespaces();
    	Iterator it = prefixToNSTable.keySet().iterator();
    	while(it.hasNext()) {
    		String pfx = (String) it.next();
    		String ns = (String) prefixToNSTable.get(pfx);
    		if(XSD_NAMESPACE.equals(ns)) {
    			prefix = pfx;
    			break;
    		}
    	}
    	return prefix;
    }
    
    private String getNamespacePrefix(XMLType type, Hashtable prefixNSTable) {
    	String targetNS = type.getSchema().getTargetNamespace();
    	Set prefixKeys = prefixNSTable.keySet();
        
        String prefix = null;
        if (targetNS != null) {
            for (Iterator itr = prefixKeys.iterator(); itr.hasNext(); ) {
                String nsPrefix = (String) itr.next();
                String ns = (String) prefixNSTable.get(nsPrefix);
                if (!Utility.isEmpty(ns) && ns.equals(targetNS)) {
                    prefix = nsPrefix;
                    break;
                }
            }
        }
        
        return prefix;
    }
    
    private String buildSimpleTypeDescription(SimpleType sType) {
    	StringBuffer desc = new StringBuffer(30);
    	desc.append("<html>");
    	desc.append("<b> <i><center> Simple Type <center> </i></b>");
    	
    	if(sType.getName() != null) {
    		desc.append("<table border=0 cellspacing=0 cellpadding=0 >");
    		desc.append("<tr> <td>&nbsp; name  </td> <td> &nbsp; : &nbsp; <b>");
    		desc.append(sType.getName());
    		desc.append("</b> </td> </tr>");
    		
    	}
    	
    	String derivationMethod = sType.getDerivationMethod();
    	if(derivationMethod != null) {
    		
    		String baseType = getXSDType(sType);
    		
	    	if(baseType != null) {
	    		desc.append("<tr> <td>&nbsp; base  </td> <td> &nbsp; : &nbsp; <b>");
	    		desc.append(baseType);
	    		desc.append("</b> </td> </tr>");
	    	}
	    	
	    	Enumeration enu = sType.getFacets();
	    	int i = 0;
	    	while(enu.hasMoreElements()) {
	    		Facet facet = (Facet) enu.nextElement();
	    		desc.append("<tr> <td>&nbsp;");
	    		desc.append(facet.getName());
	    		desc.append("</td> <td> &nbsp; : &nbsp; <b>");
	    		desc.append(facet.getValue());
	    		desc.append("</b> </td> </tr>");
	    	}
	    	
    	}
    	desc.append("</table> </html>");
    	return desc.toString();
    }
    /** Implements <code>ContentHandler</code> for document parsing.
     */
    protected class DocumentXmlParser extends DefaultHandler {
        
        /** Called when an element is started.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @param   attributes  Attributes of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void startElement(String uri, String localName, String qName,
                                 Attributes attributes) throws SAXException {
            ExtensibilityElement exten = getXmlDocument().createExtensibilityElement();
            exten.setElementType(
            			NamespaceUtility.getQNameFromURIAndQualifiedString(uri,qName));
            SAXParserSupport.setAttributes(exten, attributes);
            if (peekCurrentXMLElement() != null) {
                peekCurrentXMLElement().addExtensibilityElement(exten);
            }
            pushCurrentXMLElement(exten);
        }
        
        /** Called when an element is ended.
         * @param   uri     URI of namespace.
         * @param   localName   Local name of element.
         * @param   qName       Qualified name of element.
         * @throws  SAXException    If SAX errors occur.
         */
        public void endElement(String uri, String localName, String qName)
            throws SAXException {
            setRootExtensibilityElement(popCurrentXMLElement());
        }
    }
    
    /** @see com.sun.wsdl.model.common.model.xsd.CastorSupport#convertSchemaToExtensibilityElement(
     *  com.sun.wsdl.model.Types)
     */
    public void convertSchemaToExtensibilityElement(Types types) {
        if (null == types) {
            return;
        }
        
        mExtensibilityElementStack = null;
        mRootExten = null;
        mWsdlDoc = (WSDLDocument) types.getOwnerDocument();
        Object[] extenElems = types.getExtensibilityElements().toArray();
        for (Iterator schIter = types.getSchemas().iterator(); schIter.hasNext();) {
            XMLSchema xmlSchema = (XMLSchema) schIter.next();
            
            // Find a extensibility element schema matching this namespace
            boolean found = false;
            String schTns = getTargetNamespace(xmlSchema);
            for (int i = 0; i < extenElems.length; i++) {
                ExtensibilityElement schExtElem = (ExtensibilityElement) extenElems[i];
                Map oaMap = schExtElem.getOtherAttributes();
                if (oaMap != null) {
                    String tns = (String) oaMap.get( MESSAGES.getString("CastorSupportImpl.TARGETNAMESPACE"));
                    if ((tns != null) && (tns.equals(schTns))) {
                        found = true;
                        break;
                    }
                }
            }
            
            // None found, create one
            if (!found) {
                types.addExtensibilityElement(
                    convertSchemaToExtensibilityElement((Schema) xmlSchema.getSchema()));
            }
        }
    }
    
    /** Converts an inline Schema to equivalent ExtensibilityElement.
     *
     * @param   schema          The inline Schema to be converted.
     */
    protected ExtensibilityElement convertSchemaToExtensibilityElement(Schema schema) {
        convertSchemaToExtensibilityElement(null, schema);
        return getRootExtensibilityElement();
    }
    
    /** Converts an inline Schema to equivalent ExtensibilityElement (replacing the children of
     * the input ExtensibilityElement root).
     *
     * @param   origSchema      Original ExtensibilityElement root representing the schema.
     * @param   schema          The inline Schema to be converted.
     */
    protected void convertSchemaToExtensibilityElement(ExtensibilityElement origSchema, Schema schema) {
        XMLParseVisitorException toBeThrown = null;
        try {
            StringWriter strWr = new StringWriter();
            SchemaWriter schWr = new SchemaWriter(strWr);
            schWr.write(schema);
            StringReader reader = new StringReader(strWr.toString());
            
            XMLReader xmlReader = getXmlReader();
            xmlReader.setContentHandler(getXmlParser());
            xmlReader.parse(new InputSource(reader));

            // No original schema, just return next extensibility root
            if (null == origSchema) {
                return;
            }
            
            // remove the children of the original ExtensibilityElement root
            if (origSchema.hasChildren()) {
                XMLNode[] origKids =
                    (XMLNode[]) origSchema.getChildren().toArray(new XMLNode[origSchema.getChildren().size()]);
                for (int i = 0; i < origKids.length; i++) {
                    origSchema.removeChild((XMLNode) origKids[i]);
                }
            }
            
            // Add children of new ExtensibilityElement root back
            if (getRootExtensibilityElement() != null) {
                if (getRootExtensibilityElement().hasChildren()) {
                    for (Iterator iter = getRootExtensibilityElement().getChildren().iterator(); iter.hasNext();) {
                        origSchema.addChild((XMLNode) iter.next());
                    }
                }
                
                // Just to be safe, transfer all the attributes of the new root extensibility element
                // back to the original (overwrite)
                if (origSchema.getNamespaces() != null) {
                    origSchema.getNamespaces().clear();
                }
                if (origSchema.getOtherAttributes() != null) {
                    origSchema.setOtherAttributes((String) null, (String) null);  // clear map
                }
                if (getRootExtensibilityElement().getNamespaces() != null) {
                    origSchema.setNamespaces(getRootExtensibilityElement().getNamespaces());
                }
                if (getRootExtensibilityElement().getOtherAttributes() != null) {
                    for (Iterator iter = getRootExtensibilityElement().getOtherAttributes().entrySet().iterator();
                            iter.hasNext();) {
                        Map.Entry me = (Map.Entry) iter.next();
                        origSchema.setOtherAttributes((String) me.getKey(), (String) me.getValue());
                    }
                }
            } else {
                toBeThrown = new XMLParseVisitorException(
                        MESSAGES.getString("CastorSupportImpl.CANT_CNVRT_INLINE_SCHEMA_TO_EXTNSIBILITY_ELEM_FORM"));
            }
        } catch (Throwable trw) {
            toBeThrown = new XMLParseVisitorException(
                    MESSAGES.getString("CastorSupportImpl.CANT_CNVRT_INLINE_SCHEMA_TO_EXTNSIBILITY_ELEM_FORM", trw));
        }
        
        if (toBeThrown != null) {
            throw toBeThrown;
        }
    }
    
    /** Getter for the Lexical handler for the SAX parser.
     * @return  Lexical handler
     */
    protected LexicalHandler getXmlLexicalHandler() {
        if (null == mXmlLexicalHandler) {
            mXmlLexicalHandler = new XmlLexer();
        }
        return mXmlLexicalHandler;
    }
    
    /** Implements the Lexical handler for the SAX parser
     */
    protected class XmlLexer implements LexicalHandler {
        
        // SAX Lexical Handler interface
        
        /** Called when a XML comment is encountered
         * @param   ch      Character array for the comment.
         * @param   offset  Offset to the array where the comment starts.
         * @param   length  Length of the comment
         */
        public void comment(char[] ch, int offset, int length) {
            XMLComment tComment = new XMLCommentImpl();
            tComment.setValue(new String(ch, offset, length));
            peekCurrentXMLElement().addChild(tComment);
        }
        
        /** Called when the end of a CDATA section is detected.
         */
        public void endCDATA() {
        }
        
        /** Called when the end of a DTD definition is detected.
         */
        public void endDTD() {
        }
        
        /** Called when the end of a entity is detected.
         * @param   name    Name of the entity.
         */
        public void endEntity(String name) {
        }
        
        /** Called when the start of a DTD definition is detected.
         * @param   name        Name of the DTD.
         * @param   publicId    Public ID of the DTD.
         * @param   systemId    System ID of the DTD.
         */
        public void startDTD(String name, String publicId, String systemId) {
        }
        
        /** Called when the start of a CDATA section is detected.
         */
        public void startCDATA() {
        }
        
        /** Called when the start of a entity is detected.
         * @param   name    Name of the entity.
         */
        public void startEntity(String name) {
        }
    }
}
