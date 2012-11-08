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
 * @(#)XMLSchema.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.xsd;

import java.io.Writer;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import org.exolab.castor.xml.schema.ElementDecl;
import org.exolab.castor.xml.schema.Schema;
import org.exolab.castor.xml.schema.SimpleType;
import org.exolab.castor.xml.schema.SimpleTypesFactory;
import org.exolab.castor.xml.schema.XMLType;

import com.sun.jbi.internationalization.Messages;
import com.sun.wsdl.model.common.model.PrivateExtensionMapModel;


/**
 * Models an XML Schema. Click <a href="http://www.w3.org/XML/Schema">here</a>
 * for more information.
 * Actually, this just provides a wrapper to an existing XML
 * Schema model. This may be expanded later to provide an API
 * into the actual model implementation. As it stands right now it's just
 * a placeholder.
 * 
 * @author Sun Microsystems
 * @version 
 */
public abstract class XMLSchema {
    
    /* logger */
    private static final Messages MESSAGES = 
            Messages.getMessages(XMLSchema.class);
    private static final Logger LOGGER = 
            Messages.getLogger(XMLSchema.class);
    
    /** The XML Schema URI. */
    public static final String URI = "http://www.w3.org/2001/XMLSchema";
    
    /** The Tag for the schema element */
    public static final String TAG = "schema";
    
    /** XML Schema primitive types. */
    private static HashMap mPrimitiveTypes;    
    static {
        mPrimitiveTypes = new HashMap();
        mPrimitiveTypes.put("string", "string");
        mPrimitiveTypes.put("boolean", "boolean");
        mPrimitiveTypes.put("decimal", "decimal");
        mPrimitiveTypes.put("float", "float");
        mPrimitiveTypes.put("double", "double");
        mPrimitiveTypes.put("duration", "duration");
        mPrimitiveTypes.put("dateTime", "dateTime");
        mPrimitiveTypes.put("time", "time");
        mPrimitiveTypes.put("date", "date");
        mPrimitiveTypes.put("gYearMonth", "gYearMonth");
        mPrimitiveTypes.put("gYear", "gYear");
        mPrimitiveTypes.put("gMonthDay", "gMonthDay");
        mPrimitiveTypes.put("gDay", "gDay");
        mPrimitiveTypes.put("gMonth", "gMonth");
        mPrimitiveTypes.put("hexBinary", "hexBinary");
        mPrimitiveTypes.put("base64Binary", "base64Binary");
        mPrimitiveTypes.put("anyURI", "anyURI");
        mPrimitiveTypes.put("QName", "QName");
        mPrimitiveTypes.put("NOTATION", "NOTATION");
    };
    
    /** XML Schema built-in types. */
    private static HashMap mBuiltInTypes;
    
    private String mBaseURI = null;
    
    static {
        mBuiltInTypes = new HashMap();
        mBuiltInTypes.put("string", "string");
        mBuiltInTypes.put("boolean", "boolean");
        mBuiltInTypes.put("decimal", "decimal");
        mBuiltInTypes.put("float", "float");
        mBuiltInTypes.put("double", "double");
        mBuiltInTypes.put("duration", "duration");
        mBuiltInTypes.put("dateTime", "dateTime");
        mBuiltInTypes.put("time", "time");
        mBuiltInTypes.put("date", "date");
        mBuiltInTypes.put("gYearMonth", "gYearMonth");
        mBuiltInTypes.put("gYear", "gYear");
        mBuiltInTypes.put("gMonthDay", "gMonthDay");
        mBuiltInTypes.put("gDay", "gDay");
        mBuiltInTypes.put("gMonth", "gMonth");
        mBuiltInTypes.put("hexBinary", "hexBinary");
        mBuiltInTypes.put("base64Binary", "base64Binary");
        mBuiltInTypes.put("anyURI", "anyURI");
        mBuiltInTypes.put("QName", "QName");
        mBuiltInTypes.put("NOTATION", "NOTATION");
        mBuiltInTypes.put("anyType", "anyType");
        mBuiltInTypes.put("anySimpleType", "anySimpleType");
        mBuiltInTypes.put("normalizedString", "normalizedString");
        mBuiltInTypes.put("token", "token");
        mBuiltInTypes.put("language", "language");
        mBuiltInTypes.put("Name", "Name");
        mBuiltInTypes.put("NMTOKEN", "NMTOKEN");
        mBuiltInTypes.put("NCName", "NCName");
        mBuiltInTypes.put("NMTOKENS", "NMTOKENS");
        mBuiltInTypes.put("ID", "ID");
        mBuiltInTypes.put("IDREF", "IDREF");
        mBuiltInTypes.put("ENTITY", "ENTITY");
        mBuiltInTypes.put("IDREFS", "IDREFS");
        mBuiltInTypes.put("ENTITIES", "ENTITIES");
        mBuiltInTypes.put("integer", "integer");
        mBuiltInTypes.put("nonPositiveInteger", "nonPositiveInteger");
        mBuiltInTypes.put("long", "long");
        mBuiltInTypes.put("nonNegativeInteger", "nonNegativeInteger");
        mBuiltInTypes.put("negativeInteger", "negativeInteger");
        mBuiltInTypes.put("int", "int");
        mBuiltInTypes.put("unsignedLong", "unsignedLong");
        mBuiltInTypes.put("positiveInteger", "positiveInteger");
        mBuiltInTypes.put("short", "short");
        mBuiltInTypes.put("unsignedInt", "unsignedInt");
        mBuiltInTypes.put("byte", "byte");
        mBuiltInTypes.put("unsignedShort", "unsignedShort");
        mBuiltInTypes.put("unsignedByte", "unsignedByte");
    };

    private static final SimpleTypesFactory mSimpleTypesFactory = new SimpleTypesFactory();
    /**
     * Gets the internal schema representation.
     * @return the schema representation
     */
    public abstract Object getSchema();
    
    /**
     * Sets the internal schema representation.
     * @param schema the schema
     */
    public abstract void setSchema(Object schema);
    
    /**
     * Determines if the type is a primitive type.
     * @param typeName the type name
     * @return true if type is primitive
     */
    public static final boolean isPrimitiveType(String typeName) {
        return (mPrimitiveTypes.get(typeName) != null);
    }
    
    /**
     * Determines if the type is a primitive type.
     * @param uri the namespace URI
     * @param typeName the type name
     * @return true if the type is primitive
     */
    public static final boolean isPrimitiveType(String uri, String typeName) {
        return (uri.equals(URI) ? isPrimitiveType(typeName) : false);
    }
    
    public static Collection getAllBuiltInTypes() {
    	return Collections.unmodifiableCollection(mBuiltInTypes.values());
    }
    
    /**
     * Determines if the type is a built-in type.
     * @param typeName the type name
     * @return true if type is built-in
     */
    public static final boolean isBuiltInType(String typeName) {
        return (mBuiltInTypes.get(typeName) != null);
    }
    
    /**
     * Determines if the type is a built-in type.
     * @param uri the namespace URI it should be http://www.w3.org/2001/XMLSchema
     * @param typeName the type name
     * @return true if the type is built-in
     */
    public static final boolean isBuiltInType(String uri, String typeName) {
        return (uri.equals(URI) ? isBuiltInType(typeName) : false);
    }
    
    /**
     * Determines if the type is a built-in type.
     * @param type type QName it should have namespaceURI http://www.w3.org/2001/XMLSchema
     * @return true if the type is built-in
     */
    public static final boolean isBuiltInType(QName type) {
    	if(type == null) {
    		throw new IllegalArgumentException(
                        MESSAGES.getString(
                        "XMLSchema.CANNOT_DETERMINE_IF_XML_SCHEMA_TYPE_IS_BUILT_IN_TYPE_IS_NULL"));
    	}
    	
        //return isBuiltInType(type.getNamespaceURI(), type.getLocalName());
        return isBuiltInType(type.getNamespaceURI(), type.getLocalPart());
    }
    
    /**
     * Get built-in xml schema type.
     * @param type type QName it should have namespaceURI http://www.w3.org/2001/XMLSchema
     * @return true if the type is built-in
     */
    public static final XMLType getBuiltInType(QName type) {
    	SimpleType resultantType = null;
    	if(!isBuiltInType(type)) {
    		return null;
    	}
    	
    	//String typeLocalName = (String) mBuiltInTypes.get(type.getLocalName());
    	String typeLocalName = (String) mBuiltInTypes.get(type.getLocalPart());
    	
    	if(typeLocalName != null) {
    		resultantType = mSimpleTypesFactory.getBuiltInType(typeLocalName);  
    	}
    	
    	return resultantType;
    }
    
    /**
     * Loads the XML Schema from a reader.
     * <b>Note, this implies that import statements in the Schema CANNOT be supported</b>
     *
     * @param   rdr     Reader to the XML Schema source.
     * @since   5.1.0
     */
//    public abstract void load(Reader rdr);
    
    /**
     * Serializes the XML Schema document to the writer.
     * @param writer the writer
     */
    public abstract void serialize(Writer writer);
    
    
    /** Gets the XMLSchema to Schema correlation map.
     * @return  Correlation map.
     */
    public abstract Map getXMLSchemaSchemaMap();
    
    /** Gets the private extension map model associated with this document's associated
     *  repository object (if any).
     *
     *  @return Private extension map model for this document.
     *  @since  5.1.0
     */
    public abstract PrivateExtensionMapModel getPrivateExtensionMapModel();
    
    /**
     * set the absolute location of this schema.
     * @param baseURI
     */
    public void setBaseURI(String baseURI) {
    	this.mBaseURI = baseURI;
    }
    
    /**
     *  get the absolute location of this schema.
     * @return absolute location. 
     */
    public String getBaseURI() {
    	return this.mBaseURI;
    }
    
    public XMLType getXSDType(QName typeQName) {
    	if(typeQName != null && this.getSchema() != null) {
    		String targetNamespace = typeQName.getNamespaceURI();
    		if(targetNamespace != null) {
    			Schema schema = (Schema) getSchema();
	    		if(targetNamespace.equals(schema.getTargetNamespace())) {
	    			//return schema.getType(typeQName.getLocalName());
	    			return schema.getType(typeQName.getLocalPart());
	    		}
    		}
    	}
    	
    	return null;
    }
    
    public ElementDecl getXSDElement(QName elementQName) {
    	if(elementQName != null && this.getSchema() != null) {
    		String targetNamespace = elementQName.getNamespaceURI();
    		if(targetNamespace != null) {
    			Schema schema = (Schema) getSchema();
	    		if(targetNamespace.equals(schema.getTargetNamespace())) {
	    			//return schema.getElementDecl(elementQName.getLocalName());
	    			return schema.getElementDecl(elementQName.getLocalPart());
	    		}
    		}
    	}
    	
    	return null;
    }
}
