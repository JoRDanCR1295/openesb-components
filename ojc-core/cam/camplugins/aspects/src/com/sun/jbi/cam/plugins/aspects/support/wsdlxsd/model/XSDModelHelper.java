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
 * @(#)XSDModelHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.model;

/**
 * @author graj
 *
 */
import java.io.Serializable;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaParticle;
import org.apache.xmlbeans.SchemaType;
import org.apache.xmlbeans.SchemaTypeSystem;
import org.apache.xmlbeans.XmlException;

import com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.parsers.XSDParser;


/**
 * @author Graj
 *
 */
public class XSDModelHelper implements Serializable, XSDModel {
    
    XSDParser xsdParser = new XSDParser();
    
	private static final long serialVersionUID = 1L;
    
    /**
     * 
     */
    public XSDModelHelper() {
        super();
    }
    
    /* (non-Javadoc)
     * @see com.sun.jbi.binding.userbc.model.XSDModel#getXsdParser()
     */
    public XSDParser getXsdParser() {
        return this.xsdParser;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.binding.userbc.model.XSDModel#populate(java.util.Map, java.util.List, java.util.Map)
     */
    public SchemaTypeSystem populate(Map importMap, List typeNodes, Map namespaces) throws XmlException {
        List schemaList = null;
        SchemaTypeSystem typeSystem = this.xsdParser.loadAndCompileXSD(importMap, typeNodes, namespaces);
        if(typeSystem != null) {
        }        
        return typeSystem;
    }
    
    /* (non-Javadoc)
     * @see com.sun.jbi.binding.userbc.model.XSDModel#getSchemaTypeSystem()
     */
    public SchemaTypeSystem getSchemaTypeSystem() {
        return this.xsdParser.getSchemaTypeSystem();
    }
    
    /* (non-Javadoc)
     * @see com.sun.jbi.binding.userbc.model.XSDModel#getBuiltInTypeSystem()
     */
    public SchemaTypeSystem getBuiltInTypeSystem() {
        return this.xsdParser.getBuiltInTypeSystem();
    }    
    
    /* (non-Javadoc)
     * @see com.sun.jbi.binding.userbc.model.XSDModel#retrieveAllSchemaTypes()
     */
    public SchemaType[] retrieveAllSchemaTypes() {
        return this.getSchemaTypeSystem().documentTypes();
    }

    /**
     * Get the SchemaType associated with this elementQName
     * @param elementQName
     * @return a SchemaType object of the elementName
     */
    public SchemaType getExpandedSchemaType(QName elementQName) {
        if(elementQName == null) {
            return null;
        }
        SchemaType type = null;
        if(elementQName != null) {
            type = this.retrieveSchemaType(elementQName);
            if(type != null) {
                SchemaParticle schemaParticle = null;
                switch(type.getContentType()) {
                    case SchemaType.NOT_COMPLEX_TYPE:
                    case SchemaType.EMPTY_CONTENT:
                        break;
                    case SchemaType.SIMPLE_CONTENT:
                        schemaParticle = type.getContentModel();
                        if(schemaParticle != null) {
                            return schemaParticle.getType();
                        }
                        break;
                    case SchemaType.MIXED_CONTENT:
                        schemaParticle = type.getContentModel();
                        if(schemaParticle != null) {
                            return schemaParticle.getType();
                        }
                        break;
                    case SchemaType.ELEMENT_CONTENT:
                        schemaParticle = type.getContentModel();
                        if(schemaParticle != null) {
                            return schemaParticle.getType();
                        }
                        break;
                    default:
                        break;
                }
            }
        }
        return type;
    }    
    
    /* (non-Javadoc)
     * @see com.sun.jbi.binding.userbc.model.XSDModel#retrieveSchemaType(javax.xml.namespace.QName)
     */
    public SchemaType retrieveSchemaType(QName elementQName) {
        if(elementQName == null) {
            return null;
        }
        return this.retrieveSchemaType(elementQName.getLocalPart());
    }
    
    /* (non-Javadoc)
     * @see com.sun.jbi.binding.userbc.model.XSDModel#retrieveSchemaType(java.lang.String)
     */
    public SchemaType retrieveSchemaType(String elementName) {
        if(elementName == null) {
            return null;
        }
        /////////////////////////////////////////////////////////////
        // Try to Get the Document Type
        /////////////////////////////////////////////////////////////
        //  Simple content. See SchemaType.getContentType().
        //  int SIMPLE_CONTENT = 2;
        // ------ OR ----------
        //  Element-only content. See SchemaType.getContentType().
        //  int ELEMENT_CONTENT = 3;
        // ------ OR ----------
        //  Mixed content. SchemaType.getContentType()}.
        //  int MIXED_CONTENT = 4;
        /////////////////////////////////////////////////////////////
        SchemaType[] globalElements = this.getSchemaTypeSystem().documentTypes();
        if(globalElements != null) {
            for(int index = 0; index < globalElements.length; index++) {
                String localPart = globalElements[index].getDocumentElementName().getLocalPart();
                if(elementName.equals(localPart) == true) {
                    return globalElements[index];
                }
            }
        }
        /////////////////////////////////////////////////////////////
        SchemaType[] globalAttributeElements = this.getSchemaTypeSystem().attributeTypes();
        if(globalAttributeElements != null) {
            for(int index = 0; index < globalAttributeElements.length; index++) {
                QName qName = globalAttributeElements[index].getAttributeTypeAttributeName();
                if((qName != null) && (qName.getLocalPart() != null)) {
                    String localPart = qName.getLocalPart();
                    if(elementName.equals(localPart) == true) {
                        return globalAttributeElements[index];
                    }
                }
            }
        }
        
        SchemaType[] internalGlobalTypes = this.getSchemaTypeSystem().globalTypes();
        if(internalGlobalTypes != null) {
            for(int index = 0; index < internalGlobalTypes.length; index++) {
                QName qName = internalGlobalTypes[index].getName();
                if((qName != null) && (qName.getLocalPart() != null)) {
                    String localPart = qName.getLocalPart();
                    if(elementName.equals(localPart) == true) {
                        return internalGlobalTypes[index];
                    }
                }
            }
        }
        /////////////////////////////////////////////////////////////
        // Since it's not a Document Type,
        // Get the builtin type
        /////////////////////////////////////////////////////////////
        // Not a complex type. See SchemaType.getContentType().
        // int NOT_COMPLEX_TYPE = 0; 
        // ------ OR ----------
        // Empty content. See SchemaType.getContentType().
        // int EMPTY_CONTENT = 1;
        /////////////////////////////////////////////////////////////
        globalElements = this.getBuiltInTypeSystem().globalTypes();
        if(globalElements != null) {
            for(int index = 0; index < globalElements.length; index++) {
                if(elementName.equals(globalElements[index].getName().getLocalPart()) == true) {
                    return globalElements[index];
                }
            }
        }
        /////////////////////////////////////////////////////////////
        
        return null;
    }
    
    /* (non-Javadoc)
     * @see com.sun.jbi.binding.userbc.model.XSDModel#getDataStructure(org.apache.xmlbeans.SchemaType)
     */
    public SchemaType getDataStructure(SchemaType type) {
        if(type == null) {
            return type;
        }
        SchemaParticle schemaParticle = null;
        switch(type.getContentType()) {
            case SchemaType.NOT_COMPLEX_TYPE:
            case SchemaType.EMPTY_CONTENT:
            case SchemaType.SIMPLE_CONTENT:
                break;
            case SchemaType.MIXED_CONTENT:
            case SchemaType.ELEMENT_CONTENT:
                schemaParticle = type.getContentModel();
                if(schemaParticle != null) {
                    return schemaParticle.getType();
                }
                break;
            default:
        }
        return type;
    }
    
    /**
     * @param args
     */
    public static void main(String[] args) {
        // TODO Auto-generated method stub

    }

}
