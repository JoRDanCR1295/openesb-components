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
 * @(#)XSDParser.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.parsers;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import javax.wsdl.Import;
import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaType;
import org.apache.xmlbeans.SchemaTypeSystem;
import org.apache.xmlbeans.XmlBeans;
import org.apache.xmlbeans.XmlException;
import org.apache.xmlbeans.XmlObject;
import org.apache.xmlbeans.XmlOptions;
import org.w3c.dom.Node;

/**
 * @author graj
 *
 */
public class XSDParser implements Serializable {
    
    public static final String SOAP_ENCODING_SCHEMA_URL = "http://schemas.xmlsoap.org/soap/encoding/"; //$NON-NLS-1$
    public static final String WSDL_SOAP_SCHEMA_URL = "http://schemas.xmlsoap.org/wsdl/soap/"; //$NON-NLS-1$
    
    
    SchemaTypeSystem schemaTypeSystem = null;    
	private static final long serialVersionUID = 1L;
    /**
     * 
     */
    public XSDParser() {
        super();
    }

    /**
     * 
     * @param typeNodes
     * @param namespaces
     * @return
     * @throws XmlException
     */
    public List<XmlObject> loadXSD(List<Node> typeNodes, Map namespaces) throws XmlException {
        if(typeNodes == null) {
            return null;
        }
        XmlObject schema = null;
        List<XmlObject> xmlObjectSchemas = new ArrayList<XmlObject>();
        
        XmlOptions options = new XmlOptions();
        // Place line number annotations 
        // in the store when parsing a document.  
        // This is particularly useful when you want XmlError
        // objects to contain line numbers.
        options = options.setLoadLineNumbers();
        // Set when loading from an InputStream or File, so that
        // the loader will compute a 160-bit SHA-1 message digest of the XML
        // file while loading it and make it available via 
        // the XmlObject.documentProperties().getMessageDigest();
        // The schema compiler uses message digests to detect and eliminate 
        // duplicate imported xsd files.
        options = options.setLoadMessageDigest();
        if(namespaces != null) {
          options.setLoadAdditionalNamespaces(namespaces);
        }

        Node node = null;
        for(Iterator<Node> iterator = typeNodes.iterator();
        iterator.hasNext() == true;) {
            node = iterator.next();
            if(node != null) {
                schema = XmlObject.Factory.parse(node, options);
                if(schema != null) {
                    xmlObjectSchemas.add(schema);
                }
            }
        }
        
        return xmlObjectSchemas;
    }
    
    /**
     * 
     * @param importsMap
     * @param namespaces
     * @return
     * @throws XmlException
     */
    public List<XmlObject> loadXSD(Map importsMap, Map namespaces) throws XmlException {
        if(importsMap == null) {
            return null;
        }
        XmlObject schema = null;
        List<XmlObject> xmlObjectSchemas = new ArrayList<XmlObject>();
        List<String> schemaFileList = this.getSchemaFileList(importsMap);
        String xsdFileName = null;
        for(Iterator<String> iterator = schemaFileList.iterator();
            iterator.hasNext() == true;) {
            xsdFileName = iterator.next();
            if(xsdFileName != null) {
                schema = (XmlObject)this.loadXSD(xsdFileName, namespaces);
                if(schema != null) {
                    xmlObjectSchemas.add(schema);
                }
            }
        }

        return xmlObjectSchemas;
    }

    /**
     * 
     * @param schemaFile
     * @param namespaces
     * @return
     * @throws XmlException
     */
    public XmlObject loadXSD(String schemaFile, Map namespaces) throws XmlException {
        if(schemaFile == null) {
            return null;
        }
        XmlOptions options = new XmlOptions();
        // Place line number annotations 
        // in the store when parsing a document.  
        // This is particularly useful when you want XmlError
        // objects to contain line numbers.
        options = options.setLoadLineNumbers();
        // Set when loading from an InputStream or File, so that
        // the loader will compute a 160-bit SHA-1 message digest of the XML
        // file while loading it and make it available via 
        // the XmlObject.documentProperties().getMessageDigest();
        // The schema compiler uses message digests to detect and eliminate 
        // duplicate imported xsd files.
        options = options.setLoadMessageDigest();
        if(namespaces != null) {
            options.setLoadAdditionalNamespaces(namespaces);
        }
        XmlObject schema = XmlObject.Factory.parse(schemaFile, options);
        
        return schema;
    }    

    /**
     * 
     * @param importMap
     * @param typeNodes
     * @param namespaces
     * @return
     * @throws XmlException
     */
    public List<XmlObject> loadXSD(Map importMap, List<Node> typeNodes, Map namespaces) throws XmlException {
        List<XmlObject> schemaList = this.loadXSD(typeNodes, namespaces);
        List<XmlObject> importedXmlObjectScemas = this.loadXSD(importMap, namespaces);
        if(importedXmlObjectScemas != null) {
            schemaList.addAll(importedXmlObjectScemas);
        }
        return schemaList;
    }
    
    /**
     * Gets a list of imported XSD file names.
     * @param importsMap
     * @return an ArrayList of String denoting the names of xsd files
     */
    List<String> getSchemaFileList(Map importsMap) {
        List<String> schemaFileList = new ArrayList<String>();
        Set importKeySet = importsMap.keySet();
        String key = null;
        Vector vectorValue = null;
        String stringValue = null;
        Import importImpl = null;
        Object elementValue = null;
        // Force declaration of soap schemas
//        importsMap.put(GenericConstants.SOAP_ENCODING_SCHEMA_URL, GenericConstants.SOAP_ENCODING_SCHEMA_URL);
//        importsMap.put(GenericConstants.WSDL_SOAP_SCHEMA_URL, GenericConstants.WSDL_SOAP_SCHEMA_URL);
        for(Iterator iterator = importKeySet.iterator();
            iterator.hasNext() == true;) {
            key = (String) iterator.next();
            if(key != null) {
                elementValue = importsMap.get(key);
                if(elementValue != null) {
                    if(elementValue instanceof String) {
                        stringValue = (String) elementValue;
                        if(stringValue.endsWith(".xsd") == true) {
                            schemaFileList.add(stringValue);
                        }
                        if(stringValue.equals(SOAP_ENCODING_SCHEMA_URL) == true) {
                            schemaFileList.add(stringValue);
                        }
                        if(stringValue.equals(WSDL_SOAP_SCHEMA_URL) == true) {
                            schemaFileList.add(stringValue);
                        }
                    }
                    if(elementValue instanceof Vector) {
                        vectorValue = (Vector) elementValue;
                        for(Iterator innerIterator = vectorValue.iterator();
                            innerIterator.hasNext() == true;) {
                            importImpl = (Import) innerIterator.next();
                            if(importImpl != null) {
                                stringValue = importImpl.getLocationURI();
                                if(stringValue.endsWith(".xsd") == true) {
                                    schemaFileList.add(stringValue);
                                }
                                if(stringValue.equals(SOAP_ENCODING_SCHEMA_URL) == true) {
                                    schemaFileList.add(stringValue);
                                }
                                if(stringValue.equals(WSDL_SOAP_SCHEMA_URL) == true) {
                                    schemaFileList.add(stringValue);
                                }
                            }
                        }
                    }
                }
            }
        }
        return schemaFileList;
    }
    
    /**
     * Compiles a list of schema objects and returns a finite set of
     * XMLSchema component definitions
     * 
     * @param schemas
     * @return a SchemaTypeSystem which is a finite set of XMLSchema component definitions
     * @throws XmlException
     */
    public SchemaTypeSystem compileSchemas(XmlObject[] schemas) throws XmlException {
        XmlOptions options = new XmlOptions();
        // Enable Network import for downloads
        options = options.setCompileDownloadUrls();
        // Disable particle valid (restriction) rule
        options = options.setCompileNoPvrRule();
        // Disable unique particle attribute on rule
        options = options.setCompileNoUpaRule();
        
        SchemaTypeSystem types = XmlBeans.compileXsd(schemas, XmlBeans.getBuiltinTypeSystem(), options);
        if(types != null) {
            this.schemaTypeSystem = types;
        }
        return types;
    }
    
    /**
     * Compiles a list of schema objects and returns a finite set of
     * XMLSchema component definitions
     * 
     * @param importMap
     * @param typeNodes
     * @param namespaces
     * @return a SchemaTypeSystem which is a finite set of XMLSchema component definitions
     * @throws XmlException
     */
    public SchemaTypeSystem loadAndCompileXSD(Map importMap, List<Node> typeNodes, Map namespaces) throws XmlException {
        SchemaTypeSystem typeSystem = null;
        List<XmlObject> schemaList = this.loadXSD(importMap, typeNodes, namespaces);
        //XmlObject[] schemas = (XmlObject[])schemaList.toArray(new XmlObject[schemaList.size()]);
        XmlObject[] schemas = toArray(schemaList, XmlObject.class);
        if(schemas != null) {
            typeSystem = this.compileSchemas(schemas);
        }
        return typeSystem;
    }

    /**
     * Given an elementName, retrieve it's associated XML Schema Type
     * 
     * @param elementName
     * @return a SchemaType
     */
    public SchemaType retrieveElement(String elementName) {
        if(elementName == null) {
            return null;
        }
        SchemaType[] globalElements = this.schemaTypeSystem.documentTypes();
        SchemaType element = null;
        for(int index = 0; index < globalElements.length; index++) {
            QName qName = globalElements[index].getDocumentElementName();
            String localPart = qName.getLocalPart();
            if(elementName.equals(localPart) == true) {
                element = globalElements[index];
                break;
            }
        }
        return element;
    }
    
    /**
     * @return Returns the schemaTypeSystem.
     */
    public SchemaTypeSystem getSchemaTypeSystem() {
        return this.schemaTypeSystem;
    }
    
    /**
     * Returns the builtin type system. This SchemaTypeSystem contains
     * only the 46 builtin types defined by the XML Schema specification. 
     * @return a SchemaTypeSystem of the builtin type system 
     */
    public SchemaTypeSystem getBuiltInTypeSystem() {
        return XmlBeans.getBuiltinTypeSystem();
    }

    /**
     * We can now use this to produce lists of ints or Strings:
     * List<Integer> ints = Lists.toList(1, 2, 3);
     * List<String> names = Lists.toList("Gopalan", "Suresh", "Raj");
     * 
     * @param <T>
     * @param arr
     * @return
     */
    public static <T> List<T> toList(T... array) {
        List<T> list = new ArrayList<T>();
        for (T arrayElement : array) {
        	list.add(arrayElement);
        }
        return list;
    }
    
    /**
     * 
     * @param <T>
     * @param collection
     * @param componentType
     * @return
     */
    @SuppressWarnings("unchecked")
    static public <T> T[] toArray(Collection<T> collection, Class<T> componentType) {
        // unchecked cast
        T[] array = (T[]) java.lang.reflect.Array.newInstance(componentType,
                collection.size());
        int index = 0;
        for (T value : collection) {
            array[index++] = value;
        }
        return array;
    }        
    
    /**
     * @param args
     */
    public static void main(String[] args) {
        // TODO Auto-generated method stub

    }

}
