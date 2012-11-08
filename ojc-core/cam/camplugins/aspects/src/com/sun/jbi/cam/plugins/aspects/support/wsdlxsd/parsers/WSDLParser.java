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
 * @(#)WSDLParser.java 
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
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.wsdl.Definition;
import javax.wsdl.Types;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.schema.Schema;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;

import org.w3c.dom.Node;

/**
 * @author graj
 *
 */
public class WSDLParser implements Serializable {
    
    public static final String WSDL_VERBOSE_KEY = "javax.wsdl.verbose"; //$NON-NLS-1$
    public static final String WSDL_IMPORT_DOCUMENTS_KEY = "javax.wsdl.importDocuments"; //$NON-NLS-1$
    public static final String WSDL_SCHEMA_URL = "http://schemas.xmlsoap.org/wsdl/"; //$NON-NLS-1$
    
    
    Definition definition = null;
	private static final long serialVersionUID = 1L;


    /**
     * 
     */
    public WSDLParser() {
        super();
    }

    /**
     * @param wsdlURI a URI (can be a filename or URL) pointing to a WSDL XML definition.
     * @return the definition.
     * @throws WSDLException
     */
    public Definition loadWsdl(String wsdlURI) throws WSDLException {
        WSDLFactory factory = null;
        WSDLReader reader = null;
        Definition definition = null;

        factory = WSDLFactory.newInstance();
        reader = factory.newWSDLReader();
        // If set to true, status messages will be displayed.
        reader.setFeature(WSDL_VERBOSE_KEY, true);
        // If set to true, imported WSDL documents will be retrieved and processed
        reader.setFeature(WSDL_IMPORT_DOCUMENTS_KEY, true);

        // Read the WSDL document accessible via the specified
        // URI into a WSDL definition.
        definition = reader.readWSDL(wsdlURI);
        if (definition != null) {
            this.definition = definition;
        }

        return definition;
    }
    
    /**
     * Returns an ArrayList of org.w3c.dom.Node(s),
     * the extensibility elements for each of the XSD types
     * present in this WSDL
     * 
     * @return an ArrayList of org.w3c.dom.Node(s)
     */
    public List<Node> retrieveTypeNodes() {
        List extensibilityElements = null;
        List<Node> typeNodes = new ArrayList<Node>();
        
        Types types = this.definition.getTypes();
        Schema schema = null;
        if(types != null)  {
            extensibilityElements = types.getExtensibilityElements();
            for(Iterator iterator = extensibilityElements.iterator();
            iterator.hasNext() == true;) {
                schema = (Schema) iterator.next();
                if(schema != null) {
                    Node node = schema.getElement();
                    if(node != null) {
                        typeNodes.add(node);
                    }
                }
            }
        }
        
        return typeNodes;
    }
    
    /**
     * Returns a HashMap of namespaces defined in this WSDL document
     * @return a HashMap of String key value pairs
     */
    public Map<String /*prefix*/, String /*namespaceUri*/> retrieveNamespaces() {
        return this.definition.getNamespaces();
    }
    
    /**
     * Returns a HashMap of imports that are defined in this WSDL document
     * @return a HashMap of String key value pairs
     */
    public Map retrieveImports() {
        return this.definition.getImports();
    }
    
    

    /**
     * @return Returns the definition.
     */
    public Definition getDefinition() {
        return this.definition;
    }
    
    /**
     * @param args
     */
    public static void main(String[] args) {
        // TODO Auto-generated method stub

    }

}
