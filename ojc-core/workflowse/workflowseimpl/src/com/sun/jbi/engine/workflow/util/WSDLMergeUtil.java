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
 * @(#)$Id: WSDLMergeUtil.java,v 1.3 2010/02/15 19:24:15 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.util;

import java.net.URI;
import java.net.URL;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.Import;
import javax.wsdl.Message;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.wsdl.Types;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.schema.Schema;
import javax.wsdl.extensions.schema.SchemaImport;
import javax.wsdl.extensions.schema.SchemaReference;
import javax.xml.namespace.QName;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * This Utility merges WSDLs imported or included via wsdl:import, wsdl:include
 * into one WSDL and removes wsdl:import, wsdl:include
 * 
 * @author Sun Microsystems
 * 
 */
public class WSDLMergeUtil {

    /**
     * This method recursively merges WSDLs imported or included via
     * wsdl:import, wsdl:include into one WSDL and removes wsdl:import,
     * wsdl:include
     * 
     * @param oneWSDL
     * @param childWSDL
     * @throws WSDLException
     */
    public static void mergeWSDL(Definition oneWSDL, Definition childWSDL)
            throws WSDLException {
        // If it is the first wsdl getting processed
        if (oneWSDL.getTargetNamespace() == null
                && childWSDL.getTargetNamespace() != null) {
            oneWSDL.setTargetNamespace(childWSDL.getTargetNamespace());
            oneWSDL
                    .setDocumentationElement(childWSDL
                            .getDocumentationElement());
            oneWSDL.setDocumentBaseURI(childWSDL.getDocumentBaseURI());
            oneWSDL.setExtensionRegistry(childWSDL.getExtensionRegistry());
            oneWSDL.setTypes(oneWSDL.createTypes());
            Map<String, String> nameSpaces = childWSDL.getNamespaces();
            for (Map.Entry<String, String> mapEntry : nameSpaces.entrySet()) {
                oneWSDL.addNamespace(mapEntry.getKey(), mapEntry.getValue());
            }

        } else {
            // Add namespaces if they are new
            Map<String, String> nameSpaces = childWSDL.getNamespaces();
            for (Map.Entry<String, String> mapEntry : nameSpaces.entrySet()) {
                if (!oneWSDL.getNamespaces().values().contains(
                        ((mapEntry.getValue())))) {
                    if (oneWSDL.getNamespace(mapEntry.getKey()) == null) {
                        oneWSDL.addNamespace(mapEntry.getKey(), mapEntry
                                .getValue());
                    } else {
                        createNewPrefix(oneWSDL, mapEntry.getValue());
                    }
                }
            }
        }
            Map<QName, Object> extensionAttributes = childWSDL
                    .getExtensionAttributes();
            if (extensionAttributes != null) {
                for (Map.Entry<QName, Object> mapEntry : extensionAttributes
                        .entrySet()) {
                    if (oneWSDL.getExtensionAttribute(mapEntry.getKey()) == null) {
                        oneWSDL.getExtensionAttributes().put(mapEntry.getKey(),
                                mapEntry.getValue());
                    }
                }
            }
            // Add Types
            Types types = childWSDL.getTypes();
            // See if there are schema types
            List<ExtensibilityElement> allTypes = types
                    .getExtensibilityElements();
            for (ExtensibilityElement type : allTypes) {
                if (type instanceof Schema) {
                    Schema toAdd = setNameSpaceURIAbsolute(oneWSDL
                            .getDocumentBaseURI(), (Schema) type);
                    oneWSDL.getTypes().addExtensibilityElement(toAdd);
                } else {
                    oneWSDL.getTypes().addExtensibilityElement(type);
                }
            }
        
        // Add Import
        Map<String, List<Import>> importWSDLs = childWSDL.getImports();
        if (importWSDLs != null && importWSDLs.size() > 0) {
            Collection<List<Import>> wsdls = importWSDLs.values();
            for (List<Import> childWSDLs : wsdls) {
                for (Import cwsdl : childWSDLs) {
                    mergeWSDL(oneWSDL, cwsdl.getDefinition());
                }
            }
        }
        // Add Messages
        Map<QName, Message> msgs = childWSDL.getMessages();
        for (Map.Entry<QName, Message> mapEntry : msgs.entrySet()) {
            if (oneWSDL.getMessage(mapEntry.getKey()) == null) {
                oneWSDL.addMessage(mapEntry.getValue());
            }
        }
        // Add PortType
        Map<QName, PortType> portTypes = childWSDL.getPortTypes();
        for (Map.Entry<QName, PortType> mapEntry : portTypes.entrySet()) {
            if (oneWSDL.getPortType(mapEntry.getKey()) == null) {
                oneWSDL.addPortType(mapEntry.getValue());
            }
        }

        // Add Bindings
        Map<QName, Binding> bindings = childWSDL.getBindings();
        for (Map.Entry<QName, Binding> mapEntry : bindings.entrySet()) {
            if (oneWSDL.getBinding(mapEntry.getKey()) == null) {
                oneWSDL.addBinding(mapEntry.getValue());
            }
        }

        // Add Services
        Map<QName, Service> services = childWSDL.getServices();
        for (Map.Entry<QName, Service> mapEntry : services.entrySet()) {
            if (oneWSDL.getService(mapEntry.getKey()) == null) {
                oneWSDL.addService(mapEntry.getValue());
            }
        }

        // Add ExtensiveElements
        List<ExtensibilityElement> extensibilityElements = childWSDL
                .getExtensibilityElements();
        for (ExtensibilityElement ext : extensibilityElements) {
            oneWSDL.addExtensibilityElement(ext);
        }
    }

    private static Schema setNameSpaceURIAbsolute(String baseURI, Schema schema)
            throws WSDLException {
        // TODO Auto-generated method stub
       
        NodeList importNodes = schema.getElement().getElementsByTagNameNS("http://www.w3.org/2001/XMLSchema", "import") ;
        NodeList includeNodes = schema.getElement().getElementsByTagNameNS("http://www.w3.org/2001/XMLSchema", "include") ;
        NodeList reDefineNodes = schema.getElement().getElementsByTagNameNS("http://www.w3.org/2001/XMLSchema", "redefine") ;
        changeNameSpaceURIAbsolute (importNodes, baseURI);
        changeNameSpaceURIAbsolute (includeNodes, baseURI);
        changeNameSpaceURIAbsolute (reDefineNodes, baseURI);
            
      
        
        Map<String, List<SchemaImport>> schemaImports = schema.getImports();
        if (schemaImports != null && schemaImports.size() > 0) {
            Collection<List<SchemaImport>> schemas = schemaImports.values();
            for (List<SchemaImport> childSchemas : schemas) {
                for (SchemaImport cxsd : childSchemas) {
                    Schema importedxsd = cxsd.getReferencedSchema();
                    setNameSpaceURIAbsolute(baseURI, importedxsd);
                }
            }
        }
        List<SchemaReference> schemaIncludes = schema.getIncludes();
        for (SchemaReference cxsd : schemaIncludes) {
            Schema includedxsd = cxsd.getReferencedSchema();
            setNameSpaceURIAbsolute(baseURI, includedxsd);
        }

        List<SchemaReference> schemaIRedefines = schema.getRedefines();
        for (SchemaReference cxsd : schemaIRedefines) {
              Schema redefinedXSD = cxsd.getReferencedSchema();
            setNameSpaceURIAbsolute(baseURI, redefinedXSD);            
        }
        return schema;
    }

    private static void changeNameSpaceURIAbsolute(NodeList nodeList, String baseURI)  throws WSDLException {
        // TODO Auto-generated method stub
        for (int i = 0;  i < nodeList.getLength(); i++) {
            Node importNode = nodeList.item(i);
            String location =  ( (Element) importNode).getAttribute("schemaLocation");
            if (location != null) {
                try {
                    URI oldURI = new URI(location);
                    if (!oldURI.isAbsolute()) {
                        URL baseURL = new URL(baseURI);
                        URI newURI = (new URL(baseURL, location))
                                .toURI();
                        ( (Element) importNode).setAttribute("schemaLocation", newURI.toString());                                
                    }
                } catch (Exception e) {
                    // TODO Auto-generated catch block
                    throw new WSDLException("WLM-6110", I18n
                            .loc("WLM-6110: can not set absolute URI"),
                            e);

                }
            }            
        }          
    }

    private static void createNewPrefix(Definition oneWSDL, String value)
            throws WSDLException {
        // TODO Auto-generated method stub
        String prefixBase = "ns";
        int i = 0;
        while (true) {
            String newPrefix = prefixBase + i;
            if (oneWSDL.getNamespace(newPrefix) == null) {
                oneWSDL.addNamespace(newPrefix, value);
                return;
            }
            i++;
        }
    }

}
