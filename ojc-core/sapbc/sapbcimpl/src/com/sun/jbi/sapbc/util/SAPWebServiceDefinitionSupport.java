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
 * @(#)SAPWebServiceDefinitionSupport.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc.util;

import com.sun.jbi.internationalization.Messages;
import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.logging.Logger;
import javax.wsdl.Definition;
import javax.wsdl.Types;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.schema.Schema;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.wsdl.xml.WSDLWriter;
import javax.xml.namespace.QName;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * Utility class for the examination of Web service definitions generated
 * using SAP NetWeaver conventions.
 *
 * @author Noel Ang (noel.ang@sun.com)
 */
public class SAPWebServiceDefinitionSupport implements ExtensionDeserializer {
    
    public SAPWebServiceDefinitionSupport(Definition def) throws WSDLException {
        mDefinition = def;
        scanExtensibles(def);
    }

    public ExtensibilityElement unmarshall(
            Class parentType,
            QName elementType,
            Element el,
            Definition def,
            ExtensionRegistry extReg)
            throws WSDLException {
        
        // SAPWebServiceDefinitionSupport interjects itself as an
        // ExtensionDeserializer to capture XSD Schemas embedded in SAP
        // NetWeaver-generated WSDL.
        
        ExtensionDeserializer deserializer;
        
        // Encountered schemas are stored in an internal map for later utilization.
        mSchemaMap.put(el.getNamespaceURI(), el);
        
        // Then, for every node captured, further processing of that node is
        // delegated to the default or previous ExtensionDeserializer
        // (the deserializer that would have received the nodes in the first
        // place had this object not interjected itself.
        if (mPreviousDeserializer != null) {
            deserializer = mPreviousDeserializer;
        } else {
            deserializer = extReg.getDefaultDeserializer();
        }
        return deserializer.unmarshall(parentType, elementType, el, def, extReg);
    }

    private ExtensibilityElement lookup(QName qname) throws WSDLException {
        
        // Check the cache first
        ExtensibilityElement elem = mElementMap.get(qname);
        if (elem != null) {
            return elem;
        }
        
        // Look for a schema with a matching namespace
        final String ns = qname.getNamespaceURI();
        final Node schema = mSchemaMap.get(ns);
        if (schema == null) {
            String msg = mMessages.getString(
                    "SAPWebServiceDefinitionSupport.No_schema_for_namespace", ns);
            throw new WSDLException(WSDLException.OTHER_ERROR, msg);
        }
        
        // Search for the element in the schema
        return null;
    }
    
    private final void scanExtensibles(Definition def) throws WSDLException {
        // Re-do conversion from WSDL Document to WSDL model
        // to allow my ExtensionsDeserializer an opportunity to capture
        // XSD Schema information.
        {
            WSDLFactory wsdlFact = WSDLFactory.newInstance();
            WSDLWriter writer = wsdlFact.newWSDLWriter();
            WSDLReader reader = wsdlFact.newWSDLReader();
            
            // Convert WSDL model to XML model
            Document doc = writer.getDocument(def);
            
            // Convert XML model back to WSDL model using my ExtensionsDeserializer
            ExtensionRegistry extReg = def.getExtensionRegistry();
            registerTo(extReg);
            reader.setExtensionRegistry(extReg);
            def = reader.readWSDL(def.getDocumentBaseURI(), doc);
        }
        
        Types types = def.getTypes();
        if (types != null) {
            List elems = types.getExtensibilityElements();
            for (ListIterator iter = elems.listIterator(); iter.hasNext(); ) {
                ExtensibilityElement elem = (ExtensibilityElement) iter.next();
                System.out.println(elem.getClass().getName());
                Schema s = (Schema) elem;
            }
        }
    }

    private final void registerTo(ExtensionRegistry extReg) {
        boolean replace = false;
        
        try {
            ExtensionDeserializer serializer =
                    mPreviousDeserializer =
                    extReg.queryDeserializer(javax.wsdl.Types.class, QNAME_XSD_TYPE);
            
            replace = (serializer != null
                    && !(serializer instanceof SAPWebServiceDefinitionSupport));
        } catch (WSDLException ex) {
            replace = true;
        }
        
        if (replace) {
            extReg.registerDeserializer(javax.wsdl.Types.class, QNAME_XSD_TYPE, this);
        }
    }

    private static final QName QNAME_XSD_TYPE =
            QName.valueOf("{http://www.w3.org/2001/XMLSchema}schema");
    
    private static final Messages mMessages =
            Messages.getMessages(SAPWebServiceDefinitionSupport.class);
    
    private ExtensionDeserializer mPreviousDeserializer;
    
    private final Logger mLogger = Logger.getLogger(getClass().getName());
    
    private final Definition mDefinition;
    
    private final Map<String, Node> mSchemaMap = new HashMap<String, Node>();
    
    private final Map<QName, ExtensibilityElement> mElementMap =
            new HashMap<QName, ExtensibilityElement>();
}
