/*
 * @(#)SecondarySchemaCreator.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.netbeans.wsdl.schema;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import nu.xom.Attribute;
import nu.xom.Element;

import org.openesb.components.rules4jbi.shared.util.XOMUtils;
import org.openesb.components.rules4jbi.shared.wsdl.WSDLConstants;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
 * 
 * @since 0.1
 */
public class SecondarySchemaCreator {
    
    private final BusinessObjectManager businessObjectManager;

    public SecondarySchemaCreator(BusinessObjectManager businessObjectManager) {
        
        this.businessObjectManager = businessObjectManager;
    }

    public List<Element> createSchemas() {
        Set<Class<?>> objectTypes = businessObjectManager.getObjectTypes();
        
        Class<?>[] objectTypesArray = objectTypes.toArray(new Class<?>[objectTypes.size()]);
        
        try {
            JAXBContext context = JAXBContext.newInstance(objectTypesArray);
            DOMSchemaOutputResolver resolver = new DOMSchemaOutputResolver();
            context.generateSchema(resolver);

            List<Element> schemas = resolver.getSchemas();
            List<Element> result = new ArrayList<Element>();
            
            for (Element schema : schemas) {
                result.add(removeSchemaLocationFromImports(schema));
            }
            
            return result;
            
        } catch (IOException e) {
            throw new RuntimeException(e);
            
        } catch (JAXBException e) {
            throw new RuntimeException(e);
        }
    }
    
    static Element removeSchemaLocationFromImports(Element schema) {
        assert schema != null;
        assert schema.getLocalName().equals("schema");
        assert schema.getNamespaceURI().equals(WSDLConstants.XML_SCHEMA_NAMESPACE_URI);

        /* to avoid side-effects */
        Element result = (Element) schema.copy();
        
        List<Element> imports = XOMUtils.asList(
                result.getChildElements("import", WSDLConstants.XML_SCHEMA_NAMESPACE_URI));

        for (Element element : imports) {
            Attribute schemaLocation =  element.getAttribute("schemaLocation");
            
            if (schemaLocation != null) {
                element.removeAttribute(schemaLocation);
            }
        }

        return result;
    }
    
    //TODO: Not used anymore. We can safely remove it in the future.
    static Element removeImports(Element schema) {
        assert schema != null;
        assert schema.getLocalName().equals("schema");
        assert schema.getNamespaceURI().equals(WSDLConstants.XML_SCHEMA_NAMESPACE_URI);

        List<Element> imports = XOMUtils.asList(
                schema.getChildElements("import", WSDLConstants.XML_SCHEMA_NAMESPACE_URI));

        for (Element element : imports) {
            schema.removeChild(element);
        }

        return schema;
    }
}
