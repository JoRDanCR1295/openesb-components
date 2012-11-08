/*
 * @(#)DOMSchemaOutputResolver.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.SchemaOutputResolver;
import javax.xml.transform.Result;
import javax.xml.transform.dom.DOMResult;

import org.w3c.dom.Document;

import nu.xom.Element;
import nu.xom.converters.DOMConverter;

import org.openesb.components.rules4jbi.shared.logging.Logger;
import org.openesb.components.rules4jbi.shared.logging.LoggerImpl;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
 * 
 * @since 0.1
 */
public class DOMSchemaOutputResolver extends SchemaOutputResolver {
    
    private Logger logger = null;

    /** For each generated schema, contains the targetNamespace as key and the schema as value. */
    private Map<String, DOMResult> schemas = null;
    
    public DOMSchemaOutputResolver() {
        logger = new LoggerImpl(java.util.logging.Logger.getLogger(DOMSchemaOutputResolver.class.getName()));

        schemas = new HashMap<String, DOMResult>();
    }

    public List<Element> getSchemas() {
        List<Element> result = new ArrayList<Element>();
        
        for (DOMResult domResult : schemas.values()) {
            Document document = (Document) domResult.getNode();

            nu.xom.Document xomDocument = DOMConverter.convert(document);
            
            result.add(xomDocument.getRootElement());
        }
        
        return result;
    }

    @Override
    public Result createOutput(String namespaceUri, String suggestedFileName) throws IOException {
        logger.fine("Creating schema: namespaceURI=%s, suggestedFileName=%s", namespaceUri, suggestedFileName);

        if (namespaceUri == null) {
            throw new NullPointerException("Received null namespaceUri");
        }

        //TODO: use custom runtime exceptions here
        if (namespaceUri.equals("")) {
            throw new IOException("Empty namespace not allowed");
        }

        if (schemas.containsKey(namespaceUri)) {
            throw new IOException("This namespace is already registered");
        }

        DOMResult domResult = new DOMResult();
        domResult.setSystemId(namespaceUri);
        schemas.put(namespaceUri, domResult);

        return domResult;
    }
}
