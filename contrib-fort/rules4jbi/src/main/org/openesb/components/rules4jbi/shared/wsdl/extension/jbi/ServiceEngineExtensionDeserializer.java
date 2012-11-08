/*
 * @(#)ServiceEngineExtensionDeserializer.java        $Revision: 1.1 $ $Date: 2008/11/24 12:49:33 $
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

package org.openesb.components.rules4jbi.shared.wsdl.extension.jbi;

import java.io.Serializable;

import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;

import org.w3c.dom.Element;

/**
 * This class is responsible for deserializing <code>org.w3c.dom.Element</code>s
 * into service engine wsdl binding extensions.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/11/24 12:49:33 $
 * 
 * @see javax.wsdl.extensions.ExtensionDeserializer
 * @since 0.1
 */
public class ServiceEngineExtensionDeserializer implements ExtensionDeserializer, Serializable {
    
    private static final long serialVersionUID = -2449366228226639442L;
    
    public ExtensibilityElement unmarshall(Class parentType,
                                           QName elementType,
                                           Element element,
                                           Definition definition,
                                           ExtensionRegistry extensionRegistry) throws WSDLException {
        
        if (parentType != Binding.class) {
            throw new WSDLException(WSDLException.CONFIGURATION_ERROR,
                    "Service engine extension must be used as a child of <wsdl:binding> element");
        }

        if (!ServiceEngineExtensibilityElement.QUALIFIED_NAME.equals(elementType)) {
            throw new WSDLException(WSDLException.CONFIGURATION_ERROR,
                    "Invalid namespace and/or local name for the service engine extension: " + elementType);
        }
        
        return extensionRegistry.createExtension(Binding.class, ServiceEngineExtensibilityElement.QUALIFIED_NAME);
    }
}
