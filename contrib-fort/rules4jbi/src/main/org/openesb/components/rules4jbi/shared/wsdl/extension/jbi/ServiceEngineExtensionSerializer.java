/*
 * @(#)ServiceEngineExtensionSerializer.java        $Revision: 1.1 $ $Date: 2008/11/24 12:49:33 $
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

import java.io.PrintWriter;
import java.io.Serializable;

import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.xml.namespace.QName;

/**
 * This class is responsible for serializing service engine wsdl binding extensions
 * into a <code>PrintWriter</code>.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/11/24 12:49:33 $
 * 
 * @see javax.wsdl.extensions.ExtensionSerializer
 * @since 0.1
 */
public class ServiceEngineExtensionSerializer implements ExtensionSerializer, Serializable {
    
    private static final long serialVersionUID = -2449366228226639442L;

    public void marshall(Class parentType,
                         QName elementType,
                         ExtensibilityElement extensibilityElement,
                         PrintWriter printWriter,
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
        
        String prefix = definition.getPrefix(ServiceEngineExtensibilityElement.NAMESPACE_URI);
        
        if (prefix == null) {
            throw new WSDLException(WSDLException.NO_PREFIX_SPECIFIED, 
                    "Prefix not specified for " + ServiceEngineExtensibilityElement.NAMESPACE_URI);
        }

        StringBuilder sb = new StringBuilder();

        sb.append("    <");
        if (!prefix.equals("")) {
            sb.append(prefix);
            sb.append(":");
        }
        sb.append(ServiceEngineExtensibilityElement.LOCAL_NAME);
        sb.append("/>");

        printWriter.println(sb.toString());
    }
}
