/*
 * ChainBuilder ESB
 *          Visual Enterprise Integration
 * 
 * Copyright (C) 2006 Bostech Corporation
 * 
 * This program is free software; you can redistribute it and/or modify it 
 * under the terms of the GNU General Public License as published by the 
 * Free Software Foundation; either version 2 of the License, or (at your option) 
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, 
 * but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License 
 * for more details.
 * 
 * You should have received a copy of the GNU General Public License along with 
 * this program; if not, write to the Free Software Foundation, Inc., 
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *
 * $Id: BaseCommonAttributeDeserializer.java,v 1.1.1.1 2007/04/09 17:49:31 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.component.util.wsdl;

import java.net.URI;

import javax.jbi.messaging.MessageExchange.Role;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.w3c.dom.Element;

import com.ibm.wsdl.util.xml.DOMUtils;

public class BaseCommonAttributeDeserializer implements ExtensionDeserializer {

    private static final Log log = LogFactory.getLog(BaseCommonAttributeDeserializer.class);
    public static final String ROLE = "role";
    public static final String ROLE_CONSUMER = "consumer";
    public static final String ROLE_PROVIDER = "provider";
    
//    public static final String DEFAULT_MEP = "defaultMep";
//    public static final String DEFAULT_MEP_IN_ONLY = "in-only";
//    public static final String DEFAULT_MEP_ROBUST_IN_ONLY = "robust-in-only";
//    public static final String DEFAULT_MEP_IN_OUT = "in-out";
    
    public static final String DEFAULT_OPERATION = "defaultOperation";
    
//    public static final String WSDL2_NS = "http://www.w3.org/2004/08/wsdl/";
    
    
    
    public ExtensibilityElement unmarshall(
            Class parentType,
            QName elementType,
            Element el,
            Definition def,
            ExtensionRegistry extReg)
            throws WSDLException {
        
        BaseCommonAttribute baseCommonAttribute = (BaseCommonAttribute) extReg.createExtension(parentType, elementType);

        String role = DOMUtils.getAttribute(el, ROLE);
        log.debug("role: " + role);
        if (role == null) {
//            throw new WSDLException(WSDLException.OTHER_ERROR, "Role must be specified");
        } else if (ROLE_CONSUMER.equals(role)) {
            baseCommonAttribute.setRole(Role.CONSUMER);
        } else if (ROLE_PROVIDER.equals(role)) {
            baseCommonAttribute.setRole(Role.PROVIDER);
        } else {
            throw new WSDLException(WSDLException.OTHER_ERROR, "Unrecognized role: " + role);
        }
        
        
        String defaultMep = DOMUtils.getAttribute(el, WsdlMepConstants.DEFAULT_MEP);
        log.debug("DefeaultMep: " + defaultMep);
        if (defaultMep == null) {
            defaultMep = WsdlMepConstants.DEFAULT_MEP_IN_OUT;
        }
        if (WsdlMepConstants.DEFAULT_MEP_IN_ONLY.equals(defaultMep) ||
        		WsdlMepConstants.DEFAULT_MEP_ROBUST_IN_ONLY.equals(defaultMep) ||
        		WsdlMepConstants.DEFAULT_MEP_IN_OUT.equals(defaultMep)) {
            baseCommonAttribute.setDefaultMep(URI.create(WsdlMepConstants.WSDL2_NS + defaultMep));
        }
        
        QName defaultOperation = DOMUtils.getQualifiedAttributeValue(el, DEFAULT_OPERATION, null, false, def);
        if (defaultOperation != null) {
            baseCommonAttribute.setDefaultOperation(defaultOperation);
        }
        
        return baseCommonAttribute;
    }

}
