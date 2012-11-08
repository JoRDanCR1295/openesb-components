/*
 * @(#)PartnerLinkExtensibilityElement.java        $Revision: 1.1 $ $Date: 2008/11/24 12:49:29 $
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

package org.openesb.components.rules4jbi.shared.wsdl.extension.bpel;

import java.io.Serializable;

import javax.wsdl.Definition;
import javax.wsdl.PortType;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

import nu.xom.Attribute;
import nu.xom.Element;
import nu.xom.Nodes;
import nu.xom.XPathContext;

/**
 * This class represents WSDL BPEL extension relevant to this service engine.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/11/24 12:49:29 $
 * 
 * @see javax.wsdl.extensions.ExtensibilityElement
 * @since 0.3
 */
public class PartnerLinkExtensibilityElement implements ExtensibilityElement, Serializable {
    
    public static final String PREFERRED_NAMESPACE_PREFIX = "plnk";
    
    public static final String NAMESPACE_URI = "http://docs.oasis-open.org/wsbpel/2.0/plnktype";
    
    public static final String LOCAL_NAME = "partnerLinkType";
    
    public static final QName QUALIFIED_NAME = new QName(NAMESPACE_URI, LOCAL_NAME);

    private static final long serialVersionUID = -2449366228226639442L;
    
    private QName elementType = QUALIFIED_NAME;
    
    private Boolean required = null;
    
    private String typeName = null;
    
    private String roleName = null;
    
    private PortType portType = null;

    public QName getElementType() {
        return elementType;
    }

    public Boolean getRequired() {
        return required;
    }

    public void setElementType(QName elementType) {
        this.elementType = elementType;
    }

    public void setRequired(Boolean required) {
        this.required = required;
    }
    
    public void parse(Element element, Definition definition) throws WSDLException {
        XPathContext context = new XPathContext("plnk", PartnerLinkExtensibilityElement.NAMESPACE_URI);
        
        try {
            Nodes query = element.query("/plnk:partnerLinkType/@name", context);
            typeName = query.get(0).getValue();

            query = element.query("/plnk:partnerLinkType/plnk:role/@name", context);
            roleName = query.get(0).getValue();

            query = element.query("/plnk:partnerLinkType/plnk:role/@portType", context);
            String[] portTypeName = query.get(0).getValue().split(":");

            String namespace = definition.getNamespace(portTypeName[0]);

            portType = definition.getPortType(new QName(namespace, portTypeName[1]));

        } catch (Exception e) {
            throw new WSDLException(
                    WSDLException.INVALID_WSDL, "Could not parse the WSDL: " + e.getMessage(), e);
        }
    }
    
    public String toXML(final Definition definition) throws WSDLException {
        if (typeName == null) {
            throw new WSDLException(WSDLException.CONFIGURATION_ERROR, "Partner link type name not specified");
        }
        
        if (roleName == null) {
            throw new WSDLException(WSDLException.CONFIGURATION_ERROR, "Partner link role name not specified");
        }
        
        if (portType == null) {
            throw new WSDLException(WSDLException.CONFIGURATION_ERROR, "Port type not specified");
        }
        
        QName portTypeName = portType.getQName();
        
        final String prefix = definition.getPrefix(portTypeName.getNamespaceURI());
        
        /*
         * The port type will belong to the target namespace of its enclosing definitions
         * element, so there must be also a prefix defined (by convention, 'tns')
         * for this namespace, so that we can reference it.
         */
        if (prefix == null || prefix.equals("")) {
            throw new WSDLException(WSDLException.NO_PREFIX_SPECIFIED,
                    "Prefix not specified for port type: " + portTypeName);
        }
        
        return createPartnerLink(typeName, roleName, prefix + ":" + portTypeName.getLocalPart()).toXML();
    }
    
    static Element createPartnerLink(String typeName, String roleName, String portTypePrefixedName) {
        Element partnerLinkType = new Element(PREFERRED_NAMESPACE_PREFIX + ":" + LOCAL_NAME, NAMESPACE_URI);
        
        Attribute typeNameAttribute = new Attribute("name", typeName);
        partnerLinkType.addAttribute(typeNameAttribute);

        Element role = new Element(PREFERRED_NAMESPACE_PREFIX + ":role", NAMESPACE_URI);

        Attribute roleNameAttribute = new Attribute("name", roleName);
        role.addAttribute(roleNameAttribute);
        
        Attribute portTypeAttribute = new Attribute("portType", portTypePrefixedName);
        role.addAttribute(portTypeAttribute);
        
        partnerLinkType.appendChild(role);
        
        return partnerLinkType;
    }
    
    public String getRoleName() {
        return roleName;
    }

    public void setRoleName(String roleName) {
        this.roleName = roleName;
    }

    public String getTypeName() {
        return typeName;
    }

    public void setTypeName(String typeName) {
        this.typeName = typeName;
    }

    public PortType getPortType() {
        return portType;
    }

    public void setPortType(PortType portType) {
        this.portType = portType;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        
        sb.append("Partner Link; type name='");
        sb.append(typeName);
        sb.append("'; role name='");
        sb.append(roleName);
        sb.append("'; portType='");
        sb.append(portType.getQName());
        sb.append("'");
        
        return sb.toString();
    }
}
