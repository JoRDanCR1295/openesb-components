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
 * @(#)BPELExtDeserializer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl4j.ext.bpel.impl;

import java.io.Serializable;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;

import org.w3c.dom.Attr;
import org.w3c.dom.Element;

import com.ibm.wsdl.Constants;
import com.ibm.wsdl.util.xml.DOMUtils;
import com.ibm.wsdl.util.xml.QNameUtils;
import com.sun.wsdl4j.ext.bpel.PartnerLinkRole;
import com.sun.wsdl4j.ext.bpel.MessagePropertyAlias.Query;
import com.sun.wsdl4j.ext.bpel.impl.MessagePropertyAliasImpl.QueryImpl;
import com.sun.wsdl4j.ext.impl.NamespaceDeclarationsImpl;

/**
 * Deserializer for WSDL extensibility elements for BPEL 2.0.
 * 
 * @author Jun Xu
 * @version $Revision: 1.8 $
 */
public class BPELExtDeserializer implements ExtensionDeserializer,
        Serializable {

    private static final long serialVersionUID = 1L;

    public ExtensibilityElement unmarshall(Class parentType, QName elementType,
            Element el, Definition def, ExtensionRegistry extReg)
            throws WSDLException {
        
        if (BPELExtConstants.PROPERTY_ELEM.equals(elementType)) {
            //Property element
            return parseProperty(elementType, el, def);
        } else if (BPELExtConstants.PROPERTY_ALIAS_ELEM.equals(elementType)) {
            //Property Alias element
            return parsePropertyAlias(elementType, el, def);
        } else if (BPELExtConstants.PARTNER_LINK_TYPE_ELEM.equals(
                elementType)) {
            //Partner Link Type element
            return parsePartnerLinkType(elementType, el, def);
        } else {
            //Not recognized
            return null;
        }
    }
    
    protected MessagePropertyImpl parseProperty(QName elementType,
            Element elem, Definition wsdlDef) throws WSDLException {
        
        List remainingAttrs = DOMUtils.getAttributes(elem);
        String name = DOMUtils.getAttribute(elem, "name", remainingAttrs);
        QName qName = null;
        if (name != null) {
            qName = new QName(wsdlDef.getTargetNamespace(), name);
        }
        String prefixedType =
            DOMUtils.getAttribute(elem, "type", remainingAttrs);
        QName typeName = null;
        if (prefixedType != null) {
            typeName = DOMUtils.getQName(prefixedType, elem, wsdlDef); 
        }
        String requiredStr = DOMUtils.getAttributeNS(elem,
                Constants.NS_URI_WSDL,
                Constants.ATTR_REQUIRED);
        
        MessagePropertyImpl prop = new MessagePropertyImpl();
        prop.setElementType(elementType);
        prop.setName(qName);
        prop.setRequired(new Boolean(requiredStr));
        prop.setType(typeName);
        prop.setNamespaceDeclarations(
                NamespaceDeclarationsImpl.newInstance(elem));
        return prop;
    }
    
    protected MessagePropertyAliasImpl parsePropertyAlias(QName elementType, 
            Element elem, Definition wsdlDef) throws WSDLException {
        List<Attr> remainingAttrs = (List<Attr>)DOMUtils.getAttributes(elem);
        String name =
            DOMUtils.getAttribute(elem, "propertyName", remainingAttrs);
        QName propName = null;
        if (name != null) {
            propName = DOMUtils.getQName(name, elem, wsdlDef);
        }
        QName messageTypeName = null;
        String part = null;
        QName elementName = null;
        QName typeName = null;
        String nmPropName = null;
        if ((name = DOMUtils.getAttribute(
                elem, "messageType", remainingAttrs)) != null) {
            messageTypeName = DOMUtils.getQName(name, elem, wsdlDef); 
            part = DOMUtils.getAttribute(elem, "part", remainingAttrs); 
        } else if ((name = DOMUtils.getAttribute(
                elem, "element", remainingAttrs)) != null) {
            elementName = DOMUtils.getQName(name, elem, wsdlDef);
        } else if ((name = DOMUtils.getAttribute(
                elem, "type", remainingAttrs)) != null) {
            typeName = DOMUtils.getQName(name, elem, wsdlDef);
        }

        String requiredStr = DOMUtils.getAttributeNS(elem,
                Constants.NS_URI_WSDL,
                Constants.ATTR_REQUIRED);
        
        MessagePropertyAliasImpl propAlias =
            new MessagePropertyAliasImpl(wsdlDef);
        propAlias.setElementType(elementType);
        
        if (messageTypeName != null) {
            propAlias.setMessageTypeName(messageTypeName);
            propAlias.setPartName(part);
        } else if (elementName != null) {
            propAlias.setElementName(elementName);
        } else if (typeName != null) {
            propAlias.setTypeName(typeName);
        }
        
        for (Attr attr : remainingAttrs) {
			if ("nmProperty".equals(attr.getLocalName())) {
				propAlias.setNMProperty(attr.getValue());
				break;
			}
		}

        propAlias.setName(propName);
        propAlias.setRequired(new Boolean(requiredStr));
        propAlias.setNamespaceDeclarations(
                NamespaceDeclarationsImpl.newInstance(elem));
        
        Element tempEl = DOMUtils.getFirstChildElement(elem);

        while (tempEl != null) {
            if (QNameUtils.matches(BPELExtConstants.QUERY_ELEM, tempEl)) {
                propAlias.setQuery(parseQuery(tempEl));
            }
            tempEl = DOMUtils.getNextSiblingElement(tempEl);
        }

        return propAlias;
    }
    
    protected PartnerLinkTypeImpl parsePartnerLinkType(QName elementType,
            Element elem, Definition wsdlDef) throws WSDLException {
        List remainingAttrs = DOMUtils.getAttributes(elem);
        String name =
            DOMUtils.getAttribute(elem, "name", remainingAttrs);
        QName pltName = null;
        if (name != null) {
            pltName = new QName(wsdlDef.getTargetNamespace(), name);
        }
        String requiredStr = DOMUtils.getAttributeNS(elem,
                Constants.NS_URI_WSDL,
                Constants.ATTR_REQUIRED);
        
        PartnerLinkTypeImpl partnerLinkType = new PartnerLinkTypeImpl();
        partnerLinkType.setElementType(elementType);
        
        partnerLinkType.setName(pltName);
        partnerLinkType.setRequired(new Boolean(requiredStr));
        
        Set<PartnerLinkRole> roles = new LinkedHashSet<PartnerLinkRole>(); 
        Element tempEl = DOMUtils.getFirstChildElement(elem);

        while (tempEl != null) {
            if (QNameUtils.matches(
                    BPELExtConstants.PARTNER_LINK_ROLE_ELEM, tempEl)) {
                roles.add(parseRole(tempEl, wsdlDef));
            }
            tempEl = DOMUtils.getNextSiblingElement(tempEl);
        }
        partnerLinkType.setRoles(roles);
        partnerLinkType.setNamespaceDeclarations(
                NamespaceDeclarationsImpl.newInstance(elem));

        return partnerLinkType;
    }
    
    protected Query parseQuery(Element elem) {
        List remainingAttrs = DOMUtils.getAttributes(elem);
        String language =
            DOMUtils.getAttribute(elem, "queryLanguage", remainingAttrs);
        String queryStr = DOMUtils.getChildCharacterData(elem);
        QueryImpl query = new QueryImpl();
        query.setLanguage(language);
        query.setQueryString(queryStr);
        query.setNamespaceDeclarations(
                NamespaceDeclarationsImpl.newInstance(elem));
        return query;
    }
    
    protected PartnerLinkRole parseRole(Element elem, Definition wsdlDef)
    throws WSDLException {
        List remainingAttrs = DOMUtils.getAttributes(elem);
        String name = DOMUtils.getAttribute(elem, "name", remainingAttrs);
        String ptPrefixedName =
            DOMUtils.getAttribute(elem, "portType", remainingAttrs);
        QName ptQName = null;
        if (ptPrefixedName != null) {
            ptQName = DOMUtils.getQName(ptPrefixedName, elem, wsdlDef);
        }
        PartnerLinkRoleImpl role = new PartnerLinkRoleImpl(wsdlDef);
        role.setName(name);
        role.setPortTypeName(ptQName);
        role.setNamespaceDeclarations(
                NamespaceDeclarationsImpl.newInstance(elem));
        return role;
    }
}
