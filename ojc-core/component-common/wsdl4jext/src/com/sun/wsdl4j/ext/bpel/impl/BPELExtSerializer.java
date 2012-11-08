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
 * @(#)BPELExtSerializer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl4j.ext.bpel.impl;

import java.io.PrintWriter;
import java.io.Serializable;

import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.wsdl.extensions.http.HTTPBinding;
import javax.xml.namespace.QName;

import com.ibm.wsdl.Constants;
import com.ibm.wsdl.extensions.http.HTTPConstants;
import com.ibm.wsdl.util.xml.DOMUtils;
import com.sun.wsdl4j.ext.bpel.MessageProperty;
import com.sun.wsdl4j.ext.bpel.MessagePropertyAlias;
import com.sun.wsdl4j.ext.bpel.PartnerLinkRole;
import com.sun.wsdl4j.ext.bpel.PartnerLinkType;

/**
 * Serializer for WSDL extensibilty elements for BPEL 2.0.
 * 
 * @author Jun Xu
 * @version $Revision: 1.3 $
 */
public class BPELExtSerializer implements ExtensionSerializer, Serializable {

    private static final long serialVersionUID = 1L;

    public void marshall(Class parentType, QName elementType,
            ExtensibilityElement extension, PrintWriter pw, Definition def,
            ExtensionRegistry extReg) throws WSDLException {
        
        if (BPELExtConstants.PROPERTY_ELEM.equals(elementType)) {
            marshalProperty((MessageProperty) extension, pw, def);
        } else if (BPELExtConstants.PROPERTY_ALIAS_ELEM.equals(elementType)) {
            marshalPropertyAlias((MessagePropertyAlias) extension, pw, def);
        } else if (BPELExtConstants.PARTNER_LINK_TYPE_ELEM.equals(
                elementType)) {
            marshalPartnerLinkType((PartnerLinkType) extension, pw, def);
        }
    }
    
    protected void marshalProperty(MessageProperty prop, PrintWriter pw,
            Definition wsdlDef) throws WSDLException {
        if (prop == null) {
            return;
        }
        
        String tagName =
            DOMUtils.getQualifiedValue(
                    BPELExtConstants.PROPERTY_URL,
                    BPELExtConstants.PROPERTY_NCNAME,
                    wsdlDef);

        pw.print("    <" + tagName);
    
        DOMUtils.printAttribute(
                "name", prop.getName().getLocalPart(), pw);

        String prefixedType =
            DOMUtils.getQualifiedValue(
                    prop.getType().getNamespaceURI(),
                    prop.getType().getLocalPart(),
                    wsdlDef);
        DOMUtils.printAttribute("type", prefixedType, pw);
        
        Boolean required = prop.getRequired();
    
        if (required != null) {
            DOMUtils.printQualifiedAttribute(
                    Constants.Q_ATTR_REQUIRED,
                    required.toString(),
                    wsdlDef,
                    pw);
        }
        pw.println("/>");
    }
    
    protected void marshalPropertyAlias(MessagePropertyAlias propAlias,
            PrintWriter pw, Definition wsdlDef) throws WSDLException {
        if (propAlias == null) {
            return;
        }
        
        String tagName =
            DOMUtils.getQualifiedValue(
                    BPELExtConstants.PROPERTY_URL,
                    BPELExtConstants.PROPERTY_ALIAS_NCNAME,
                    wsdlDef);

        pw.print("    <" + tagName);
    
        String prefixedName =
            DOMUtils.getQualifiedValue(
                    propAlias.getName().getNamespaceURI(),
                    propAlias.getName().getLocalPart(),
                    wsdlDef);
        DOMUtils.printAttribute("propertyName", prefixedName, pw);

        prefixedName =
            DOMUtils.getQualifiedValue(
                    propAlias.getMessageType().getQName().getNamespaceURI(),
                    propAlias.getMessageType().getQName().getLocalPart(),
                    wsdlDef);
        DOMUtils.printAttribute("messageType", prefixedName, pw);
        
        DOMUtils.printAttribute("part", propAlias.getPartName(), pw);
        
        Boolean required = propAlias.getRequired();
        
        if (required != null) {
            DOMUtils.printQualifiedAttribute(
                    Constants.Q_ATTR_REQUIRED,
                    required.toString(),
                    wsdlDef,
                    pw);
        }
        
        if (propAlias.getQuery() != null) {
            pw.println(">");
            String queryTagName =
                DOMUtils.getQualifiedValue(
                        BPELExtConstants.PROPERTY_URL,
                        BPELExtConstants.QUERY_NCNAME,
                        wsdlDef);
            pw.print("        <" + queryTagName);
            if (propAlias.getQuery().getLanguage() != null) {
                DOMUtils.printAttribute("queryLanguage",
                        propAlias.getQuery().getLanguage(), pw);
            }
            pw.print(
                    DOMUtils.cleanString(
                            propAlias.getQuery().getQueryString()));
            pw.println("/>");
            pw.println("    </" + tagName + ">");
        } else {
            pw.println("/>");
        }
    }
    
    protected void marshalPartnerLinkType(PartnerLinkType plType,
            PrintWriter pw, Definition wsdlDef) throws WSDLException {
        if (plType == null) {
            return;
        }
        
        String tagName =
            DOMUtils.getQualifiedValue(
                    BPELExtConstants.PARTNER_LINK_URL,
                    BPELExtConstants.PARTNER_LINK_TYPE_NCNAME,
                    wsdlDef);

        pw.print("    <" + tagName);
    
        DOMUtils.printAttribute("name", plType.getName().getLocalPart(), pw);
        
        Boolean required = plType.getRequired();
        
        if (required != null) {
            DOMUtils.printQualifiedAttribute(
                    Constants.Q_ATTR_REQUIRED,
                    required.toString(),
                    wsdlDef,
                    pw);
        }
        
        if (!plType.getRoles().isEmpty()) {
            pw.println(">");
            for (PartnerLinkRole role : plType.getRoles()) {
                String roleTagName =
                    DOMUtils.getQualifiedValue(
                            BPELExtConstants.PARTNER_LINK_URL,
                            BPELExtConstants.ROLE_NCNAME,
                            wsdlDef);
                pw.print("        <" + roleTagName);
                if (role.getName() != null) {
                    DOMUtils.printAttribute("name", role.getName(), pw);
                }
                if (role.getPort() != null
                        && role.getPort().getQName() != null) {
                    String prefixedPortName = 
                        DOMUtils.getQualifiedValue(
                                role.getPort().getQName().getNamespaceURI(),
                                role.getPort().getQName().getLocalPart(),
                                wsdlDef);
                    DOMUtils.printAttribute("portType", prefixedPortName, pw);
                    
                }
                pw.println("/>");
            }
            pw.println("    </" + tagName + ">");
        } else {
            pw.println("/>");
        }
    }
}
