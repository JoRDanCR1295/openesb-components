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
 * @(#)DOMUtil.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.impl;

import javax.xml.namespace.QName;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.xml.transform.sware.schema.SwareTypeSystem;

/**
 * A utility class providing methods that facilitate DOM handling.
 * More methods can be added later.
 * 
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
public class DOMUtil {
    
    /**
     * XML namespace namespace.
     */
    public final static String XMLNS_URI =
        "http://www.w3.org/2000/xmlns/".intern();

    /**
     * Copies all attributes from one element to another element.
     * 
     * @param from the element from which the attributes are being copied
     * @param to the element to which the attributes are being copied
     */
    public static void copyAttributes(Element from, Element to) {
        if (!from.hasAttributes()) {
            return;
        }
        NamedNodeMap attrs = from.getAttributes();
        Document toDoc = to.getOwnerDocument();
        for (int i = 0; i < attrs.getLength(); i++) {
            Attr attr = (Attr) attrs.item(i);
            if (XMLNS_URI.equals(attr.getNamespaceURI())) {
                continue;
            }
            if (to.getAttributeNodeNS(
                    attr.getNamespaceURI(), attr.getLocalName()) == null) {
                Attr attrNode =
                    toDoc.createAttributeNS(
                            attr.getNamespaceURI(), attr.getLocalName());
                attrNode.setPrefix(attr.getPrefix());
                attrNode.setNodeValue(attr.getValue());
                to.setAttributeNodeNS(attrNode);
            }
        }
    }

    /**
     * Copies over all child nodes from one element to another.
     * 
     * @param from the element from which the child nodes are copied from
     * @param to the element to which the child nodes are copied to
     */
    public static void copyAllChildNodes(Element from, Element to) {
        if (!from.hasChildNodes()) {
            return;
        }
        NodeList nodeList = from.getChildNodes();
        Document toDoc = to.getOwnerDocument();
        for (int i = 0; i < nodeList.getLength(); i++) {
            to.appendChild(toDoc.importNode(nodeList.item(i), true));
        }
    }

    /**
     * Gets the value (converted to QName) of the xsi:type attribute on a
     * given DOM element.
     *  
     * @param domElem a DOM element
     * @return an instance of QName representing the value of the xsi:type
     *         attribute, or <code>null</code> if the attribute is not
     *         presented.
     */
    public static QName getXsiType(Element domElem) {
        String xsiType =
            domElem.getAttributeNS(
                SwareTypeSystem.XSI_NS_URI, "type");
        if (xsiType != null && xsiType.length() > 0) {
            //has xsi:type attribute
            String prefix = (xsiType.indexOf(':') >= 0 ?
                    xsiType.substring(0, xsiType.indexOf(':')) : null);
            String uri =
                domElem.lookupNamespaceURI(prefix == null ? "" : prefix);
            if (uri != null && uri.length() == 0) {
                uri = null;
            }
            String localPart = (prefix == null ? xsiType
                    : xsiType.substring(prefix.length() + 1));
            return new QName(uri, localPart);
        }
        return null;
    }
    
    /**
     * Gets the qualified name of a DOM node.
     * 
     * @param domNode the DOM node
     * @return the qualified name of the node
     */
    public static QName getQName(Node domNode) {
        return new QName(domNode.getNamespaceURI(), domNode.getLocalName());
    }
}
