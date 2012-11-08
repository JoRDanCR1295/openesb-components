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
 * @(#)BPELSEDOMNodePointer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.xpath.dom;

import java.util.Locale;

import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.JXPathException;
import org.apache.commons.jxpath.ri.QName;
import org.apache.commons.jxpath.ri.compiler.NodeTest;
import org.apache.commons.jxpath.ri.model.NodeIterator;
import org.apache.commons.jxpath.ri.model.NodePointer;
import org.apache.commons.jxpath.ri.model.dom.DOMNodePointer;
import org.apache.xmlbeans.SchemaField;
import org.apache.xmlbeans.SchemaType;
import org.apache.xmlbeans.XmlBoolean;
import org.apache.xmlbeans.XmlQName;
import org.w3c.dom.Attr;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import com.sun.jbi.engine.bpel.core.bpel.util.DOMHelper;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;


/**
 * A Pointer that points to a DOM node.
 *
 * @author Malkit Bhasin
 * @version  
 */
public class BPELSEDOMNodePointer extends DOMNodePointer {

    private static final long serialVersionUID = 1L;

    public BPELSEDOMNodePointer(Node node, Locale locale) {
        super(node, locale);
    }

    public BPELSEDOMNodePointer(Node node, Locale locale, String id) {
        super(node, locale, id);
    }

    public BPELSEDOMNodePointer(NodePointer parent, Node node) {
        super(parent, node);
    }

    public Object getSchemaType() {
        return node.getUserData("schemaType");
    }
 
    public NodeIterator childIterator(
        NodeTest test,
        boolean reverse,
        NodePointer startWith) 
    {
        return new BPELSEDOMNodeIterator(this, test, reverse, startWith);
    }
    
    public Object getValue() {
        Object schemaType = node.getUserData("schemaType");
        if (schemaType != null) {
            if (schemaType instanceof SchemaType
                    && XmlBoolean.type.isAssignableFrom((SchemaType) schemaType)) {
                return getBooleanValue(); 
            } else if (schemaType instanceof SchemaField) {
                SchemaType schType = ((SchemaField) schemaType).getType();
                if (XmlBoolean.type.isAssignableFrom(schType)) {
                    return getBooleanValue(); 
                }
            }
        }
        return super.getValue();
    }

    public void setValue(Object value) {
        value = Utility.adjustValueForDouble(node, value);
        value = adjustValueForQName(node, value);
        super.setValue(value);
    }
    
    private Object adjustValueForQName(Node node, Object value) {
        Element elem;
        if (node instanceof Element) {
            elem = (Element) node;
        } else if (node instanceof Attr) {
            elem = ((Attr) node).getOwnerElement();
            if (elem == null) {
                return value;
            }
        } else {
            return value;
        }
        Object schemaField = node.getUserData("schemaType");
        SchemaType type;
        if (schemaField instanceof SchemaType) {
            type = (SchemaType) schemaField;
        } else if (schemaField instanceof SchemaField) {
            type = ((SchemaField) schemaField).getType();
        } else {
            return value;
        }
        if (type == null || !type.isSimpleType()
                && type.getContentType() != SchemaType.SIMPLE_CONTENT) {
            return value;
        }
        while (type != null && !type.isSimpleType() && !type.isURType()) {
            type = type.getBaseType();
        }
        if (type == null || !type.isSimpleType()) {
            return value;
        }
        if (XmlQName.type.isAssignableFrom(type)) {
            String strVal = (String) value;
            String prefix = DOMHelper.getPrefix(strVal);
            String uri;
            if (prefix.length() > 0) {
                uri = getNamespaceResolver().getNamespaceURI(prefix);
            } else {
                uri = getNamespaceResolver().getNamespaceURI("");
                if (uri == null) {
                    uri = getNamespaceResolver().getNamespaceURI(null);
                }
            }
            if (DOMHelper.declareNamespace(prefix, uri, elem, null)) {
                return value;
            }
            //Unable to declare the namespace
            if (uri != null && uri.length() > 0) {
                prefix = DOMHelper.getUnusedPrefix(elem, null);
                if (DOMHelper.declareNamespace(prefix, uri, elem, null)) {
                    //Adjust the value 
                    return prefix + ":" + DOMHelper.getLocalName(strVal);
                }
            }
        }
        return value;
    }
    
    public static boolean isNodeQNameType(Node node) {
        Object schemaField = node.getUserData("schemaType");
        SchemaType type = null;
        if (schemaField instanceof SchemaType) {
            type = (SchemaType) schemaField;
        } else if (schemaField instanceof SchemaField) {
            type = ((SchemaField) schemaField).getType();
        }
        if (type == null || !type.isSimpleType()
                && type.getContentType() != SchemaType.SIMPLE_CONTENT) {
            return false;
        }
        while (type != null && !type.isSimpleType() && !type.isURType()) {
            type = type.getBaseType();
        }
        if (type == null || !type.isSimpleType()) {
            return false;
        }
        if (XmlQName.type.isAssignableFrom(type)) {
            return true;
        }
        return false;
    }
    
    public static boolean isNodeSimpleType(Node node) {
        Object schemaField = node.getUserData("schemaType");
        SchemaType type = null;
        if (schemaField instanceof SchemaType) {
            type = (SchemaType) schemaField;
        } else if (schemaField instanceof SchemaField) {
            type = ((SchemaField) schemaField).getType();
        }
        if (type == null || !type.isSimpleType()) {
            return false;
        }
        return true;
    }
    
    public static boolean isAttrNodeQNameType(Node node) {
        Object schemaField = node.getUserData("schemaType");
        SchemaType type = null;
        if (schemaField instanceof SchemaType) {
            type = (SchemaType) schemaField;
        } else if (schemaField instanceof SchemaField) {
            type = ((SchemaField) schemaField).getType();
        }
        if (type == null || !type.isSimpleType()) {
            return false;
        }
        if (XmlQName.type.isAssignableFrom(type)) {
            return true;
        }
        return false;
    }
    
    private Boolean getBooleanValue() {
        String nodeText = (String) super.getValue();
        if (nodeText.equals("true")) {
            return new Boolean(true);
        }
        return new Boolean(false);
    }
    public NodePointer createAttribute(JXPathContext context, QName name) {
        if (!(node instanceof Element)) {
            return super.createAttribute(context, name);
        }
        Element element = (Element) node;
        String prefix = name.getPrefix();
        if (prefix != null) {
            String ns = getNamespaceResolver().getNamespaceURI(prefix);
            if (ns == null) {
                throw new JXPathException(
                    "Unknown namespace prefix: " + prefix);
            }
            element.setAttributeNS(ns, name.toString(), "");
        }
        else {
            if (!element.hasAttribute(name.getName())) {
                element.setAttribute(name.getName(), "");
            }
        }
        NodeIterator it = attributeIterator(name);
        it.setPosition(1);
        return it.getNodePointer();
    }
    public NodeIterator attributeIterator(QName name) {
        return new BPELSEDOMAttributeIterator(this, name);
    }
}
