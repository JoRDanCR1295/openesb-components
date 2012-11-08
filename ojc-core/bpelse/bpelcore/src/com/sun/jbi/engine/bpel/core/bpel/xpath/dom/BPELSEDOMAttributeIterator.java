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
 * @(#)BPELSEDOMAttributeIterator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.xpath.dom;

import org.apache.commons.jxpath.ri.QName;
import org.apache.commons.jxpath.ri.model.NodePointer;
import org.apache.commons.jxpath.ri.model.dom.DOMAttributeIterator;
import org.apache.commons.jxpath.ri.model.dom.DOMAttributePointer;
import org.apache.xmlbeans.SchemaField;
import org.apache.xmlbeans.SchemaType;

public class BPELSEDOMAttributeIterator extends DOMAttributeIterator {
	private NodePointer parent;
	private QName name;

	public BPELSEDOMAttributeIterator(NodePointer parent, QName name) {
		super(parent, name);
		this.parent = parent;
		this.name = name;
	}

	public NodePointer getNodePointer() {
        DOMAttributePointer pointer = (DOMAttributePointer) super.getNodePointer();
        String nsUri = null;
        String prefix = name.getPrefix();
        if (prefix != null) {
            nsUri = parent.getNamespaceResolver().getNamespaceURI(prefix);
        }
        // compute the org.apache.xmlbeans.SchemaType of the attribute
        javax.xml.namespace.QName qName;
        if (nsUri == null) {
            qName = new javax.xml.namespace.QName(name.getName());
        } else {
            qName = new javax.xml.namespace.QName(nsUri, name.getName());
        }

        Object type = ((BPELSEDOMNodePointer) parent).getSchemaType();
        SchemaType schemaType = null;
        if (type != null) {
            if (type instanceof SchemaType) {
                schemaType = (SchemaType) type;
            } else if (type instanceof SchemaField) {
                schemaType = ((SchemaField) type).getType();
            }
            SchemaField attrField = schemaType.getAttributeModel().getAttribute(qName);
            if (attrField != null) {
                schemaType = attrField.getType();
            }
        }

        BPELSEDOMAttributePointer attrPointer = new BPELSEDOMAttributePointer(pointer, parent, name);
        attrPointer.setAttrSchemaType(schemaType);
        return attrPointer;
    }
	
}
