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
 * @(#)BPELSEDOMNodeIterator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.xpath.dom;

import org.apache.commons.jxpath.ri.compiler.NodeTest;
import org.apache.commons.jxpath.ri.model.NodePointer;
import org.apache.commons.jxpath.ri.model.dom.DOMNodeIterator;
import org.apache.xmlbeans.SchemaField;
import org.apache.xmlbeans.SchemaType;
import org.w3c.dom.Text;

/**
 * An iterator of children of a DOM Node.
 *
 * @author Malkit Bhasin
 * @version  
 */
public class BPELSEDOMNodeIterator extends DOMNodeIterator {
    
    public BPELSEDOMNodeIterator(
        NodePointer parent,
        NodeTest nodeTest,
        boolean reverse,
        NodePointer startWith) 
    {
        super(parent, nodeTest, reverse, startWith);
    }

    public NodePointer getNodePointer() {
        if (position == 0) {
            setPosition(1);
        }
        if (child == null) {
            return null;
        }
        child.setUserData("schemaType", getSchemaType(), null);
        return new BPELSEDOMNodePointer(parent, child);
    }
    
    public Object getSchemaType() {
        if (child instanceof Text) {
            return null;
        }
        if(child.getLocalName() == null){
        	return null;
        }
        javax.xml.namespace.QName qName = new javax.xml.namespace.QName(child.getNamespaceURI(),
                child.getLocalName());
        Object schemaType = ((BPELSEDOMNodePointer) parent).getSchemaType();
        if (schemaType instanceof SchemaField) {
            return Utility.getSchemaType((SchemaField)schemaType, qName);
        } else if (schemaType instanceof SchemaType) {
            return Utility.getSchemaType((SchemaType)schemaType, qName);
        }
        return null;
    }    
}
