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
 * @(#)TestDOMFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.model.dom;

import org.apache.commons.jxpath.AbstractFactory;
import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.Pointer;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

/**
 * Test AbstractFactory.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class TestDOMFactory extends AbstractFactory {

    /**
     * Return <b>false</b> if this factory cannot create the requested object.
     */
    public boolean createObject(
        JXPathContext context,
        Pointer pointer,
        Object parent,
        String name,
        int index) 
    {
        if (name.equals("location")
            || name.equals("address")
            || name.equals("street")) {
            addDOMElement((Node) parent, index, name, null);
            return true;
        }
        if (name.startsWith("price:")) {
            String namespaceURI = context.getNamespaceURI("price");
            addDOMElement((Node) parent, index, name, namespaceURI);
            return true;
        }
        return false;
    }

    private void addDOMElement(Node parent, int index, String tag, String namespaceURI) {
        Node child = parent.getFirstChild();
        int count = 0;
        while (child != null) {
            if (child.getNodeName().equals(tag)) {
                count++;
            }
            child = child.getNextSibling();
        }

        // Keep inserting new elements until we have index + 1 of them
        while (count <= index) {
            Document doc = parent.getOwnerDocument();
            Node newElement;
            if (namespaceURI == null) {
                newElement = doc.createElement(tag);
            } 
            else {
                newElement = doc.createElementNS(namespaceURI, tag);
            }
       
            parent.appendChild(newElement);
            count++;
        }
    }

    public boolean declareVariable(JXPathContext context, String name) {
        return false;
    }
}
