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
 * @(#)DOMNamespaceIterator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.model.dom;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.jxpath.ri.model.NodeIterator;
import org.apache.commons.jxpath.ri.model.NodePointer;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;

/**
 * An iterator of namespaces of a DOM Node.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class DOMNamespaceIterator implements NodeIterator {
    private NodePointer parent;
    private List attributes;
    private int position = 0;

    public DOMNamespaceIterator(NodePointer parent) {
        this.parent = parent;
        attributes = new ArrayList();
        collectNamespaces(attributes, (Node) parent.getNode());
    }

    private void collectNamespaces(List attributes, Node node) {
        Node parent = node.getParentNode();
        if (parent != null) {
            collectNamespaces(attributes, parent);
        }
        if (node.getNodeType() == Node.DOCUMENT_NODE) {
            node = ((Document) node).getDocumentElement();
        }
        if (node.getNodeType() == Node.ELEMENT_NODE) {
            NamedNodeMap map = node.getAttributes();
            int count = map.getLength();
            for (int i = 0; i < count; i++) {
                Attr attr = (Attr) map.item(i);
                String prefix = DOMNodePointer.getPrefix(attr);
                String name = DOMNodePointer.getLocalName(attr);
                if ((prefix != null && prefix.equals("xmlns"))
                    || (prefix == null && name.equals("xmlns"))) {
                    attributes.add(attr);
                }
            }
        }
    }

    public NodePointer getNodePointer() {
        if (position == 0) {
            if (!setPosition(1)) {
                return null;
            }
            position = 0;
        }
        int index = position - 1;
        if (index < 0) {
            index = 0;
        }
        String prefix = "";
        Attr attr = (Attr) attributes.get(index);
        String name = attr.getPrefix();
        if (name != null && name.equals("xmlns")) {
            prefix = DOMNodePointer.getLocalName(attr);
        }
        return new NamespacePointer(parent, prefix, attr.getValue());
    }

    public int getPosition() {
        return position;
    }

    public boolean setPosition(int position) {
        this.position = position;
        return position >= 1 && position <= attributes.size();
    }
}
