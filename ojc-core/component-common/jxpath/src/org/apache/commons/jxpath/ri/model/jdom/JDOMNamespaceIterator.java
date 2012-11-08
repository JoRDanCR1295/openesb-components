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
 * @(#)JDOMNamespaceIterator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.model.jdom;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.jxpath.ri.model.NodeIterator;
import org.apache.commons.jxpath.ri.model.NodePointer;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.Namespace;

/**
 * An iterator of namespaces of a DOM Node.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class JDOMNamespaceIterator implements NodeIterator {
    private NodePointer parent;
    private List namespaces;
    private Set prefixes;
    private int position = 0;

    public JDOMNamespaceIterator(NodePointer parent) {
        this.parent = parent;
        Object node = parent.getNode();
        if (node instanceof Document) {
            node = ((Document)node).getRootElement();
        }
        if (node instanceof Element) {
            namespaces = new ArrayList();
            prefixes = new HashSet();
            collectNamespaces((Element) node);
        }
    }

    private void collectNamespaces(Element element) {
        Namespace ns = element.getNamespace();
        if (ns != null && !prefixes.contains(ns.getPrefix())) {
            namespaces.add(ns);
            prefixes.add(ns.getPrefix());
        }
        List others = element.getAdditionalNamespaces();
        for (int i = 0; i < others.size(); i++) {
            ns = (Namespace) others.get(i);
            if (ns != null && !prefixes.contains(ns.getPrefix())) {
                namespaces.add(ns);
                prefixes.add(ns.getPrefix());
            }
        }
        Object parent = element.getParent();
        if (parent instanceof Element) {
            collectNamespaces((Element)parent);
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
        Namespace ns = (Namespace) namespaces.get(index);
        return new JDOMNamespacePointer(parent, ns.getPrefix(), ns.getURI());
    }

    public int getPosition() {
        return position;
    }

    public boolean setPosition(int position) {
        if (namespaces == null) {
            return false;
        }
        this.position = position;
        return position >= 1 && position <= namespaces.size();
    }
}
