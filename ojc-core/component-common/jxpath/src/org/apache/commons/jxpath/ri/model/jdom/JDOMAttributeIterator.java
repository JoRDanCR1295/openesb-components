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
 * @(#)JDOMAttributeIterator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.model.jdom;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.commons.jxpath.ri.QName;
import org.apache.commons.jxpath.ri.model.NodeIterator;
import org.apache.commons.jxpath.ri.model.NodePointer;
import org.jdom.Attribute;
import org.jdom.Element;
import org.jdom.Namespace;

/**
 * An iterator of attributes of a DOM Node.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class JDOMAttributeIterator implements NodeIterator {
    private NodePointer parent;
    private QName name;
    private List attributes;
    private int position = 0;

    public JDOMAttributeIterator(NodePointer parent, QName name) {
        this.parent = parent;
        this.name = name;
        if (parent.getNode() instanceof Element) {
            Element element = (Element) parent.getNode();
            String prefix = name.getPrefix();
            Namespace ns;
            if (prefix != null) {
                if (prefix.equals("xml")) {
                    ns = Namespace.XML_NAMESPACE;
                }
                else {
                    ns = element.getNamespace(prefix);
                    if (ns == null) {
                        // TBD: no attributes
                        attributes = Collections.EMPTY_LIST;
                        return;
                    }
                }
            }
            else {
                ns = Namespace.NO_NAMESPACE;
            }

            String lname = name.getName();
            if (!lname.equals("*")) {
                attributes = new ArrayList();
                if (ns != null) {
                    Attribute attr = element.getAttribute(lname, ns);
                    if (attr != null) {
                        attributes.add(attr);
                    }
                }
            }
            else {
                attributes = new ArrayList();
                List allAttributes = element.getAttributes();
                for (int i = 0; i < allAttributes.size(); i++) {
                    Attribute attr = (Attribute) allAttributes.get(i);
                    if (attr.getNamespace().equals(ns)) {
                        attributes.add(attr);
                    }
                }
            }
        }
    }

    /*
    private boolean testAttr(Attr attr, QName testName) {
        String nodePrefix = DOMNodePointer.getPrefix(attr);
        String nodeLocalName = DOMNodePointer.getLocalName(attr);

        if (nodePrefix != null && nodePrefix.equals("xmlns")) {
            return false;
        }

        if (nodePrefix == null && nodeLocalName.equals("xmlns")) {
            return false;
        }

        String testLocalName = name.getName();
        if (testLocalName.equals("*") || testLocalName.equals(nodeLocalName)) {
            String testPrefix = testName.getPrefix();

            if (equalStrings(testPrefix, nodePrefix)) {
                return true;
            }

            String testNS = null;
            if (testPrefix != null) {
                testNS = parent.getNamespaceURI(testPrefix);
            }

            String nodeNS = null;
            if (nodePrefix != null) {
                nodeNS = parent.getNamespaceURI(nodePrefix);
            }
            return equalStrings(testNS, nodeNS);
        }
        return false;
    }

    private static boolean equalStrings(String s1, String s2) {
        if (s1 == null && s2 != null) {
            return false;
        }
        if (s1 != null && !s1.equals(s2)) {
            return false;
        }
        return true;
    }

    private Attr getAttribute(Element element, QName name) {
        String testPrefix = name.getPrefix();
        String testNS = null;

        if (testPrefix != null) {
            testNS = parent.getNamespaceURI(testPrefix);
        }

        if (testNS != null) {
            Attr attr = element.getAttributeNodeNS(testNS, name.getName());
            if (attr == null) {
                // This may mean that the parser does not support NS for
                // attributes, example - the version of Crimson bundled
                // with JDK 1.4.0
                NamedNodeMap nnm = element.getAttributes();
                for (int i = 0; i < nnm.getLength(); i++) {
                    attr = (Attr)nnm.item(i);
                    if (testAttr(attr, name)) {
                        return attr;
                    }
                }
            }
            return attr;
        }
        else {
            return element.getAttributeNode(name.getName());
        }
    }
*/
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
        return new JDOMAttributePointer(
            parent,
            (Attribute) attributes.get(index));
    }

    public int getPosition() {
        return position;
    }

    public boolean setPosition(int position) {
        if (attributes == null) {
            return false;
        }
        this.position = position;
        return position >= 1 && position <= attributes.size();
    }
}
