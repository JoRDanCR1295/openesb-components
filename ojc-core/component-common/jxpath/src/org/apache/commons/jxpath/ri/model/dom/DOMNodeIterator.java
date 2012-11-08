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
 * @(#)DOMNodeIterator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.model.dom;

import org.apache.commons.jxpath.ri.compiler.NodeTest;
import org.apache.commons.jxpath.ri.model.NodeIterator;
import org.apache.commons.jxpath.ri.model.NodePointer;
import org.w3c.dom.Node;

/**
 * An iterator of children of a DOM Node.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class DOMNodeIterator implements NodeIterator {
    protected NodePointer parent;
    private NodeTest nodeTest;
    private Node node;
    protected Node child = null;
    private boolean reverse;
    protected int position = 0;

    public DOMNodeIterator(
        NodePointer parent,
        NodeTest nodeTest,
        boolean reverse,
        NodePointer startWith) 
    {
        this.parent = parent;
        this.node = (Node) parent.getNode();
        if (startWith != null) {
            this.child = (Node) startWith.getNode();
        }
        this.nodeTest = nodeTest;
        this.reverse = reverse;
    }

    public NodePointer getNodePointer() {
        if (position == 0) {
            setPosition(1);
        }
        if (child == null) {
            return null;
        }
        return new DOMNodePointer(parent, child);
    }

    public int getPosition() {
        return position;
    }

    public boolean setPosition(int position) {
        while (this.position < position) {
            if (!next()) {
                return false;
            }
        }
        while (this.position > position) {
            if (!previous()) {
                return false;
            }
        }
        return true;
    }

    private boolean previous() {
        position--;
        if (!reverse) {
            if (position == 0) {
                child = null;
            }
            else if (child == null) {
                child = node.getLastChild();
            }
            else {
                child = child.getPreviousSibling();
            }
            while (child != null && !testChild()) {
                child = child.getPreviousSibling();
            }
        }
        else {
            child = child.getNextSibling();
            while (child != null && !testChild()) {
                child = child.getNextSibling();
            }
        }
        return child != null;
    }

    private boolean next() {
        position++;
        if (!reverse) {
            if (position == 1) {
                if (child == null) {
                    child = node.getFirstChild();
                }
                else {
                    child = child.getNextSibling();
                }
            }
            else {
                child = child.getNextSibling();
            }
            while (child != null && !testChild()) {
                child = child.getNextSibling();
            }
        }
        else {
            if (position == 1) {
                if (child == null) {
                    child = node.getLastChild();
                }
                else {
                    child = child.getPreviousSibling();
                }
            }
            else {
                child = child.getPreviousSibling();
            }
            while (child != null && !testChild()) {
                child = child.getPreviousSibling();
            }
        }
        return child != null;
    }

    private boolean testChild() {
        return DOMNodePointer.testNode(child, nodeTest);
    }
}
