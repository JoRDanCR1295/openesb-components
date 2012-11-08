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
 * @(#)DescendantContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.axes;

import java.util.Stack;

import org.apache.commons.jxpath.Pointer;
import org.apache.commons.jxpath.ri.Compiler;
import org.apache.commons.jxpath.ri.EvalContext;
import org.apache.commons.jxpath.ri.compiler.NodeTest;
import org.apache.commons.jxpath.ri.compiler.NodeTypeTest;
import org.apache.commons.jxpath.ri.model.NodeIterator;
import org.apache.commons.jxpath.ri.model.NodePointer;

/**
 * An EvalContext that walks the "descendant::" and "descendant-or-self::"
 * axes.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class DescendantContext extends EvalContext {
    private NodeTest nodeTest;
    private boolean setStarted = false;
    private Stack stack;
    private NodePointer currentNodePointer;
    private boolean includeSelf;
    private static final NodeTest ELEMENT_NODE_TEST =
            new NodeTypeTest(Compiler.NODE_TYPE_NODE);
                        
    public DescendantContext(
            EvalContext parentContext,
            boolean includeSelf,
            NodeTest nodeTest) 
    {
        super(parentContext);
        this.includeSelf = includeSelf;
        this.nodeTest = nodeTest;
    }

    public boolean isChildOrderingRequired() {
        return true;
    }

    public NodePointer getCurrentNodePointer() {
        if (position == 0) {
            if (!setPosition(1)) {
                return null;
            }
        }
        return currentNodePointer;
    }

    public void reset() {
        super.reset();
        setStarted = false;
    }

    public boolean setPosition(int position) {
        if (position < this.position) {
            reset();
        }

        while (this.position < position) {
            if (!nextNode()) {
                return false;
            }
        }
        return true;
    }

    public boolean nextNode() {
        if (!setStarted) {
            setStarted = true;
            stack = new Stack();
            currentNodePointer = parentContext.getCurrentNodePointer();
            if (currentNodePointer != null) {
                if (!currentNodePointer.isLeaf()) {
                    stack.push(
                        currentNodePointer.childIterator(
                            ELEMENT_NODE_TEST,
                            false,
                            null));
                }
                if (includeSelf) {
                    if (currentNodePointer.testNode(nodeTest)) {
                        position++;
                        return true;
                    }
                }
            }
        }

        while (!stack.isEmpty()) {
            NodeIterator it = (NodeIterator) stack.peek();
            if (it.setPosition(it.getPosition() + 1)) {
                currentNodePointer = it.getNodePointer();
                if (!isRecursive()) {
                    if (!currentNodePointer.isLeaf()) {
                        stack.push(
                            currentNodePointer.childIterator(
                                ELEMENT_NODE_TEST,
                                false,
                                null));
                    }
                    if (currentNodePointer.testNode(nodeTest)) {
                        position++;
                        return true;
                    }
                }
            }
            else {
                // We get here only if the name test failed 
                // and the iterator ended
                stack.pop();
            }
        }
        return false;
    }

    /**
     * Checks if we are reentering a bean we have already seen and if so
     * returns true to prevent infinite recursion.
     */
    private boolean isRecursive() {
        Object node = currentNodePointer.getNode();
        for (int i = stack.size() - 1; --i >= 0;) {
            NodeIterator it = (NodeIterator) stack.get(i);
            Pointer pointer = it.getNodePointer();
            if (pointer != null && pointer.getNode() == node) {
                return true;
            }
        }
        return false;
    }
}
