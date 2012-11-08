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
 * @(#)PrecedingOrFollowingContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.axes;

import java.util.Stack;

import org.apache.commons.jxpath.ri.EvalContext;
import org.apache.commons.jxpath.ri.compiler.NodeTest;
import org.apache.commons.jxpath.ri.model.NodeIterator;
import org.apache.commons.jxpath.ri.model.NodePointer;
import org.apache.commons.jxpath.ri.model.beans.PropertyIterator;

/**
 * EvalContext that walks the "preceding::" and "following::" axes.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class PrecedingOrFollowingContext extends EvalContext {
    private NodeTest nodeTest;
    private boolean setStarted = false;
    private boolean started = false;
    private Stack stack;
    private Stack nameStack;
    private NodePointer currentNodePointer;
    private NodePointer currentRootLocation;
    private boolean reverse;

    public PrecedingOrFollowingContext(
        EvalContext parentContext,
        NodeTest nodeTest,
        boolean reverse) 
    {
        super(parentContext);
        this.nodeTest = nodeTest;
        this.reverse = reverse;
    }

    public NodePointer getCurrentNodePointer() {
        return currentNodePointer;
    }

    public int getDocumentOrder() {
        return reverse ? -1 : 1;
    }

    public void reset() {
        super.reset();
        stack = new Stack();
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
            currentRootLocation = parentContext.getCurrentNodePointer();
            NodePointer parent = currentRootLocation.getParent();
            if (parent != null) {
                // TBD: check type
                stack.push(
                    parent.childIterator(null, reverse, currentRootLocation));
            }
        }

        while (true) {
            if (stack.isEmpty()) {
                currentRootLocation = currentRootLocation.getParent();

                if (currentRootLocation == null
                    || currentRootLocation.isRoot()) {
                    break;
                }

                NodePointer parent = currentRootLocation.getParent();
                if (parent != null) {
                    stack.push(
                        parent.childIterator(
                            null,
                            reverse,
                            currentRootLocation));
                }
            }

            while (!stack.isEmpty()) {
                if (!reverse) {
                    NodeIterator it = (NodeIterator) stack.peek();
                    if (it.setPosition(it.getPosition() + 1)) {
                        currentNodePointer = it.getNodePointer();
                        if (!currentNodePointer.isLeaf()) {
                            stack.push(
                                currentNodePointer.childIterator(
                                    null,
                                    reverse,
                                    null));
                        }
                        if (currentNodePointer.testNode(nodeTest)) {
                            super.setPosition(getCurrentPosition() + 1);
                            return true;
                        }
                    }
                    else {
                        // We get here only if the name test failed 
                        // and the iterator ended
                        stack.pop();
                    }
                }
                else {
                    NodeIterator it = (NodeIterator) stack.peek();
                    if (it.setPosition(it.getPosition() + 1)) {
                        currentNodePointer = it.getNodePointer();
                        if (!currentNodePointer.isLeaf()) {
                            stack.push(
                                currentNodePointer.childIterator(
                                    null,
                                    reverse,
                                    null));
                        }
                        else if (currentNodePointer.testNode(nodeTest)) {
                            super.setPosition(getCurrentPosition() + 1);
                            return true;
                        }
                    }
                    else {
                        stack.pop();
                        if (!stack.isEmpty()) {
                            it = (PropertyIterator) stack.peek();
                            currentNodePointer = it.getNodePointer();
                            if (currentNodePointer.testNode(nodeTest)) {
                                super.setPosition(getCurrentPosition() + 1);
                                return true;
                            }
                        }
                    }
                }
            }
        }
        return false;
    }
}
