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
 * @(#)ChildContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.axes;

import org.apache.commons.jxpath.Pointer;
import org.apache.commons.jxpath.ri.EvalContext;
import org.apache.commons.jxpath.ri.compiler.NodeTest;
import org.apache.commons.jxpath.ri.model.NodeIterator;
import org.apache.commons.jxpath.ri.model.NodePointer;

/**
 * EvalContext that can walk the "child::", "following-sibling::" and
 * "preceding-sibling::" axes.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class ChildContext extends EvalContext {
    private NodeTest nodeTest;
    private boolean startFromParentLocation;
    private boolean reverse;
    private NodeIterator iterator;

    public ChildContext(
        EvalContext parentContext,
        NodeTest nodeTest,
        boolean startFromParentLocation,
        boolean reverse) 
    {
        super(parentContext);
        this.nodeTest = nodeTest;
        this.startFromParentLocation = startFromParentLocation;
        this.reverse = reverse;
    }

    public NodePointer getCurrentNodePointer() {
        if (position == 0) {
            if (!setPosition(1)) {
                return null;
            }
        }
        if (iterator != null) {
            return iterator.getNodePointer();
        }
        else {
            return null;
        }
    }

    /**
     * This method is called on the last context on the path when only
     * one value is needed.  Note that this will return the whole property,
     * even if it is a collection. It will not extract the first element
     * of the collection.  For example, "books" will return the collection
     * of books rather than the first book from that collection.
     */
    public Pointer getSingleNodePointer() {
        if (position == 0) {
            while (nextSet()) {
                prepare();
                if (iterator == null) {
                    return null;
                }
                // See if there is a property there, singular or collection
                NodePointer pointer = iterator.getNodePointer();
                if (pointer != null) {
                    return pointer;
                }
            }
            return null;
        }
        return getCurrentNodePointer();
    }

    public boolean nextNode() {
        return setPosition(getCurrentPosition() + 1);
    }

    public void reset() {
        super.reset();
        iterator = null;
    }

    public boolean setPosition(int position) {
        int oldPosition = getCurrentPosition();
        super.setPosition(position);
        if (oldPosition == 0) {
            prepare();
        }
        if (iterator == null) {
            return false;
        }
        return iterator.setPosition(position);
    }

    /**
     * Allocates a PropertyIterator.
     */
    private void prepare() {
        NodePointer parent = parentContext.getCurrentNodePointer();
        if (parent == null) {
            return;
        }
        if (startFromParentLocation) {
            NodePointer pointer = parent.getParent();
            iterator = pointer.childIterator(nodeTest, reverse, parent);
        }
        else {
            iterator = parent.childIterator(nodeTest, reverse, null);
        }
    }
}
