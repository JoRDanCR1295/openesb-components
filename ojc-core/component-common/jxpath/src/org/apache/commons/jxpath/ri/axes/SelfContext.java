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
 * @(#)SelfContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.axes;

import org.apache.commons.jxpath.Pointer;
import org.apache.commons.jxpath.ri.EvalContext;
import org.apache.commons.jxpath.ri.compiler.NodeTest;
import org.apache.commons.jxpath.ri.model.NodePointer;

/**
 * EvalContext  that returns the current node from the parent context if the
 * test succeeds.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class SelfContext extends EvalContext {
    private NodeTest nodeTest;
    private boolean startedSet = false;
    private NodePointer nodePointer;

    public SelfContext(EvalContext parentContext, NodeTest nodeTest) {
        super(parentContext);
        this.nodeTest = nodeTest;
    }

    public Pointer getSingleNodePointer() {
        return parentContext.getSingleNodePointer();
    }

    public NodePointer getCurrentNodePointer() {
        if (position == 0) {
            if (!setPosition(1)) {
                return null;
            }
        }
        return nodePointer;
    }

    public boolean nextNode() {
        return setPosition(getCurrentPosition() + 1);
    }

    public void reset() {
        super.reset();
        startedSet = false;
    }

    public boolean setPosition(int position) {
        if (position != 1) {
            return false;
        }
        super.setPosition(position);
        if (!startedSet) {
            startedSet = true;
            nodePointer = (NodePointer) parentContext.getCurrentNodePointer();
        }

        if (nodePointer == null) {
            return false;
        }

        return nodeTest == null || nodePointer.testNode(nodeTest);
    }
}
