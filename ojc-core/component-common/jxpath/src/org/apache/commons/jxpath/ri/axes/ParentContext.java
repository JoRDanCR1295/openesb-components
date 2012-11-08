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
 * @(#)ParentContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.axes;

import org.apache.commons.jxpath.ri.EvalContext;
import org.apache.commons.jxpath.ri.compiler.NodeTest;
import org.apache.commons.jxpath.ri.model.NodePointer;

/**
 * EvalContext that walks the "parent::" axis.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class ParentContext extends EvalContext {
    private NodeTest nodeTest;
    private boolean setStarted = false;
    private NodePointer currentNodePointer;

    public ParentContext(EvalContext parentContext, NodeTest nodeTest) {
        super(parentContext);
        this.nodeTest = nodeTest;
    }

    public NodePointer getCurrentNodePointer() {
        return currentNodePointer;
    }

    public int getCurrentPosition() {
        return 1;
    }

    public int getDocumentOrder() {
        return -1;
    }

    public void reset() {
        super.reset();
        setStarted = false;
    }

    public boolean setPosition(int position) {
        super.setPosition(position);
        return position == 1;
    }

    public boolean nextNode() {
        // Each set contains exactly one node: the parent
        if (setStarted) {
            return false;
        }
        setStarted = true;
        NodePointer thisLocation = parentContext.getCurrentNodePointer();
        currentNodePointer = thisLocation.getImmediateParentPointer();
        while (currentNodePointer != null
            && currentNodePointer.isContainer()) {
            currentNodePointer = currentNodePointer.getImmediateParentPointer();
        }
        if (currentNodePointer != null
            && currentNodePointer.testNode(nodeTest)) {
            position++;
            return true;
        }
        return false;
    }
}
