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
 * @(#)AncestorContext.java 
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
 * EvalContext that walks the "ancestor::" and "ancestor-or-self::" axes.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class AncestorContext extends EvalContext {
    private NodeTest nodeTest;
    private boolean setStarted = false;
    private NodePointer currentNodePointer;
    private boolean includeSelf;

    /**
     * @param parentContext represents the previous step on the path
     * @param  includeSelf differentiates between "ancestor::" and "ancestor-
     * or-self::" axes
     * @param nameTest is the name of the element(s) we are looking for
     */
    public AncestorContext(
        EvalContext parentContext,
        boolean includeSelf,
        NodeTest nodeTest) 
    {
        super(parentContext);
        this.includeSelf = includeSelf;
        this.nodeTest = nodeTest;
    }

    public NodePointer getCurrentNodePointer() {
        return currentNodePointer;
    }

    public int getDocumentOrder() {
        return -1;
    }

    public void reset() {
        super.reset();
        setStarted = false;
    }

    public boolean setPosition(int position) {
        if (position < getCurrentPosition()) {
            reset();
        }

        while (getCurrentPosition() < position) {
            if (!nextNode()) {
                return false;
            }
        }
        return true;
    }

    public boolean nextNode() {
        if (!setStarted) {
            setStarted = true;
            currentNodePointer = parentContext.getCurrentNodePointer();
            if (includeSelf) {
                if (currentNodePointer.testNode(nodeTest)) {
                    position++;
                    return true;
                }
            }
        }

        while (true) {
            currentNodePointer = currentNodePointer.getImmediateParentPointer();

            if (currentNodePointer == null) {
                return false;
            }

            if (currentNodePointer.testNode(nodeTest)) {
                position++;
                return true;
            }
        }
    }
}
