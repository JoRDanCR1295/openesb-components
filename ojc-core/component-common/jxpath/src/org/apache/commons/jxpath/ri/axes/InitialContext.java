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
 * @(#)InitialContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.axes;

import org.apache.commons.jxpath.Pointer;
import org.apache.commons.jxpath.ri.EvalContext;
import org.apache.commons.jxpath.ri.model.NodePointer;

/**
 * A single-set EvalContext that provides access to the current node of
 * the parent context and nothing else.  It does not pass the iteration
 * on to the parent context.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class InitialContext extends EvalContext {
    private boolean startedSet = false;
    private boolean started = false;
    private boolean collection;
    private NodePointer nodePointer;

    public InitialContext(EvalContext parentContext) {
        super(parentContext);
        nodePointer =
            (NodePointer) parentContext.getCurrentNodePointer().clone();
        if (nodePointer != null) {
            collection =
                (nodePointer.getIndex() == NodePointer.WHOLE_COLLECTION);
        }
    }

    public Pointer getSingleNodePointer() {
        return nodePointer;
    }

    public NodePointer getCurrentNodePointer() {
        return nodePointer;
    }
    
    public Object getValue() {
        return nodePointer.getValue();
    }
    
    public boolean nextNode() {
        return setPosition(position + 1);
    }

    public boolean setPosition(int position) {
        this.position = position;
        if (collection) {
            if (position >= 1 && position <= nodePointer.getLength()) {
                nodePointer.setIndex(position - 1);
                return true;
            }
            return false;
        }
        else {
            return position == 1;
        }
    }

    public boolean nextSet() {
        if (started) {
            return false;
        }
        started = true;
        return true;
    }
}
