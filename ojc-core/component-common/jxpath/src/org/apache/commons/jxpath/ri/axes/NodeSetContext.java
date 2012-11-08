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
 * @(#)NodeSetContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.axes;

import org.apache.commons.jxpath.NodeSet;
import org.apache.commons.jxpath.ri.EvalContext;
import org.apache.commons.jxpath.ri.model.NodePointer;

/**
 * A simple context that is based on a NodeSet.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class NodeSetContext extends EvalContext {
    private boolean startedSet = false;
    private NodeSet nodeSet;

    public NodeSetContext(EvalContext parentContext, NodeSet nodeSet) {
        super(parentContext);
        this.nodeSet = nodeSet;
    }
    
    public NodeSet getNodeSet() {
        return nodeSet;
    }

    public NodePointer getCurrentNodePointer() {
        if (position == 0) {
            if (!setPosition(1)) {
                return null;
            }
        }
        return (NodePointer) nodeSet.getPointers().get(position - 1);
    }

    public boolean setPosition(int position) {
        super.setPosition(position);
        return position >= 1 && position <= nodeSet.getPointers().size();
    }

    public boolean nextSet() {
        if (startedSet) {
            return false;
        }
        startedSet = true;
        return true;
    }

    public boolean nextNode() {
        return setPosition(position + 1);
    }
}
