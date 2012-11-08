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
 * @(#)UnionContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.axes;

import java.util.HashSet;

import org.apache.commons.jxpath.BasicNodeSet;
import org.apache.commons.jxpath.ri.EvalContext;
import org.apache.commons.jxpath.ri.model.NodePointer;

/**
 * EvalContext that represents a union between other contexts - result
 * of a union operation like (a | b)
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class UnionContext extends NodeSetContext {
    private boolean startedSet = false;
    private EvalContext contexts[];
    private boolean prepared = false;

    public UnionContext(EvalContext parentContext, EvalContext contexts[]) {
        super(parentContext, new BasicNodeSet());
        this.contexts = contexts;
    }

    public int getDocumentOrder() {
        if (contexts.length > 1) {
            return 1;
        }
        return super.getDocumentOrder();
    }

    public boolean setPosition(int position) {
        if (!prepared) {
            prepared = true;
            BasicNodeSet nodeSet = (BasicNodeSet) getNodeSet();
            HashSet set = new HashSet();
            for (int i = 0; i < contexts.length; i++) {
                EvalContext ctx = (EvalContext) contexts[i];
                while (ctx.nextSet()) {
                    while (ctx.nextNode()) {
                        NodePointer ptr = ctx.getCurrentNodePointer();
                        if (!set.contains(ptr)) {
                            nodeSet.add(ptr);
                            set.add(ptr);
                        }
                    }
                }
            }
        }
        return super.setPosition(position);
    }
}
