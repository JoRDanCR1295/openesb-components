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
 * @(#)CoreOperationAnd.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.compiler;

import org.apache.commons.jxpath.ri.EvalContext;
import org.apache.commons.jxpath.ri.InfoSetUtil;

/**
 * Implementation of Expression for the operation "and".
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class CoreOperationAnd extends CoreOperation {

    public CoreOperationAnd(Expression[] args) {
        super(args);
    }

    public Object computeValue(EvalContext context) {
        for (int i = 0; i < args.length; i++) {
            if (!InfoSetUtil.booleanValue(args[i].computeValue(context))) {
                return Boolean.FALSE;
            }
        }
        return Boolean.TRUE;
    }
    
    protected int getPrecedence() {
        return 1;
    }

    protected boolean isSymmetric() {
        return true;
    }
    
    public String getSymbol() {
        return "and";
    }
}
