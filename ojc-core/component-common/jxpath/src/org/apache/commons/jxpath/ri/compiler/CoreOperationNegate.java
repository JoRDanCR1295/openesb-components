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
 * @(#)CoreOperationNegate.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.compiler;

import org.apache.commons.jxpath.ri.EvalContext;
import org.apache.commons.jxpath.ri.InfoSetUtil;

/**
 * Implementation of Expression for the operation unary "-".
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class CoreOperationNegate extends CoreOperation {

    public CoreOperationNegate(Expression arg) {
        super(new Expression[] { arg });
    }

    public Object computeValue(EvalContext context) {
        double a = InfoSetUtil.doubleValue(args[0].computeValue(context));
        return new Double(-a);
    }
    
    protected int getPrecedence() {
        return 6;
    }

    protected boolean isSymmetric() {
        return false;
    }
    
    public String getSymbol() {
        return "-";
    }
}
