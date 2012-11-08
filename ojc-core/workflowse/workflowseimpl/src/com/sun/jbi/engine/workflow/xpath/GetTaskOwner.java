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
 * @(#)$Id: GetTaskOwner.java,v 1.2 2010/02/15 19:24:47 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.xpath;

import java.util.logging.Logger;

import org.apache.commons.jxpath.ExpressionContext;
import org.apache.commons.jxpath.Function;
import org.apache.commons.jxpath.JXPathContext;

import com.sun.jbi.workflow.model.RuntimeVariables;

public class GetTaskOwner implements Function {
    
    private static final Logger LOGGER = Logger.getLogger(GetTaskOwner.class.getName());

    public Object invoke(ExpressionContext ctx, Object[] arg1) {
        // TODO Auto-generated method stub
        JXPathContext context = ctx.getJXPathContext();
        if (context.getVariables().isDeclaredVariable(RuntimeVariables.TASK_INSTANCE_OWNER)) {
            String taskOwner = (String) context.getValue("$" + RuntimeVariables.TASK_INSTANCE_OWNER);
            return taskOwner;
        }
        return null;
    }

}
