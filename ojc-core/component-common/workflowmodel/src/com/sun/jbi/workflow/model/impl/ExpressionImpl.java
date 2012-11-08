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
 * @(#)$Id: ExpressionImpl.java,v 1.4 2010/02/04 02:53:27 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.workflow.model.impl;

import org.apache.commons.jxpath.JXPathContext;
import org.w3c.dom.Node;

import com.sun.jbi.workflow.model.ModelElement;
import com.sun.jbi.workflow.model.ModelException;
import com.sun.jbi.workflow.model.utl.ModelUtil;
import com.sun.jbi.workflow.model.xmlbeans.TExpression;

public class ExpressionImpl extends ModelElementImpl {
    


    protected TExpression mExpression;

    public ExpressionImpl(TExpression subject, ModelElement parent) {
        super(subject, parent);
        mExpression = subject;
    }

    public String getContent(JXPathContext context) {
//        boolean isXpath20 = false;
//        if (mExpression.getExpressionLanguage() != null && mExpression.getExpressionLanguage().equals(XPATH20)) {
//            isXpath20 = true;
//        }
//        Node domNode = this.mExpression.getDomNode();
//        String text = null;
//        if (domNode != null) {
//            text = ModelUtil.getText(domNode);
//            String strVal = null;
//            try {
//                if (text != null && text.trim().length() > 0) {
//                    if (isXpath20) {
//                        strVal = ModelUtil.getStringValueXpath20(context, text, getTask(), "");
//                    } else {
//                        strVal = ModelUtil.getStringValue(context, text, "");
//                    }
//                }
//            } catch (Exception e) {
//                // TODO Auto-generated catch block
//                throw new ModelException(e);
//            }
//            return strVal;
//        }
        return ModelUtil.getExpressionContent(context, mExpression, getTask (), "");
    }
    
    
    
}
