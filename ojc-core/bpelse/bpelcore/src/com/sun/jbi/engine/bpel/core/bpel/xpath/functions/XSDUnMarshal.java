/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */
 /*
  * $Id: XSDUnMarshal.java,v 1.1 2007/10/09 06:03:46 mpottlapelli Exp $
  *
  * Copyright 2006 Sun Microsystems, Inc. All Rights Reserved.  */
package com.sun.jbi.engine.bpel.core.bpel.xpath.functions;

import org.apache.commons.jxpath.ExpressionContext;
import org.apache.commons.jxpath.Function;
import org.apache.commons.jxpath.Pointer;
import org.apache.commons.jxpath.ri.EvalContext;
import org.w3c.dom.Node;

import com.sun.jbi.engine.bpel.core.bpel.util.DOMHelper;

/**
 * WS-BPEL Extension function XSDMarshal implementation
 *
 * @author Sun Microsystems
 */
public class XSDUnMarshal implements Function {

    /*
     * @see org.apache.commons.jxpath.Function#invoke(org.apache.commons.jxpath.ExpressionContext, java.lang.Object[])
     */
    public Object invoke(ExpressionContext context, Object[] parameters) {
        String input = (String)convertParam(parameters[0]);
        input = input.trim(); //It is required to for removing leading spaces so the parser understands prolog with no error
        return DOMHelper.createDOM(input);
    }

    private Object convertParam(Object src) {
        if (src instanceof Pointer) {
            return ((Pointer) src).getValue();
        }
        else if (src instanceof EvalContext) {
            return convertParam(((EvalContext) src).getCurrentNodePointer());
        }
        return null;
    }

}
