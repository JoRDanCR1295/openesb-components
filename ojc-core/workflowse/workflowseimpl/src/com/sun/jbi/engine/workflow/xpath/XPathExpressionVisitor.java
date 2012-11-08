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
 * @(#)XPathExpressionVisitor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.xpath;

import java.util.ArrayList;
import java.util.List;

import com.sun.xpath.XPathCoreFunction;
import com.sun.xpath.XPathCoreOperation;
import com.sun.xpath.XPathExpressionPath;
import com.sun.xpath.XPathExtensionFunction;
import com.sun.xpath.XPathVariableReference;
import com.sun.xpath.visitor.AbstractXPathVisitor;

/**
 * Visitor to get the list of xpath variables for the bpel constructs that contain
 * dollar sign for referencing the variables.
 * 
 * @author Sun Microsystems
 *
 */
public class XPathExpressionVisitor extends AbstractXPathVisitor {

    List expressionList = new ArrayList();
    
    public void visit(XPathExpressionPath expressionPath) {
        expressionList.add(expressionPath.getExpressionString());
    }
    
    public void visit(XPathVariableReference arg0) {
        expressionList.add(arg0.getExpressionString());
    }

    public void visit(XPathCoreOperation coreOperation) {
        visitChildren(coreOperation);
    }

    public void visit(XPathCoreFunction coreFunction) {
        visitChildren(coreFunction);
    }

    public void visit(XPathExtensionFunction extensionFunction) {
        visitChildren(extensionFunction);
    }
    
    public List getExpressionList(){
        return expressionList;
    }
}
