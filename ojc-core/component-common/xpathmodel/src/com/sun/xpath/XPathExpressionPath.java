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
 * @(#)XPathExpressionPath.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xpath;



/**
 * @author Sun Microsystems
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public interface XPathExpressionPath extends XPathExpression {
	
	/**
     * Gets the steps of the location path.
     * @return the steps
     */
    LocationStep[] getSteps();
    
    
    /**
     * Sets the steps of the location path.
     * @param steps the steps
     */
    void setSteps(LocationStep[] steps);


    /**
     * set root expression of this expression path.
     * @param rootExpression root expression of this expression path.
     */
    void setRootExpression(XPathExpression rootExpression);
    
    /**
     * get root expression of this expression path.
     * @return root expression of this expression path
     * @return
     */
    XPathExpression getRootExpression();
    
    
    /**
     * Describe <code>setSimplePath</code> method here.
     *
     * @param isSimplePath a <code>boolean</code> value
     */
    void setSimplePath(boolean isSimplePath);

    /**
     * Describe <code>isSimplePath</code> method here.
     *
     * @return a <code>boolean</code> value
     */
    boolean isSimplePath();
    
    /**
     * get expression exclusing root expression
     * @return
     */
    String getExpressionStringExcludingRootExpression();
}
