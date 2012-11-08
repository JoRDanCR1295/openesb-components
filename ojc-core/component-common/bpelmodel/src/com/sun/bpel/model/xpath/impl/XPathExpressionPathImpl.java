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
 * @(#)XPathExpressionPathImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.xpath.impl;

import com.sun.bpel.model.xpath.LocationStep;
import com.sun.bpel.model.xpath.XPathExpression;
import com.sun.bpel.model.xpath.XPathExpressionPath;
import com.sun.bpel.model.xpath.XPathVisitable;
import com.sun.bpel.model.xpath.XPathVisitor;

/**
 * @author Sun Microsystems
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class XPathExpressionPathImpl extends XPathExpressionImpl 
	implements XPathExpressionPath, XPathVisitable {
	
	/** The steps. */
    private LocationStep[] mSteps;
    
    
    private XPathExpression mRootExpression;
    
    /** The absolute flag; defaults to false. */
    private boolean mAbsolute;

    /** Flag to figure out if it is a simple path 
     * Recognized paths formatted as foo/bar[3]/baz[@name = 'biz'] .
     */
    private boolean mIsSimplePath;
    
    
    /**
     * Constructor.
     * @param steps the steps
     * @param isSimplePath flag whether path is simple
     */
    public XPathExpressionPathImpl(XPathExpression rootExpression,
    							   LocationStep[] steps, 
    							   boolean isSimplePath) {
        super();
        setSteps(steps);
        setRootExpression(rootExpression);
        setSimplePath(isSimplePath);
    }

    /**
     * Gets the steps of the location path.
     * @return the steps
     */
    public LocationStep[] getSteps() {
        return mSteps;
    }
    
    
    /**
     * Sets the steps of the location path.
     * @param steps the steps
     */
    public void setSteps(LocationStep[] steps) {
        mSteps = steps;
    }
    
    /**
     * set root expression of this expression path.
     * @param rootExpression root expression of this expression path.
     */
    public void setRootExpression(XPathExpression rootExpression) {
    	this.mRootExpression = rootExpression;
    }
    
    /**
     * get root expression of this expression path.
     * @return root expression of this expression path
     * @return root expression 
     */
    public XPathExpression getRootExpression() {
    	return this.mRootExpression;
    }
    
    /**
     * Describe <code>isSimplePath</code> method here.
     *
     * @return a <code>boolean</code> value
     */
    public boolean isSimplePath() {
        return mIsSimplePath;
    }

    /**
     * Describe <code>setSimplePath</code> method here.
     *
     * @param isSimplePath a <code>boolean</code> value
     */
    public void setSimplePath(boolean isSimplePath) {
        mIsSimplePath = isSimplePath;
    }

    /**
     * get expression exclusing root expression
     * @return
     */
    public String getExpressionStringExcludingRootExpression() {
    	XPathVisitor visitor = new ExpressionWriter();
        LocationStep[] steps = getSteps();
        if(steps != null) {
        	for(int i = 0; i < steps.length; i++) {
        		visitor.visit(steps[i]);
        	}
        }
        
        return ((ExpressionWriter) visitor).getString();
    }
    
    /**
     * Calls the visitor.
     * @param visitor the visitor
     */
    public void callVisitor(XPathVisitor visitor) {
        boolean result = visitor.visit(this);
        
        if (result) {
            // do nothing
        }
    }
}
