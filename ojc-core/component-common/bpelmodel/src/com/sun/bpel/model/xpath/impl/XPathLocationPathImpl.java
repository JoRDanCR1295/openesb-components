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
 * @(#)XPathLocationPathImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.xpath.impl;

import com.sun.bpel.model.xpath.LocationStep;
import com.sun.bpel.model.xpath.XPathLocationPath;
import com.sun.bpel.model.xpath.XPathVisitable;
import com.sun.bpel.model.xpath.XPathVisitor;

/**
 * @todo PUT DESCRIPTION HERE
 * 
 * @author Sun Microsystems
 * @version 
 */
public class XPathLocationPathImpl
    extends XPathExpressionImpl
    implements XPathLocationPath, XPathVisitable {
        
    /** The steps. */
    LocationStep[] mSteps;
    
    /** The absolute flag; defaults to false. */
    boolean mAbsolute;

    /** Flag to figure out if it is a simple path 
     * Recognized paths formatted as foo/bar[3]/baz[@name = 'biz'] .
     */
    private boolean mIsSimplePath;
    
    /**
     * Constructor.
     * @param steps the steps
     */
    public XPathLocationPathImpl(LocationStep[] steps) {
        this(steps, false, true);
    }
    
    
    /**
     * Constructor.
     * @param steps the steps
     * @param absolute flag
     * @param isSimplePath flag whether path is simple
     */
    public XPathLocationPathImpl(LocationStep[] steps, boolean absolute
                                 , boolean isSimplePath) {
        super();
        setSteps(steps);
        setAbsolute(absolute);
        mIsSimplePath = isSimplePath;
    }
    
    
    /**
     * Gets the flag the tells whether this is an absolute path or not.
     * @return flag
     */
    public boolean getAbsolute() {
        return mAbsolute;
    }
    
    
    /**
     * Sets the flag that tells whether this is an absolute path or not.
     * @param absolute flag
     */
    public void setAbsolute(boolean absolute) {
        mAbsolute = absolute;
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
