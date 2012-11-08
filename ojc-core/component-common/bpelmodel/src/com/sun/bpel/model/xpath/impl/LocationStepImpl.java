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
 * @(#)LocationStepImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.xpath.impl;

import com.sun.bpel.model.xpath.LocationStep;
import com.sun.bpel.model.xpath.StepNodeTest;
import com.sun.bpel.model.xpath.XPathExpression;

/**
 * Represents a location path step.
 * 
 * @author Sun Microsystems
 * @version 
 */
public class LocationStepImpl implements LocationStep {
    
    /** The axis. */
    private int mAxis;
    
    /** The node test. */
    private StepNodeTest mNodeTest;

    /** predicates */
    private XPathExpression[] mPredicates = null;
    
    /** Constructor. */
    public LocationStepImpl() {
        this(0, null, null);
    }


    /**
     * Constructor.
     * @param axis the axis
     * @param nodeTest the node test
     */
    public LocationStepImpl(int axis, StepNodeTest nodeTest, XPathExpression[] predicates) {
        setAxis(axis);
        setNodeTest(nodeTest);
        setPredicates(predicates);
    }
    
    
    /**
     * Gets the axis.
     * @return the axis
     */
    public int getAxis() {
        return mAxis;
    }
    
    
    /**
     * Sets the axis.
     * @param axis the axis
     */
    public void setAxis(int axis) {
        mAxis = axis;
    }
    
    
    /**
     * Gets the node test.
     * @return the node test
     */
    public StepNodeTest getNodeTest() {
        return mNodeTest;
    }
    
    
    /**
     * Sets the node test.
     * @param nodeTest the node test
     */
    public void setNodeTest(StepNodeTest nodeTest) {
        mNodeTest = nodeTest;
    }
    
    /**
     * Gets the Predicates
     * @return the predicates
     */
    public XPathExpression[] getPredicates() {
        return mPredicates;
    }
    
    
    /**
     * Sets the Predicates
     * @param predicates list of predicates
     */
    public void setPredicates(XPathExpression[] predicates) {
        mPredicates = predicates;
    }
    
    /**
     * Gets the string representation.
     * @return the string representation
     */
    public String getString() {
        StringBuffer sb = new StringBuffer();
        
        if (LocationStep.AXIS_ATTRIBUTE == getAxis()) {
            sb.append('@');
        }
        
        StepNodeTest nodeTest = getNodeTest();
        if (nodeTest instanceof StepNodeNameTest) {
            sb.append(((StepNodeNameTest) nodeTest).getNodeName());
        } else if (nodeTest instanceof StepNodeTypeTest) {
            sb.append(((StepNodeTypeTest) nodeTest).getNodeTypeString());
        }
        
        return sb.toString();
    }
}
