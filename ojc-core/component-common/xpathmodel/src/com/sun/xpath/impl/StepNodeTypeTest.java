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
 * @(#)StepNodeTypeTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xpath.impl;

import com.sun.xpath.LocationStep;
import com.sun.xpath.StepNodeTest;


/**
 * Represents a node test on type.
 * 
 * @author Sun Microsystems
 * @version 
 */
public class StepNodeTypeTest extends StepNodeTest {

    /** The node type. */
    private int mNodeType;
    
    
    /**
     * Constructor.
     * @param nodeType the node type
     */
    public StepNodeTypeTest(int nodeType) {
        super();
        mNodeType = nodeType;
    }
    
    
    /**
     * Gets the node type.
     * @return the node type
     */
    public int getNodeType() {
        return mNodeType;
    }
    
    
    /**
     * Gets the display string.
     * @return the display string or null if invalid
     */
    public String getNodeTypeString() {
        int nodeType = getNodeType();
        
        switch (nodeType) {
        case LocationStep.NODETYPE_NODE:
            return "node()";
        case LocationStep.NODETYPE_TEXT:
            return "text()";
        case LocationStep.NODETYPE_COMMENT:
            return "comment()";
        case LocationStep.NODETYPE_PI:
            return "processing-instruction()";
        }
        
        return null;
    }
}
