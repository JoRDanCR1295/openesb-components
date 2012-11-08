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
 * @(#)XPathLocationPath.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xpath;

/**
 * Represents a location path.
 * 
 * @author Sun Microsystems
 * @version 
 */
public interface XPathLocationPath extends XPathExpression {
    
    /**
     * Gets the flag the tells whether this is an absolute path or not.
     * @return flag
     */
    boolean getAbsolute();
    
    
    /**
     * Sets the flag that tells whether this is an absolute path or not.
     * @param absolute flag
     */
    void setAbsolute(boolean absolute);
    
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
}
