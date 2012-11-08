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
 * @(#)Activity.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.model;


/**
 * Basic interface for transformation process activities.
 * 
 * @author Kevan Simpson
 */
public interface Activity {
    /**
     * Returns the name of this <code>Activity</code>.
     * @return the name of this <code>Activity</code>.
     */
    public String getName();
    
    /**
     * Returns the validation to be applied to this <code>Activity</code>.
     * <p>
     * The value of this attribute is applied as follows:<br>
     * If the value is
     * <ul>
     *      <li>empty or <code>null</code> - no validation is applied.</li>
     *      <li>&quot;true&quot; - if validation fails, an ERROR status will be returned.</li>
     *      <li>a <code>NMToken</code> - if the implemented operation defines
     *          a fault with the same name, a <code>Fault</code> will be returned;
     *          else, an ERROR status.</li>
     *      <li>a <code>QName</code> -  if the implemented operation defines
     *          a fault with the same type, a <code>Fault</code> will be returned;
     *          else, an ERROR status.</li>
     * </ul>
     * @return the validation to be applied to this <code>Activity</code>.
     */
//    public String getValidate();
    
    /**
     * Sets the validation to be applied to this <code>Activity</code>.
     * @param attr The validation.
     * @see #getValidate()
     */
//    public void setValidate(String attr);
}
