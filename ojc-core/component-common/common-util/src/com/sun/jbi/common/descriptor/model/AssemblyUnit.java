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
 * @(#)AssemblyUnit.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.descriptor.model;

/**
 * Represents a service unit configuration in a service assembly descriptor.
 * @author Kevan Simpson
 */
public class AssemblyUnit {
    private Target mTarget;
    private Identification mId;
    
    /**
     * Constructs an <code>AssemblyUnit</code>.
     * @param target The unit's artifact and component.
     * @param id The unit's name and description.
     */
    public AssemblyUnit(Target target, Identification id) {
        mTarget = target;
        mId = id;
    }

    /**
     * Returns the unit's id.
     * @return the unit's id.
     */
    public Identification getId() {
        return mId;
    }

    /**
     * Returns the unit's target.
     * @return the unit's target.
     */
    public Target getTarget() {
        return mTarget;
    }
}
