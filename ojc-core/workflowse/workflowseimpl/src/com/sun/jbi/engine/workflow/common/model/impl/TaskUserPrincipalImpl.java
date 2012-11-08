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
 * @(#)TaskUserPrincipalImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.common.model.impl;

import com.sun.jbi.engine.workflow.common.model.TaskPrincipal;

/**
 *
 * 
 */
public class TaskUserPrincipalImpl implements TaskPrincipal {
    
    private String mName;
    
    /** Creates a new instance of TaskPrincipalImpl */
    public TaskUserPrincipalImpl(String name) {
        this.mName = name;
    }

    public String getName() {
        return this.mName;
    }
    
    
    public int hashCode() {
        if(this.mName != null) {
            return this.mName.hashCode();
        }
        
        return super.hashCode();
    }

    public boolean equals(Object obj) {
        if (! (obj instanceof TaskUserPrincipalImpl))
            return false;
        if(this.mName != null) {
            return this.mName.equals(((TaskUserPrincipalImpl) obj).getName());
        }
        
        return super.equals(obj);
    }

    public PrincipalType getType() {
        return PrincipalType.User;
    }
    
    public String toString() {
    	return getName();
    }
}
