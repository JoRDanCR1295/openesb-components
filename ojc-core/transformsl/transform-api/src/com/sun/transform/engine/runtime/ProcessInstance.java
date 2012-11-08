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
 * @(#)ProcessInstance.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.runtime;

import com.sun.transform.descriptor.TransformEndpoint;
import com.sun.transform.engine.model.Activity;
import com.sun.transform.engine.model.ProcessDef;

/**
 * A <code>ProcessInstance</code> is a list of runtime activities
 * executed as the implementation of a provisioned transformation service.
 * 
 * @author Kevan Simpson
 */
public interface ProcessInstance extends InvocationUnit {
    /**
     * Utility method to create an {@link ActivityUnit} using this instance's
     * {@link ProcessUnitFactory}.
     * 
     * @param act An activity definition.
     * @return An executable unit of the specified activity.
     */
    public ActivityUnit createUnit(Activity act);
    
	/**
	 * Returns the currently executing activity or <code>null</code>
	 * if the process instance is complete or not yet started.
	 * @return the currently executing activity or <code>null</code>.
	 */
	public ActivityUnit currentActivity();
	
	/**
	 * Returns the next activity to execute or <code>null</code>
	 * if the process instance is complete.
	 * @return the next activity to execute or <code>null</code>.
	 */
	public ActivityUnit nextActivity();
	
	/**
	 * Returns <code>true</code> if the process instance is complete,
	 * meaning that all its activities have completed execution.
	 * @return <code>true</code> if the process instance is complete, else <code>false</code>.
	 */
	public boolean isComplete();
	
	/**
	 * Returns the model definition of this process instance.
	 * @return the model definition of this process instance.
	 */
	public ProcessDef getProcessDef();
	
	/**
	 * Returns the provisioning endpoint of this process instance.
	 * @return the provisioning endpoint of this process instance.
	 */
	public TransformEndpoint getEndpoint();
}
