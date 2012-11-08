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
 * @(#)ProcessDef.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.model;

/**
 * Defines a list of activities representing a transformation process.
 * @author Kevan Simpson
 */
public interface ProcessDef extends Activity {
	/**
	 * Adds an {@link Activity} to the process definition.
	 * @param act The activity to add.
	 */
	public void addActivity(Activity act);
	
	/**
	 * Returns the number of activities in the process definition.
	 * @return the number of activities in the process definition.
	 */
	public int countActivities();
	
	/**
	 * Fetches the {@link Activity} at the specified index.
	 * @param index The specified index.
	 * @return the <code>Activity</code> at the specified index.
	 */
	public Activity getActivity(int index);

	/**
	 * Returns the {@link Invocation} this process definition implements.
	 * @return the <code>Invocation</code> this process definition implements.
	 */
	public Invocation getInvocation();
	
	/**
	 * Returns the target namespace for this process definition.
	 * @return the target namespace for this process definition.
	 */
	public String getTargetNamespace();
}
