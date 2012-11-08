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
 * @(#)ActivityUnit.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.runtime;

import com.sun.transform.engine.model.Activity;

/**
 * Basic runtime activity unit.
 * @author Kevan Simpson
 */
public interface ActivityUnit {
	/**
	 * Executes this activity with the specified {@link Engine}.
	 * @param eng A runtime engine.
	 * @return <code>true</code> if the activity completes execution, else <code>false</code>.
	 * @throws ProcessingException if an error occurs during execution.
	 */
	public boolean execute(Engine eng) throws ProcessingException;
	
	/**
	 * Returns the activity model definition.
	 * @return the activity model definition.
	 */
	public Activity getActivity();
	
	/**
	 * Returns the {@link ProcessInstance} to which this activity belongs.
	 * @return the <code>ProcessInstance</code> to which this activity belongs.
	 */
	public ProcessInstance getEnclosingProcess();
	
	/**
	 * Returns the variable context associated with this activity.
	 * @return the variable context associated with this activity.
	 */
	public VariableContext getVariableContext();
}
