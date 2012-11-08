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
 * @(#)Invocation.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.model;

import java.util.Iterator;
import javax.wsdl.Operation;

import com.sun.jbi.common.descriptor.EndpointInfo;


/**
 * Describes an invocation activity in a transformation process.
 * @author Kevan Simpson
 */
public interface Invocation extends Activity {
	/**
	 * Returns entry information related to the endpoint to invoke.
	 * @return entry information related to the endpoint to invoke.
	 */
	public EndpointInfo getInfo();
	
	/**
	 * Returns the operation consumed or provisioned by this invocation.
	 * @return the operation consumed or provisioned by this invocation.
	 */
	public Operation getOperation();
	
	/**
	 * Returns the variable reference of the incoming message.
	 * @return the variable reference of the incoming message.
	 */
	public String getInputVariable();
	
	/**
	 * Returns the variable reference of the outgoing message.
	 * @return the variable reference of the outgoing message.
	 */
	public String getOutputVariable();

	/**
	 * Adds a {@link Transform} to apply to a received fault message.
	 * @param tr The activity to apply to a received fault message.
	 */
	public void addTransform(Transform tr);
	
	/**
	 * Fetches a fault-handling {@link Transform} activity by name.
	 * @param name The name of the fault-handling activity.
	 * @return a fault-handling <code>Transform</code> activity by name.
	 */
	public Transform getTransform(String name);
	
	public Iterator<Transform> getFaultHandlers();
}
