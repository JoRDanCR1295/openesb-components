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
 * @(#)ProcessUnitFactory.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.runtime;

import javax.xml.namespace.QName;

import com.sun.transform.descriptor.TransformEndpoint;
import com.sun.transform.engine.model.Activity;
import com.sun.transform.engine.model.Invocation;
import com.sun.transform.engine.model.Transform;

/**
 * Factory to create runtime activity units.
 * @author Kevan Simpson
 */
public interface ProcessUnitFactory {
	/**
	 * Creates an {@link ActivityUnit} based on the specified activity model
	 * and appends it to a {@link ProcessInstance}. This should be the only
	 * method called for most use cases.
	 * 
	 * @param act The activity model.
	 * @param proc The process instance to which the activity is added.
	 * @return a runtime activity unit.
	 */
	public ActivityUnit create(Activity act, ProcessInstance proc);
	
	/**
	 * Returns an implementation of {@link ProcessInstance}.
	 * @param operation The name of the operation the process implements.
	 * @param endpt The provisioning endpoint of the new instance.
	 * @return a process instance.
	 */
	public ProcessInstance create(QName operation, TransformEndpoint endpt);
	
	/**
	 * Returns an implementation of {@link InvocationUnit}, which
	 * is added to the specified process instance.
	 * 
	 * @param inv The invocation model.
	 * @param proc The process instance to which the activity is added. 
	 * @return a runtime invocation unit.
	 */
	public InvocationUnit create(Invocation inv, ProcessInstance proc);

	/**
	 * Returns an implementation of {@link TransformUnit}, which
	 * is added to the specified process instance.
	 * 
	 * @param tr The transformation model.
	 * @param proc The process instance to which the activity is added. 
	 * @return a runtime transformation unit.
	 */
	public TransformUnit create(Transform tr, ProcessInstance proc);
}
