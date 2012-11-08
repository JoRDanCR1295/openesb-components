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
 * @(#)ProcessFactory.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.model;

import javax.wsdl.Operation;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.transform.engine.model.Param.Type;

/**
 * Factory to create transformation process activities.
 * @author Kevan Simpson
 */
public interface ProcessFactory {
	/**
	 * Creates an {@link Invocation} instance.
     * @param name The name of the invocation.
	 * @param info The service information for the endpoint to invoke.
	 * @param op The operation consumed or provisioned by this invocation.
	 * @param inputVar The variable reference of the incoming message.
	 * @param outputVar The variable reference of the outgoing message.
	 * @return an <code>Invocation</code>.
	 */
	public Invocation createInvocation(String name,
	                                   EndpointInfo info, 
									   Operation op,
									   String inputVar, 
									   String outputVar);
	/**
	 * Creates a {@link Param} instance.
	 * @param name The parameter's name.
	 * @param type The parameter's type.
	 * @return a <code>Param</code>.
	 */
	public Param createParam(String name, Type type);

	/**
	 * Creates a {@link ProcessDef} instance.
	 * @param invoke The {@link Invocation} the process definition implements.
	 * @return a <code>ProcessDef</code>.
	 */
	public ProcessDef createProcessDef(Invocation invoke);
	
	/**
	 * Creates a {@link Transform} instance.
     * @param name The name of the transformation.
	 * @param file The transformation stylesheet filename.
	 * @param source The variable reference for transformation source.
	 * @param result The variable reference for transformation result.
	 * @return a <code>Transform</code>.
	 */
	public Transform createTransform(String name, String file, 
									 String source, String result);
}
