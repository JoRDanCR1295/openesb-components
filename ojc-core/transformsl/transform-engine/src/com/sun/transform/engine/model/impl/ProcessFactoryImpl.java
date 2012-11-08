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
 * @(#)ProcessFactoryImpl.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.model.impl;

import javax.jbi.management.DeploymentException;
import javax.wsdl.Operation;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.transform.I18n;
import com.sun.transform.engine.model.Invocation;
import com.sun.transform.engine.model.Param;
import com.sun.transform.engine.model.ProcessDef;
import com.sun.transform.engine.model.ProcessFactory;
import com.sun.transform.engine.model.Transform;
import com.sun.transform.engine.model.Param.Type;
import com.sun.transform.engine.runtime.ProcessingException;

/**
 * Factory to create process activities.
 * @author Kevan Simpson
 */
public class ProcessFactoryImpl implements ProcessFactory {
    /** @see com.sun.transform.engine.model.ProcessFactory#createInvocation(java.lang.String, com.sun.jbi.common.descriptor.EndpointInfo, javax.wsdl.Operation, java.lang.String, java.lang.String) */
	public Invocation createInvocation(String name, EndpointInfo info,
									   Operation op,
									   String inputVar, String outputVar) {
		return new InvocationImpl(name, info, op, inputVar, outputVar);
	}

	/** @see com.sun.transform.engine.model.ProcessFactory#createParam(java.lang.String, com.sun.transform.engine.model.Param.Type) */
	public Param createParam(String name, Type type) {
		return new ParamImpl(name, type);
	}

	/** @see com.sun.transform.engine.model.ProcessFactory#createProcessDef(com.sun.transform.engine.model.Invocation) */
	public ProcessDef createProcessDef(Invocation invoke) {
		return new ProcessDefImpl(invoke);
	}

	/** @see com.sun.transform.engine.model.ProcessFactory#createTransform(java.lang.String, java.lang.String, java.lang.String, java.lang.String) */
	public Transform createTransform(String name, String file, 
									 String inputPart, String outputPart) {
	    return new TransformImpl<Object>(name, file, inputPart, outputPart) {
            public void compile(String rootPath) throws DeploymentException {
                throw new DeploymentException(I18n.loc(
                        "TRANSL-6050: Compilation not supported by this activity!"));
            }

            public Object newTransformer() throws ProcessingException {
                throw new ProcessingException(I18n.loc(
                        "TRANSL-6051: newTransformer not supported by this activity!"));
            }
        };
	}
}
