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
 * @(#)Transform.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.model;

import java.util.Map;

import javax.jbi.management.DeploymentException;

import com.sun.transform.engine.runtime.ProcessingException;

/**
 * Represents a generic transformation activity.
 * @author Kevan Simpson
 */
public interface Transform<T> extends Activity {
    /**
     * Compiles the activity's transformation file using the specified root path.
     * @param rootPath The root path at which the transformation file can be found.
     * @throws DeploymentException if the stylesheet cannot be compiled.
     */
    public void compile(String rootPath) throws DeploymentException;
    
    /**
     * Returns an object capable of executing a transformation.
     * @return a transformer.
     * @throws ProcessingException if a transformer cannot be created.
     */
    public T newTransformer() throws ProcessingException;
    
    /**
     * Adds an invocation definition to this transformation.
     * @param invoke An invocation.
     */
    public void addInvocation(Invocation invoke);
    
    /**
     * Fetches the definitions of invocations that <b>MAY</b> occur during a transformation.
     * @return the definitions of invocations that <b>MAY</b> occur during a transformation.
     */
    public Map<String, Invocation> getInvocations();
    
	/**
	 * Adds a {@link Param} to this transformation.
	 * @param p A stylesheet param.
	 */
	public void addParam(Param p);

	/**
	 * Returns parameters associated with this transformation.
	 * @return parameters associated with this transformation.
	 */
	public Param[] getParams();

	/**
	 * Returns the filename of the transformation stylesheet.
	 * @return the filename of the transformation stylesheet.
	 */
	public String getFile();

	/**
	 * Returns the variable reference for transformation source.
	 * @return the variable reference for transformation source.
	 */
	public String getSource();

	/**
	 * Returns the variable reference for transformation result.
	 * @return the variable reference for transformation result.
	 */
	public String getResult();
}
