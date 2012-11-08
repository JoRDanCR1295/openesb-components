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
 * @(#)TaskModelFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.common.model;

import javax.xml.transform.Source;
import org.w3c.dom.Element;

import com.sun.jbi.engine.workflow.WorkflowException;

/**
 *
 *TODO: use typed exception
 */
public abstract class TaskModelFactory {
    
    private static TaskModelFactory mInstance;
            
    public abstract TaskPrincipal createPrincipal(String name, TaskPrincipal.PrincipalType type);
    
    
    public abstract TaskInput createTaskInput(Source source) throws WorkflowException;
    
    public abstract TaskOutput createTaskOutput(Source source)throws WorkflowException;
    
    public abstract TaskInput createTaskInput(Element source) throws WorkflowException;
    
    public abstract TaskOutput createTaskOutput(Element source)throws WorkflowException;
    
    public abstract TaskPrincipal createDefaultPrincipal ();
    
    public static synchronized TaskModelFactory getInstance() throws WorkflowException {
        try {
    	if(mInstance == null) {
            String factoryClass = System.getProperty("com.sun.jbi.engine.workflow.common.model.TaskModelFactory", "com.sun.jbi.engine.workflow.common.model.impl.TaskModelFactoryImpl");
            Class cls = Class.forName(factoryClass);
            mInstance = (TaskModelFactory) cls.newInstance();
        }
        }catch(Exception ex) {
        	throw new WorkflowException(ex);
        }

        return mInstance;
    }
    
}
