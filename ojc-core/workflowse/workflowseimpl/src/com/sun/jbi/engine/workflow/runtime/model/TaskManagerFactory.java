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
 * @(#)TaskManagerFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.runtime.model;

import java.util.Properties;

import com.sun.jbi.engine.workflow.util.Util;

/**
 *  TaskManagerFactory is the factory for creating TaskManger.
 *  TaskManagerFactory is abstract and there could be multiple implementation
 *  of this. One could be Memory based other could be DB based etc.
 * 
 */
public abstract class TaskManagerFactory {
    
    private static TaskManagerFactory mInstance;
    
    private static final String PROP_FILE = "taskManager.properties";
    
    public static synchronized  TaskManagerFactory getInstance() throws TaskException {
        try {
            if(mInstance == null) {
                String factoryClass = System.getProperty("com.sun.jbi.engine.workflow.runtime.model.TaskManagerFactory");
                if (factoryClass == null) {
                    Properties prop = Util.loadProps(TaskManagerFactory.class, PROP_FILE);
                    factoryClass = prop.getProperty("com.sun.jbi.engine.workflow.runtime.model.TaskManagerFactory");
                }
                Class cls = Class.forName(factoryClass);
                mInstance = (TaskManagerFactory) cls.newInstance();
            }
        } catch(Exception ex) {
            throw new TaskException(ex);
        }
        
        return mInstance;
        
    }

    public abstract TaskManager getTaskManager();
}
