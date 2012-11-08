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
 * @(#)HandlerFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.process;

import java.util.List;
import java.util.logging.Logger;

import com.sun.jbi.engine.workflow.process.notification.ChangeVariableHandler;
import com.sun.jbi.engine.workflow.process.notification.NotificationHandler;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTaskTimer;
import com.sun.jbi.engine.workflow.runtime.model.TaskException;
import com.sun.jbi.engine.workflow.runtime.model.TaskManager;
import com.sun.jbi.workflow.model.ChangeVariables;
import com.sun.jbi.workflow.model.Escalation;
import com.sun.jbi.workflow.model.ModelElement;
import com.sun.jbi.workflow.model.Notification;
import com.sun.jbi.workflow.model.Timeout;


public class HandlerFactory {
	
	private static final Logger LOGGER = Logger.getLogger(HandlerFactory.class.getName());

	
	private static HandlerFactory mInstance;
	
    
	public static synchronized HandlerFactory getInstance() throws TaskException {
        if(mInstance == null) {
            mInstance = new HandlerFactory();
        }
        
        return mInstance;
    }
	
    public Handler newHandler(RuntimeTaskTimer rTimer, TaskHandlerManager handerManager, TaskManager taskManager) throws TaskException {
            Handler handler = null;
            ModelElement deadlineOrDuration = (ModelElement) rTimer.getTimerMetaData();
            if(deadlineOrDuration instanceof Timeout) {
                    handler = new TimeOutHandler(rTimer, handerManager, taskManager);
            } else if (deadlineOrDuration instanceof Escalation) {
                    handler = new EscalationHandler(rTimer,  taskManager);
            } else {
                    throw new TaskException("No handler found for model element" + deadlineOrDuration.getNodeName());
            }

            return handler;

    }

    public Handler newNotificationHandler(Notification notification,
    						  RuntimeTask.TaskState state,
    						  TaskHandlerManager handerManager, 
    						  TaskManager taskManager, Object modelRef) throws TaskException {
        Handler handler =  new NotificationHandler(notification,
        										   state,
        										   handerManager, 
        										   taskManager, modelRef);
        

        return handler;

}
    
    public Handler newChangeVaHandler (List<ChangeVariables> changeVariables, RuntimeTask.TaskState newState,
            TaskManager taskManager) throws TaskException {
        Handler handler =  new ChangeVariableHandler (changeVariables, newState, taskManager);
        
        return handler;
        
    }

    
}
