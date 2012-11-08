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
 * @(#)TaskHandlerManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.process;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import org.w3c.dom.Element;

import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTaskTimer;
import com.sun.jbi.engine.workflow.runtime.model.TaskException;
import com.sun.jbi.engine.workflow.runtime.model.TaskManager;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask.TaskState;
import com.sun.jbi.engine.workflow.util.I18n;
import com.sun.jbi.workflow.model.Action;
import com.sun.jbi.workflow.model.ChangeVariables;
import com.sun.jbi.workflow.model.Escalation;
import com.sun.jbi.workflow.model.LocalNotification;
import com.sun.jbi.workflow.model.Notification;
import com.sun.jbi.workflow.model.Task;
import com.sun.jbi.workflow.model.xmlbeans.TActionType;

public class TaskHandlerManager {

    private static final Logger LOGGER = Logger.getLogger(HandlerFactory.class
            .getName());

    private static TaskHandlerManager mInstance;

    private HandlerFactory mHandlerFactory;
    
    private ScheduledExecutorService mScheduledPool = Executors.newScheduledThreadPool(20);

    /** Creates a new instance of TaskHandlerManager */
    private TaskHandlerManager() throws TaskException {
        try {
            mHandlerFactory = HandlerFactory.getInstance();
        } catch (Exception ex) {
            LOGGER.log(Level.SEVERE, I18n
                    .loc("WLM-6048: Failed to get handler factory"), ex);
        }
    }

    public static synchronized TaskHandlerManager getInstance()
            throws TaskException {
        if (mInstance == null) {
            mInstance = new TaskHandlerManager();
        }

        return mInstance;
    }

    public void processTaskTimerHandler(Collection<RuntimeTaskTimer> taskTimers,
            TaskManager manager) throws TaskException {
        synchronized (manager) {
        Iterator<RuntimeTaskTimer> it = taskTimers.iterator();
        while (it.hasNext()) {
            RuntimeTaskTimer rtt = it.next();
            Handler h = mHandlerFactory.newHandler(rtt, this, manager);
            h.execute();
        }
        }
    }

    private void processTaskEscalationNotification(
            List<Escalation> escalations, TaskManager manager)
            throws TaskException {
        Iterator<Escalation> it = escalations.iterator();
        while (it.hasNext()) {
            Escalation e = it.next();
            List<LocalNotification> lnList = e.getLocalNotifications();
            if (lnList != null && lnList.size() > 0) {
                processLocalNotification(lnList, manager,
                        RuntimeTask.TaskState.ESCALATED, e);
            }
        }
    }

    private void processTaskActionNotification(List<Action> actions, 
			   								   TaskManager manager) throws TaskException {
			Iterator<Action> it = actions.iterator();
			while(it.hasNext()) {
			Action a = it.next();
			List<LocalNotification> lnList = a.getLocalNotifications();
			RuntimeTask.TaskState state = converActionType(a);
            if (lnList != null && lnList.size() > 0) {
                processLocalNotification(lnList, 
									 manager,
									 state, null);
            }
            List<ChangeVariables> varList = a.getChangeVariables();
            if (varList != null && varList.size() > 0) {
                processChangeVariable(varList, manager, state);
			}
            }
	}

    private void processChangeVariable(List<ChangeVariables> varList,
            TaskManager manager, TaskState state) throws TaskException {
        // TODO Auto-generated method stub
         Handler varhandler = mHandlerFactory.newChangeVaHandler(varList, state, manager);
         varhandler.execute();

    }

    private void processLocalNotification(List<LocalNotification> lnList,
            TaskManager manager, RuntimeTask.TaskState state, Object modelRef)
            throws TaskException {

        Iterator<LocalNotification> it = lnList.iterator();

        while (it.hasNext()) {
            LocalNotification ln = it.next();
            Notification notification = ln.getReferencedNotification();
            if (notification != null) {
                Handler h = mHandlerFactory.newNotificationHandler(notification, state,
                        this, manager, modelRef);
                h.execute();
            }
        }
    }

    public void processNotificationHandler(Task taskMeta,  TaskManager manager)
            throws TaskException {
        List<Escalation> escalations =taskMeta.getTaskEscalations();
        List<Action> actions = taskMeta.getTaskActions();
        if ( (escalations != null && escalations.size() >0) || (actions  != null  && actions.size() >0 )) {
            QName taskQName =taskMeta.getQName();
            synchronized (manager) {
//          TaskStateListener is processed once for each task definition
                if (!manager.hasTaskStateListeners(taskQName)) {
                    processTaskEscalationNotification(escalations, manager);                
                    processTaskActionNotification(actions, manager);
                }
            }
        }
    }

    public void processTaskStateChange(RuntimeTask task,
            RuntimeTask.TaskState oldState, RuntimeTask.TaskState newState) {

    }

    private Set<TaskHandlerListener> listeners = new CopyOnWriteArraySet<TaskHandlerListener>();

    public void addTaskListener(TaskHandlerListener l) {
        listeners.add(l);
    }

    public void removeTaskListener(TaskHandlerListener l) {
        listeners.remove(l);
    }

    public void notifyOnTimeout(Long taskId, Element output, String taskRelId) {
        TaskEvent evt = new TaskEvent(this, taskId, taskRelId, output);
        for (TaskHandlerListener listener : listeners) {
            listener.onTimeout(evt);
        }
    }

    public void notifyOnNotify(Element output, QName portTypeQName,
            String operationName) {
        TaskNotificationEvent evt = new TaskNotificationEvent(this, output,
                portTypeQName, operationName);
        for (TaskHandlerListener listener : listeners) {
            listener.onNotify(evt);
        }
    }
    
    public ScheduledExecutorService getTimerPool () {
        return mScheduledPool;
    }
    
    private RuntimeTask.TaskState converActionType(Action a) {
        String actionType = a.getType().toString();
        RuntimeTask.TaskState state = RuntimeTask.TaskState
                .getTaskState(actionType);
        return state;
    }   


}
