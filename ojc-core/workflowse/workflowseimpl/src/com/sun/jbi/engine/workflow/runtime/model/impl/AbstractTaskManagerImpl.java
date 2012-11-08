/* 
 * The contents of this file are subject to the terms 
 * of the Common Development and Distribution License 
 * (the "License").  You may not use this file except 
 * in compliance with the License. 
 * 
 * You can obtain a copy of the license at 
 * https://glassfish.dev.java.net/public/CDDLv1.0.html. 
 * See the License for the specific language governing 
 * permissions and limitations under the License. 
 * 
 * When distributing Covered Code, include this CDDL 
 * HEADER in each file and include the License file at 
 * https://glassfish.dev.java.net/public/CDDLv1.0.html. 
 * If applicable add the following below this CDDL HEADER, 
 * with the fields enclosed by brackets "[]" replaced with 
 * your own identifying information: Portions Copyright 
 * [year] [name of copyright owner] 
 */
/* 
 * $Id: AbstractTaskManagerImpl.java,v 1.16 2010/02/15 19:24:12 fkieviet Exp $
 * 
 * Copyright 2006 Sun Microsystems, Inc. All Rights Reserved.  */

package com.sun.jbi.engine.workflow.runtime.model.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

import javax.naming.NamingException;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.xml.namespace.QName;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.jbi.engine.workflow.EngineContext;
import com.sun.jbi.engine.workflow.WorkflowMapEntry;
import com.sun.jbi.engine.workflow.clientapi.operations.TasklistResult;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask;
import com.sun.jbi.engine.workflow.runtime.model.TaskException;
import com.sun.jbi.engine.workflow.runtime.model.TaskManager;
import com.sun.jbi.engine.workflow.runtime.model.TaskManagerListener;
import com.sun.jbi.engine.workflow.runtime.model.TaskStateEvent;
import com.sun.jbi.engine.workflow.runtime.model.TaskStateListener;
import com.sun.jbi.engine.workflow.util.JBIMessageUtil;
import com.sun.jbi.engine.workflow.util.Util;
import com.sun.jbi.workflow.model.DeadlineOrDuration;
import com.sun.jbi.workflow.model.ModelException;
import com.sun.jbi.workflow.model.Task;
import com.sun.jbi.workflow.taskcommon.xmlbeans.QueryType;

public abstract class AbstractTaskManagerImpl implements TaskManager {

    private Set<TaskManagerListener> listeners = new CopyOnWriteArraySet<TaskManagerListener>();

    private Hashtable<QName, List<TaskStateListener>> stateListeners = new Hashtable<QName, List<TaskStateListener>>();

    private EngineContext mEngineContext;

    public void addTaskListener(TaskManagerListener l) {
        listeners.add(l);
    }

    public void removeTaskListener(TaskManagerListener l) {
        listeners.remove(l);
    }

    public void removeTaskStateListeners(QName taskName) {
        stateListeners.remove(taskName);
    }

    public void addTaskStateListener(TaskStateListener l) {
        synchronized (stateListeners) {
            List<TaskStateListener> lStateListeners = stateListeners.get(l
                    .getTaskDefName());
            if (lStateListeners == null) {
                lStateListeners = new ArrayList<TaskStateListener>();
            }
            lStateListeners.add(l);
            stateListeners.put(l.getTaskDefName(), lStateListeners);
        }
    }

    public void removeTaskStateListener(TaskStateListener l) {
        synchronized (stateListeners) {
            List<TaskStateListener> lStateListeners = stateListeners.get(l
                    .getTaskDefName());
            if (lStateListeners != null) {
                lStateListeners.remove(l);
            }
        }
    }

    void fireTaskStateChange(RuntimeTask.TaskState oldState,
            RuntimeTask.TaskState newState, RuntimeTask task) {
        Task taskMeta = task.getTaskMeta();
        QName taskQName = taskMeta.getQName ();
        synchronized (stateListeners) {
            TaskStateEvent evt = new TaskStateEvent(this, oldState, newState,
                    task);
            List<TaskStateListener> lStateListeners = stateListeners
                    .get(taskQName);
            if (lStateListeners != null && lStateListeners.size() > 0) {
                for (TaskStateListener listener : lStateListeners) {
                    listener.onStateChange(evt);
                }
            }
        }

    }
    
    void fireTaskStateChange(RuntimeTask.TaskState oldState,
            RuntimeTask.TaskState newState, RuntimeTask task, Object modelRef) {
        Task taskMeta = task.getTaskMeta();
        QName taskQName = taskMeta.getQName ();
        synchronized (stateListeners) {
            TaskStateEvent evt = new TaskStateEvent(this, oldState, newState,
                    task, modelRef);
            List<TaskStateListener> lStateListeners = stateListeners
                    .get(taskQName);
            if (lStateListeners != null && lStateListeners.size() > 0) {
                for (TaskStateListener listener : lStateListeners) {
                    listener.onStateChange(evt);
                }
            }
        }

    }    
    
    

    protected void notifyOnComplete(String exchangeId, Long taskId, Element output) {
        for (TaskManagerListener listener : listeners) {
            listener.taskComplete(exchangeId,  taskId, output);
        }
    }

//    protected void processTaskMetaData(RuntimeTask task) throws TaskException {
//        TaskHandlerManager hManager = TaskHandlerManager.getInstance();
//        hManager.processHandler(task, this);
//    }
  
    protected void processTaskStateChange(RuntimeTask task,
            RuntimeTask.TaskState oldState, RuntimeTask.TaskState newState) {

    }

    protected void processNotification(RuntimeTask task) {

    }


    protected void removeActiveTimer(Long taskId, Long timerId) {

    }

    protected Element makeJBIReply(RuntimeTask task) throws Exception {
        Map<String, Element> partsMap = new HashMap<String, Element>();
        Task taskModel = task.getTaskMeta();
        Operation opt = taskModel.getWSDLOperation();

        Map parts = opt.getOutput().getMessage().getParts();
        String partName = (String) parts.keySet().iterator().next();
        Part part = (Part) parts.get(partName);
        QName elName = part.getElementName();
        Element wrappedEl = task.getOutput().getOutput();
        Element replyEl = wrappedEl;
        if (wrappedEl != null) {
            if (elName != null) {
                replyEl = wrappedEl.getOwnerDocument().createElementNS(
                        elName.getNamespaceURI(), elName.getLocalPart());
                NodeList nodeList = wrappedEl.getChildNodes();
                if (nodeList != null) {
                    List<Node> children = new ArrayList<Node>();
                    for (int i = 0; i < nodeList.getLength(); i++) {
                        children.add(nodeList.item(i));
                    }
                    for (int i = 0; i < children.size(); i++) {
                        replyEl.appendChild(children.get(i));
                    }

                }

            } else {
                replyEl = wrappedEl;
            }
        }
        partsMap.put(partName, replyEl);

        return JBIMessageUtil.makeJBIMessage(partsMap, opt);
    }

    protected boolean  validateDeadlineOrDuration(
            DeadlineOrDuration deadlineOrDuration, RuntimeTask task)
            throws TaskException {
        Date currentDate = new Date();
        boolean result = false;
        try {
            Date deadline = deadlineOrDuration.getDeadlineObject(task
                    .getJXpathContext());
            Date durationDate = deadlineOrDuration.getDurationDate(task
                    .getJXpathContext());
            boolean check1 = true;
            boolean check2 = true;
            if (deadline != null && deadline.getTime() < currentDate.getTime()) {
                    check1 = false;
            }

            if (durationDate != null
                    && durationDate.getTime() < currentDate.getTime()) {
                  check2 = false;
            }
            result = check1 || check2;

        } catch (ModelException ex) {
            throw new TaskException(ex);
        }
        return result;
    }

    public void setContext(EngineContext context) throws TaskException, NamingException {
        mEngineContext = context;
    }

    public EngineContext getContext() {
        return mEngineContext;
    }

    public Element getTaskXform(Long taskId) throws TaskException {
        RuntimeTask task = getTask(taskId);

        Task taskmeta = task.getTaskMeta();

        WorkflowMapEntry entry = getContext().getWorkflowMapEntryTable()
                .findWorkflowEntry(taskmeta);
        if (entry != null) {
            return entry.getXform();
        }
        return null;
    }

    public Element getTaskInputXformInstance(Long taskId) throws TaskException {
        RuntimeTask task = getTask(taskId);

        Task taskmeta = task.getTaskMeta();

        WorkflowMapEntry entry = getContext().getWorkflowMapEntryTable()
                .findWorkflowEntry(taskmeta);
        if (entry != null) {
            return entry.getInputXformInstance();
        }
        return null;
    }

    public Element getTaskOutputXformInstance(Long taskId) throws TaskException {
        RuntimeTask task = getTask(taskId);

        Task taskmeta = task.getTaskMeta();

        return Util.getOutputInstance(taskmeta, getContext());
    }

    public boolean hasTaskStateListeners(QName name) {
        // TODO Auto-generated method stub
        synchronized (stateListeners) {
            List<TaskStateListener> lStateListeners = stateListeners.get(name);
            if (lStateListeners != null && lStateListeners.size() > 0) {
                return true;
            }
        }
        return false;
    }
    
  public TasklistResult getAssignedTaskList (QueryType query,  int startIndex, int pageSize)   throws TaskException {
      return null;
  }
}
