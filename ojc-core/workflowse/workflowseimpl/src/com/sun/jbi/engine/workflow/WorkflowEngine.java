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
 * @(#)WorkflowEngine.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.naming.NamingException;
import javax.security.auth.Subject;
import javax.wsdl.Operation;
import javax.xml.namespace.QName;
import javax.xml.transform.dom.DOMSource;

import org.w3c.dom.Element;

import com.sun.jbi.engine.workflow.clientapi.StaticOperation;
import com.sun.jbi.engine.workflow.clientapi.StaticOperationFactory;
import com.sun.jbi.engine.workflow.clientapi.StaticOperation.StaticOperationReply;
import com.sun.jbi.engine.workflow.common.model.TaskInput;
import com.sun.jbi.engine.workflow.common.model.impl.TaskInputImpl;
import com.sun.jbi.engine.workflow.process.ConsumerCallBack;
import com.sun.jbi.engine.workflow.process.InOutCallBack;
import com.sun.jbi.engine.workflow.process.TaskEvent;
import com.sun.jbi.engine.workflow.process.TaskHandlerListener;
import com.sun.jbi.engine.workflow.process.TaskHandlerManager;
import com.sun.jbi.engine.workflow.process.TaskNotificationEvent;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask;
import com.sun.jbi.engine.workflow.runtime.model.TaskException;
import com.sun.jbi.engine.workflow.runtime.model.TaskManager;
import com.sun.jbi.engine.workflow.runtime.model.TaskManagerFactory;
import com.sun.jbi.engine.workflow.runtime.model.TaskManagerListener;
import com.sun.jbi.engine.workflow.util.I18n;
import com.sun.jbi.engine.workflow.util.Util;
import com.sun.jbi.engine.workflow.util.XmlUtil;
import com.sun.jbi.workflow.model.Task;

/**
 *
 * @author Sun Microsystems
 */
public class WorkflowEngine implements TaskManagerListener, TaskHandlerListener {
    
    private static final Logger LOGGER = Logger.getLogger(WorkflowEngine.class.getName());

    
    private TaskManager mTaskManager;
    private StaticOperationFactory mStaticOperationFactory;
    
    private InOutCallBack mInOutCallBack;
    private ConsumerCallBack mConsumerCallBack;
    private WorkflowThread mThread;
    private List<WorkflowRequest> mNewRequests = Collections.synchronizedList(new ArrayList<WorkflowRequest> ());
//    private Map<String, WorkflowRequest> mPendingRequestTable = Collections.synchronizedMap(new LinkedHashMap<String, WorkflowRequest> ());
    private Map<Long, String> mTaskRequestMap = new Hashtable<Long, String> (); 
    private Map<String, List<TaskEvent>> mPendingTimeoutEvent = new Hashtable<String,  List<TaskEvent> >();
    private Hashtable<String, Element> mPendingReply = new Hashtable<String, Element>();
    private EngineContext mEngineContext;
    
    
    
    public void init () throws JBIException {
        try {
            mThread = new WorkflowThread (this);
            mThread.setStatus(ThreadStatus.STOPPED);
            TaskHandlerManager.getInstance().addTaskListener(this);
            mTaskManager = TaskManagerFactory.getInstance().getTaskManager();
            mTaskManager.addTaskListener(this);
            mStaticOperationFactory = StaticOperationFactory.getInstance();
            
        } catch (Exception e) {
            // TODO Auto-generated catch block
            String msg = I18n.loc("WLM-6000: Can not initialize work list manager engine");
            LOGGER.log(Level.WARNING, msg, e);
            throw new JBIException (msg);
        }        
    }    
    
    public void setContext (EngineContext context) throws JBIException, NamingException {
        mEngineContext = context;
        try {
            mTaskManager.setContext(context);
        }catch (WorkflowException e) {
            throw new JBIException (e);
        }
      
    }
    
    public EngineContext getContext ()  {
        return  mEngineContext;
    }    
    
    public void recover() throws JBIException 
    {
    /*    try 
        {
            mTaskManager.recover();
        } 
        catch (WorkflowException e) 
        {
            throw new JBIException (e);
        }*/
    }
    
    public void setInOutCallBack (InOutCallBack callback) {
        mInOutCallBack = callback;
        mThread.setCallBack(callback);
    }
    
    public void setConsumerCallBack (ConsumerCallBack callback) {
        mConsumerCallBack = callback;
        
    }
    
    public void acceptRequest (WorkflowRequest workflowReq) {
        synchronized (mNewRequests) {
            mNewRequests.add(workflowReq);
            mNewRequests.notify();            
        }

    }
    
    public void start () {
        mThread.setStatus(ThreadStatus.RUNNING); 
        if (mThread.getState() == Thread.State.NEW) {
            mThread.start();
        }
    }
    
    public void stop () {
        mThread.setStatus(ThreadStatus.STOPPED);
     }

    public void shutdown () {
        mTaskManager.shutDown();
        mThread.setStatus(ThreadStatus.SHUTDOWN);
        mThread.cease();
    }
    
    public void setMaxThreadCount(int threadCount) {
    	mThread.setMaxThreadCount(threadCount);
    }
    
    public void taskComplete(String exchangeId, Long taskId, Element output) {        
        // TODO Auto-generated method stub        
        String requestId = mTaskRequestMap.remove(taskId);
//        mPendingRequestTable.remove(requestId);
        if (requestId != null) {
            mInOutCallBack.onReply(requestId, new DOMSource(output));
        } else {
            mPendingReply.put(exchangeId, output);
        }
    }    
    
    public void onTimeout(TaskEvent evt) {
    	long taskId = evt.getTaskId(); 
    	Element timeoutReply = evt.getElement();        
    	String requestId = mTaskRequestMap.get(taskId);
        //If bpel has sent the request already
        if (requestId != null) {
            mTaskRequestMap.remove(taskId);
//            mPendingRequestTable.remove(requestId);
            mInOutCallBack.onTimeout(requestId, new DOMSource(timeoutReply));
        } else {
            //move this event to pending map, when bpel recovery invokes wfse, the map will be checked
            // see taskCreated (Long taskId, String taskRelId, String requestId) 
            synchronized (mPendingTimeoutEvent) {
                List<TaskEvent> pendingEvents = mPendingTimeoutEvent.get(evt.getTaskRelId());
                if (pendingEvents == null) {
                    pendingEvents = new ArrayList<TaskEvent> ();
                    mPendingTimeoutEvent.put(evt.getTaskRelId(), pendingEvents);
                }
                pendingEvents.add(evt);
            }
        }
    }
    
    public void onNotify(TaskNotificationEvent evt) {
    	Element reply = evt.getElement();
    	QName portTypeQName = evt.getPortTypeQName();
		WorkflowMapEntryTable mapEntryTable = this.getContext().getWorkflowMapEntryTable();
    	WorkflowMapEntry entry = mapEntryTable.findWorkflowEntry(evt.getOperationName(), portTypeQName);
    	if(entry != null && mConsumerCallBack != null) {
    		mConsumerCallBack.onNotify(entry, new DOMSource(reply));
    	}
    }
    
//    public void taskFaulted (Long taskId, Element fault) {
//        String requestId = mTaskRequestMap.remove(taskId);
//        mPendingRequestTable.remove(requestId);
//        mInOutCallBack.onFault(requestId, new DOMSource(fault));
//    }
    
    private void taskCreated (Long taskId, String taskRelId, String requestId) {
        mTaskRequestMap.put(taskId, requestId);
        List<TaskEvent> pendingEvents = mPendingTimeoutEvent.get(taskRelId);
        if (pendingEvents != null && pendingEvents.size () > 0) {
            for (TaskEvent event : pendingEvents) {
                onTimeout(event);
            }
        }
    }
    
    
    private static class WorkflowWorkerCallable implements Runnable {
        
        private WorkflowRequest mRequest;        
        private WorkflowEngine mEngine;
        private WorkflowThread mThread;

        WorkflowWorkerCallable (WorkflowRequest request, WorkflowThread thread, WorkflowEngine engine) {
            mRequest = request;
            mEngine = engine;
            mThread = thread;            
        }

        public StaticOperationReply call() throws Exception {
            // TODO Auto-generated method stub
            StaticOperationReply reply = null;
            if (mRequest.getType() == WorkflowRequest.RequestType.BPEL) {
                processBPELRequest ((BPELWorkflowRequest) mRequest);
            } else if (mRequest.getType() == WorkflowRequest.RequestType.Client) {
                reply = processClientRequest ((ClientWorkflowRequest) mRequest);
            }
            return reply;
        }

        private StaticOperationReply processClientRequest(ClientWorkflowRequest request) throws Exception {
            // TODO Auto-generated method stub
            Operation opt = request.getOperation();
            Subject subject = request.getSubject();
            StaticOperation staticOpt = mEngine.mStaticOperationFactory.newOperation(opt, request.getInput(), subject);
            staticOpt.execute();                       
            return staticOpt.getOutput();
        }

        private void processBPELRequest(BPELWorkflowRequest request) throws Exception {
            // TODO Auto-generated method stub
            TaskInput input = new TaskInputImpl (request.getInput());
            RuntimeTask runtimeTask = mEngine.mTaskManager.createTask(request.getRelId(), input, (Task) request.getTaskModel());
            if (runtimeTask.isNew()) {
                runtimeTask.execute();
            } else if (runtimeTask.getState() != null && runtimeTask.getState() == RuntimeTask.TaskState.COMPLETED) {
                Element output = mEngine.mPendingReply.remove(request.getId());
                if (output != null) {
                    mEngine.mInOutCallBack.onReply(request.getId(), new DOMSource(output));
                } else {
                    //need to recreate the response
                    Element el = Util.makeJBIReply(runtimeTask);
                    mEngine.mInOutCallBack.onReply(request.getId(), new DOMSource(el));
                }
                return;
            }  else if (runtimeTask.getState() != null && runtimeTask.getState() == RuntimeTask.TaskState.EXPIRED) {
                Element output = mEngine.mPendingReply.remove(request.getId());
                if (output != null) {
                    mEngine.mInOutCallBack.onTimeout(request.getId(), new DOMSource(output));
                } else {
                    //need to recreate the response
                    Element el = Util.makeTimeoutFault();
                    mEngine.mInOutCallBack.onTimeout(request.getId(), new DOMSource(el));
                }                
                return;
            } 
            
            mEngine.taskCreated(runtimeTask.getId(), request.getRelId(), request.getId());            
        }

        public void run() {

            StaticOperationReply reply;
            try {
                reply = call();
            } catch (Exception e) {
                // TODO Auto-generated catch block
                mThread.taskFaulted(mRequest.getId(), e);
                return;
            }
            if (reply != null) {
                mThread.taskDone (mRequest.getId(), reply);
            }
        }
    }
    
    private static enum ThreadStatus {
        STOPPED ("STOPPED"),
        RUNNING ("RUNNING"),
        SHUTDOWN ("SHUTDOWN");
        
        private String mDescription;
        
        private ThreadStatus (String desc) {
            mDescription = desc;
        }
        public String getDescription() {
            return mDescription;
        }
        @Override
        public String toString() {
            // TODO Auto-generated method stub
            return mDescription;
        }

    }
    
    private static class WorkflowThread extends Thread {

        private static final Logger LOGGER = Logger.getLogger(WorkflowThread.class.getName());
 
        
        private InOutCallBack mInOutCallBack;
        private ThreadStatus mStatus;       
        private WorkflowEngine mEngine;
        
        //TODO: make it configurable
        private ExecutorService mService = Executors.newFixedThreadPool(EnginePropertyConstants.MAXIMUM_THREADCOUNT_DEFAULT);
   
        WorkflowThread (WorkflowEngine workflowEngine) {
            mEngine = workflowEngine;            
        }
        
        void taskFaulted(String id, Exception e) {
            // TODO Auto-generated method stub
            LOGGER.log(Level.WARNING, I18n.loc("WLM-6001: Exception occurred in executing the task"),e);
            Element faultElment = XmlUtil.buildFault (e);
            mInOutCallBack.onFault(id, new DOMSource(faultElment));
//            mEngine.mPendingRequestTable.remove(id);
        }

        void taskDone(String id, StaticOperationReply reply) {
            // TODO Auto-generated method stub
            LOGGER.log(Level.CONFIG, I18n.loc("WLM-4001: Workflow Request Id : {0} is done", id));
            if (reply.isFaulted()) {
                mInOutCallBack.onFault(id, new DOMSource(reply.getFault()));
            } else {
                mInOutCallBack.onReply(id, new DOMSource(reply.getReply()));
            }
//            mEngine.mPendingRequestTable.remove(id);            
        }
        private void setCallBack (InOutCallBack callBack) {
            mInOutCallBack = callBack;
        }
        
        private void setStatus (ThreadStatus status) {
            mStatus = status;
        }       
        
        @Override
        public void run() {
            // TODO Auto-generated method stub
            while (mStatus == ThreadStatus.RUNNING) {
                WorkflowRequest request = null;
                synchronized (mEngine.mNewRequests) {
                    while (mEngine.mNewRequests.isEmpty()) {
                        try {
                            mEngine.mNewRequests.wait();
                        } catch (InterruptedException e) {
                            // TODO Auto-generated catch block
//                            e.printStackTrace();
                        }
                    }
                    if (mStatus == ThreadStatus.RUNNING) {
                        request = mEngine.mNewRequests.remove(0);
                    } else {
                        return;
                    }
                    
                }
//                mEngine.mPendingRequestTable.put(request.getId(), request);
                synchronized (mService) {
                	mService.submit(new WorkflowWorkerCallable (request,this,mEngine));
                }
            }
        }
        
        public void cease () {
            
            this.interrupt();
            mService.shutdown();
            
        }
        
        public void setMaxThreadCount(int threadCount) {
        	mService.shutdown();
        	synchronized (mService) {
        		mService = Executors.newFixedThreadPool(threadCount);
			}
        	
        	
        }
    }

    public StaticOperationFactory getStaticOperationFactory() {
        return mStaticOperationFactory;
    }

    private void initTaskManager() throws TaskException {
    	if(mTaskManager != null) {
    		mTaskManager.removeTaskListener(this);
    	} 
    	
    	mTaskManager = TaskManagerFactory.getInstance().getTaskManager();
        mTaskManager.addTaskListener(this);
        
    }

    public void removeTaskStateListeners(QName name) {
        // TODO Auto-generated method stub
        if(mTaskManager != null) {
            mTaskManager.removeTaskStateListeners (name);
        } 
    }
    
}
