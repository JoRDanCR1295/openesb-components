/*
 * WorkflowEngine.java
 * 
 * Created on Jul 23, 2007, 11:33:22 AM
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.mock.participant1;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.MessageExchange;
import javax.transaction.Transaction;

import com.sun.jbi.crl.mep.AcceptManager;
import com.sun.jbi.crl.messaging.api.CorrelatedMessageExchangeFactory;
import com.sun.jbi.crl.messaging.api.MessageExchangeHelperFactory;

/**
 *
 * @author radval
 */
public class Participant1Engine {
	
	private static final Logger LOGGER = Logger.getLogger(Participant1Engine.class.getName());
	
	private ParticipantSE1InOutConsumer mInOutConsumer;
	
	private ParticipantSE1InOutProvider mInOutProvider;
	
	private CorrelatedMessageExchangeFactory mCoorelatedExchangeFactory = null;
	
	private AcceptManager mAcceptManager;
	
    public Participant1Engine(AcceptManager acceptManager) {
    	this.mAcceptManager = acceptManager;
    	
    	try {
			mCoorelatedExchangeFactory = MessageExchangeHelperFactory.getDefault().newCorrelatedMessageExchangeFactory(this.mAcceptManager. getComponentManager().getComponentContext());
		}catch(Exception ex) {
			LOGGER.log(Level.SEVERE, "failed to get correlated exchange factory", ex);
		}
    }
    
    public void initialize(ParticipantSE1InOutConsumer inOutConsumer,
			  ParticipantSE1InOutProvider inOutProvider) {
    	this.mInOutConsumer = inOutConsumer;
    	this.mInOutProvider = inOutProvider;
    }
    
    public  synchronized MessageExchange startInOutExchangeWithParticipant2(MessageExchange parentExchange) {
    	return mInOutConsumer.startChildExchange(parentExchange);
    }
    
//    public  synchronized MessageExchange startInOutExchangeWithParticipant2(Transaction t) {
//    	return mInOutConsumer.startChildExchange(t);
//    }
    
    public synchronized void replyPositiveToInOutParentExchange(MessageExchange child) {
    	this.mInOutProvider.replyPositiveToInOutParentExchange(child);
    }
    
    public synchronized void replyNegativeToInOutParentExchange(MessageExchange child) {
    	this.mInOutProvider.replyNegativeToInOutParentExchange(child);
    }
    
    /*
    private List<WorkflowRequest> mNewRequests = Collections.synchronizedList(new ArrayList<WorkflowRequest> ());
    
    private Map<String, WorkflowRequest> mPendingRequestTable = Collections.synchronizedMap(new LinkedHashMap<String, WorkflowRequest> ());
    
    private WorkflowThread mThread;
    
    private InOutCallBack mInOutCallBack;
    
    private ConsumerCallBack mConsumerCallBack;
    
    public InitiatorEngine() {
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
        mThread.setStatus(ThreadStatus.SHUTDOWN);
        mThread.cease();
    }
    
    private static class WorkflowThread extends Thread {

        private static final Logger LOGGER = Logger.getLogger(WorkflowThread.class.getName());
        
        private InOutCallBack mInOutCallBack;
        private ThreadStatus mStatus;       
        private InitiatorEngine mEngine;
        
        //TODO: make it configurable
        private ExecutorService mService = Executors.newFixedThreadPool(10);
   
        WorkflowThread (InitiatorEngine workflowEngine) {
            mEngine = workflowEngine;            
        }
        
        void taskFaulted(String id, Exception e) {
            // TODO Auto-generated method stub
            LOGGER.log(Level.WARNING, MESSAGES.getString("WorkflowEngine.Exception_occured"),e);
            Element faultElment = XmlUtil.buildFault (e);
            mInOutCallBack.onFault(id, new DOMSource(faultElment));
            mEngine.mPendingRequestTable.remove(id);
        }

        void taskDone(String id, StaticOperationReply reply) {
            // TODO Auto-generated method stub
            LOGGER.log(Level.INFO, MESSAGES.getString("WorkflowEngine.TaskDone", id));
            if (reply.isFaulted()) {
                mInOutCallBack.onFault(id, new DOMSource(reply.getFault()));
            } else {
                mInOutCallBack.onReply(id, new DOMSource(reply.getReply()));
            }
            mEngine.mPendingRequestTable.remove(id);            
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
                            e.printStackTrace();
                        }
                    }
                    if (mStatus == ThreadStatus.RUNNING) {
                        request = mEngine.mNewRequests.remove(0);
                    } else {
                        return;
                    }
                    
                }
                mEngine.mPendingRequestTable.put(request.getId(), request);
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
            RuntimeTask runtimeTask = mEngine.mTaskManager.createTask(request.getRelId(), input, (Task) request.getTaskModel().getTasks().get(0));
            if (runtimeTask.isNew()) {
                runtimeTask.execute();
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
    */
    
    public CorrelatedMessageExchangeFactory getCorrelatedMessageExchangeFactory() {
		return this.mCoorelatedExchangeFactory;
	}
}
