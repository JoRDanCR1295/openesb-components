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
 * @(#)InboundProcessor.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.pojose.jbi.thread;

import javax.management.Notification;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchange.Role;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.AttributeChangeNotification;
import javax.management.NotificationListener;
import org.glassfish.openesb.pojose.core.pool.DocumentBuilderPool;
import org.glassfish.openesb.pojose.core.pool.JBIPartExprPool;
import org.glassfish.openesb.pojose.core.pool.TransformerPool;
import org.glassfish.openesb.pojose.jbi.I18n;
import org.glassfish.openesb.pojose.jbi.POJOComponentContext;
import org.glassfish.openesb.pojose.jbi.PojoSEConfigurationMBean;
import org.glassfish.openesb.pojose.jbi.su.MasterTracker;

/**
 *
 * @author gpatil
 */
public class InboundProcessor extends Thread implements MessageProcessor,
	NotificationListener {

    private POJOComponentContext compCtx;
    private MessagingChannel msgChannel;
    private final POJOSEThreadPoolExecutor mcProcessorPool;
    private final int defKeepAliveTime = 5;  // 5 min
    private final long defWaitTime = 3 * 1000; //3 secs
    private final long defPoolTerminateWaitTime = 60 * 1; // 1 Min
    private BlockingQueue bq = null;
    private Logger logger = Logger.getLogger(InboundProcessor.class.getName());
    private volatile boolean started = false;
    private final ReentrantLock startedLock = new ReentrantLock();
    private final MasterTracker mt = MasterTracker.getInstance();
    private final ConcurrentLinkedQueue<BaseTask> bufferQueue = new ConcurrentLinkedQueue<BaseTask>();
    private volatile int maxPoolSize = PojoSEConfigurationMBean.DEFAULT_MAX_THREAD_POOL_SIZE;

    private static volatile InboundProcessor instance;

    public static synchronized InboundProcessor getInstance(POJOComponentContext cc) {
	if (instance == null) {
	    instance = new InboundProcessor(cc);
	}
	return instance;
    }

    // Private constructor to make sure atmost only one instance is present at any moment
    private InboundProcessor(POJOComponentContext cc) {
	this.compCtx = cc;

	int corePool = PojoSEConfigurationMBean.DEFAULT_CORE_THREAD_POOL_SIZE;
	this.maxPoolSize = PojoSEConfigurationMBean.DEFAULT_MAX_THREAD_POOL_SIZE;
	int queueSize = PojoSEConfigurationMBean.DEFAULT_THREAD_POOL_BLOCKING_QUEUE_SIZE;

	PojoSEConfigurationMBean mbean = this.compCtx.getConfigMbean();

	if (mbean != null) {
	    corePool = mbean.getCoreThreadPoolSize();
	    maxPoolSize = mbean.getMaxThreadPoolSize();
	    queueSize = mbean.getThreadPoolBlockingQueueSize();
	}

	this.bq = new ArrayBlockingQueue(queueSize);

	this.mcProcessorPool = new POJOSEThreadPoolExecutor(corePool,
		maxPoolSize, defKeepAliveTime, TimeUnit.SECONDS,
		bq, EngineThreadFactory.getFactory(),
		new POJOSERejectedExecutionHandler(this.bufferQueue));

	//Set pool size to core pool size.
	DocumentBuilderPool.getInstance().setMaxPoolSize(corePool);
	TransformerPool.getInstance().setMaxPoolSize(corePool);
	JBIPartExprPool.getInstance().setMaxPoolSize(corePool);

	//this.logger = cc.getCompLifeCycle().getCompLifeCycleLogger();
	this.setName("POJO SE DC Listener");//NOI18N
    }

    private int parseInt(Object val) {
	int ret = -1;
	try {
	    ret = Integer.parseInt(val.toString());
	} catch (Exception ex) {
	    ret = -1;
	}
	return ret;
    }

    public void handleNotification(Notification ntf, Object handback) {
	if ((ntf != null)
		&& (AttributeChangeNotification.ATTRIBUTE_CHANGE.equals(ntf.getType()))) {
	    AttributeChangeNotification acn = (AttributeChangeNotification) ntf;
	    if (PojoSEConfigurationMBean.CORE_THREAD_POOL_SIZE.equals(
		    acn.getAttributeName())) {
		int size = parseInt(acn.getNewValue());
		if (size != -1) {
		    this.mcProcessorPool.setCorePoolSize(size);
		}
	    } else if (PojoSEConfigurationMBean.MAX_THREAD_POOL_SIZE.equals(
		    acn.getAttributeName())) {
		int size = parseInt(acn.getNewValue());
		if (size != -1) {
		    this.maxPoolSize = size;
		    this.mcProcessorPool.setMaximumPoolSize(size);
		}
	    }
	}
    }

    @Override
    public void run() {
	MessageExchange me = null;
	try {
	    this.msgChannel = compCtx.getDC();
	    // Loop forever accepting incoming MessagesExchanges, including POJO SE
	    // responses to the POJO SE initiated MEs. Util stop is signalled.
	    for (;;) {
		try {
		    this.startedLock.lock();
		    if (!this.started) {
			break;
		    }
		} finally {
		    this.startedLock.unlock();
		}

		try {
		    // accept while we have still lock on startLock instead here?
		    me = this.msgChannel.accept(defWaitTime);

		    if (me != null) {
			if (logger.isLoggable(Level.FINEST)) {
			    logger.finest("Accepted ME:" + me.getExchangeId());
			}
			executeME(me);
		    }

		    if (this.bufferQueue.size() > 0) {
			checkAndExecuteBufferQueueTasks();
		    }
		} catch (Exception ex) {
		    String m = I18n.loc("POJOSE-7522: Exception while executing for ME {0}. {1}", me, ex);
		    logger.log(Level.SEVERE, m, ex);
		}
	    }
	} catch (Exception ex) {
	    String m = I18n.loc("POJOSE-7520: Exception while accepting ME from delivery channel. {0}", ex);
	    logger.log(Level.SEVERE, m, ex);
	}

	if (logger.isLoggable(Level.FINE)) {
	    String m = I18n.loc("POJOSE-1508: Stopped listening to Delivery Channel.");//NOI18N
	    logger.log(Level.FINE, m);
	}
    }

    // Put the ME/InMEProcessor into our ThreadPoolExecutor queue
    // and return quickly.
    private void executeME(MessageExchange me) {
	try {
	    if (this.mcProcessorPool.getActualActiveExecutorTaskCount() >= this.maxPoolSize) { // All thread are busy or blocked.
		if (this.mt.getActiveSynchCalls() >= this.maxPoolSize) { // All are blocked by sendSynch
		    // if role is provider and no active call for the service exists, then run using grace thread.
		    if (Role.PROVIDER.equals(me.getRole())) {
			InMsgTask it = new InMsgTask(compCtx, me);
			if (this.mt.getActiveInRequestForPojo(it.getPojoServiceClassName()) == 0) {
			    //Run in grace thread
			    String m = I18n.loc("POJOSE-6506: Possible deadlock condition detected, allocated threads and blocking queue are full. Executing incoming request in a \"grace\" thread. ME id:{0}",
				    me.getExchangeId());
			    logger.warning(m);
			    EngineThreadFactory.getFactory().newGraceThread(it).start();
			} else {
			    addToBufferQueue(it);
			}
		    } else {
			// handled by Response message processor.
			RespMsgTask rt = new RespMsgTask(compCtx, me);
			addToBufferQueue(rt);
		    }
		} else {
		    if (Role.PROVIDER.equals(me.getRole())) {
			InMsgTask it = new InMsgTask(compCtx, me);
			addToBufferQueue(it);
		    } else {
			// handled by Response message processor.
			RespMsgTask rt = new RespMsgTask(compCtx, me);
			addToBufferQueue(rt);
		    }
		}
	    } else {
		ServiceEndpoint sept = me.getEndpoint();
		if (logger.isLoggable(Level.FINE)) {
		    String msg = I18n.lf("POJOSE-1509: Processing for Endpoint:{0}", sept);//NOI18N
		    logger.fine(msg);
		}

		if (Role.PROVIDER.equals(me.getRole())) {
		    mcProcessorPool.execute(new InMsgTask(compCtx, me));
		} else {
		    // handled by Response message processor.
		    mcProcessorPool.execute(new RespMsgTask(compCtx, me));
		}
	    }
	} catch (RejectedExecutionException ree) { // TODO remove, control should not come here.
	    String m = I18n.loc("POJOSE-7517: Allocated threads and blocking queue are full. Please configure to allocate  higher resources: Core and Max pool size and blocking queue size of ThreadPoolExecutor.");
	    logger.severe(m);
	    Exception ex = new Exception(m, ree);
	    try {
		me.setError(ex);
		this.msgChannel.send(me);
	    } catch (Throwable th) {
		m = I18n.loc("POJOSE-7500: Exception while sending error status {0}.", th);
		logger.severe(m);
	    }
	}
    }

    private void addToBufferQueue(BaseTask task) {
	String m = I18n.loc("POJOSE-6504: Allocated threads and blocking queue are full. Adding accepted ME to buffer task queue.");
	logger.warning(m);
	bufferQueue.add(task);
    }

    private void submit2Executor(BaseTask task) {
	MessageExchange me = task.getTaskTriggerME();
	ServiceEndpoint sept = me.getEndpoint();
	if (logger.isLoggable(Level.FINE)) {
	    String msg = I18n.lf("POJOSE-1509: Processing for Endpoint:{0}", sept);//NOI18N
	    logger.fine(msg);
	}

	if (Role.PROVIDER.equals(me.getRole())) {
	    mcProcessorPool.execute(new InMsgTask(compCtx, me));
	} else {
	    // handled by Response message processor.
	    mcProcessorPool.execute(new RespMsgTask(compCtx, me));
	}
    }

    //BufferQueue size is > 0 when called.
    private void checkAndExecuteBufferQueueTasks() {
	int freeThreads = this.maxPoolSize - (int) this.mcProcessorPool.getActualActiveExecutorTaskCount();
	if (freeThreads > 0) {
	    BaseTask bt = null;
	    for (int i = 0; i < freeThreads; i++) {
		bt = this.bufferQueue.poll();
		if (bt != null) {
		    submit2Executor(bt);
		} else {
		    break;
		}
	    }
	}
    }

    public void startProcessor() {
	try {
	    this.startedLock.lock();
	    if (!this.started) {
		this.started = true;
		this.start();
	    }
	} finally {
	    this.startedLock.unlock();
	}
    }

    public void stopProcessor(boolean now) throws InterruptedException {
	if (logger.isLoggable(Level.FINEST)) {
	    try {
		logger.finest("DC.toString():" + this.compCtx.getJBIComponentContext().getDeliveryChannel().toString()); //NOI18N
	    } catch (Throwable t) {
		//ignore
	    }
	}
	boolean shuttingDown = true;
	try {
	    this.startedLock.lock();
	    if (this.started) {
		this.started = false;
		if (now) {
		    this.mcProcessorPool.shutdownNow();
		} else {
		    this.mcProcessorPool.shutdown();
		}
		if (msgChannel != null) {
		    try {
			msgChannel.close();
		    } catch (Exception ex) {
			String m = I18n.loc("POJOSE-7541: Failed to close MessagingChannel {0}.", ex);
			logger.log(Level.SEVERE, m, ex);
		    }
		}
	    } else {
		shuttingDown = false;
	    }
	} finally {
	    this.startedLock.unlock();
	}

	if (shuttingDown) {
	    if ((!this.mcProcessorPool.isTerminated()) && (this.mcProcessorPool.isTerminating())) {
		this.mcProcessorPool.awaitTermination(defPoolTerminateWaitTime,
			TimeUnit.SECONDS);
	    }
	}
    }

    public void stopProcessor() throws InterruptedException {
	stopProcessor(false);
    }
}
