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
 * @(#)OutboundReceiver.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Vector;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;

import javax.management.NotificationListener;
import javax.management.Notification;
import javax.management.AttributeChangeNotification;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.internationalization.Messages;

import com.sun.jbi.jmsbc.mbeans.RuntimeConfiguration;
import com.sun.jbi.jmsbc.util.AlertsUtil;
import com.sun.jbi.jmsbc.util.Executor;
import com.sun.jbi.jmsbc.util.JMSBCContext;
import com.sun.jbi.jmsbc.jms.ChannelManager;

/**
 * Wait for responses/requests from the service engine and 
 * use thread pools to process them. 
 * 
 * Thir receiver not only processes "outbound" requests initiated by the 
 * Service engine, but also responds to requests this adapter has sent the SE.
 */             
public class OutboundReceiver {

    // Pool settings for processing SE "outbound" requests initated by the SE.
    private Collection mServiceUnits;
    private ChannelManager mJMSChannelMgr;
    private ComponentContext mComponentContext;
    private Map mInboundMessageExchanges;
    private boolean mStop;
    
    private static final Messages mMessages =
        Messages.getMessages(OutboundReceiver.class);
    private static final Logger mLog =
        Messages.getLogger(OutboundReceiver.class);
    private static final long RECEIVE_WAIT = 1000 * 5;

    
    // Inner class to handle configuration change notifications
    private NotificationListener listener = new NotificationListener() {
        public void handleNotification(Notification notification, Object obj) {
            if (notification instanceof AttributeChangeNotification) {
                AttributeChangeNotification attrNotif =
                    (AttributeChangeNotification) notification;
                String attrName = attrNotif.getAttributeName();
                if (attrName.equals("Threads")) {
                    Integer newVal = (Integer) (attrNotif.getNewValue());
                    setThreads(newVal.intValue());
                }
            }
        }
    };
    
    /**
     * Construct a receiver for a given binding channel and the relevant endpoints
     * @param bc the binding channel to receive from
     * @param endpoints EndointBean instances with information about the endpoints
     */
    public OutboundReceiver(ComponentContext context,
                            Collection serviceUnits,
                            int threadCount,
                            ChannelManager jmsChannelMgr,
                            Map inboundMessageExchanges) {
        mComponentContext = context;
        mServiceUnits = serviceUnits;
        mJMSChannelMgr = jmsChannelMgr;
        mInboundMessageExchanges = inboundMessageExchanges;
        mStop = false;

        // Apply existing configuration.  This will start workers if the
        // current number is too low.  It will stop workers if there are
        // too many working.
        if(threadCount!=0)
        	setThreads(threadCount);
        
        Thread processorThread = new Thread(new MessageProcessor());
        processorThread.start();
    }
    
    public NotificationListener getNotificationListener(){
    	return listener;
    }


    /**
     * Set the number or processing threads to use
     */
    public void setThreads(int threadCount) {
    	Executor.getReference().setMaxThreads(threadCount);
    }
    
    /**
     * Stop the receiver thread.
     */
    public void stopReceiving() {
        if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLog.log(LogSupport.LEVEL_DEBUG, "OutboundReceiver_STOP");
        }
        // Stop outbound threads
        mStop = true;
    }
    
    private boolean isStopped(){
    	return mStop;
    }
    
	private LinkedList<OutboundMessageProcessor> mProcessorsCache = new LinkedList<OutboundMessageProcessor>();
    private class MessageProcessor implements Runnable{

		public void run() {
	        while(!isStopped()){
	            try {                
	                MessageExchange msgExchange = JMSBCContext.getRef().getChannel()
	                                                               .accept(RECEIVE_WAIT);
	                if (msgExchange != null) {
	                    
	                    if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
	                        mLog.log(LogSupport.LEVEL_DEBUG, 
	                                "OutboundMessageProcessor_NMR_ACCEPT_MXCH",
	                                new Object[]{msgExchange.getExchangeId(), msgExchange.getPattern().toString()});
	                    }
	                    
	                    if(msgExchange.getStatus() == ExchangeStatus.DONE || msgExchange.getStatus() == ExchangeStatus.ERROR){
	                    	Executor.getReference().executeDoneOrError(new Task(msgExchange));
	                    }else{
		                    Executor.getReference().execute(new Task(msgExchange));
	                    }
	                }
	            } catch (Throwable ex) {
	                mLog.log(Level.SEVERE, 
	                         mMessages.getString("JMSBC-E0743.MessageExchangeUnexpectedError",
	                             new Object[]{ex.getLocalizedMessage()}),
	                         ex);
	               AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0743.MessageExchangeUnexpectedError",
	                             new Object[]{ex.getLocalizedMessage()}), 
                    JMSBindingComponent.SHORT_DISPLAY_NAME, 
                    null, 
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "JMSBC-E0743");
	            }
	        }
		}
    	
    }
    
    private class Task implements Runnable{
    	MessageExchange mMessageExchange;
    	
    	Task(MessageExchange msgExchange){
    		this.mMessageExchange = msgExchange;
    	}
		public void run() {
			OutboundMessageProcessor proc = null;
			synchronized (mProcessorsCache) {
				if(!mProcessorsCache.isEmpty()){
					proc = mProcessorsCache.removeFirst();
				}
			}
			try{
				if(proc == null){
	                proc = new OutboundMessageProcessor(mComponentContext,
	                                                 mServiceUnits,
	                                                 mJMSChannelMgr,
	                                                 mInboundMessageExchanges);
				}
				proc.setMessageExchange(mMessageExchange);
				proc.run();
			} catch (Exception e) {
                mLog.log(Level.SEVERE, 
                        mMessages.getString("JMSBC-E0743.MessageExchangeUnexpectedError",
                            new Object[]{e.getLocalizedMessage()}),
                        e);                                                
                AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0743.MessageExchangeUnexpectedError",
	                             new Object[]{e.getLocalizedMessage()}), 
                    JMSBindingComponent.SHORT_DISPLAY_NAME, 
                    null, 
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "JMSBC-E0743");
         
			}finally{
				if(proc != null){
					synchronized (mProcessorsCache) {
						mProcessorsCache.addLast(proc);
					}
				}
			}
		}
    }
}
