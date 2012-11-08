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
 * @(#)BPELSEHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;

import com.sun.jbi.common.qos.messaging.ExchangeUtil;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.util.I18n;


/**
 * member data moved out from BPELSEInOutThread
 * @author Saslam-Mir
 * Singleton to hold BPELSEInOutThread state variable info
 */
public class BPELSEHelper {
    
	private static final Logger LOGGER = Logger.getLogger(BPELSEHelper.class.getName());    
    
    private Map mRequestMap;
    private Map mResponseMap;
    private ComponentContext mContext;
    private DeliveryChannel mDeliveryChannel;
    private MessageExchangeFactory mFactory;
    private DeploymentBindings mDepBindings;
    /**
     * member data moved out from BPELSEInOutThread
     */
    EngineChannel mEngineChannel;
    /**
     * member data moved out from BPELSEInOutThread
     */
    Engine mEngine;
    
    /**
     * non singleton ctor 
     * class cardinality is supposed to be one per BPELSE instance
     * @Exception java.lang.Exception thrown  if  anyone forces
     *               cardinality  greater than 1 per BPELSE
     */
    public BPELSEHelper() { 
        mRequestMap  = new HashMap();
        mResponseMap = new HashMap();    
    }    
    
    /**
     * threadsafe insertion
     * @param exchangeId key
     * @param messageExchange value 
     * @throws RuntimeException If the MessageExchange corresponding to the
     * exchangeId is already being processed
     */
    public void putinRequestMap(String exchangeId, MessageExchange messageExchange) {
        synchronized (mRequestMap) {
            if(mRequestMap.containsKey(exchangeId)) {
            	String errorMessage = I18n.loc("BPJBI-7014: Already processing a MessageExchange with the " + 
            			"ExchangeId {0}. Cannot process another message with the same ExchangeId. " + 
            			"Details of MessageExchange received : {1}", exchangeId, messageExchange.toString());
                throw new RuntimeException(errorMessage);
            } else {
                mRequestMap.put(exchangeId, messageExchange);
            }
        }
    }
    
    /**
     * 
     * @param k key
     * @return value pointed to by key
     */
    public Object removefromRequestMap(Object k) {
        synchronized (mRequestMap) {
            return mRequestMap.remove(k);    
        }
    }   
    
    /**
     * 
     * @param k key
     * @param v value
     */
    public void putinResponseMap(Object k, Object v) {       
        synchronized (mResponseMap) {
            mResponseMap.put(k, v);
        }
    }
    
    /**
     * 
     * @param k key
     * @return object
     */
    public Object removefromResponseMap(Object k) {      
        synchronized (mResponseMap) {
            return mResponseMap.remove(k);          
        }
    }
        
    /**
     * 
     * @param context JBI Component Context
     */
    public void setComponentContext(ComponentContext context) {
        mContext = context;   
    }
    
    /**
     * 
     * @return returns a JBI Component Context
     */
    public ComponentContext getComponentContext() {
        return mContext;
    }

    /**
     * 
     * @param channel engine channel
     * @return boolean
     */
    public boolean setExchangeFactory(DeliveryChannel channel) {
        boolean ret = false;
        if (channel != null) {
            mFactory = channel.createExchangeFactory();
            ret = true;
        }
        return ret;
    }
    
    /**
     * 
     * @return the ExcangeFactory
     */
    public MessageExchangeFactory getExchangeFactory() {
        return mFactory;
    }
    
    /**
     * 
     * @param channel Delivery Channel being used by BPELSE
     */
    public void setDeliveryChannel(DeliveryChannel channel) {
        mDeliveryChannel = channel;
    }
    
    /**
     * 
     * @return Delivery Channel used by all threads and BPELSE being service by them
     */
    public DeliveryChannel getDeliveryChannel() {
        return mDeliveryChannel;
    }
    
    /**
     * 
     * @param depBindings deployment binding
     */
    public void setDeploymentBindings(DeploymentBindings depBindings) {
        mDepBindings = depBindings;
    }
    
    /**
     * 
     * @return deployment binding
     */
    public DeploymentBindings getDeploymentBindings() {
        return mDepBindings;
    }

    /**
     * send message exchange after setting with error
     * 
     * @param me message exchange
     * @param error exception
     */
    public void sendError(MessageExchange me, Exception error) {

    	String faultCode = getFaultCode(error);
    	String faultString = error.getMessage();
    	String faultActor = "sun-bpel-engine";
    	String faultDetails = getFaultDetails(error);
    	
    	String message = I18n.loc("BPJBI-6001: Sending ERROR status (Service Name = {0}, Endpoint Name = {1}, Operation Name = {2}, Message Exchange Id = {3})\n Error properties\n  {4} = {5}\n  {6} = {7}\n  {8} = {9}\n  {10} =\n   {11}", 
    			me.getEndpoint().getServiceName() , me.getEndpoint().getEndpointName(), 
    			me.getOperation(), me.getExchangeId(), 
    			ExchangeUtil.FAULTCODE_PROPERTY_NAME, faultCode, 
    			ExchangeUtil.FAULTSTRING_PROPERTY_NAME, faultString,
    			ExchangeUtil.FAULTACTOR_PROPERTY_NAME, faultActor,
    			ExchangeUtil.FAULTDETAIL_PROPERTY_NAME, faultDetails); 

    	LOGGER.log(Level.WARNING, message);

    	me.setError(error);
    	me.setProperty(ExchangeUtil.FAULTCODE_PROPERTY_NAME, faultCode);
    	me.setProperty(ExchangeUtil.FAULTSTRING_PROPERTY_NAME, faultString);
    	me.setProperty(ExchangeUtil.FAULTACTOR_PROPERTY_NAME, faultActor);
    	me.setProperty(ExchangeUtil.FAULTDETAIL_PROPERTY_NAME, faultDetails);    	

    	try {
    		getDeliveryChannel().send(me);
    	} catch (MessagingException ex) {
    		LOGGER.log(Level.WARNING, I18n.loc("BPJBI-6002: Unable to reply to an InOut"), ex);
    		//It is OK to swallow the exception. This could only happen if something went really bad.
    	}
    }
    
    private String getFaultCode(Exception e) {
    	return ExchangeUtil.FaultCode.Server.toString();
    }
    
    private String getFaultDetails(Exception e) {
    	String details = e.getMessage();
    	if(e.getCause() != null) {
    		details += "\n   Caused by: " + getFaultDetails((Exception)e.getCause());
    	}
    	
    	return details;
    }
}
