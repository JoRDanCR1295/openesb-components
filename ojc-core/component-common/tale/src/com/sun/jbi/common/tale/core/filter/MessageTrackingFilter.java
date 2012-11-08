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
 * @(#)MessageTrackingFilter.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.core.filter;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.sql.Timestamp;
import java.util.Calendar;
import java.util.GregorianCalendar;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange.Role;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.messaging.AppInfo;
import com.sun.jbi.common.qos.messaging.filter.AbstractExchangeFilter;
import com.sun.jbi.common.qos.messaging.filter.FilterBase;
import com.sun.jbi.common.qos.messaging.tracking.TrackingConfig;
import com.sun.jbi.common.qos.messaging.tracking.MessageTracking;
import com.sun.jbi.common.qos.redelivery.Redelivery;

import com.sun.jbi.common.tale.client.TaleClient;
import com.sun.jbi.common.tale.client.TaleClientFactory;
import com.sun.jbi.common.tale.core.domain.EnvironmentInfo;
import com.sun.jbi.common.tale.core.domain.ExchangeInfo;
import com.sun.jbi.common.tale.core.domain.Payload;
import com.sun.jbi.common.tale.core.domain.SourceInfo;
import com.sun.jbi.common.tale.core.domain.TaleRequest;

/**
 * 
 * @author Sun MicroSystems
 */
public class MessageTrackingFilter extends AbstractExchangeFilter {

    //Reserved Logger Codes used for MessageTracking
    public static final int OUTGOING_REQUEST = 1;
    public static final int OUTGOING_RESPONSE = 2;
    public static final int OUTGOING_STATUS_DONE_CONSUMER = 3;
    public static final int OUTGOING_STATUS_DONE_PROVIDER = 4;
    public static final int OUTGOING_STATUS_ERROR_CONSUMER = 5;
    public static final int OUTGOING_STATUS_ERROR_PROVIDER = 6;
    
    public static final int INCOMING_REQUEST = 7;
    public static final int INCOMING_RESPONSE = 8;
    public static final int INCOMING_STATUS_DONE_CONSUMER = 9;
    public static final int INCOMING_STATUS_DONE_PROVIDER = 10;
    public static final int INCOMING_STATUS_ERROR_CONSUMER = 11;
    public static final int INCOMING_STATUS_ERROR_PROVIDER = 12;
    
    public static final String JBI_APPLICATION = "JBI Application";
    public static final String SERVICE_ENGINE = "Service Engine";
    public static final String BINDING_COMPONENT = "Binding Component";

    private TaleClient mTaleClient;
    
	/**
	 * @param base
	 */
	public MessageTrackingFilter(FilterBase base) {
		super(base);
		// TODO component should supply TaleClient
		// TODO add static factory method to create this filter, see RedeliveryFilter.createRedeliveryFilter
		mTaleClient = TaleClientFactory.newALEClient(null);
	}

	/** @see com.sun.jbi.common.qos.messaging.filter.AbstractExchangeFilter#acceptsServiceQuality(com.sun.jbi.common.qos.ServiceQuality) */
	protected boolean acceptsServiceQuality(ServiceQuality srvcQual) {
		return (srvcQual instanceof TrackingConfig);
	}

    /** @see com.sun.jbi.common.qos.messaging.filter.AbstractExchangeFilter#processIncomingExchange(javax.jbi.messaging.MessageExchange, java.lang.Object[]) */
    public MessageExchange processIncomingExchange(MessageExchange msg,
												   Object... params) 
			throws MessagingException {
        TrackingConfig config = null;        
        if (Role.PROVIDER.equals(msg.getRole())) {
            config = (TrackingConfig)msg.getProperty(MessageTracking.TRACKING_CONFIG);              
        } else if (Role.CONSUMER.equals(msg.getRole())) {
            config = (TrackingConfig) getServiceQuality(msg); 
        }
        if (config != null && config.isTrackingEnabled()) {
            int code = getCodeForIncomingMsgEx(msg);
            doMessageTracking(msg, code);
        }        
        return msg;    
    }

    /** @see com.sun.jbi.common.qos.messaging.filter.AbstractExchangeFilter#processOutgoingExchange(javax.jbi.messaging.MessageExchange, java.lang.Object[]) */
    public MessageExchange processOutgoingExchange(MessageExchange msg,
												   Object... params) 
			throws MessagingException {
        TrackingConfig config = null;
        int code = getCodeForOutgoingMsgEx(msg);
        if (Role.PROVIDER.equals(msg.getRole())) {
            config = (TrackingConfig)msg.getProperty(MessageTracking.TRACKING_CONFIG);    
        } else if (Role.CONSUMER.equals(msg.getRole())) {
            config = (TrackingConfig) getServiceQuality(msg); 
            if (code == OUTGOING_REQUEST) {
                msg.setProperty(MessageTracking.TRACKING_CONFIG, config);
            }
        }        
        if (config != null && config.isTrackingEnabled()) {
            doMessageTracking(msg, code);
        }    
        return msg;    
    }

	/** @see com.sun.jbi.common.qos.messaging.filter.AbstractExchangeFilter#addServiceQuality(com.sun.jbi.common.descriptor.EndpointInfo, com.sun.jbi.common.qos.ServiceQuality[]) */
    public void addServiceQuality(EndpointInfo endpoint, ServiceQuality... qos) {
        // load TrackingConfig into map
        super.addServiceQuality(endpoint, qos);
        // generate TaleRequest template
        AppInfo info = getBase().getAppInfo(endpoint);
        if (info != null) {
            getClient().addRequestTemplate(
                    endpoint.toString(),    // guaranteed unique 
                    createRequestTemplate(endpoint, info));
        }
    }

    protected TaleRequest createRequestTemplate(EndpointInfo endpt, AppInfo info) {
        // Set the values that can be fetched from service unit and assembly        
        TaleRequest template = getClient().newRequest();        
        template.getSourceInfo().setApplicationName(info.getAssembly().getIdentification().getName());
        template.getSourceInfo().setModuleName(info.getServiceUnit());
        return template;
    }
    
    protected TaleClient getClient() {
	    return mTaleClient;
	}
	
    private void doMessageTracking(MessageExchange msg, int code) {    
        EndpointInfo endpt = EndpointInfo.valueOf(msg.getEndpoint(), Role.PROVIDER.equals(msg.getRole()));
        TaleRequest aleRequest = null;
        aleRequest = getClient().newRequest(endpt.toString());
        //TODO aleRequest won't be set for Providers.
        if (aleRequest == null) {
            aleRequest = getClient().newRequest();
            aleRequest.getSourceInfo().setApplicationName("Service-Assembly_Name");
            aleRequest.getSourceInfo().setModuleName("Service-Unit_Name");
        }
        // Set the Source Info
        SourceInfo info = aleRequest.getSourceInfo();
        // Get the current TimeStamp value
        Calendar cal = new GregorianCalendar();
        info.setDateTimeStamp(new Timestamp(cal.getTimeInMillis()));
        info.setApplicationType(JBI_APPLICATION);
        
        // Set the componentName and componentType value
        String compName = getBase().getContext().getComponentName();
        info.setComponentName(compName);
        if (compName != null) {
            if (compName.toLowerCase().endsWith("engine")) {
                info.setComponentType(SERVICE_ENGINE);
            } else if (compName.toLowerCase().endsWith("binding")) {
                info.setComponentType(BINDING_COMPONENT);
            }
        }
        info.setServiceName("");
        
        //TODO Get the UnitName and InstanceName from the MessageExchange
        info.setUnitName("BPEL-ID");
        info.setInstanceName("InstanceId");  
        
        //Set the AppMsgId
        info.setAppMessageID(Redelivery.getUniqueId(msg));        
        
        EnvironmentInfo envInfo = aleRequest.getEnvironmentInfo();        
        try {
            envInfo.setHostName(InetAddress.getLocalHost().getCanonicalHostName());
        } catch (UnknownHostException ex) {
            envInfo.setHostName("");
        }
        envInfo.setServerName(System.getProperty("com.sun.jbi.instanceName"));
        //TODO Set the actual values for Environment info
        envInfo.setEnvName("LocalEnvironment");
        envInfo.setLogicalHostName("LogicalHost1");        
        envInfo.setServerType("GlassFishServer");
        
        //Set the Exchange info
        ExchangeInfo exInfo = aleRequest.getExchangeInfo();
        exInfo.setExchangeID(msg.getExchangeId());
        exInfo.setServiceName(msg.getEndpoint().getServiceName().toString());     
        exInfo.setEndPointName(msg.getEndpoint().getEndpointName());
        exInfo.setOperationName(msg.getOperation().toString());
        
        //TODO: Add MessageTracking Ids
        aleRequest.addMsgTracking("GUID");    
        
        aleRequest.setCode(code);
        aleRequest.setDetails("Add some more details for Log Message");
        aleRequest.setDisplayMessage("Message Tracking Log");
        
        Payload payload = new Payload();
        payload.setPayloadMessage("This is the payload message");
        aleRequest.setPayload(payload);

        getClient().sendLog(aleRequest);    
    }
    
    /**
     * Gets the logging code for the InComing MessageExchange
     * @param msgEx MessageExchange
     * @return the reserved LoggerCode
     */
    private int getCodeForIncomingMsgEx(MessageExchange msgEx) {
        int code = 0;
        ExchangeStatus status = msgEx.getStatus();            
        Role role = msgEx.getRole();
        if (Role.CONSUMER.equals(role)) {
            if (ExchangeStatus.ACTIVE.equals(status)) {
                code = INCOMING_RESPONSE;
            } else if (ExchangeStatus.DONE.equals(status)) {
                code = INCOMING_STATUS_DONE_CONSUMER;
            } else if (ExchangeStatus.ERROR.equals(status)) {
                code = INCOMING_STATUS_ERROR_CONSUMER;
            }
        } else if (Role.PROVIDER.equals(role)) {
            if (ExchangeStatus.ACTIVE.equals(status)) {
                code = INCOMING_REQUEST;
            } else if (ExchangeStatus.DONE.equals(status)) {
                code = INCOMING_STATUS_DONE_PROVIDER;
            } else if (ExchangeStatus.ERROR.equals(status)) {
                code = INCOMING_STATUS_ERROR_PROVIDER;
            }
        }        
        return code;
    }
    
    /**
     * Gets the logging code for the OutGoing MessageExchange
     * @param msgEx MessageExchange
     * @return the reserved LoggerCode
     */
    private int getCodeForOutgoingMsgEx(MessageExchange msgEx) {
        int code = 0;
        ExchangeStatus status = msgEx.getStatus();            
        Role role = msgEx.getRole();
        if (Role.CONSUMER.equals(role)) {
            if (ExchangeStatus.ACTIVE.equals(status)) {
                code = OUTGOING_REQUEST;
            } else if (ExchangeStatus.DONE.equals(status)) {
                code = OUTGOING_STATUS_DONE_CONSUMER;
            } else if (ExchangeStatus.ERROR.equals(status)) {
                code = OUTGOING_STATUS_ERROR_CONSUMER;
            }
        } else if (Role.PROVIDER.equals(role)) {
            if (ExchangeStatus.ACTIVE.equals(status)) {
                code = OUTGOING_RESPONSE;
            } else if (ExchangeStatus.DONE.equals(status)) {
                code = OUTGOING_STATUS_DONE_PROVIDER;
            } else if (ExchangeStatus.ERROR.equals(status)) {
                code = OUTGOING_STATUS_ERROR_PROVIDER;
            }
        }        
        return code;
    }

}
