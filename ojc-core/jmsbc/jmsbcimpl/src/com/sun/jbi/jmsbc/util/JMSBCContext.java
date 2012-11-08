package com.sun.jbi.jmsbc.util;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessagingException;

import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig.Failure;
import com.sun.jbi.common.qos.throttling.ThrottlingConfig;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.jmsbc.Endpoint;
import com.sun.jbi.jmsbc.extensions.JMSOperation;
import com.sun.jbi.jmsbc.mbeans.JMSBCManagement;
import com.sun.jbi.jmsbc.mbeans.JMSBCManagementMBean;
import com.sun.jbi.jmsbc.util.AlertsUtil;
import com.sun.jbi.jmsbc.JMSBindingComponent;
import com.sun.jbi.alerter.NotificationEvent;

public class JMSBCContext {
    private static final Messages mMessages =
        Messages.getMessages(JMSBCContext.class);
    private static final Logger mLog =
        Messages.getLogger(JMSBCContext.class);
	
	private ComponentContext mContext;
	private DeliveryChannel mChannel;
	private static JMSBCContext mRef = new JMSBCContext();
	
	private JMSBCContext(){
		
	}

	public ComponentContext getContext() {
		return mContext;
	}

	public void setContext(ComponentContext context) {
		mContext = context;
	}

	public DeliveryChannel getChannel() {
		return mChannel;
	}

	public void setChannel(DeliveryChannel channel) {
		mChannel = channel;
	}

	public static JMSBCContext getRef() {
		return mRef;
	}
	
	public DeliveryChannel getUnWrapperChannel() throws MessagingException{
		return getContext().getDeliveryChannel(); 
	}
	
	
	public ServiceQuality[] getQOSConfigConfigurationsValidForMEP(JMSOperation jmsOp, Endpoint endpoint){
        //Now apply throttling QOS to the channel
        ServiceQuality[] qos = endpoint.getServiceQualities();
        List<ServiceQuality> result = new ArrayList<ServiceQuality>(5);
        if(qos!=null){
        	for(int i=0; i<qos.length; ++i){
        		if(qos[i] instanceof ThrottlingConfig){
        			result.add(qos[i]); ////This is applied both MEP "IN-ONLY" and "IN-OUT"
        		}if(qos[i] instanceof RedeliveryConfig){
        			String mep = jmsOp.getMEP();
                    if (mep.equals(Endpoint.EndpointMessageType.IN_ONLY)) {
            			result.add(qos[i]);
                    }else if(mep.equals(Endpoint.EndpointMessageType.IN_OUT)) {
                    	Failure failure = ((RedeliveryConfig)qos[i]).getFailure();
                    	if(failure == Failure.error || failure == Failure.suspend){
                    		//only "error" and "suspend" are suppoted in IN-OUT
                			result.add(qos[i]);
                    	}else{
                    		//Log this fact
                            String option = (failure == Failure.redirect)? "redirect" : "delete";
                    		String msg = mMessages
									.getString(
											"JMSBC-I0701.Unsupported_onfailure_option",
											new String[] {
													option,
													endpoint.getServiceName()
															.toString(),
													endpoint.getEndpointName(),
													jmsOp.getBindingOperation()
															.getOperation()
															.getName() });
                    		mLog.log(Level.WARNING, msg);
                    		AlertsUtil.getAlerter().warning(msg, 
                                    JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                    null, 
                                    AlertsUtil.getServerType(),
                                    AlertsUtil.COMPONENT_TYPE_BINDING,
                                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                    NotificationEvent.EVENT_TYPE_ALERT,
                                    "JMSBC-I0701"); 
                    	}
                    }
        		}
        	}
        }
        return result.toArray(new ServiceQuality[0]);
	}

}
