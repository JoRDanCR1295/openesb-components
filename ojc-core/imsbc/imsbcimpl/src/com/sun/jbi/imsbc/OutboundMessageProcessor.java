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

package com.sun.jbi.imsbc;

import java.net.URI;
import java.util.Map;
import java.util.Iterator;
import java.util.Collection;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import com.sun.jbi.imsbc.ims.Channel;
import com.sun.jbi.imsbc.ims.SendChannelImpl;
import com.sun.jbi.imsbc.Endpoint.EndpointState;
import com.sun.jbi.imsbc.Endpoint.EndpointType;
import com.sun.jbi.imsbc.extensions.IMSOperation;
import com.sun.jbi.imsbc.extensions.IMSInput;
import com.sun.jbi.imsbc.extensions.IMSOutput;
import com.sun.jbi.imsbc.extensions.IMSMessage;
import com.sun.jbi.imsbc.extensions.IMSConstants;
import com.sun.jbi.imsbc.extensions.IMSAddress;
import com.sun.jbi.imsbc.IMSException;
import com.sun.jbi.imsbc.util.IMSUtil;
import com.sun.jbi.imsbc.util.AlertsUtil;

import com.sun.jbi.nms.exchange.ExchangePattern;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.internationalization.Messages;

import net.java.hulp.measure.Probe;

import com.sun.jbi.common.qos.messaging.MessagingChannel;

/**
 * Process replies/requests received from the SE.
 *
 * @author Sun Microsystems
 */
public class OutboundMessageProcessor implements Runnable {

    private static final Messages mMessages = Messages.getMessages(OutboundMessageProcessor.class);

    private static final Logger mLogger = Messages.getLogger(OutboundMessageProcessor.class);

    private Collection mServiceUnits;

    private MessagingChannel mChannel;

    private Map mInboundExchanges;

    private Object mMonitor;

    private IMSDenormalizer imsDenormalizer;

    private IMSNormalizer imsNormalizer;

    private ComponentContext mComponentContext;

    private MessageExchange msgExchange;
	
	public static final String PROVISIONING_ID = "Provider";
    
	public static final String CONSUMING_ID = "Consumer";
	
	private Channel imsChannel;


    public OutboundMessageProcessor(ComponentContext componentContext, MessagingChannel chnl, Collection serviceUnits,
             						Map inboundMessageExchanges, MessageExchange mExchange) throws Exception {
        mComponentContext = componentContext;
        mChannel = chnl;
        mServiceUnits = serviceUnits;
        mInboundExchanges = inboundMessageExchanges;
        mMonitor = new Object();
        msgExchange = mExchange;
        imsDenormalizer = IMSDenormalizer.getInstance();
        imsNormalizer = IMSNormalizer.getInstance();
    }

    /**
     * Main entry point to execute this in a thread. Calls accept on the JBI NMR and process the
     * MessageExchange it receives. Delegates the real work of processing the MessageExchange to
     * <code>execute</code>
     */
    public void run() {
		if (mLogger.isLoggable(Level.INFO)) 
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00410.OMP_Service_Loop_Enter"));
        try {
                if (msgExchange != null) {
						if (mLogger.isLoggable(Level.INFO)) 
							mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00411.OMP_Nmr_Accept_Mxch",
											new Object[] { msgExchange.getExchangeId() }));

                        Map inboundMessageExchanges = InboundMessageProcessor.getInboundExchanges();
                        int endpointType;
                        if (inboundMessageExchanges.containsKey(msgExchange.getExchangeId()) && !isRequest(msgExchange)) {
                                endpointType = EndpointType.INBOUND;
                        } else {
                                endpointType = EndpointType.OUTBOUND;
                        }

                        execute(msgExchange, endpointType);
						if (mLogger.isLoggable(Level.INFO)) 
							mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00412.OMP_Nmr_Complete_Mxch",
											new Object[] { msgExchange.getExchangeId() }));
                }
        } catch (Throwable ex) {
			String exMsg = mMessages.getString("IMSBC-E00413.OMP_Unexpected_Error", new Object[] { ex.getLocalizedMessage() });
            mLogger.log(Level.SEVERE, exMsg);

             AlertsUtil.getAlerter().critical(exMsg, 
									IMSBindingComponent.SHORT_DISPLAY_NAME, 
									null, 
									AlertsUtil.getServerType(),
									AlertsUtil.COMPONENT_TYPE_BINDING,
									NotificationEvent.OPERATIONAL_STATE_RUNNING, 
									NotificationEvent.EVENT_TYPE_ALERT,
									"IMSBC-E00413");
        }
		if (mLogger.isLoggable(Level.INFO)) 
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00414.OMP_Service_Loop_Exit"));
    }

    /**
     * Process the message exchange
     */
    public void execute(MessageExchange msgExchange, int endpointType) {

        String exchangeId = msgExchange.getExchangeId();
        boolean inbound = endpointType == EndpointType.INBOUND;

        Endpoint destination = null;

		destination = getEndpoint(msgExchange.getEndpoint().getServiceName(), msgExchange.getEndpoint().getEndpointName()); 

        if (destination == null) {
			if (mLogger.isLoggable(Level.SEVERE)) 
				mLogger.log(Level.SEVERE, mMessages.getString("IMSBC-E00416.OMP_Endpoint_Undefined", new Object[] {
						msgExchange.getEndpoint().toString(), msgExchange.getExchangeId() }));

            setErrorStatusAndSend(msgExchange);

            return;
        }

        int state = destination.getState();
        if (!(state == EndpointState.RUNNING)) {
            String strState = (state == EndpointState.STOPPED ? "STOPPED" : "SHUTDOWN");

            // If the endpoint is not in the RUNNING state
            // (i.e. is stopped or shutdown), ignore the message
			if (mLogger.isLoggable(Level.SEVERE)) 
				mLogger.log(Level.SEVERE, mMessages.getString("IMSBC-E00417.OMP_Endpoint_Not_Running", new Object[] {
						destination.getEndpointName(), strState, msgExchange.getExchangeId() }));

            setErrorStatusAndSend(msgExchange);
        } else {
            URI pattern = msgExchange.getPattern();
            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00418.OMP_Mxch_Pattern", new Object[] {
                        msgExchange.getExchangeId(), msgExchange.getPattern().toString() }));
            }
            switch (ExchangePattern.valueOf(msgExchange)) {
                case IN_OUT:
                    if (!inbound) 
                        processRequestReplyOutbound((InOut) msgExchange, 
                                                    destination);
                    break;
                case IN_ONLY:
					handleUnsupportedMsgExchange(msgExchange);				
                        break;
                default:                    
					handleUnsupportedMsgExchange(msgExchange);				
                    break;
            }
        }            
    }

    private boolean isRequest(MessageExchange msgX) {
        boolean request = false;
        if (msgX instanceof InOut) {
            InOut inout = (InOut) msgX;
            request = inout.getOutMessage() == null && inout.getFault() == null
                    && inout.getStatus() == ExchangeStatus.ACTIVE;
        } else if (msgX instanceof InOnly) {
            InOnly inonly = (InOnly) msgX;
            request = inonly.getFault() == null && inonly.getStatus() == ExchangeStatus.ACTIVE;
        }
        return request;
    }

    private void processRequestReplyOutbound(InOut inout, Endpoint destination) {
    	
    	Object replyContextKey = null;
        if (inout.getStatus() == ExchangeStatus.DONE) {            
        	destination.getEndpointStatus().incrementReceivedDones();                                    
        } else if (inout.getStatus() == ExchangeStatus.ERROR) {
        	destination.getEndpointStatus().incrementReceivedErrors();
        } else { 
        	destination.getEndpointStatus().incrementReceivedRequests();
            
        // Send request to IMS, reply from IMS will be received synchronously.
  
        try {
            ServiceEndpoint serviceEndpoint = inout.getEndpoint();
            QName opQname = inout.getOperation();
            Map imsOps = destination.getIMSOperations();
            IMSOperation imsOperation = null;
            Iterator imsItr = imsOps.entrySet().iterator();
            while(imsItr.hasNext())
            {
            	Map.Entry entry = (Map.Entry) imsItr.next();
            	if(entry.getKey().toString().equalsIgnoreCase(opQname.getLocalPart())){
            		imsOperation = (IMSOperation) entry.getValue();
            	}
            }
            IMSInput imsInput = destination.getIMSOperationInput(imsOperation);
            String msgXchangeID = inout.getExchangeId();
            NormalizedMessage normalizedMsg = inout.getInMessage();
            String imsReplyMsg = null;
			
			String endPointID = destination.getServiceName().getNamespaceURI() + "," + destination.getServiceName().getLocalPart()
								+ "," + destination.getEndpointName() + "," + PROVISIONING_ID;

			Probe denormalizationMeasurement = Probe.info(getClass(),
											   endPointID,
											   IMSBindingComponent.PERF_CAT_DENORMALIZATION);
			
			Object imsServerLoc = normalizedMsg.getProperty(IMSComponentContext.NM_PROP_IMSBC_ADDRESS_IMSSERVER_LOCATION);
			if(imsServerLoc != null){
				IMSAddress imsAddress = new IMSAddress();
				imsAddress.setServerLocation(imsServerLoc.toString());
				destination.setIMSAddress(imsAddress);
			}
			
			imsChannel = new SendChannelImpl(destination, imsOperation, mComponentContext); 			

            String imsMsg =  imsDenormalizer.denormalize(imsChannel,
														normalizedMsg,
														opQname,
														imsInput.getImsMessage());
			if (mLogger.isLoggable(Level.INFO)) {
				mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00420.OMP_End_Normalize_Message",
						new Object[]{imsMsg}));
				mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00421.OMP_Ims_Send_Destination",
						new Object[] { imsInput.getImsMessage().getIrmDestId()}));
			}

			if (denormalizationMeasurement != null) {
				denormalizationMeasurement.end();
			}  
			
			imsReplyMsg = imsChannel.send(imsMsg);

            if (imsReplyMsg == null) {
                String errMsg = mMessages.getString("IMSBC-R00422.OMP_No_Reply_Received", new Object[]{inout.getExchangeId()});

             AlertsUtil.getAlerter().critical(errMsg, 
									IMSBindingComponent.SHORT_DISPLAY_NAME, 
									null, 
									AlertsUtil.getServerType(),
									AlertsUtil.COMPONENT_TYPE_BINDING,
									NotificationEvent.OPERATIONAL_STATE_RUNNING, 
									NotificationEvent.EVENT_TYPE_ALERT,
									"IMSBC-R00422");
                throw new Exception (errMsg); // throw and catch
            } else {
                // Got a reply ims response/acknowledgement message, send back to consumer
			   Probe normalizationMeasurement = Probe.info(getClass(),
												endPointID,
												IMSBindingComponent.PERF_CAT_NORMALIZATION);

                NormalizedMessage outNormalizedMsg = imsNormalizer.normalize(inout,
                                                                             opQname,
                                                                             imsInput.getImsMessage(),
                                                                             false,
                                                                             destination,
                                                                             imsReplyMsg);
				if (normalizationMeasurement != null) {
					normalizationMeasurement.end();
				}
                inout.setOutMessage(outNormalizedMsg);
				if (mLogger.isLoggable(Level.INFO)) 
					mLogger.log(Level.INFO,
								 mMessages.getString("IMSBC-R00423.OMP_Sending_Back_Output",
								new Object[]{inout.getExchangeId()}));                    
            }
	     } catch (Exception ex)  {           
	            destination.getEndpointStatus().incrementSentErrors();            	            
	            mLogger.log(Level.SEVERE, 
	                     mMessages.getString("IMSBC-E00424.OMP_Mxch_Error",
	                     new Object [] {inout.getExchangeId(), 
	                                    IMSUtil.getStackTraceAsString(ex)}));
	                                
	            // setError should set status to error, but just in case...
	            try {
	                inout.setStatus(ExchangeStatus.ERROR);
					if (mLogger.isLoggable(Level.INFO)) 
	                    mLogger.log(Level.INFO,
	                            mMessages.getString("IMSBC-E00426.OMP_Mxch_Set_Status",
	                             new Object[]{"ERROR", inout.getExchangeId()}));
	            } catch (MessagingException ex2) {
					String errMsg = mMessages.getString("IMSBC-E00425.OMP_Mxch_Set_Status_Error",
														new Object[]{inout.getPattern(),
														inout.getExchangeId(),
														IMSUtil.getStackTraceAsString(ex2)});
	                 mLogger.log(Level.WARNING, errMsg);
					 AlertsUtil.getAlerter().critical(errMsg, 
												IMSBindingComponent.SHORT_DISPLAY_NAME, 
												null, 
												AlertsUtil.getServerType(),
												AlertsUtil.COMPONENT_TYPE_BINDING,
												NotificationEvent.OPERATIONAL_STATE_RUNNING, 
												NotificationEvent.EVENT_TYPE_ALERT,
												"IMSBC-E00426");
	            }                    
	        } finally {
	            // IMS "send" failed so send back fault or error to NMR
	            try {
	                // Send back exchange fault or error
	                mChannel.send(inout);
	            } catch (Exception mex2) {
	                mLogger.log(Level.SEVERE, 
	                        mMessages.getString("IMSBC-E00427.OMP_Nmr_Send_Status_Error",
	                        new Object [] {inout.getExchangeId(), 
	                                       IMSUtil.getStackTraceAsString(mex2)}));
	            }
	        }
        } 
    }

    private void setErrorStatusAndSend(MessageExchange msg) {
        try {
            msg.setStatus(ExchangeStatus.ERROR);
			if (mLogger.isLoggable(Level.INFO)) 
				mLogger.log(Level.INFO, mMessages.getString("IMSBC-E00426.OMP_Mxch_Set_Status", new Object[] { "ERROR",
						msg.getExchangeId() }));

        } catch (MessagingException ex) {
            mLogger.log(Level.WARNING, mMessages.getString("IMSBC-E00425.OMP_Mxch_Set_Status_Error", new Object[] { "in-only",
                    msg.getExchangeId(), ex }));
        }

        try {
            // Send back exchange status
            mChannel.send(msg);
        } catch (MessagingException mex) {
            mLogger.log(Level.WARNING, mMessages.getString("IMSBC-E00427.OMP_Nmr_Send_Status_Error", msg.getExchangeId()));
        }

    }

	private Endpoint getEndpoint(QName serviceName, String endpointName) {

		Endpoint endpoint = null;
        Iterator it = mServiceUnits.iterator();
        while (it.hasNext()) {
            Iterator it2 = ((ServiceUnit) it.next()).getEndpoints().iterator();
            while (it2.hasNext()) {
                Endpoint epoint = (Endpoint) it2.next();
                if (epoint.getServiceName().equals(serviceName) && epoint.getEndpointName().equals(endpointName)) {
					endpoint = epoint;
					break;
                }
            }
        }
		return endpoint;
	}
    private void handleUnsupportedMsgExchange(MessageExchange exchange) {      
		mLogger.log(Level.SEVERE, mMessages.getString("IMSBC-E00419.OMP_Mxch_Pattern_Invalid",
				new Object[] { exchange.getExchangeId() }));
    }


}
