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
 * @(#)EngineExchangeHandler.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.runtime.impl;

import java.util.logging.Level;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;
import com.sun.jbi.common.qos.messaging.ExchangeUtil;
import com.sun.jbi.common.qos.messaging.ExchangeUtil.FaultCode;
import com.sun.jbi.common.qos.redelivery.Redelivery;
import com.sun.jbi.common.qos.redelivery.RedeliveryStatus;
import com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler;
import com.sun.jbi.component.toolkit.lifecycle.impl.AbstractExchangeHandler;
import com.sun.jbi.component.toolkit.util.ExchangePattern;
import com.sun.transform.I18n;
import com.sun.transform.descriptor.TransformEndpoint;
import com.sun.transform.engine.EngineConfig;
import com.sun.transform.engine.EngineConfigurationError;
import com.sun.transform.engine.model.Invocation;
import com.sun.transform.engine.runtime.Engine;
import com.sun.transform.engine.runtime.InvocationUnit;
import com.sun.transform.engine.runtime.ProcessInstance;
import com.sun.transform.engine.runtime.ProcessUnitFactory;
import com.sun.transform.engine.runtime.ProcessingException;

/**
 * 
 * @author Kevan Simpson
 */
public abstract class EngineExchangeHandler extends AbstractExchangeHandler {
    private static ProcessUnitFactory mUnitFactory = new ProcessUnitFactoryImpl();

    private Engine mEngine;
    
    protected EngineExchangeHandler(ComponentContext ctx) {
        super(ctx);
    }
    
	/** @see com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler#handleExchange(javax.jbi.messaging.MessageExchange) */
    public void handleExchange(MessageExchange msg) throws JBIException {
        try {
        	PatternRole key = PatternRole.toKey(msg);
        	switch (key) {
        		case IN_OUT_PROVIDER: {
        			if (ExchangeStatus.ACTIVE.equals(msg.getStatus())) {
        				provisionService(msg);
        			}
        			else {
        				Invoker.logStatus(log(), getContext().getComponentContext(), msg);
    				    // remove process, ERROR or DONE
    				    getContext().getCorrelationMap().remove(msg.getExchangeId());
        			}
        			break;
        		}
        		case IN_OUT_CONSUMER: {
        			if (redeliver(msg)) {    // will redeliver or relay
        			    return;
        			}
        			else if (msg.getFault() != null) {
        			    handleFault(msg);
                    }
        			else {	// ACTIVE, should never be DONE
        				continueProcessing(msg);
        			}
        			break;
        		}
        		case IN_ONLY_PROVIDER: {
        			provisionService(msg);
        			break;
        		}
        		case IN_ONLY_CONSUMER: {	// always a status response
    		    	if (Invoker.logStatus(log(), getContext().getComponentContext(), msg)) {    // false on ERROR status
    		    	    continueProcessing(msg);
    		    	}
    		    	else {   // will redeliver or relay
    		    	    redeliver(msg);
    		    	}
        			break;
        		}
				default: {	// unsupported pattern
					String err = I18n.loc(
							"TRANSL-7001: {0} does not support {1} exchange pattern", 
							this.getClass().getSimpleName(), 
							String.valueOf(ExchangePattern.valueOf(msg)));
					log().severe(err);
					if (ExchangeStatus.ACTIVE.equals(msg.getStatus())) {
					    ExchangeUtil.setErrorData(msg, err, FaultCode.Client, null, getComponentName());
					    sendError(msg, new Exception(err));
					}
					else if (ExchangeStatus.ERROR.equals(msg.getStatus())) {
					    Invoker.logError(log(), getContext().getComponentContext(), msg);
					}
				}
        	}
        }
        catch (Exception e) {
        	// processing exceptions are handled, but anything here means we can't send
        	// log and throw
            throw error(I18n.loc("TRANSL-7002: Failed to complete exchange({0}) for endpoint({1}-{2}): {3}",
            					 msg.getExchangeId(), msg.getEndpoint().getServiceName(),
            					 msg.getEndpoint().getEndpointName(), e.getMessage()), 
                        e);
        }
	}

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractExchangeHandler#start() */
    @Override
    public void start() throws JBIException {
        // set the transformation engine
        String engineImpl = getContext().getConfiguration().getProperty(
                EngineConfig.TRANSFORM_ENGINE_PROPERTY).getValue();
        try {
            setEngine(createEngine(engineImpl));
        }
        catch (EngineConfigurationError ece) {
            throw error(I18n.loc(
                    "TRANSL-6042: Failed to load transformation engine: {0}", 
                    ece.getMessage()), ece);
        }
        
        super.start();
    }

    protected abstract Engine createEngine(String engineImpl);
    
	/**
	 * Executes the specified {@link ProcessInstance}. Implementations of this 
	 * {@link ExchangeHandler} are expected to pass the process instance to the
	 * appropriate transformation engine, such as {@link com.sun.transform.engine.xslt.XsltEngine}.
	 */
	protected abstract void process(ProcessInstance proc) 
			throws ProcessingException;

	/**
	 * Continues executing a transformation process after receiving a reply.
	 * @param msg The reply message.
	 * @throws JBIException if an error occurs completing process.
	 */ 
	protected void continueProcessing(MessageExchange msg) 
			throws JBIException {
		// lookup process awaiting response, decorrelate
		ProcessInstance proc = (ProcessInstance) 
				getContext().getCorrelationMap().remove(msg.getExchangeId());
		try {
    		// then continue processing
    		process(proc);
		}
		catch (ProcessingException pe) {
            // sending ERROR is appropriate for InOut and InOnly
		    String err = I18n.loc(
                    "TRANSL-6041: A processing error occurred provisioning a {0} service: {1}", 
                    getComponentName(), pe.getMessage());
		    ExchangeUtil.setErrorData(proc.getMessageExchange(), 
		            err, FaultCode.Server, null, getComponentName());
		    sendError(proc.getMessageExchange(), pe);
        }
	}

	/**
	 * Handles a returned {@link Fault} from consumed {@link InOut} service.
	 * @param msg The reply message containing a <code>Fault</code>.
	 * @throws Exception If an error occurs identifying fault or replying.
	 */
	protected void handleFault(MessageExchange msg) throws Exception {
	    boolean error = false;
	    try {
    	    // basic handling, this is a template method if needed
            FaultHandler fh = new FaultHandler(getContext(), getEngine());
            fh.handleExchange(msg);
	    }
	    catch (Exception ex) {
	        error = true;
	        sendError(msg, ex);
	        throw ex;
	    }
        finally {
            if (!error) {
                msg.setStatus(ExchangeStatus.DONE);
                send(msg);
            }
        }
	}
	
	/**
	 * Forwards a received {@link ExchangeStatus#ERROR} to the consumer of this service.
	 * 
	 * @param msg The received message with <code>ERROR</code> status.
	 * @throws JBIException If an error occurs forwarding the error.
	 */
	protected void relayError(MessageExchange msg) 
			throws JBIException {
	    Invoker.logError(log(), getContext().getComponentContext(), msg);
		// InOut:Consumer - lookup process awaiting response, decorrelate
		ProcessInstance proc = (ProcessInstance) 
				getContext().getCorrelationMap().remove(msg.getExchangeId());
		MessageExchange sendMsg = proc.getMessageExchange();
		// propagate error details and send
		ExchangeUtil.propagateErrorData(msg, sendMsg);
		// from systemics wiki: "Even when component relays the failure it should set its name."
		sendMsg.setProperty(ExchangeUtil.FAULTACTOR_PROPERTY_NAME, getComponentName());
		sendError(sendMsg, sendMsg.getError());
	}

	/**
	 * Evaluates message for status and whether to redeliver or relay if 
	 * {@link ExchangeStatus#ERROR}, at which point this method returns <code>true</code>.
	 * 
	 * @param msg The exchange in need of redelivery.
	 * @return <code>true</code> if message has <code>ERROR</code> status, else <code>false</code>.
	 * @throws JBIException if an error occurs sending the message exchange.
	 */
	protected boolean redeliver(MessageExchange msg) throws JBIException {
	    if (ExchangeStatus.ERROR.equals(msg.getStatus())) {
            RedeliveryStatus status = Redelivery.getRedeliveryStatus(msg);
            if (status != null && !status.hasFailed()) {
                resend(msg);
            }
            else {
                // otherwise, relay error back to original consumer
                relayError(msg);
            }
            return true;
	    }
	    
	    return false;
	}
	
    /**
     * Redelivers the specified message exchange content in a new exchange.
     * 
     * @param msg The exchange in need of redelivery.
     * @throws JBIException if an error occurs resending the message.
     */
	protected void resend(MessageExchange msg) throws JBIException {
	    // declared outside try for error message, if needed
	    ProcessInstance proc = null;
	    Invocation invoke = null;
	    try {
    	    proc = (ProcessInstance) 
                    getContext().getCorrelationMap().remove(msg.getExchangeId());
    	    // it's expected that the current activity is an Invoke
    	    InvocationUnit unit = (InvocationUnit) proc.currentActivity();
    	    invoke = (Invocation) unit.getActivity();
    	    // create new message...
    	    MessageExchange invokeMsg = createExchange(ExchangePattern.valueOf(msg)); 
    	    ServiceEndpoint endpt = getContext().getComponentContext()
    	            .getEndpoint(invoke.getInfo().getServiceName(), 
    	                         invoke.getInfo().getEndpointName());
    	    invokeMsg.setEndpoint(endpt);
    	    // recorrelate process with new exchange and activity unit...
    	    getContext().getCorrelationMap().put(invokeMsg.getExchangeId(), proc);
    	    unit.setMessageExchange(invokeMsg);
    	    // propagate systemics from initiating message to new exchange
    	    ExchangeUtil.propagateSystemics(proc.getMessageExchange(), invokeMsg);
    	    Redelivery.setUniqueId(invokeMsg, Redelivery.getUniqueId(msg));
    	    // and resend content...
    	    invoke(invokeMsg, ExchangeUtil.getInMessage(msg).getContent(), msg.getOperation());
	    }
        catch (Exception e) {
            // sending ERROR is appropriate for InOut and InOnly
            String err = I18n.loc(
                    "TRANSL-6057: {0} failed retry attempt for {1}-{2}: {3}", 
                    getComponentName(),
                    (proc == null) ? "Unknown Process" : String.valueOf(proc),
                    (invoke == null) ? "Unknown Invoke" : String.valueOf(invoke),
                    e.getMessage());
            // log
            log().log(Level.WARNING, err, e);
            // ...and throw
            if (proc == null) {
                throw new JBIException(err, e);
            }
            else {  // or reply with ERROR
                MessageExchange recv = proc.getMessageExchange();
                Exception sent = new Exception(err, e);
                sendError(recv, sent);
            }
        }
	}
	
	/**
	 * Looks up the appropriate provisioning service and processes the message.
	 * <b>NOTE:</b> Only {@link ExchangeStatus#ACTIVE} messages are expected.
	 * 
	 * @param msg The incoming message exchange.
	 * @throws JBIException If an error occurs provisioning service.
	 */
	protected void provisionService(MessageExchange msg) 
			throws JBIException {
		// acquire endpoint
        TransformEndpoint endpt = (TransformEndpoint) findProvisioningEndpoint(msg);
        // consumer has initiated exchange with a message
        try {
        	// create process, verify definition
	        ProcessInstance proc = mUnitFactory.create(msg.getOperation(), endpt);
	        if (proc.getProcessDef() == null) {
				// this operation is not defined
				String faultstring = I18n.loc(
						"TRANSL-6040: The requested operation \"{0}\" is not provisioned on Endpoint[{1}-{2}]", 
						msg.getOperation().getLocalPart(), 
						endpt.getInfo().getServiceName(), 
						endpt.getInfo().getEndpointName());
				String detail = I18n.loc(
						"TRANSL-5003: Please define a transformation process for operation \"{0}\".", 
						msg.getOperation());
				ExchangeUtil.setErrorData(msg, faultstring, FaultCode.Client, detail, getComponentName());
				msg.setError(new Exception(faultstring));
				getContext().getMessagingChannel().send(msg);
	        }
	        else {
	            if (log().isLoggable(Level.FINER)) {
	                log().finer("TRANSL-2008: Executing "+ getComponentName() 
	                            +" process: "+ String.valueOf(proc.getProcessDef()));
	            }
	            
	        	proc.setMessageExchange(msg);
	        	process(proc);
	        }
		}
		catch (ProcessingException pe) {
			// sending ERROR is appropriate for InOut and InOnly
		    String err = I18n.loc(
                    "TRANSL-6041: A processing error occurred provisioning a {0} service: {1}", 
                    getComponentName(), pe.getMessage());
		    sendError(msg, new Exception(err, pe));
		}
	}
	
    /** 
     * Returns the engine.
     * @return the engine. 
     */
    protected Engine getEngine() {
        return mEngine;
    }

    /**
     * Sets the engine. 
     * @param engine The engine to set. */
    protected void setEngine(Engine engine) {
        mEngine = engine;
        if (mEngine != null) {
            mEngine.setContext(getContext());
        }
    }
}
