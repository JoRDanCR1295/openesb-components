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
 * @(#)JbiEngine.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.runtime.impl;

import java.util.logging.Level;
import javax.jbi.JBIException;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.transform.Source;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.messaging.ExchangeUtil;
import com.sun.jbi.common.qos.messaging.ExchangeUtil.FaultCode;
import com.sun.jbi.common.qos.throttling.ThrottlingConfig;
import com.sun.jbi.common.xml.XmlUtil;
import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;
import com.sun.jbi.component.toolkit.util.ExchangePattern;
import com.sun.transform.I18n;
import com.sun.transform.engine.model.Invocation;
import com.sun.transform.engine.runtime.ActivityUnit;
import com.sun.transform.engine.runtime.InvocationUnit;
import com.sun.transform.engine.runtime.ProcessInstance;
import com.sun.transform.engine.runtime.ProcessingException;
import com.sun.transform.engine.runtime.WSMessage;

/**
 * A specialized transformation engine for the JBI environment, 
 * primarily handling of runtime invocations.
 * 
 * @author Kevan Simpson
 */
public class JbiEngine extends EngineImpl {
    public enum Ack { done, fault, error }
    
	private ThrottlingQueue mQueue;
	
	/** No-arg constructor for reflection-instantiated subclasses. */
	protected JbiEngine() {
        mQueue = new ThrottlingQueue();
	}

	protected JbiEngine(ManagerContext ctx) {
	    super(ctx);
	    mQueue = new ThrottlingQueue();
	}
	
	/** @see com.sun.transform.engine.runtime.impl.EngineImpl#process(com.sun.transform.engine.runtime.ProcessInstance) */
	public boolean process(ProcessInstance proc) throws ProcessingException {
		if (runProcess(proc)) {
			reply(proc, Ack.done);
			return true;
		}
		return false;
	}

	/** @see com.sun.transform.engine.runtime.Engine#process(com.sun.transform.engine.runtime.InvocationUnit) */
	public boolean process(InvocationUnit unit) throws ProcessingException {
		try {
			Invocation invoke = (Invocation) unit.getActivity();
			ProcessInstance proc = unit.getEnclosingProcess();
			EndpointInfo info = invoke.getInfo();
            
			if (unit.getMessageExchange() == null) {	// handle incoming request
		        // is throttling configured AND maxed out? then queue the exchange
		        ThrottlingConfig config = getThrottlingConfig(info);
		        if (config != null && // do we need to throttle? 
		            mQueue.getInvokeCount(info) >= config.getMaxConcurrencyLimit()) {
		            mQueue.queue(unit);   // invoke later
		        }
		        else {
		            mQueue.increment(info);
		            doInvoke(unit);
		        }
		        
		        return false;	// the activity is not complete
			}
			else {	// handle invoke's reply
			    mQueue.decrement(info);  // decrement count for received reply
			    MessageExchange response = unit.getMessageExchange();
                handleResponse(proc, response, invoke);
                
			    // then see if throttling has queued any exchanges
			    InvocationUnit queued = mQueue.dequeue(info);
			    if (queued != null) {
			        mQueue.increment(info);
			        doInvoke(queued);
			    }
			    
			    // validate response? InOnly MEx will always pass validation
			    // KPS: not implemented...will return true
			    return validate(unit);
			}
		}
		catch (ProcessingException pe) {
			// already logged, rethrow
			throw pe;
		}
		catch (MessagingException me) {
			throw error(me, I18n.loc("TRANSL-6030: Unable to create invokable exchange: {0}", 
								 	 me.getMessage()));
		}
		catch (JBIException jbi) {
			throw error(jbi, I18n.loc("TRANSL-6031: Invocation failed: {0}", 
									  jbi.getMessage()));
		}
		catch (Exception e) {
			throw error(e, I18n.loc("TRANSL-6032: An unexpected error occurred during invoke: {0}", 
									e.getMessage()));
		}
	}

    protected MessageExchange createExchange(ExchangePattern ptrn) throws MessagingException {
        MessageExchangeFactory factory = getContext().getMessagingChannel().getExchangeFactory(); 
        switch (ptrn) {
            case IN_ONLY: {
                return factory.createInOnlyExchange();
            }
            case IN_OUT: {
                return factory.createInOutExchange();
            }
            default: {
                String err = I18n.loc(
                        "TRANSL-6021: Failed to create exchange - unsupported pattern: {0}", 
                        ptrn);
                log().warning(err);
                throw new MessagingException(err);
            }
        }
    }

    /**
     * Executes an invocation.
     * @param unit The invocation activity.
     * @throws ProcessingException if an error occurs processing activity.
     * @throws JBIException if an error occurs sending exchange.
     */
    protected void doInvoke(InvocationUnit unit) throws ProcessingException, JBIException {
        Invocation invoke = (Invocation) unit.getActivity();
        ProcessInstance proc = unit.getEnclosingProcess();

        MessageExchange mex = getInvoker().createExchange(
                invoke.getInfo(), invoke.getOperation());
        unit.setMessageExchange(mex);
        // register invoke's input and output variables
        registerInvocationVariables(unit);
        getContext().getCorrelationMap().put(
                mex.getExchangeId(), proc);
        WSMessage wsm = (WSMessage) 
                proc.getVariableContext().getVariable(invoke.getInputVariable());
        // Create new message and populate its content
        getInvoker().setInput(mex, wsm);
        // systemics
        getInvoker().setUniqueId(mex, invoke.getName(), proc.getMessageExchange());
        getInvoker().propagateSystemics(proc.getMessageExchange(), mex);

        getContext().getMessagingChannel().send(mex);
    }
    
	protected void reply(ProcessInstance proc, Ack ack) throws ProcessingException {
		ExchangePattern ptrn = com.sun.jbi.component.toolkit.util.ExchangePattern.valueOf(proc.getMessageExchange());
		MessageExchange msg = proc.getMessageExchange();
		try {
            if (log().isLoggable(Level.FINER)) {
                log().finer("TRANSL-2009: "+ getName() +"'s "+ 
                            String.valueOf(proc.getProcessDef()) +" is replying");
            }

			switch (ptrn) {
				case IN_OUT: {
				    switch (ack) {
				        case done: {
        					// we are an InOut service, reply w/ content
        					WSMessage outMsg = (WSMessage) 
        							proc.getVariableContext().getVariable(
        									proc.getProcessDef().getInvocation().getOutputVariable());
        					Source content = outMsg.toSource();
        					if (log().isLoggable(Level.FINEST)) {
        						log().finest("TRANSL-1001: "+ getName() +
        									  " replying to ME (id={"+ 
        									  msg.getExchangeId() +") with content: "
        									  + XmlUtil.print(content));
        					}
        			        NormalizedMessage xOut = msg.createMessage();
        			        // set NM properties on Out NM
        			        outMsg.initMessage(xOut);
        			        // set NM content = response
        			        xOut.setContent(content);
        			        ((InOut) msg).setOutMessage(xOut);
        			        break;
    				    }
				        case error: {
    			            msg.setStatus(ExchangeStatus.ERROR);
    			            break;
    			        }
				        case fault: {
				            break;  // fault should already be set
				        }
				    }
				    
			        getContext().getMessagingChannel().send(msg);
					break;
				}
				case IN_ONLY: {
					// reply w/ DONE status
					msg.setStatus(ack == Ack.error 
					        ? ExchangeStatus.ERROR : ExchangeStatus.DONE);
					getContext().getMessagingChannel().send(msg);
					break;
				}
				default: {
					throw new IllegalStateException(
							I18n.loc("TRANSL-6033: Unsupported exchange pattern: {0}", ptrn));
				}
			}
		}
		catch (Exception e) {
			try {
				ProcessingException pex = 
						error(e, I18n.loc("TRANSL-6045: {0} failed reply to complete process, ME (id={1}): {2}", 
										  getName(), msg.getExchangeId(), e.getMessage()));
				// reply with error status to complete ME
				ExchangeUtil.setErrorData(msg, 
				                          pex.getMessage(), 
				                          FaultCode.Server, 
				                          e.getMessage(), 
				                          getContext().getComponentContext().getComponentName());
				msg.setError(pex);
				getContext().getMessagingChannel().send(msg);
			}
			catch (Exception foobar) {
				throw error(foobar, I18n.loc(
						"TRANSL-6035: {0} failed to send ERROR status on ME (id={1}): {2}",  
						getName(), msg.getExchangeId(), foobar.getMessage()));
			}
		}
	}

	protected boolean validate(ActivityUnit unit) throws ProcessingException {
	    return true;   // not implemented
//	    Validator v = new Validator(getContext());
//	    return unit.execute(v);
	}
	
	protected void handleResponse(ProcessInstance proc, 
						 		  MessageExchange response, 
						 		  Invocation invoke) throws ProcessingException {
		// pass TX to previous exchange in process
		getInvoker().propagateSystemics(response, proc.getMessageExchange());

		ExchangePattern ptrn = ExchangePattern.valueOf(response);
		switch (ptrn) {
			case IN_OUT: {
			    getInvoker().completeInOut(proc, (InOut) response, invoke);
				break;
			}
			case IN_ONLY: {
			    getInvoker().completeInOnly(proc, (InOnly) response, invoke);
				break;
			}
			default: {
				throw new IllegalStateException(
						I18n.loc("TRANSL-6033: Unsupported exchange pattern: {0}", 
								 ptrn));
			}
		}
	}

	protected ThrottlingConfig getThrottlingConfig(EndpointInfo info) throws MessagingException {
	    if (getContext().getMessagingChannel() != null) {
	        ThrottlingConfig config = (ThrottlingConfig) 
	                getContext().getMessagingChannel()
	                        .getServiceQuality(info, ThrottlingConfig.class);
	        return config;
	    }
	    
	    return null;
	}
	
	protected ProcessingException error(Exception cause, String msg) {
    	if (cause == null) {
    		log().warning(msg);
    		return new ProcessingException(msg);
    	}
    	else {
    		log().log(Level.WARNING, msg, cause);
    		if (cause instanceof ProcessingException) {
    			return (ProcessingException) cause;
    		}
    		else {
    			return new ProcessingException(msg, cause);
    		}
    	}
    }
}
