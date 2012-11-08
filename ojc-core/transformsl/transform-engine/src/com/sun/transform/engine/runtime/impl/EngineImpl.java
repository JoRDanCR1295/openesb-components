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
 * @(#)EngineImpl.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.runtime.impl;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;

import com.sun.jbi.common.qos.messaging.ExchangeUtil;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.common.xml.XmlUtil;
import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;
import com.sun.jbi.component.toolkit.util.ExchangePattern;
import com.sun.transform.I18n;
import com.sun.transform.engine.model.Invocation;
import com.sun.transform.engine.runtime.ActivityUnit;
import com.sun.transform.engine.runtime.Engine;
import com.sun.transform.engine.runtime.InvocationUnit;
import com.sun.transform.engine.runtime.ProcessInstance;
import com.sun.transform.engine.runtime.ProcessingException;
import com.sun.transform.engine.runtime.TransformUnit;
import com.sun.transform.engine.runtime.WSMessage;

/**
 * Basic implementation of a transformation engine.
 * @author Kevan Simpson
 */
public class EngineImpl implements Engine {
	// shows up in every logging statement, ergo cached
	private String mName = this.getClass().getSimpleName();
    private Invoker mInvoker;
    private Logger mLogger;
    private ManagerContext mContext = null;

    protected EngineImpl() {
        mLogger = Logger.getLogger(this.getClass().getName());
    }
    
    public EngineImpl(ManagerContext ctx) {
        setContext(ctx);
    }

	/** @see com.sun.transform.engine.runtime.Engine#process(com.sun.transform.engine.runtime.ProcessInstance) */
	public boolean process(ProcessInstance proc) throws ProcessingException {
		try {
			return runProcess(proc);
		}
		catch (ProcessingException pe) {
			log().log(Level.WARNING,
						I18n.loc("TRANSL-6026: A processing error occurred in {0}: {1}", 
								 getName(), pe.getMessage()), 
						pe);
			throw pe;
		}
		catch (Exception e) {
			log().log(Level.WARNING,
						I18n.loc("TRANSL-6027: An unexpected error occurred in {0}: {1}", 
								 getName(), e.getMessage()), 
						e);
			throw new ProcessingException(e);
		}
	}

	/** @see com.sun.transform.engine.runtime.Engine#process(com.sun.transform.engine.runtime.InvocationUnit) */
	public boolean process(InvocationUnit inv) throws ProcessingException {
		return true;
	}

	/** @see com.sun.transform.engine.runtime.Engine#process(com.sun.transform.engine.runtime.TransformUnit) */
	public boolean process(TransformUnit tr) throws ProcessingException {
		return true;
	}

	/** @see com.sun.jbi.component.toolkit.lifecycle.ContextAware#getContext() */
    public ManagerContext getContext() {
        return mContext;
    }

    /**
     * Sets the component's {@link ManagerContext} on this engine and 
     * initializes an {@link Invoker} utility and a
     * {@link ComponentContext context-aware} {@link Logger}.
     * 
     * @param ctx The component's manager context.
     */
    public void setContext(ManagerContext ctx) {
        mContext = ctx;
        mInvoker = new Invoker(ctx);
        mLogger = Util.getLogger(ctx.getComponentContext(), this.getClass().getName());
    }

    protected Invoker getInvoker() {
        return mInvoker;
    }
    
	/** 
	 * Executes the activities in the specified {@link ProcessInstance}.
	 * 
	 * @param proc The specified process instance.
	 * @return <code>true</code> if the process completes execution, else <code>false</code>.
	 * @throws ProcessingException if an error occurs during execution.
	 */
	protected boolean runProcess(ProcessInstance proc) throws ProcessingException {
		ActivityUnit unit = proc.currentActivity();
		if (unit == null) {   // provisioning service
			// check to see if process is complete...
			if (proc.isComplete()) {
				Invocation srvc = proc.getProcessDef().getInvocation();
				String msg = 
						I18n.loc("TRANSL-5002: {0} cannot run completed process [srvc={1},op={2}]", 
								 getName(), 
								 srvc.getInfo().getServiceName(), 
								 srvc.getOperation().getName());
				log().info(msg);
				throw new ProcessingException(msg);
			}
			
			registerInvocationVariables(proc);
		}
		else {    // reply from Invoke activity
			// continue current activity processing before continuing to subsequent activities
			if (log().isLoggable(Level.FINER)) {
				log().finer("TRANSL-2002: "+ getName() +
							  " is continuing a process at activity: " +
							  String.valueOf(unit));
			}
			unit.execute(this);
		}			
		
		while ((unit = proc.nextActivity()) != null) {
			if (log().isLoggable(Level.FINER)) {
				log().finer("TRANSL-2003: "+ getName() +
							  " is executing activity: "+ String.valueOf(unit));
			}
			if (!unit.execute(this)) {
				if (log().isLoggable(Level.FINER)) {
					log().finer("TRANSL-2004: "+ getName() +
								  " is suspending process until activity completes..."
								  + String.valueOf(unit));
				}
				return false;
			}
		}

		return true;
	}
	
	/**
	 * Registers the input and output variables defined by an invocation.
	 * 
	 * @param unit The runtime invocation unit.
	 * @throws ProcessingException if an error occurs registering variables.
	 */
	protected void registerInvocationVariables(InvocationUnit unit) throws ProcessingException {
		Invocation invoke = null;
		Source src = null;
		
		try {
		    boolean provisioning = unit instanceof ProcessInstance;
			if (provisioning) {
				invoke = ((ProcessInstance) unit).getProcessDef().getInvocation();
				src = verifyContent(unit.getMessageExchange());
			}
			else {
				invoke = (Invocation) unit.getActivity();
				src = new DOMSource(XmlUtil.newDocument());	// request, no content, don't verify
			}

			getInvoker().registerInvocationVariables(unit.getVariableContext(),
			                                         invoke, 
			                                         src);
			if (provisioning) {
			    // initialize WSMessage var w/ NormalizedMessage
			    WSMessage wsm = (WSMessage)
			            unit.getVariableContext().getVariable(
			                    invoke.getInputVariable());
			    wsm.initMessage(
			            ExchangeUtil.getInMessage(unit.getMessageExchange()));
			}
		}
		catch (ProcessingException pe) {
		    // already logged, rethrow
		    throw pe;
		}
		catch (Exception e) {
			String msg = I18n.loc("TRANSL-6028: {0} failed to register invocation variables: {1}", 
					 			  getName(), e.getMessage());
			log().log(Level.WARNING, msg, e);
			throw new ProcessingException(e);
		}
	}
	
    protected Source verifyContent(MessageExchange me) throws ProcessingException {
    	NormalizedMessage msg = ExchangeUtil.getInMessage(me);
    	if(msg == null || msg.getContent() == null) {
    		String err = 
    				I18n.loc("TRANSL-6029: An {0} message exchange (id={1}) is missing normalized content!", 
    						 ExchangePattern.valueOf(me), me.getExchangeId());
    		log().warning(err);
    		throw new ProcessingException(err);
    	}
    	else {
    		return msg.getContent();
    	}
    }

    protected Logger log() {
    	return mLogger;
    }
    
    /** Fetches the cached name of this engine. */
    protected String getName() {
    	return mName;
    }
}
