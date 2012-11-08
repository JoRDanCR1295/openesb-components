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
 * @(#)Invoker.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.runtime.impl;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Operation;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;

import org.w3c.dom.Document;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.messaging.ExchangeUtil;
import com.sun.jbi.common.qos.messaging.ExchangeUtil.FaultCode;
import com.sun.jbi.common.qos.redelivery.Redelivery;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.common.xml.XmlUtil;
import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;
import com.sun.jbi.component.toolkit.util.ExchangePattern;
import com.sun.transform.I18n;
import com.sun.transform.engine.model.Invocation;
import com.sun.transform.engine.model.Transform;
import com.sun.transform.engine.runtime.MessageUtil;
import com.sun.transform.engine.runtime.ProcessInstance;
import com.sun.transform.engine.runtime.ProcessingException;
import com.sun.transform.engine.runtime.VariableContext;
import com.sun.transform.engine.runtime.WSMessage;

/**
 * Invoker context.
 * @author Kevan Simpson
 */
public class Invoker<E> {
    private Logger mLogger;
    private ManagerContext mCtx;
    private Transform<E> mTransform;
    private ProcessInstance mProcess;
    private String mName;
    
    public Invoker(ManagerContext ctx) {
        this(ctx, null, null);
    }

    public Invoker(ManagerContext ctx, 
                   Transform<E> transform,
                   ProcessInstance process) {
        mCtx = ctx;
        mTransform = transform;
        mProcess = process;
        mName = this.getClass().getSimpleName();
        mLogger = Util.getLogger(ctx.getComponentContext(), Invoker.class.getName());
    }
    
    /**
     * Logs a status message describing the status of the specified message exchange.
     * @param msg The message exchange to log.
     * @return <code>true</code> if the status of the exchange is ACTIVE or DONE.
     */
    public static boolean logStatus(Logger log, ComponentContext ctx, MessageExchange msg) { 
        if (ExchangeStatus.ERROR.equals(msg.getStatus())) {
            logError(log, ctx, msg);
            return false;
        }
        else if (log.isLoggable(Level.FINER)) {   // DONE
            log.finer(I18n.loc(
                        "TRANSL-2006: Exchange({0}) is complete with DONE status.", 
                        msg.getExchangeId()));
        }
        return true;
    }
    
    public static void logError(Logger log, ComponentContext ctx, MessageExchange msg) {
        ServiceEndpoint endpt = msg.getEndpoint();
        log.warning(I18n.loc(
                "TRANSL-6043: Exchange({0}) terminated with ERROR status by {1}.\n"+
                    "Service: {2}, Endpoint: {3}, Operation: {4}\n"+
                    "Cause: {5} - {6}\nActor: {7}, Detail: {8}", 
                msg.getExchangeId(), ctx.getComponentName(),
                endpt.getServiceName(), endpt.getEndpointName(), 
                msg.getOperation().getLocalPart(),
                msg.getProperty(ExchangeUtil.FAULTCODE_PROPERTY_NAME),
                msg.getProperty(ExchangeUtil.FAULTSTRING_PROPERTY_NAME),
                msg.getProperty(ExchangeUtil.FAULTACTOR_PROPERTY_NAME),
                msg.getProperty(ExchangeUtil.FAULTDETAIL_PROPERTY_NAME)));
    }

    protected ManagerContext getContext() {
        return mCtx;
    }
    
    public ExchangeStatus completeInOnly(ProcessInstance proc,
                                         InOnly response,
                                         Invocation invoke) {
        ExchangeStatus status = response.getStatus();
        // invoke activity completed, set status variable
        if (!Util.isEmpty(invoke.getOutputVariable())) {
            proc.getVariableContext().setVariable(
                    invoke.getOutputVariable(), status);
        }
        
        return status;
    }
    
    /**
     * Completes an {@link InOut} exchange by updating the variable context
     * with the OUT message and sending a status response to the provisioner.
     * 
     * @param proc The process instance.
     * @param inOut The reply from an InOut service.
     * @param invoke Definition of the invoked service.
     * @throws ProcessingException if the response cannot be handled.
     */
    public void completeInOut(ProcessInstance proc, 
                              InOut inOut, 
                              Invocation invoke) throws ProcessingException {
        try {
            // if ERROR, exchange is terminated... log
            if (inOut.getStatus().equals(ExchangeStatus.ERROR)) {
               logError(log(), getContext().getComponentContext(), inOut);
               throw error(inOut.getError(), I18n.loc(
                       "TRANSL-6058: Received ERROR status from ServiceEndpoint[{0}-{1}]", 
                       invoke.getInfo().getServiceName(), 
                       invoke.getInfo().getEndpointName()));
            }
            
            // variable context will unwrap message wrapper and assign part values
            DOMSource response = XmlUtil.toDOMSource(inOut.getOutMessage().getContent());
            proc.getVariableContext().setVariable(invoke.getOutputVariable(), 
                                                  ((Document) response.getNode()).getDocumentElement());
            inOut.setStatus(ExchangeStatus.DONE);
            getContext().getMessagingChannel().send(inOut);
        }
        catch (ProcessingException pe) {
            throw pe;   // we want this to bubble up...
        }
        catch (Exception e) {
            try {
                ProcessingException pex = 
                        error(e, I18n.loc("TRANSL-6034: {0} failed to reply to ME (id={1}): {2}", 
                                          getName(), inOut.getExchangeId(), e.getMessage()));
                
                // reply with error status to complete ME
                ExchangeUtil.setErrorData(inOut, 
                                          pex.getMessage(), 
                                          FaultCode.Server, 
                                          e.getMessage(), 
                                          getContext().getComponentContext().getComponentName());
                inOut.setError(pex);
                getContext().getMessagingChannel().send(inOut);
            }
            catch (Exception foobar) {
                throw error(foobar, I18n.loc(
                        "TRANSL-6035: {0} failed to send ERROR status on ME (id={1}): {2}",  
                        getName(), inOut.getExchangeId(), foobar.getMessage()));
            }
        }
    }

    public MessageExchange createExchange(EndpointInfo info, Operation op)
            throws ProcessingException, JBIException {
        ExchangePattern ptrn = identifyPattern(op);
        MessageExchange msg = null;
        MessageExchangeFactory factory = getContext().getMessagingChannel().getExchangeFactory(); 
        switch (ptrn) {
            case IN_ONLY: {
                msg = factory.createInOnlyExchange();
                break;
            }
            case IN_OUT: {
                msg = factory.createInOutExchange();
                break;
            }
            default: {
                // TODO throw unsupported error
                return null;
            }
        }
        
        ServiceEndpoint srvcEndpoint = 
                getContext().getComponentContext().getEndpoint(
                        info.getServiceName(),
                        info.getEndpointName());
        msg.setEndpoint(srvcEndpoint);
        msg.setOperation(new QName(
                info.getInterfaceName().getNamespaceURI(), op.getName()));
        return msg;
    }

    public ProcessInstance getProcess() {
        return mProcess;
    }
    
    /**
     * Fetches an {@link Invocation} by name.
     * @param name The invocation's name.
     * @return an <code>Invocation</code> or <code>null</code>.
     */
    public Invocation lookupInvoke(String name) {
        return mTransform.getInvocations().get(name);
    }
    
    /**
     * Propogates any security credentials and transaction information 
     * from one message exchange to another.
     * 
     * @param from the incoming message
     * @param to the outgoing message
     */
    public void propagateSystemics(MessageExchange from, MessageExchange to) {
        boolean ok = ExchangeUtil.propagateSystemics(from, to);
        if (mLogger.isLoggable(Level.FINEST)) {
            mLogger.finest("TRANSL-1002: Propogation of security/transaction from Exchange("+
                           ((from == null) ? "null" : from.getExchangeId()) +") to Exchange("+
                           ((to == null) ? "null" : to.getExchangeId()) +") "+
                           ((ok) ? "completed successfully!" : "failed!"));
        }
    }

    public void registerInvocationVariables(VariableContext varCtx, 
                                            Invocation invoke, 
                                            Source src) throws ProcessingException {
        if (log().isLoggable(Level.FINE)) {
            log().fine("TRANSL-3004: Registering invocation variables: in="+
                       invoke.getInputVariable() +", out="+ invoke.getOutputVariable());
        }
  
        try {
            Operation op = invoke.getOperation();
            // set input variable
            WSMessage input = MessageUtil.createWSMessage(
                    src, op.getInput().getMessage());
            varCtx.setVariable(invoke.getInputVariable(), input);
            // set output variable, if one exists
            if (!Util.isEmpty(invoke.getOutputVariable()) && op.getOutput() != null) {
                WSMessage output = MessageUtil.createWSMessage(
                        new DOMSource(XmlUtil.newDocument()), 
                        op.getOutput().getMessage());
                varCtx.setVariable(invoke.getOutputVariable(), output);
            }
        }
        catch (Exception e) {
            String msg = I18n.loc("TRANSL-6028: {0} failed to register invocation variables: {1}", 
                                  getName(), e.getMessage());
            log().log(Level.WARNING, msg, e);
            throw new ProcessingException(e);
        }
    }
    
    public boolean sendSync(MessageExchange msg) throws MessagingException {
        return getContext().getMessagingChannel().sendSync(msg);
    }
    
    public void setInput(MessageExchange msg, WSMessage wsm) throws MessagingException {
        Source input = wsm.toSource();
        if (msg != null && input != null) {
            NormalizedMessage nm = msg.createMessage();
            wsm.initMessage(nm);
            nm.setContent(input);
            ExchangeUtil.setInMessage(msg, nm);
        }
    }
    
    public void setUniqueId(MessageExchange mex, 
                            String invokeName, 
                            MessageExchange parent) {
        // generate message id for Redelivery and WS-Reliability support
        QName qualifiedInvoke = new QName(
                mex.getEndpoint().getServiceName().getNamespaceURI(), invokeName);
        String msgId = String.valueOf(qualifiedInvoke) +"-";
        String incomingMsgId = Redelivery.getUniqueId(parent);
        msgId += (Util.isEmpty(incomingMsgId)) 
                ? parent.getExchangeId() : incomingMsgId;
        Redelivery.setUniqueId(mex, msgId);
    }
    
    protected ExchangePattern identifyPattern(Operation op) throws ProcessingException {
        ExchangePattern ptrn = ExchangePattern.identifyPattern(op);

        if (ptrn == null) {
            throw error(null, I18n.loc("TRANSL-6036: Failed to identify ME pattern for operation: {0}", 
                                       String.valueOf(op)));
        }
        
        return ptrn;
    }

    private String getName() {
        return mName;
    }

    private Logger log() {
        return mLogger;
    }
    
    private ProcessingException error(Exception cause, String msg) {
        if (cause == null) {
            mLogger.warning(msg);
            return new ProcessingException(msg);
        }
        else {
            mLogger.log(Level.WARNING, msg, cause);
            if (cause instanceof ProcessingException) {
                return (ProcessingException) cause;
            }
            else {
                return new ProcessingException(msg, cause);
            }
        }
    }
}
