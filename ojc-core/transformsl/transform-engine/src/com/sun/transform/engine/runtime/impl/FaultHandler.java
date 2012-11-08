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
 * @(#)FaultHandler.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.runtime.impl;

import java.util.logging.Level;
import javax.jbi.JBIException;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import com.sun.jbi.common.qos.messaging.ExchangeUtil;
import com.sun.jbi.common.qos.messaging.WrapperUtil;
import com.sun.jbi.common.qos.messaging.ExchangeUtil.FaultCode;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.common.xml.XmlUtil;
import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;
import com.sun.jbi.component.toolkit.lifecycle.impl.AbstractExchangeHandler;
import com.sun.transform.I18n;
import com.sun.transform.engine.model.Invocation;
import com.sun.transform.engine.model.Transform;
import com.sun.transform.engine.runtime.ActivityUnit;
import com.sun.transform.engine.runtime.Engine;
import com.sun.transform.engine.runtime.InvocationUnit;
import com.sun.transform.engine.runtime.ProcessInstance;
import com.sun.transform.engine.runtime.TransformUnit;

/**
 * Handles the case when an invocation replies with a {@link Fault}.
 * @author Kevan Simpson
 */
public class FaultHandler extends AbstractExchangeHandler {
    private Fault mFault;
    private Element mElement;
    private String mName;
//    private QName mType;
    private Engine mEngine;
    
    public FaultHandler(ManagerContext ctx, Engine eng) {
        super(ctx.getComponentContext());
        setContext(ctx);
        mEngine = eng;
    }

    
    /** @see com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler#handleExchange(javax.jbi.messaging.MessageExchange) */
    public void handleExchange(MessageExchange msg) throws JBIException {
        // lookup process awaiting InOut response + decorrelate
        ProcessInstance proc = (ProcessInstance) 
                getContext().getCorrelationMap().remove(msg.getExchangeId());
        Fault fault = msg.getFault();

        try {
            MessageExchange reply = proc.getMessageExchange();
            Invocation invoke = proc.getProcessDef().getInvocation();
            readFault(msg); // looks for name and type attributes in fault content
            javax.wsdl.Fault faultDef = getDefinedFault(proc);
            
            // has user configured Transform to apply to fault content?
            TransformUnit unit = lookupTransform(proc);
            if (unit != null) {
                // variable context will unwrap message wrapper and assign part values
                proc.getVariableContext().setVariable(
                        ((Transform) unit.getActivity()).getSource(),
                        getFaultMessage());
                if (log().isLoggable(Level.FINE)) {
                    log().fine("TRANSL-3002: Applying transformation "+
                            unit.getActivity().getName() +" to fault received from "+
                            String.valueOf(invoke.getInfo()));
                }
                unit.execute(getEngine());
                // if implemented op has no fault defined...
                // then take type from invocation's fault reply...must have type!
                QName type = (faultDef == null)
                        ? WrapperUtil.getMessageType(msg.getFault())
                        : faultDef.getMessage().getQName();
                Element fmsg = (Element) proc.getVariableContext().getVariable(
                                ((Transform) unit.getActivity()).getResult());
                fault = createFault(reply, fmsg, type);
            }
            
            // if fault is defined on this operation, propagate fault
            if (faultDef != null) {
                if (log().isLoggable(Level.FINE)) {
                    log().fine("TRANSL-3005: Propagating fault from exchange "+ msg.getExchangeId()
                               +" to "+ reply.getExchangeId());
                    if (log().isLoggable(Level.FINEST)) {
                        log().finest("TRANSL-1006: Fault content: "+ 
                                     XmlUtil.print(fault.getContent()));
                    }
                }

                reply.setFault(fault);
                send(reply);
            }
            else {  // propagate fault as ERROR
                propagateFaultAsError(msg, proc, fault);
            }
        }
        catch (JBIException je) {
            // problem sending, don't retry...
            throw je;
        }
        catch (Exception e) {
            log().log(Level.WARNING, I18n.loc(
                    "TRANSL-6060: Failed to handle fault for message exchange {0}: {1}", 
                    msg.getExchangeId(), e.getMessage()), e);
            propagateFaultAsError(msg, proc, fault);
        }
    }

    protected Engine getEngine() {
        return mEngine;
    }
    
    /**
     * Returns the fault message payload.
     * @return the fault message payload.
     */
    protected Element getFaultMessage() {
        return mElement;
    }

    /**
     * Returns the fault name.
     * @return the fault name.
     */
    protected String getFaultName() {
        return mName;
    }

    /**
     * Returns the wsdl-defined fault, if one exists, for the received fault message.
     * @param proc A transformation process.
     * @return the wsdl-defined fault or <code>null</code>.
     */
    protected javax.wsdl.Fault getDefinedFault(ProcessInstance proc) {
        return (proc == null || getFaultName() == null) ? null 
                : proc.getProcessDef().getInvocation().getOperation().getFault(getFaultName());
    }
    
    /**
     * Returns an executable {@link TransformUnit} if a {@link Transform} 
     * activity is defined on the specified process' current activity.
     * 
     * @param proc A process instance.
     * @return A <code>TransformUnit</code> or <code>null</code>.
     */
    protected TransformUnit lookupTransform(ProcessInstance proc) {
        ActivityUnit unit = (proc == null) ? null : proc.currentActivity();
        if (unit instanceof InvocationUnit) {
            InvocationUnit invoke = (InvocationUnit) unit;
            Transform tr = ((Invocation) invoke.getActivity()).getTransform(mName);
            if (tr != null) {
                return (TransformUnit) proc.createUnit(tr);
            }
        }
        
        return null;
    }
    
    protected void propagateFaultAsError(MessageExchange msg, ProcessInstance proc, Fault fault) throws JBIException {
        ServiceEndpoint sept = msg.getEndpoint();
        Element[] parts = null;
        try {
            parts = ExchangeUtil.extractParts(fault);
        }
        catch (Exception e) {
            log().log(Level.WARNING, I18n.loc(
                    "TRANSL-6061: Failed to extract fault parts during propagation for {0}: {1}",
                    String.valueOf(sept), e.getMessage()), e);
        }
        
        Source content = (parts != null && parts.length > 0) 
                ? new DOMSource((Element) parts[0].getFirstChild()) 
                : fault.getContent();;
        String err = I18n.loc(
                "TRANSL-6044: Propagating fault as ERROR, received from service \"{0}-{1}\"", 
                sept.getServiceName(), sept.getEndpointName()),
               detail = (fault == null) ? I18n.loc(
                       "TRANSL-5004: Please review logs for cause of propagated fault.")
                       : XmlUtil.print(content);
        log().warning(err);
        if (log().isLoggable(Level.FINE)) {
            log().fine("TRANSL-3001: Propagated fault as ERROR, content: "+ 
                    XmlUtil.print(content));
        }
        MessageExchange reply = proc.getMessageExchange();
        ExchangeUtil.setErrorData(reply, err, FaultCode.Server, detail, getComponentName());
        sendError(reply, new Exception(err));
    }

    protected void readFault(MessageExchange mex) throws Exception {
        if (mex != null) {
            mFault = mex.getFault();
            if (mFault == null) {
                return;
            }
            
            Element[] parts = ExchangeUtil.extractParts(mex.getFault());
            mElement = (parts != null && parts.length > 0) 
                    ? (Element) parts[0].getFirstChild() : null;
            boolean finer = log().isLoggable(Level.FINER);
            if (mElement != null) {
                mName = mElement.getAttribute("name");
//              String type = mElement.getAttribute("type");
//              mType = (Util.isEmpty(type)) ? null : QName.valueOf(type);
                if (finer) {
                    log().finer(I18n.format(
                            "TRANSL-2007: Identified fault element with name: {0}", 
                            mName));
                }
            }
            
            if (finer && Util.isEmpty(mName)) {
                log().finer(I18n.format(
                        "TRANSL-2011: No fault name identified from {0} parts", 
                        String.valueOf(parts.length)));
                log().finest(I18n.format(
                        "TRANSL-1007: Fault content without name attribute: {0}", 
                        XmlUtil.print(mex.getFault().getContent())));
            }
        }
    }
    
    public static Fault createFault(MessageExchange reply, Element part, QName type) 
            throws Exception {
        // create new fault with transformed message
        Document doc = XmlUtil.newDocument();
        Element wrapper = WrapperUtil.createJBIMessageWrapper(doc, type, null);
        wrapper.appendChild(WrapperUtil.importJBIWrappedPart(doc, part));
        Fault fault = reply.createFault();
        fault.setContent(new DOMSource(doc));
        return fault;
    }
}
