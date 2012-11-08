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
 * @(#)AbstractTransformEngine.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.xslt;

import java.util.logging.Level;
import javax.jbi.messaging.ExchangeStatus;
import javax.xml.transform.dom.DOMSource;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import com.sun.jbi.common.xml.XmlUtil;
import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;
import com.sun.transform.I18n;
import com.sun.transform.engine.model.Param;
import com.sun.transform.engine.model.Transform;
import com.sun.transform.engine.runtime.ProcessingException;
import com.sun.transform.engine.runtime.TransformUnit;
import com.sun.transform.engine.runtime.VariableContext;
import com.sun.transform.engine.runtime.WSMessage;
import com.sun.transform.engine.runtime.VariableContext.VariableType;
import com.sun.transform.engine.runtime.impl.Invoker;
import com.sun.transform.engine.runtime.impl.JbiEngine;

/**
 * Multi-templated base type for transformation processors.
 * <p>
 * Templated-type descriptions:
 * <ul>
 *      <li>E - The actual transformer, such as {@link javax.xml.transform.Transformer}</li>
 *      <li>T - A custom {@link Transform} implementation.</li>
 *      <li>S - The transformation source.</li>
 *      <li>D - The transformation destination.</li>
 * </ul>
 * 
 * @author Kevan Simpson
 */
public abstract class AbstractTransformEngine<E, T extends Transform<E>> 
        extends JbiEngine {
    /**
     * 
     */
    public AbstractTransformEngine() {
    }

    /**
     * @param ctx
     */
    public AbstractTransformEngine(ManagerContext ctx) {
        super(ctx);
    }

    protected abstract void setParameter(E xsl, String param, Object value) 
            throws ProcessingException;
    
    protected abstract Element transform(E xsl, DOMSource src) 
            throws ProcessingException;
    
    /** @see com.sun.transform.engine.runtime.Engine#process(com.sun.transform.engine.runtime.TransformUnit) */
    public boolean process(TransformUnit tr) throws ProcessingException {
        if (tr.getActivity() instanceof Transform) {
            return transform((T) tr.getActivity(), tr);
        }
        
        log().warning(I18n.loc(
                "TRANSL-6037: {0} skipped processing non-XSLT transform activity: {1}", 
                this.getClass().getSimpleName(), tr.getActivity().getClass().getName()));
        return false;
    }

    /**
     * Processes an XSLT transformation activity.
     * 
     * @param tr The transformation definition.
     * @param unit The runtime transformation activity.
     * @return <code>true</code> if the processing completes, else <code>false</code>.
     * @throws ProcessingException if an error occurs during processing.
     */
    protected boolean transform(T tr, TransformUnit unit) throws ProcessingException {
        try {
            VariableContext ctx = unit.getVariableContext();
            Object var = ctx.getVariable(tr.getSource());
            VariableType type = ctx.getType(tr.getSource());
            DOMSource payload = null;//
            switch (type) {
                case NODE: {
                    payload = new DOMSource((Node) var);
                    break;
                }
                case MESSAGE: { // likely because entry is transformJBI
                    payload = XmlUtil.toDOMSource(((WSMessage) var).toSource());
                    break;
                }
                default: {
                    // I've decided in my XsltSE divinity to grant empty transform activities
                    if (log().isLoggable(Level.FINER)) {
                        log().finer("TRANSL-2005: Aborting transformation - cannot transform input of type "
                                      + String.valueOf(type) +" (name="+ 
                                      tr.getSource() +",value="+ String.valueOf(var) +")");
                    }
                    return true;
                }
            }

            E xsl = tr.newTransformer();
            Object pVal = null;
            for (Param p : tr.getParams()) {
                switch (p.getType()) {
                    case PART: {
                        pVal = ctx.getVariable(String.valueOf(p.getValue()));
                        if (pVal instanceof ExchangeStatus) {
                            pVal = String.valueOf(pVal);
                        }
                        break;
                    }
                    default: {
                        pVal = p.getValue();
                        break;
                    }
                }
                
                if (log().isLoggable(Level.FINEST)) {
                    log().finest("TRANSL-1003: Setting param \""+ p.getName()
                                   +"\" with value: "+ String.valueOf(pVal));
                }
                setParameter(xsl, p.getName(), pVal);
            }

            if (log().isLoggable(Level.FINEST)) {
                log().finest("TRANSL-1004: Source of transformation: "+
                               XmlUtil.print(payload));
            }

            // always set Invoker
            Invoker<E> invoker = new Invoker<E>(
                    getContext(), tr, unit.getEnclosingProcess());
            
            if (log().isLoggable(Level.FINER)) {
                log().finer("TRANSL-2010: Setting Invoker on "+ tr.getName());
            }
            setParameter(xsl, "xsltse", invoker);
            
            // transform payload and set variable ref
            Element elem = transform(xsl, payload);
            ctx.setVariable(tr.getResult(), elem);

            if (log().isLoggable(Level.FINEST)) {
                log().finest("TRANSL-1005: Result of transformation: "+
                               XmlUtil.print(new DOMSource(elem)));
            }

            // validate result
            // KPS: not implemented...will return true
            return validate(unit);
        }
        catch (ProcessingException pe) {
            // will have already been logged...just rethrow
            throw pe;
        }
        catch (Exception e) {
            String msg = I18n.loc(
                    "TRANSL-6039: An error occurred processing transform activity: {0}", 
                    e.getMessage());
            log().log(Level.WARNING, msg, e);
            throw new ProcessingException(msg, e);
        }
    }

    protected ProcessingException transformError(Exception e) {
        String msg = I18n.loc(    
                "TRANSL-6038: Xslt transformation failed: {0}", e.getMessage());
        log().log(Level.WARNING, msg, e);
        return new ProcessingException(msg, e);
    }
}
