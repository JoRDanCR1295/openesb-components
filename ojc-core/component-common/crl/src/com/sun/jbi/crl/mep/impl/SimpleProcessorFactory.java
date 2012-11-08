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
 * @(#)SimpleProcessorFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.impl;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;

import com.sun.jbi.crl.mep.Callback;
import com.sun.jbi.crl.mep.proc.Processor;
import com.sun.jbi.crl.mep.proc.ProcessorFactory;
import com.sun.jbi.crl.util.I18n;

/**
 * Multi-purpose {@link ProcessorFactory} implementation.
 * <p>
 * There are four use cases for providing {@link Processor} and/or
 * {@link Callback} instances:
 * <dl>
 *      <dt>Stateless <code>Processor</code>, no <code>Callback</code><dt>
 *      <dd>Use {@link SimpleProcessorFactory#SimpleProcessorFactory(Processor)}</dd>
 *      <dt>Stateless <code>Processor</code>, stateful <code>Callback</code><dt>
 *      <dd>Use {@link SimpleProcessorFactory#SimpleProcessorFactory(Processor, Class)}</dd>
 *      <dt>Stateful <code>Processor</code>, no <code>Callback</code><dt>
 *      <dd>Use {@link SimpleProcessorFactory#SimpleProcessorFactory(Class)}</dd>
 *      <dt>Stateful <code>Processor</code>, stateful <code>Callback</code></dt>
 *      <dd>Use {@link SimpleProcessorFactory#SimpleProcessorFactory(Class, Class)}</dd>
 * </dl>
 * <p>
 * <b>Note 1:</b> Stateless <code>Processor</code> implementations are expected
 * to be thread-safe, whereas stateful implementations are not.
 * <b>Note 2:</b> Stateless <code>Callback</code> implementations are expected
 * to be thread-safe, whereas stateful implementations are not.
 * 
 * @author Kevan Simpson
 */
public class SimpleProcessorFactory implements ProcessorFactory {
    private static final Logger mLogger = Logger.getLogger(SimpleProcessorFactory.class.getName());
    
    private NewInstanceFactory mProcFactory = null, mCbackFactory = null;
    private Processor mProcessor = null;
    
    public SimpleProcessorFactory(Processor proc) {
        setProcessorInstance(proc);
    }
    public SimpleProcessorFactory(Processor proc, Class cbackType) {
        this(proc);
        initCallbackFactory(cbackType);
    }
    
    public SimpleProcessorFactory(Class procType) {
        initProcessorFactory(procType);
    }
    public SimpleProcessorFactory(Class procType, Class cbackType) {
        this(procType);
        initCallbackFactory(cbackType);
    }
    
    /** @see com.sun.jbi.crl.mep.proc.ProcessorFactory#getProcessor() */
    public Processor getProcessor() throws JBIException {
        Processor proc = getProcessorInstance();
        if (proc == null) {
            final Class type = getProcessorFactory().getType();
            try {
                return (Processor) getProcessorFactory().newInstance(type);
            }
            catch (Exception e) {
                String msg = I18n.loc("CRL-6046: Failed to instantiate processor (type={0}): {1}", 
                					  String.valueOf(type), e.getMessage());
                mLogger.log(Level.WARNING, msg, e);
                throw new JBIException(msg, e);
            }
        }
        return proc;
    }
    
    /** @see com.sun.jbi.crl.mep.proc.ProcessorFactory#getCallback() */
    public Callback getCallback() throws JBIException {
        NewInstanceFactory fac = getCallbackFactory();
        if (fac != null) {
            final Class type = fac.getType();
            try {
                return (Callback) fac.newInstance(type);
            }
            catch (Exception e) {
                String msg = I18n.loc("CRL-6047: Failed to instantiate callback (type={0}): {1}", 
                		              String.valueOf(type), e.getMessage());
                mLogger.log(Level.WARNING, msg, e);
                throw new JBIException(msg, e);
            }
        }
        return null;
    }

    protected NewInstanceFactory getCallbackFactory() {
        return mCbackFactory;
    }
    protected Processor getProcessorInstance() {
        return mProcessor;
    }
    protected NewInstanceFactory getProcessorFactory() {
        return mProcFactory;
    }
    
    protected void initCallbackFactory(Class cbackType) {
        if (cbackType != null) {
            mCbackFactory = new NewInstanceFactory(cbackType);
        }
    }
    protected void setProcessorInstance(Processor proc) {
        mProcessor = proc;
    }
    protected void initProcessorFactory(Class procType) {
        if (procType != null) {
            mProcFactory = new NewInstanceFactory(procType);
        }
    }
}
