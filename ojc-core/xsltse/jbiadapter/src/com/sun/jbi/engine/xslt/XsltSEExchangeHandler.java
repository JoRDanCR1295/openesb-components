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
 * @(#)AleSEExchangeHandler.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.xslt;

import java.util.logging.Level;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import com.sun.jbi.common.classloader.CustomClassLoaderUtil.SwitchType;
import com.sun.jbi.common.descriptor.ServiceUnit;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler;
import com.sun.transform.engine.EngineConfig;
import com.sun.transform.engine.EngineConfigurationError;
import com.sun.transform.engine.EngineConfig.TransformEngine;
import com.sun.transform.engine.runtime.Engine;
import com.sun.transform.engine.runtime.ProcessInstance;
import com.sun.transform.engine.runtime.ProcessingException;
import com.sun.transform.engine.runtime.impl.EngineExchangeHandler;

/**
 * Handles exchanges for XsltSE.
 * @author Kevan Simpson
 */
public class XsltSEExchangeHandler extends EngineExchangeHandler {
    /** Constructs an {@link ExchangeHandler} for XsltSE. */
    public XsltSEExchangeHandler(ComponentContext ctx) {
        super(ctx);
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractExchangeHandler#start() */
    @Override
    public void start() throws JBIException {
        super.start();

        log().config(I18n.loc(
                "XSLTSE-4003: Starting XsltSE with transformation engine: {0}", 
                getEngine().getClass().getName()));
    }

    /** @see com.sun.transform.engine.runtime.impl.EngineExchangeHandler#createEngine(java.lang.String) */
    protected Engine createEngine(String engineImpl) {
        Engine eng = null;
        EngineConfigurationError ece = null;
        
        if (!Util.isEmpty(engineImpl)) {// && !engineImpl.equals(XsltEngine.class.getName())) {
            try {
                TransformEngine te = TransformEngine.valueOf(engineImpl);
                EngineConfig engineConfig = EngineConfig.newInstance(te);
                eng = engineConfig.getEngine();
            }
            catch (Exception e) {
                ece = new EngineConfigurationError(e, I18n.loc(
                        "XSLTSE-6001: Failed to load Xslt Engine {0}: {1}", 
                        engineImpl, e.getMessage()));
                
            }
            catch (NoClassDefFoundError ncdfe) {
                // default to xslt 1.0 below
                ece = new EngineConfigurationError(ncdfe, I18n.loc(
                        "XSLTSE-6002: Failed to load Xslt Engine {0} as the class {1} could not be loaded!", 
                        engineImpl, ncdfe.getMessage()));
            }
        }

        if (ece != null) {
            log().log(Level.WARNING, ece.getMessage(), ece);
            throw ece;
        }
        
        return eng;
    }
    
    /** @see com.sun.transform.engine.runtime.impl.EngineExchangeHandler#process(com.sun.transform.engine.runtime.ProcessInstance) */
    protected void process(ProcessInstance proc) throws ProcessingException {
        ServiceUnit su = getContext().getEndpointManager()
                .lookupServiceUnit(proc.getMessageExchange().getEndpoint());
        // switch the thread context class-loader if there are any jar files in this service unit.
        // will be switched only if there are jar files.
        getContext().getCustomClassLoaderUtil()
                .switchClassLoader(su.getName(), SwitchType.service_classloader);
        try {
            proc.execute(getEngine());
        } 
        finally {
            // switch back to the the original thread context-class loader. 
            getContext().getCustomClassLoaderUtil()
                    .switchClassLoader(su.getName(), SwitchType.context_classloader);
        }
    }
}
