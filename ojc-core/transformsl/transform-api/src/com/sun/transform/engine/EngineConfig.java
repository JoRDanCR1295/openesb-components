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
 * @(#)EngineConfig.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine;

import java.util.logging.Level;
import java.util.logging.Logger;
import com.sun.jbi.common.util.Util;
import com.sun.transform.api.I18n;
import com.sun.transform.engine.model.ProcessFactory;
import com.sun.transform.engine.runtime.Engine;


/**
 * 
 * @author Kevan Simpson
 */
public abstract class EngineConfig {
    public static final String TRANSFORM_ENGINE_PROPERTY = "TransformEngine";

    public enum TransformEngine {
        XSLT_1_0("com.sun.transform.engine.xslt.XsltConfig"), 
        XSLT_2_0("com.sun.transform.engine.xslt.saxon.SaxonConfig");
        
        String mImplClazz;
        
        TransformEngine(String clz) {
            mImplClazz = clz;
        }
        
        public String getImplementation() {
            return mImplClazz;
        }
    }

    private static Logger mLogger = Logger.getLogger(EngineConfig.class.getName());

    /**
     * Returns a factory to create transformmap model constructs.
     * @return a factory to create transformmap model constructs.
     */
    public abstract ProcessFactory getProcessFactory();
    
    /**
     * Returns an engine to execute transformmap activities.
     * @return an engine to execute transformmap activities.
     */
    public abstract Engine getEngine();

    public static EngineConfig newInstance(TransformEngine config) throws EngineConfigurationError {
        if (config == null) {
            throw error(null, I18n.loc(""));
        }
        
        return newInstance(config.getImplementation());
    }

    protected static EngineConfig newInstance(String config) throws EngineConfigurationError {
        try {
            return (EngineConfig) Util.find(
            /* The default property name as defined in this class */
            "com.sun.transform.engine.EngineConfig",
            /* The fallback implementation class name, which supports Xslt 1.0 */
            (Util.isEmpty(config)) ? TransformEngine.XSLT_1_0.getImplementation() 
                                   : config, // or the specified classname
            /* Pass this class to make its classloader available, if needed */
            EngineConfig.class);
        } 
        catch (EngineConfigurationError e) {
            throw error(e, I18n.loc(
                    "TRAPI-6001: Failed to create {0} instance: {1}", 
                    "EngineConfig", e.getMessage()));
        }
        catch (Exception e) {
            throw error(e, I18n.loc(
                    "TRAPI-6001: Failed to create {0} instance: {1}", 
                    "EngineConfig", e.getMessage()));
        }
    }

    protected static EngineConfigurationError error(Throwable thrown, String msg) {
        if (thrown == null) {
            mLogger.warning(msg);
        }
        else {
            mLogger.log(Level.WARNING, msg, thrown);
        }
        
        if (thrown instanceof EngineConfigurationError) {
            return (EngineConfigurationError) thrown;
        }
        else if (thrown instanceof Exception) {
            return new EngineConfigurationError((Exception) thrown, msg);
        }
        
        return new EngineConfigurationError(msg);
    }
}
