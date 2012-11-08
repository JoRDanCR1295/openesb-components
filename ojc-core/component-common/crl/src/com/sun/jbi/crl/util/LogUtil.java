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
 * @(#)LogUtil.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.util;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;

/**
 * Logging utility which caches {@link Logger} instances
 * retrieved from a component's context.
 * 
 * @author Kevan Simpson
 */
public class LogUtil {
	// don't cache for now
//    private static EntryRegistry<String, Logger> mLogRegistry =
//            new EntryRegistry<String, Logger>();
    
    private LogUtil() {
    }

    /**
     * Acquires a Logger from the specified {@link ComponentContext}.
     * <p>
     * If a Logger cannot be acquired from the context, a standard
     * Logger will be returned.
     * 
     * @param ctx a component's context.
     * @param logger the logger name.
     * @return a <code>Logger</code> or <code>null</code> if the context is <code>null</code>.
     */
    public static Logger getLogger(ComponentContext ctx, String logger) {
    	try {
        	return (ctx == null) ? null : ctx.getLogger(logger, null);
    	}
    	catch (JBIException jbi) {	
    		// failed to acquire Logger
    		Logger log = Logger.getLogger(logger);
    		if (log.isLoggable(Level.FINE)) {
    			log.fine("CRL-3023: Failed to acquire Logger from "+ 
    					 ctx.getComponentName() +" component context:"+ jbi.getMessage());
    		}
    		return log;
    	}
    }

    /**
     * Acquires a {@link Logger} by the specified class' fully-qualified name.
     *
     * @deprecated
     * 		Use {@link Logger#getLogger(String)}.
     * @param source The class for which to acquire a Logger.
     * @return a Logger.
     */
    @Deprecated  public static Logger getLogger(Class source) {
    	return (source == null) ? null : Logger.getLogger(source.getName());
//        Logger log = mLogRegistry.lookup(source.getName());
//        if (log == null) {
//        	// TODO make Loggers CCA
//            log = Logger.getLogger(source.getName());//Messages.getLogger(source);
//            mLogRegistry.register(source.getName(), log);
//        }
//        return log;
    }
}
