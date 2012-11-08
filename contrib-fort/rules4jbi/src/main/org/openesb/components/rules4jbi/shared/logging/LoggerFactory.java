/*
 * @(#)LoggerFactory.java        $Revision: 1.2 $ $Date: 2008/07/05 04:01:46 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.shared.logging;

import java.util.HashMap;
import java.util.Map;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;

import net.jcip.annotations.GuardedBy;
import net.jcip.annotations.ThreadSafe;

/**
 * Factory to create loggers. It is a singleton, only one instance of it exists
 * under each classloader. Note that the classloaders used during installation are
 * different from the classloaders used during normal component runtime.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/07/05 04:01:46 $
 * 
 * @since 0.1
 */
@ThreadSafe
//@Deprecated
public final class LoggerFactory {
    
    /*
     * This class is now effectively deprecated. We use DI to inject loggers into production classes,
     * that are used inside the JBI environment. Right now, only the functional test classes make use
     * of this class.
     */

    /**
     * The only allowed instance of this class.
     */
    private static final LoggerFactory INSTANCE = new LoggerFactory();
    
    /**
     * Cached instance of <code>ComponentContext</code> used to produce loggers.
     */
    @GuardedBy("this")
    private ComponentContext componentContext = null;
    
    /**
     * Prefix used for all loggers produced by this factory.
     */
    @GuardedBy("this")
    private String prefix = null;
    
    /**
     * Indicates whether this factory was initialized.
     * @see #init()
     */
    @GuardedBy("this")
    private boolean initialized = false;
    
    /**
     * Global logger that we gracefully degrade to in case we cannot obtain
     * a component specific logger.
     */
    private final Logger globalLogger;
    
    /**
     * Map of existing loggers. By convention, each class has assigned its own logger,
     * to enable fine graned control of logging from this project.
     */
    @GuardedBy("this")
    private final Map<Class<?>, Logger> loggers;

    /**
     * Don't allow instantiation of this class.
     */
    private LoggerFactory() {
        globalLogger = new LoggerImpl(Logger.GLOBAL_PREFIX,
                java.util.logging.Logger.getLogger(java.util.logging.Logger.GLOBAL_LOGGER_NAME));

        loggers = new HashMap<Class<?>, Logger>();
        
        globalLogger.fine("Logger factory created");
    }

    /**
     * Static factory; returns the only allowed instance of this class.
     * 
     * @return the only allowed instance of this class.
     */
    public static LoggerFactory getInstance() {
        return INSTANCE;
    }

    /**
     * Initializes this factory. 
     * 
     * @param prefix prefix used for all loggers created by this factory.
     * @param componentContext component context used to retrieve loggers for this jbi component
     *        from the jbi runtime.
     */
    public synchronized void init(String prefix, ComponentContext componentContext) {
        if (componentContext == null) {
            throw new NullPointerException("Component context cannot be null");
        }
        
        if (prefix == null || prefix.trim().equals("")) {
            throw new IllegalArgumentException("Invalid prefix: " + prefix);
        }
        
        this.prefix = prefix;
        this.componentContext = componentContext;
        
        initialized = true;
        
        getLogger(LoggerFactory.class).fine("ComponentContext set: %s", componentContext.toString());
        getLogger(LoggerFactory.class).exiting(LoggerFactory.class, "init");
    }

    /**
     * Returns logger associated with the class clazz.
     * 
     * @param clazz the class for which to return logger.
     * @return logger associated with the given class.
     * @throws NullPointerException if clazz is null.
     */
    public synchronized Logger getLogger(Class<?> clazz) {
        if (clazz == null) {
            throw new NullPointerException("Class may not be null");
        }

        if (initialized) {
            Logger logger = null;

            if ((logger = loggers.get(clazz)) == null) {
                try {
                    logger = new LoggerImpl(prefix, componentContext.getLogger(clazz.getName(), null));
                    loggers.put(clazz, logger);

                } catch (JBIException e) {

                    // we don't use resource bundles, so it shouldn't happen.
                    throw new AssertionError(e);
                }
            }

            return logger;
        }

        return globalLogger;
    }
}
