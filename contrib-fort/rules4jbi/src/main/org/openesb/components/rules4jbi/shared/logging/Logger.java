/*
 * @(#)Logger.java        $Revision: 1.3 $ $Date: 2008/07/05 04:01:46 $
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

/**
 * Yet another logger abstraction with printf-style logging methods.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.3 $ $Date: 2008/07/05 04:01:46 $
 * 
 * @since 0.1
 */
public interface Logger {
    
    /**
     * Prefix used for the global logger.
     */
    String GLOBAL_PREFIX = "GLOBAL";
    
    /**
     * Prefix used for the normal jbi component.
     */
    String NORMAL_PREFIX = "RULES4JBI";
    
    /**
     * Prefix used for the test jbi component.
     */
    String TEST_PREFIX = "TEST4RULES";
    
    void severe(String msg);

    void severe(String msg, Object... args);

    void severe(String msg, Throwable thrown);

    void severe(String msg, Throwable thrown, Object... args);
    
    void warning(String msg);

    void warning(String msg, Object... args);

    void warning(String msg, Throwable thrown);

    void warning(String msg, Throwable thrown, Object... args);
    
    void info(String msg);

    void info(String msg, Object... args);

    void info(String msg, Throwable thrown);

    void info(String msg, Throwable thrown, Object... args);
    
    void config(String msg);

    void config(String msg, Object... args);

    void config(String msg, Throwable thrown);

    void config(String msg, Throwable thrown, Object... args);

    void fine(String msg);

    void fine(String msg, Object... args);

    void fine(String msg, Throwable thrown);

    void fine(String msg, Throwable thrown, Object... args);

    void finer(String msg);

    void finer(String msg, Object... args);

    void finer(String msg, Throwable thrown);

    void finer(String msg, Throwable thrown, Object... args);

    void finest(String msg);

    void finest(String msg, Object... args);

    void finest(String msg, Throwable thrown);

    void finest(String msg, Throwable thrown, Object... args);
    
    void entering(Class<?> sourceClass, String sourceMethod, Object... args);

    void exiting(Class<?> sourceClass, String sourceMethod);

    void important(String msg);
    
    void important(String msg, Object... args);
}
