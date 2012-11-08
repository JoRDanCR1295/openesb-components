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
 * @(#)Utils.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc;

/**
 * Utility methods for the SAPBC
 *
 * @author jknight
 */
public class Utils {
    
    /**
     * Check if a message of the given level would actually be logged by this logger before creating the log message
     *
     * @param         logger      Logger to be used
     * @param         level       Level for used for logging
     * @param         message     Message to be logged
     *
     */
    public static void checkLog(java.util.logging.Logger logger, java.util.logging.Level level, String message)  {
        if (logger.isLoggable(level)) {
            logger.log(level, message);
        }
        
    }
    
    /**
     * Check if a message of the given level would actually be logged by this logger before creating the log message
     *
     * @param         logger      Logger to be used
     * @param         level       Level for used for logging
     * @param         message     Message to be logged
     * @param         params      Parameter to the message
     */
    public static void checkLog(java.util.logging.Logger logger, java.util.logging.Level level, String message, Object params)  {
        if (logger.isLoggable(level)) {
            logger.log(level, message, params);
        }
        
    }
    
    /**
     * Check if a message of the given level would actually be logged by this logger before creating the log message
     *
     * @param         logger      Logger to be used
     * @param         level       Level for used for logging
     * @param         message     Message to be logged
     * @param         params      Parameter to the message
     */
    public static void checkLog(java.util.logging.Logger logger, java.util.logging.Level level, String message, Object[] params)  {
        if (logger.isLoggable(level)) {
            logger.log(level, message, params);
        }
        
    }
    
    /**
     * Check if a message of the given level would actually be logged by this logger before creating the log message
     *
     * @param         logger      Logger to be used
     * @param         level       Level for used for logging
     * @param         message     Message to be logged
     * @param         thrown      Throwable associated with log message.
     */
    public static void checkLog(java.util.logging.Logger logger, java.util.logging.Level level, String message, Throwable thrown)  {
        if (logger.isLoggable(level)) {
            logger.log(level, message, thrown);
        }
        
    }
}
