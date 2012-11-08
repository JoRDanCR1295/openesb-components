/*
 * @(#)ConcurrencyUtils.java        $Revision: 1.2 $ $Date: 2008/07/14 16:30:27 $
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

package org.openesb.components.rules4jbi.engine.util;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

import org.openesb.components.rules4jbi.shared.logging.Logger;

/**
 * This class provides various concurrency utilities.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/07/14 16:30:27 $
 * 
 * @since 0.1
 */
public final class ConcurrencyUtils {
    
    /** Wait time in seconds. */
    private static final long SHUTDOWN_WAIT_TIME = 5;
    
    /* We do not want to instantiate this class */
    private ConcurrencyUtils() {}

    public static void shutdownExecutorService(Logger logger, String serviceName, ExecutorService service) {
        logger.fine("Terminating executor service '%s'", serviceName);
        
        service.shutdown();
        
        boolean terminated = false;
        boolean interrupted = false;
        
        try {
            terminated = service.awaitTermination(SHUTDOWN_WAIT_TIME, TimeUnit.SECONDS);
            
        } catch (InterruptedException e) {
            interrupted = true;
        }
        
        if (!terminated) {
            try {
                service.shutdownNow();
                
                terminated = service.awaitTermination(SHUTDOWN_WAIT_TIME, TimeUnit.SECONDS);
                
            } catch (InterruptedException e) {
                interrupted = true;
            }
        }

        if (!terminated) {
            logger.warning("Executor service '%s' did not terminate", serviceName);
            
        } else {
            logger.fine("Executor service '%s' terminated successfully", serviceName);
        }
        
        if (interrupted) {
            Thread.currentThread().interrupt();
        }
    }
}
