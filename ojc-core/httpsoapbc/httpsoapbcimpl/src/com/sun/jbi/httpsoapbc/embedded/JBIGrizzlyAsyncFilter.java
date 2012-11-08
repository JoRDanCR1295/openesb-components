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
 * @(#)JBIGrizzlyAsyncFilter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.embedded;


import com.sun.enterprise.web.connector.grizzly.AsyncExecutor;
import com.sun.enterprise.web.connector.grizzly.AsyncFilter;
import com.sun.enterprise.web.connector.grizzly.AsyncHandler;
import com.sun.enterprise.web.connector.grizzly.AsyncTask;
import com.sun.enterprise.web.connector.grizzly.DefaultProcessorTask;
import com.sun.enterprise.web.connector.grizzly.ProcessorTask;
import com.sun.jbi.internationalization.Messages;

import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Map;
import java.util.Queue;

import org.apache.coyote.Request;
import org.apache.coyote.tomcat5.CoyoteResponse;


/**
 * Grizzly filter to asynchronously invoke JBI HTTP BC 
 *
 * Also has support to mark an exchange as synchronous when needed, e.g. for 
 * reporting errors
 *
 */
public class JBIGrizzlyAsyncFilter implements AsyncFilter {

    private final static Logger logger =
        Messages.getLogger(JBIGrizzlyAsyncFilter.class);

    /**
     * Mapping from the request to the processing task, used by the adapter 
     * to retrieve the task associated with a request.
     */
    private static Map requestToTask = new java.util.concurrent.ConcurrentHashMap();
    
    /**
     * List of task instances that should be handled in a synchronous manner.
     * By the adapter adding tasks to this queue it can control whether 
     * a given request is dealt with in a synchronous or asynchronous fashion.
     */
    private static Queue respondSynchronously = new java.util.concurrent.ConcurrentLinkedQueue();

    /**
     * Filter implementation, invoke the BC adapter implementation.
     */
    public boolean doFilter(AsyncExecutor asyncExecutor) {
        
        boolean continueSynchronously = false;
        
        AsyncTask asyncProcessorTask = asyncExecutor.getAsyncTask();
        // In Grizzly v1, the asynchronous extenstions are tied to the DefaultProcessorTask
        DefaultProcessorTask defaultProcTask = (DefaultProcessorTask) asyncProcessorTask.getProcessorTask();
        Request req = defaultProcTask.getRequest();

        if (logger.isLoggable(Level.FINEST)) {
            logger.log(Level.FINEST, "doFilter on request " + req.toString() + ", asyncProcessorTask " + asyncProcessorTask.toString());
        }
        
        requestToTask.put(req, asyncProcessorTask);

        try {
            asyncProcessorTask.getProcessorTask().invokeAdapter();        
            boolean wasPresent = respondSynchronously.remove(asyncProcessorTask);
            continueSynchronously = wasPresent;
        } catch (RuntimeException ex) {
            logger.log(Level.WARNING, "HTTPBC-W00641.Adapter_invoke_exception", ex);
            // make sure this is removed; just in case.
            respondSynchronously.remove(asyncProcessorTask);
            continueSynchronously = true;
        } finally {
            // make sure this mapping is cleaned up; just in case
            requestToTask.remove(req);
        }

        if (logger.isLoggable(Level.FINEST)) {
            logger.log(Level.FINEST, "Continue synchronously flag set to " + continueSynchronously);
        }
        
        return continueSynchronously;
    }
    
    /**
     * Mark request as responding synchronously, from the same thread as the request thread.
     */
    public static void finishResponseSynchronously(AsyncTask asyncProcessorTask) {

        if (asyncProcessorTask != null) {
            DefaultProcessorTask task = (DefaultProcessorTask) asyncProcessorTask.getProcessorTask();
            AsyncHandler asyncHandler = task.getAsyncHandler();
            asyncHandler.removeFromInterruptedQueue(asyncProcessorTask);
       
            // Mark task as synchronous
            respondSynchronously.add(asyncProcessorTask);
            if (logger.isLoggable(Level.FINEST)) {
                logger.log(Level.FINEST, "Marking exchange as synchronous");
            }
        }
        
    }

    /**
     * Finish the response asynchronously, i.e. from a different thread than the request thread.
     */
    public static void finishResponse(AsyncTask asyncProcessorTask) { 
        if (asyncProcessorTask != null) {
            // In Grizzly v1, the asynchronous extenstions are tied to the DefaultProcessorTask
            DefaultProcessorTask task = (DefaultProcessorTask) asyncProcessorTask.getProcessorTask();

            if (task != null) {
                AsyncHandler asyncHandler = task.getAsyncHandler();
                if (asyncHandler != null) {
                    if (logger.isLoggable(Level.FINEST)) {
                        logger.log(Level.FINEST, "Finish response for asyncProcessorTask "
                                + asyncProcessorTask.toString());
                    }
                    asyncHandler.handle(asyncProcessorTask);
                } else {
                    logger.log(Level.SEVERE, "HTTPBC-E00642.No_response_handler_for_request");
                }
            } else {
                logger.log(Level.WARNING, "HTTPBC-W00642.No_correlating_request_for_response");
            }
        } else {
            logger.log(Level.WARNING, "HTTPBC-W00643.Null_response");
        }
    }   
    
    /**
     * Remove the task mapping for a given request
     * @param request the request 
     * @return the task if there was a mapping for the request, null if not
     */
    public static AsyncTask removeTaskMapping(Request request) {
        return (AsyncTask) requestToTask.remove(request);
    }
}
