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
 *
 * @author Alexander Lomov
 *
 * Copyright 2011 Open-ESB Community.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.httpsoapbc.async;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.ws.AsyncHandler;
import javax.xml.ws.Response;
import javax.xml.ws.handler.MessageContext;
import net.java.hulp.measure.Probe;


public class AsyncResponseHandler<T> implements AsyncHandler<T> {

    private AsyncResponseDispatcher dispatcher;
    private AsyncRequestContext context;
    private String threadName;

    private final Logger logger = Logger.getLogger(AsyncResponseHandler.class.getName());
    

    AsyncResponseHandler(AsyncResponseDispatcher receiver, AsyncRequestContext context){
        this.dispatcher = receiver;
        this.context = context;
        this.threadName = Thread.currentThread().getName();

    }

    public void handleResponse(Response<T> res) {
        if (!isContextSet()){
            if (logger.isLoggable(Level.WARNING)){
                logger.log(Level.WARNING, "AsyncResponseContext is not set for " + threadName);
            }
            return;
        }

        Probe probe = context.getProbe();
        if (probe != null)
            probe.end();

        T r = null;
        try {
            r = res.get();
            Map<String, List<String>> headers = (Map<String, List<String>>) res.getContext().get(MessageContext.HTTP_RESPONSE_HEADERS);
            context.setResponseHeaders(headers);
            AsyncResponseProcessor<T> p = new AsyncResponseProcessor<T>(r, context);
            dispatcher.dispatch(p);
        }
        catch(ExecutionException exe){
            Throwable t = exe.getCause();
            AsyncResponseProcessor<Throwable> p = new AsyncResponseProcessor<Throwable>(t, context);
            dispatcher.dispatch(p);
        }

        catch(InterruptedException ie){
            if (logger.isLoggable(Level.WARNING)){
                logger.log(Level.WARNING, "Thread " + threadName + " execution was interrupted: " + ie.getCause().getLocalizedMessage());
            }
        }

    }

    private boolean isContextSet()  {
        return context != null;
    }

    public AsyncRequestContext getContext() {
        return context;
    }

    public void setContext(AsyncRequestContext context) {
        this.context = context;
    }



}
