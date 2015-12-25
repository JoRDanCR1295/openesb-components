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
 * @(#)InvokerImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.jaxwssupport;

import com.sun.xml.ws.api.message.Packet;
import com.sun.xml.ws.api.server.Invoker;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import javax.xml.ws.WebServiceContext;
import javax.xml.ws.Provider;



/**
 * Implements JAXWS's Invoker interface to call the endpoint method
 *
 * JAX-WS has a default implementation that we may want to use instead
 */
public class InvokerImpl extends Invoker {
    protected Object invokeObject;
    protected NewWebServiceContextImpl injectedWSCtxt;
    
    public InvokerImpl(Object inv, NewWebServiceContextImpl w) {
        this.invokeObject = inv;
        this.injectedWSCtxt = w;
    }
    
    /**
     * Called when WSEndpoint is being setup
     * We use this to delegate WSCtxt stuff
     */
    public void start(WebServiceContext wsc) {
        if(this.injectedWSCtxt != null) {
            injectedWSCtxt.setContextDelegate(wsc);
        }
    }

    /**
     * Here is where we actually call the endpoint method
     */
    public Object invoke(Packet p, Method m, Object... args ) 
                                throws InvocationTargetException, IllegalAccessException {
        Object ret = null;
        if(this.invokeObject != null) {
            ret = m.invoke(this.invokeObject, args);
        }
        return ret;
    }
}
