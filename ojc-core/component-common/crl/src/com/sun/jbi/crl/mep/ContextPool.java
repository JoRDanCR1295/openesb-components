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
 * @(#)ContextPool.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep;

import java.util.LinkedList;

import com.sun.jbi.crl.mep.impl.DefaultExchangeContext;
import com.sun.jbi.crl.mep.proc.Processor;

/**
 * Simple pool of runtime contexts used by {@link Processor} 
 * implementations to handle message exchanges.
 * 
 * @author Kevan Simpson
 */
public class ContextPool {
    private static int MAX_SIZE = 25;
    
    private LinkedList<ExchangeContext> mPool = new LinkedList<ExchangeContext>();
    private ListenerContext mListenerCtx = null;
    
    /**
     * 
     * @param channel
     * @param delegator
     */
    public ContextPool(ListenerContext ctx) {
        mListenerCtx = ctx;
    }
    
    public ExchangeContext acquireContext() throws Exception {
        synchronized(mPool) {
            if (mPool.size() > 0) {
                return (ExchangeContext) mPool.removeFirst();   // FIFO
            }
            else {
                return createNewContext();
            }
        }
    }

    public void releaseContext(ExchangeContext ctx) {
        synchronized (mPool) {
            if (mPool.size() >= MAX_SIZE) return;
            mPool.addLast(ctx);
        }
    }

    protected ListenerContext getListenerContext() {
        return mListenerCtx;
    }

    /** Creates new instances of {@link ExchangeContext}.  Can be overridden by subclasses as needed. */
    protected ExchangeContext createNewContext() throws Exception {
        return new DefaultExchangeContext(this,
                                          getListenerContext());
    }
}
