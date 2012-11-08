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
 * @(#)POJOSERejectedExecutionHandler.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.pojose.jbi.thread;

import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.logging.Logger;
import org.glassfish.openesb.pojose.jbi.I18n;

/**
 *
 * @author gpatil
 */
public class POJOSERejectedExecutionHandler implements RejectedExecutionHandler{
    private ConcurrentLinkedQueue bufferQueue;
    private Logger logger = Logger.getLogger(InboundProcessor.class.getName());
    
    POJOSERejectedExecutionHandler(ConcurrentLinkedQueue bufferQ){
        this.bufferQueue = bufferQ;
    }
    
    public void rejectedExecution(Runnable r, ThreadPoolExecutor executor) {
        addToBufferQueue(r);
    }

    private void addToBufferQueue(Runnable task){
        String m = I18n.loc("POJOSE-6505: ThreadPoolExecutor rejected submitted task. Allocated threads and blocking queue are full. Adding accepted ME to buffer task queue.");
        logger.warning(m);
        this.bufferQueue.add(task);
    }
}
