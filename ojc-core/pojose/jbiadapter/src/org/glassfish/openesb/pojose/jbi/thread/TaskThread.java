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
 * @(#)TaskThread.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.pojose.jbi.thread;

import java.util.concurrent.atomic.AtomicLong;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author gpatil
 */
public class TaskThread  extends Thread {
    private static final String POJOSE_THREAD_NAME_PREFIX = "pojo-se-"; //NOI18N
    private static final AtomicLong taskThreadsCreated = new AtomicLong();
    private static final AtomicLong taskThreadsRunning = new AtomicLong();
    private static final Logger logger = Logger.getLogger(
            org.glassfish.openesb.pojose.jbi.nmr.BasePojoExecutor.class.getName()); //PojoSE Executor
    private BaseTask target;

    public TaskThread(ThreadGroup group, Runnable tgt) {
        super(group, tgt, POJOSE_THREAD_NAME_PREFIX + taskThreadsCreated. incrementAndGet());

        if (tgt instanceof BaseTask){
            this.target = (BaseTask)tgt;
        }
    }

    public BaseTask getTarget(){
        return this.target;
    }

    public void setTarget(Runnable tgt){
        if ( (tgt != null) && (tgt instanceof BaseTask)){
            this.target = (BaseTask)tgt;
        } else {
            tgt = null;
        }
    }
    
    public static long getTaskThreadsCreated() {
        return taskThreadsCreated.get();
    }

    public static long getTaskThreadsRunning() {
        return taskThreadsRunning.get();
    }
    
    @Override
    public void run() {
        try {
            taskThreadsRunning.incrementAndGet();
            if (logger.isLoggable(Level.FINEST)) {
                logger.finest("Thread " + this.getName() + " started running."); //NOI18N
            }

            super.run();
            
        } finally {
            taskThreadsRunning.decrementAndGet();
            if (logger.isLoggable(Level.FINEST)) {
                logger.finest("Thread " + this.getName() + " finished running."); //NOI18N
            }
        }
    }
}
