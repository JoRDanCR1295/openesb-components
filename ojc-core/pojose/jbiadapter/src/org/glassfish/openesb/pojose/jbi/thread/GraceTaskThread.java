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
 * @(#)GraceTaskThread.java
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
public class GraceTaskThread extends Thread {
    private static final String POJOSE_THREAD_NAME_PREFIX = "pojo-se-grace"; //NOI18N
    private static final AtomicLong graceTaskThreadsCreated = new AtomicLong();
    private static final AtomicLong graceTaskThreadsRunning = new AtomicLong();
    private static final Logger logger = Logger.getLogger(
            org.glassfish.openesb.pojose.jbi.nmr.BasePojoExecutor.class.getName()); //PojoSE Executor

    public GraceTaskThread(ThreadGroup group, Runnable target) {
        super(group, target, POJOSE_THREAD_NAME_PREFIX + graceTaskThreadsCreated. incrementAndGet());
    }

    public static long getTaskThreadsCreated() {
        return graceTaskThreadsCreated.get();
    }

    public static long getTaskThreadsRunning() {
        return graceTaskThreadsRunning.get();
    }

    @Override
    public void run() {
        try {
            graceTaskThreadsRunning.incrementAndGet();
            if (logger.isLoggable(Level.FINEST)) {
                logger.finest("Thread " + this.getName() + " started running."); //NOI18N
            }

            super.run();

        } finally {
            graceTaskThreadsRunning.decrementAndGet();
            if (logger.isLoggable(Level.FINEST)) {
                logger.finest("Thread " + this.getName() + " finished running."); //NOI18N
            }
        }
    }
}
