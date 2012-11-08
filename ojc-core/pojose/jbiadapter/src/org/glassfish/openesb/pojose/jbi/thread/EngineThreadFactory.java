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
 * @(#)ThreadFactory.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.pojose.jbi.thread;

import java.util.concurrent.ThreadFactory;
import java.util.concurrent.locks.ReentrantLock;

/**
 *
 * @author gpatil
 */
public class EngineThreadFactory implements ThreadFactory {   
    private static volatile EngineThreadFactory inFactory;
    private static ReentrantLock factCreateLock = new ReentrantLock();

    private volatile ThreadGroup inGroup;
    
    private EngineThreadFactory(){
        inGroup = new ThreadGroup("POJO SE Inbound Threads"); //NOI18N
    }
    
    public static EngineThreadFactory getFactory(){
        EngineThreadFactory ret = null;

        if (inFactory == null){
            try {
                factCreateLock.lock();
                if (inFactory == null){
                    inFactory = new EngineThreadFactory();
                }
            } finally {
                factCreateLock.unlock();
            }
        }
        
        ret = inFactory;
        
        return ret;
    }
    
    public Thread newThread(Runnable r) {
        return new TaskThread(inGroup, r);
    }

    public Thread newGraceThread(Runnable r){
        return new GraceTaskThread(inGroup, r);
    }
}