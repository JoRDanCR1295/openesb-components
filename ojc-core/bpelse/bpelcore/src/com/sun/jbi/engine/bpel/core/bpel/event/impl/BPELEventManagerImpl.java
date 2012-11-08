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
 * @(#)$Id: BPELEventManagerImpl.java,v 1.1 2008/02/06 21:40:38 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.event.impl;

import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.engine.bpel.core.bpel.event.BPELEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.BPELEventDispatcher;
import com.sun.jbi.engine.bpel.core.bpel.event.BPELEventListener;
import com.sun.jbi.engine.bpel.core.bpel.event.BPELEventManager;
import com.sun.jbi.engine.bpel.core.bpel.util.EventProcessHelper;
import com.sun.jbi.engine.bpel.core.bpel.util.FactoryFinder;

public class BPELEventManagerImpl implements BPELEventManager {

    private BPELEventDispatcher[] mDispatchers;

    private boolean mSync;

    private BPELEventListener[] mEventListeners;
    
    private static final Logger LOGGER = Logger.getLogger(BPELEventManagerImpl.class.getName());
    
    private int mLastDispatcherNo = 0;

    public BPELEventManagerImpl(int no, boolean sync) {
        mSync = sync;
        if (!mSync) {
            mDispatchers = new BPELEventDispatcherImpl[no];
            for (int i = 0; i < no; i++) {
                BPELEventDispatcher dispatcher = new BPELEventDispatcherImpl();
                mDispatchers[i] = dispatcher;
            }
        }
        //Find all listners and init
        Object[] foundlistners = FactoryFinder
                .findJarServiceProvider(BPELEventListener.class.getName());
        if (foundlistners != null) {
            mEventListeners = new BPELEventListener[foundlistners.length];
            int i = 0;
            for (Object listner : foundlistners) {
                mEventListeners[i] = (BPELEventListener) listner;
                if (!mSync) {
                    for (int j = 0; j < mDispatchers.length; j++) {
                        mDispatchers[j].addListener(mEventListeners[i]);
                    }
                }
                ++i;
            }
        }
    }

    private BPELEventDispatcher findTheMinLoad() {
        int min = Integer.MAX_VALUE;
        int which = 0;
        double total = 0;
        if (mDispatchers.length < 10) {
            for (int i = 0; i < mDispatchers.length; i++) {
                int len = mDispatchers[i].queueLength();
                if (len < min) {
                    min = mDispatchers[i].queueLength();
                    which = i;
                }
                total = total + len;
            }
        } else if (mDispatchers.length > 0) {
            int len = mDispatchers[mLastDispatcherNo].queueLength();
            mLastDispatcherNo = (mLastDispatcherNo + 1) % mDispatchers.length;
            BPELEventDispatcher dispatcher = mDispatchers[mLastDispatcherNo];
            total = (mDispatchers[mLastDispatcherNo].queueLength() + len)
                    * (mDispatchers.length / 2.0);
        }
        if (total > 20) {
            LOGGER.log(Level.WARNING, "!!!!Queue is backing up: " + total);
        }
        return mDispatchers[which];
    }

    public void processEvent(BPELEvent ev) {
        if (!mSync) {
            if (ev.getEventType() == BPELEvent.EventType.BP_SYNCHRONIZE) {
                mDispatchers[0].addNew(ev);
            } else  if (ev.getEventType() == BPELEvent.EventType.BP_START) {
                findTheMinLoad().addNew(ev);
            } else {
                boolean canProcess = false;
                for (int i = 0; i < mDispatchers.length; i++) {
                    if (mDispatchers[i].canQueue(ev)) {
                        canProcess = true;
                        break;
                    }
                }
                if (!canProcess) {
                    findTheMinLoad().addNew(ev);
                }
            }
        } else {
            for (int i = 0; i < mEventListeners.length; i++) {
                mEventListeners[i].processEvent(ev);
            }
        }

    }

    public void startQueue() {
        if (!mSync) {
            for (int i = 0; i < mDispatchers.length; i++) {
                ((Thread) mDispatchers[i]).start();
            }
        }
    }

    public void shutdown() {
        if (!mSync) {
            for (int i = 0; i < mDispatchers.length; i++) {
                mDispatchers[i].shutdown();
            }
        } else {
            for (int i = 0; i < mEventListeners.length; i++) {
                mEventListeners[i].shutdown();
            }            
        }
    }

    public void forceShutdown() {
        if (!mSync) {
            for (int i = 0; i < mDispatchers.length; i++) {
                Thread dispatcher = (Thread) mDispatchers[i];
                if (dispatcher != null && dispatcher.isAlive()) {
                    try {
                        dispatcher.join(10);
                        if (dispatcher.isAlive()) {
                            dispatcher.interrupt();
                        }
                    } catch (InterruptedException e) {
                        // ignore it
                    }
                }
            }
        }
    }

    public void init(EventProcessHelper eventHelper) {
        for (int i = 0; i < mEventListeners.length; i++) {
            mEventListeners[i].init(eventHelper);
        }
    }

    public void resetProperties(Properties properties) {
        for (int i = 0; i < mEventListeners.length; i++) {
            mEventListeners[i].resetProperties(properties);
        }
    }

}
