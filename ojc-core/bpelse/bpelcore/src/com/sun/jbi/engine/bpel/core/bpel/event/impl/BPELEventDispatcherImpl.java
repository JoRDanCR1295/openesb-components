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
 * @(#)$Id: BPELEventDispatcherImpl.java,v 1.4 2009/04/01 01:47:29 vinayram Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.event.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Vector;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import com.sun.jbi.engine.bpel.core.bpel.event.BPELEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.BPELEventDispatcher;
import com.sun.jbi.engine.bpel.core.bpel.event.BPELEventListener;

public class BPELEventDispatcherImpl extends Thread implements
		BPELEventDispatcher {

	private BlockingQueue<BPELEvent> mqueue;
    
    private Collection<Integer> mInstances = Collections.synchronizedCollection(new ArrayList<Integer> (5));

	private Vector<BPELEventListener> mListerners = new Vector<BPELEventListener>();
	
	private boolean mShutdown;

	public BPELEventDispatcherImpl() {
		super();
		this.mqueue =  new LinkedBlockingQueue<BPELEvent> ();
	}

	@Override
	public void run() {
		try {
			while (!mShutdown) {
				dispatchEvent(mqueue.take());
			}
		} catch (InterruptedException ex) {
		}

	}

	public void addListener(BPELEventListener listener) {
		mListerners.add(listener);
	}

	public void dispatchEvent(BPELEvent event) {
		for (BPELEventListener l : mListerners) {
			l.processEvent(event);
		}
		BPELEvent.EventType evType = event.getEventType();
		if (evType == BPELEvent.EventType.BP_COMPLETE || 
				evType == BPELEvent.EventType.BP_FAULT ||
				evType == BPELEvent.EventType.BP_TERMINATE ) {
			mInstances.remove(event.getInstanceHash());
		}

	}

	public void shutdown() {
		mShutdown = true;
        for (BPELEventListener l : mListerners) {
            l.shutdown();
        }
	}

    public void addNew(BPELEvent ev) {
        if (ev.getEventType() != BPELEvent.EventType.BP_SYNCHRONIZE) {
         if (!mInstances.contains(ev.getInstanceHash())) {
             mInstances.add(ev.getInstanceHash());
         }        
        }
        mqueue.add(ev);
    }

    public boolean canQueue(BPELEvent ev) {
        if (mInstances.contains(ev.getInstanceHash())) {
            mqueue.add(ev);
            return true;
        }
        return false;
    }

    public int queueLength() {
        return mqueue.size();
    }

}
