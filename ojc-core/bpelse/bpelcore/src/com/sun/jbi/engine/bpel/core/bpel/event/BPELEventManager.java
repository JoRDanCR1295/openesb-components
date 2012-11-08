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
 * @(#)$Id: BPELEventManager.java,v 1.1 2008/02/06 21:40:38 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.event;

import java.util.Properties;

import com.sun.jbi.engine.bpel.core.bpel.util.EventProcessHelper;

/**
 * BPELEventManager manages the all BPELEvent listeners by looking up all registered BPELEventListener
 * interface in the classpath and manages the processing of the events
 * by the BPELEvent listeners. BPEL Events can be processed
 * either synchronously, i.e, in the same thread that sends the event, or asynchronously,
 * i.e. it is saved into a queue to be picked by one of the BPELEventDispatcher thread. 
 * 
 *
 */
public interface BPELEventManager {
    
    /**
     * BPEL Events can be processed either synchronously, i.e, in the same thread that sends the event, or asynchronously,
     * i.e. it is saved into a queue to be picked by one of the BPELEventDispatcher thread. 
     * If asynchronous, this method decides which BPELEventDispatcher thread is going to process it, in eithe round-robin or
     * load-balancing fashion, it then enqueques the event into the queue of chosen BPELEventDispatcher.
     * If synchronous, this method makes each BPELEventListener process the event synchronously
     * 
     * @param ev  BPELEvent
     */
    void processEvent (BPELEvent ev);
    
    /**
     * If asynchronous, this method starts each  BPELEventDispatcher thread.
     *
     */
    void startQueue ();
    
    /**
     * If synchronous, this method shutsdown each BPELEventDispatcher thread.
     *
     */
    void shutdown ();
    
    /**
     * Initialize with EventProcessHelper for each BPELEventListener
     * @param eventHelper
     */
    void init(EventProcessHelper eventHelper);
    
    /**
     * Notified by the engine for any property change from MBean, and calls resetProperties on each BPELEventListener
     * @param properties
     */
    void resetProperties(Properties properties);    
    
    /**
     * If asynchronous, this method forcibly shuts down any running BPELEventDispatcher threads
     *
     */
    void forceShutdown ();    

}
