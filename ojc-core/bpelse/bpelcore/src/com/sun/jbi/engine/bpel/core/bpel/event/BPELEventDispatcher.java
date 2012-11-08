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
 * @(#)$Id: BPELEventDispatcher.java,v 1.3 2008/02/06 21:40:37 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.event;

/**
 * The event dispatcher that takes BPELEvent off the event queue
 * and dispatches it to each listener.
 * The event dispatcher starts its own thread and is only used when 
 * BPEL events are processed asynchronously
 * 
 *
 */
public interface BPELEventDispatcher {
	
    /**
     * Add a BPELEventListener
     * @param listener
     */
	void addListener (BPELEventListener listener );
	
    /**
     * Dispatches the event to each BPELEventListener
     * @param event   The BPELEvent
     */
	void dispatchEvent (BPELEvent event);
	
    /**
     * Shutdown 
     *
     */
	void shutdown ();
    
    /**
     * Retuns the length of the queue
     * @return  The length of the queue
     */
    int queueLength ();
    
    /**
     * If the instance of which the event is associated with is in the queue,
     * returns true, otherwise, returns false
     * @param ev  The BPELEvent
     * @return       Whether the instance is in the queue already
     */
    boolean canQueue (BPELEvent ev); 
    
    /**
     * Add the event to the event queue.
     * @param ev    The BPELEvent
     */
    void addNew (BPELEvent ev);     
    
}
