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
 * @(#)$Id: BPELEventListener.java,v 1.3 2008/02/06 21:40:38 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.event;

import java.util.Properties;

import com.sun.jbi.engine.bpel.core.bpel.util.EventProcessHelper;

/**
 * The event listener that processes the event
 * @author mei
 *
 */
public interface BPELEventListener {
	
	/**
	 * Process the event. The eventListener processes the event based on the relavence  of the event
	 * and the status of itself
	 * @param event
	 */
	void processEvent (BPELEvent event);
	
	/**
	 * Initialize with EventProcessHelper
	 * @param eventHelper
	 */
	void init(EventProcessHelper eventHelper);
	
	/**
	 * Notified by the engine for any property change from MBean
	 * @param properties
	 */
	void resetProperties(Properties properties);
    
    /**
     * Cleans up when shutdown is called
     *
     */
    void shutdown ();

}
