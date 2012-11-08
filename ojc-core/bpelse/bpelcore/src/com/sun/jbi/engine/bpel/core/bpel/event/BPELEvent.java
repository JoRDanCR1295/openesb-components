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
 * @(#)$$Id: BPELEvent.java,v 1.7 2010/02/04 02:51:14 fkieviet Exp $$
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.event;

import java.sql.Timestamp;

public interface  BPELEvent {
	
	 static enum EventType {
		BP_START,
		BP_COMPLETE,
		BP_TERMINATE,
		BP_SUSPEND,
		BP_RESUME,
		BP_FAULT,
		BP_SYNCHRONIZE,
		BP_RECOVERED,
	    ACTIVITY_START,
	    ACTIVITY_COMPLETED,
	    ACTIVITY_FAULTED,
	    ACTIVITY_TERMINATE,
	    VARIABLE_CHANGED
	 }
	 
	EventType getEventType ();
	
	String getEventId ();	
	
	Timestamp  getTimeStamp ();
	
	String getEngineId ();
    
    String getInstanceId ();    
    
    int getInstanceHash ();
    
}
