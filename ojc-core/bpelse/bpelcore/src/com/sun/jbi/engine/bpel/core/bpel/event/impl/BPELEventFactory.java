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
 * @(#)$Id: BPELEventFactory.java,v 1.4 2008/02/06 21:40:38 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.event.impl;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.List;
import java.util.Map;
import java.util.Random;

import javax.xml.namespace.QName;

import com.sun.jbi.engine.bpel.core.bpel.event.ActivityEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.BPELEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.BPELInstanceEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.Variable;
import com.sun.jbi.engine.bpel.core.bpel.event.VariableEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.BPELEvent.EventType;
import com.sun.jbi.engine.bpel.core.bpel.event.VariableEvent.VariableType;

public class BPELEventFactory {


	private static long netAddress = 0;
		
	private static  long getUID  () {
		if (netAddress == 0) {
			try {
				netAddress = (long) ((long) InetAddress.getLocalHost().hashCode()) <<32;
			} catch (UnknownHostException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		Random rnd=new Random();
		long uniqueId = (long) netAddress + (((System.currentTimeMillis()+rnd.nextLong()) << 32) >>>32);
		return uniqueId;
	}
	
	public static  String getNextEventId () {
		long uid = getUID ();
		return Long.toString(uid);
	}
	
	public static void main(String[] args) {
		for (int i = 0; i < 10; i++) {
			long uid = getUID();
			System.out.println("BIN="+Long.toBinaryString(uid)) ;
			System.out.println("HEX="+Long.toHexString(uid));

		}		
	}	
	
	public static BPELInstanceEvent createBPELInstanceEvent (String engineId, QName bpname, String instanceId, BPELEvent.EventType type) {
		BPELInstanceEvent event = new BPELInstanceEventImpl (engineId,bpname, instanceId, getNextEventId(), type);
		return event;
	}
	
	public static ActivityEvent createActivityEvent (String engineId, QName bpName, String instanceId,  EventType type, long activityId, String activityName, String xpath) {
		ActivityEvent event = new ActivityEventImpl (engineId, bpName, instanceId, getNextEventId(), type, activityId, activityName, xpath);
		return event;
	}
	
	public static VariableEvent createVariableEvent (String engineId, QName bpName, String instanceId, EventType type, long activityId, Map<VariableType, List<Variable>> variables, String xpath) {
		VariableEvent event = new VariableEventImpl (engineId,bpName, instanceId, getNextEventId(), type, activityId, variables, xpath);
		return event;
	}		
	
	public static BPELEvent createBPELSynchronizeEvent(String engineId) {
		BPELEvent bpEvent = new BPELSynchronizationEventImpl(engineId,  getNextEventId(), EventType.BP_SYNCHRONIZE);
		return bpEvent;
	}

}
