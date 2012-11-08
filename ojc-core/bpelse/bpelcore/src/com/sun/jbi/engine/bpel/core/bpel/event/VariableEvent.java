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
 * @(#)$Id: VariableEvent.java,v 1.5 2008/02/06 21:40:38 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.event;

import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

public interface VariableEvent  extends BPELEvent {

	public static enum VariableType {
			INPUT,
			OUTPUT,		
			COUNTER,
			FAULT,
//			OLD,
			NEW
		 }

	long getActivityId ();
	
	String getActivityXpath ();
	
	Map<VariableType, List<Variable>> getVariables ();	 
	
	String getInvokeCRMPID ();
		
	void setInvokeCRMPID (String crmpId);
		
	String getReceiveCRMPID ();	
		
	void setReceiveCRMPID (String crmpId);	 
	
	QName getBPELName ();
	
	String getInstanceId ();
	
}
