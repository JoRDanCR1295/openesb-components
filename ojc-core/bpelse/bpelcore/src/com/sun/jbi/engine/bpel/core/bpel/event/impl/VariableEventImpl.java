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
 * @(#)$Id: VariableEventImpl.java,v 1.5 2008/02/06 21:40:38 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.event.impl;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import com.sun.jbi.engine.bpel.core.bpel.event.Variable;
import com.sun.jbi.engine.bpel.core.bpel.event.VariableEvent;

public class VariableEventImpl extends BPELEventImpl implements VariableEvent {

	private long mActivityId;
	
    private Map<VariableType, List<Variable>> mVariables = new HashMap<VariableType, List<Variable>> ();
    
    private String mXpath;    
	
	private String mInvokeCRMPID;	
	
	private String mReceiveCRMPId;	    
	
	private QName mBPELName;
	
	private String mBPId;	
    
    
    VariableEventImpl(String engineId, QName name, String id, String eventId, EventType type, long activity, Map<VariableType, List<Variable>> variables, String xpath) {
		super(engineId, eventId, type);
		mActivityId = activity;
		mVariables = variables;
		mXpath = xpath;
		mBPELName = name;
		mBPId = id;				
	}

	public long getActivityId() {
		// TODO Auto-generated method stub
		return mActivityId;
	}

	public Map<VariableType, List<Variable>> getVariables() {
		// TODO Auto-generated method stub
		return mVariables;
	}

	public String getActivityXpath() {
		// TODO Auto-generated method stub
		return mXpath;
	}
	public QName getBPELName() {
		// TODO Auto-generated method stub
		return mBPELName;
	}

	public String getInstanceId() {
		// TODO Auto-generated method stub
		return mBPId;
	}
	@Override
	public String toString() {
		// TODO Auto-generated method stub
		StringBuffer buffer = new StringBuffer ();
		buffer.append("<<<<<<<<<<<VariableEvent:");
		buffer.append(getEventType());
		buffer.append("   ");
		buffer.append(mXpath);
		buffer.append("<<<<<<<<<<<\n");
		
		buffer.append("BPELName");
		buffer.append(mBPELName);		
		buffer.append("\n");		

		buffer.append("ActivityId:");
		buffer.append(mActivityId);
		buffer.append("\n");
		
		
//		if (mInvokeCRMPID != null) {
//			buffer.append("InvokeCRMPID:");
//			buffer.append(mInvokeCRMPID);
//			buffer.append("\n");			
//		}		
//		
//		if (mReceiveCRMPId != null) {
//			buffer.append("ReceiveCRMPId");
//			buffer.append(mReceiveCRMPId);		
//			buffer.append("\n");					
//		}				
		
		for (Map.Entry<VariableType, List<Variable>> entry : mVariables.entrySet()) {

			List<Variable> vars = entry.getValue();
			for (Variable var : vars) {
				buffer.append("\n");	
				buffer.append("VariableType:");
				buffer.append(entry.getKey());				
				buffer.append("\n");	
				buffer.append(var);				
			}
//			buffer.append(entry.getValue());
			buffer.append("\n");	
		}

		buffer.append("XPath:");
		buffer.append(mXpath);
		buffer.append("\n");
		
		buffer.append(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n");		
		return buffer.toString();
	}
	

        public String toXML() {
		StringBuffer buffer = new StringBuffer ();
		buffer.append("<msgns:VariableEvent>"+getEventType()+"</msgns:VariableEvent>");
		buffer.append("<msgns:Xpath>"+mXpath+"</msgns:Xpath>");
		buffer.append("<msgns:BPELName>"+mBPELName+"</msgns:BPELName>");
		buffer.append("<msgns:ActivityId>"+mActivityId+"</msgns:ActivityId>");
		for (Map.Entry<VariableType, List<Variable>> entry : mVariables.entrySet()) {
			List<Variable> vars = entry.getValue();
			for (Variable var : vars) {
                                buffer.append(var.toXML());   // Uncomment
			}
		}
		return buffer.toString();
	}
	

	public String getInvokeCRMPID() {
		// TODO Auto-generated method stub
		return mInvokeCRMPID;
	}

	public void setInvokeCRMPID (String crmpId) {
		mInvokeCRMPID = crmpId;
	}

	public String getReceiveCRMPID() {
		// TODO Auto-generated method stub
		return mReceiveCRMPId;
	}
	public void  setReceiveCRMPID(String crmpId) {
		// TODO Auto-generated method stub
		mReceiveCRMPId = crmpId;
	}	
}
