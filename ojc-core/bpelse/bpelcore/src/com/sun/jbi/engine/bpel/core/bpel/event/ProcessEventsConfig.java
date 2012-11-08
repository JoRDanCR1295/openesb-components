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
 * @(#)ProcessEventsConfig.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.event;

import javax.xml.namespace.QName;

/**
*
* @author Sun Microsystems
*/
public class ProcessEventsConfig {

	private QName mBPELProcessId;
	
	private String mServiceUnitName;
	
	private boolean mProcessEventsFlag;
	
	/**
	 * 
	 * @param bpelId
	 * @param suName
	 */
	public ProcessEventsConfig(QName bpelId, String suName) {
		mBPELProcessId = bpelId;
		mServiceUnitName = suName;
		mProcessEventsFlag = true; // default value
	}
	
	/**
	 * 
	 * @return
	 */
	public QName getBPELProcessId() {
		return mBPELProcessId;
	}
	
	/**
	 * 
	 * @return
	 */
	public String getServiceUnitName() {
		return mServiceUnitName;
	}
	
	/**
	 * 
	 * @return
	 */
	public boolean getProcessEventsFlag() {
		return mProcessEventsFlag;
	}
	
	/**
	 * 
	 * @param value
	 */
	public void setProcessEventsFlag(boolean value) {
		mProcessEventsFlag = value;
	}
}
