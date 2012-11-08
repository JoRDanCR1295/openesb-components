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
 * @(#)StandardFaults.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.visitor;

import java.util.ArrayList;
import java.util.List;
import javax.xml.namespace.QName;

/**
 * @author Sun Microsystems
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class StandardFaults {
	
	/**
	 * Thrown when a selection operation performed either in
	 	a function such as bpws:getContainerData, or in
	 	an assignment, encounters an error.
	 */
	public static final String selectionFailure = "selectionFailure"; 
	
	
	/**
	 *Thrown when more than one receive activity or
	 	equivalent (currently, onMessage branch in a
	 	pick activity) are enabled simultaneously
	 	for the same partner, port type, and
	 	operation.
	 */
	public static final String conflictingReceive = "conflictingReceive";
	
	
	/**
	 * Thrown when more than one synchronous inbound
	 	request from the same partner for a particular port type
		and operation are active.
	 */
	public static final String conflictingRequest = "conflictingRequest";

	
	/**
		Thrown when incompatible types are encountered in an
		assign activity.
	*/
	public static final String mismatchedAssignmentFailure = "mismatchedAssignmentFailure";
	  
	
	/**
		Thrown when the join condition of an activity evaluates
		to false.
	*/
	public static final String joinFailure  = "joinFailure";
	
	/**
		Thrown as a result of a fault in an enclosing scope.
	*/
	public static final String forcedTermination = "forcedTermination";
	
	/**
		Thrown when the contents of the messages that are
		processed in an invoke, receive, or reply activity
		do not match specified correlation information.
	*/
	public static final String correlationViolation = "correlationViolation";
	
	/**
		Thrown when there is an attempt to access the value of
		an uninitialized part in a message container.

	*/
	public static final String uninitializedContainer = "uninitializedContainer";
	
	/**
		Thrown when an installed compensation handler is
		invoked more than once.
	*/
	public static final String repeatedCompensation = "repeatedCompensation";
	
	private static List standardFaults = new ArrayList();
	
	static {
		standardFaults.add(selectionFailure);
		standardFaults.add(conflictingReceive);
		standardFaults.add(conflictingRequest);
		standardFaults.add(mismatchedAssignmentFailure);
		standardFaults.add(joinFailure);
		standardFaults.add(forcedTermination);
		standardFaults.add(correlationViolation);
		standardFaults.add(uninitializedContainer);
		standardFaults.add(repeatedCompensation);
		
	}
	
	public static boolean isStandardFault(QName faulQName) {
		if(faulQName != null) {
			return standardFaults.contains(faulQName.getLocalPart());
		}
		
		return false;
	}
}
