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
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.bpel.model;

import javax.xml.namespace.QName;

/**
 *
 * @author Sun Microsystems
 *
 */
public class SystemFault {

	// Constants for defining a BPEL-SE system fault definition.
	private static final String MESSAGE_TYPE = "faultMessage";
	private static String MESSAGE_PREFIX = "sxeh";

	public static final String NAMESPACE = "http://www.sun.com/wsbpel/2.0/process/executable/SUNExtension/ErrorHandling";
	public static final QName MESSAGE_QNAME = new QName(NAMESPACE, MESSAGE_TYPE, MESSAGE_PREFIX);
	public static final String MESSAGE_PARTNAME = "fault";

	private static final String FAULT_NAMESPACE = "http://www.sun.com/wsbpel/2.0/process/executable/SUNExtension/ErrorHandling"; //$NON-NLS-1$	
	private static final String FAULT_NAME = "systemFault"; //$NON-NLS-1$	
	public static final QName FAULT_QNAME = new QName(FAULT_NAMESPACE, FAULT_NAME);	

	// Constants to define the fault part namespace
	public static final String XML_NAMESPACE_URL = "http://www.w3.org/2001/XMLSchema";
	public static final String XMS_ANY_TYPE = "anyType";
	public static final String XML_TYPE = "xsd";
	
}
