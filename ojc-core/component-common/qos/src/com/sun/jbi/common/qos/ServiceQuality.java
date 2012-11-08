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
 * @(#)ServiceQuality.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos;

/**
 * Marker interface for service qualities.
 * @author Kevan Simpson
 */
public interface ServiceQuality {
	public static final String QoS_NS = "http://www.sun.com/jbi/qos";
	
	/** 
	 * Group Id property key, set on the message exchange, used in 
	 * implementation of systemic qualities, and not visible to applications. 
	 */
	public static final String GROUP_ID 	= "com.sun.jbi.messaging.groupid";
    /** 
     * Message Id property key, set on the message exchange, used in 
     * implementation of systemic qualities, and not visible to applications. 
     */
	public static final String MESSAGE_ID	= "com.sun.jbi.messaging.messageid";
	
    /** 
     * Group Id property key, set on the normalized message and 
     * visible to applications. 
     */
	public static final String PUBLIC_GROUP_ID = "org.glassfish.openesb.messaging.groupid";
    /** 
     * Message Id property key, set on the normalized message and 
     * visible to applications. 
     */
	public static final String PUBLIC_MESSAGE_ID = "org.glassfish.openesb.messaging.messageid";
}
