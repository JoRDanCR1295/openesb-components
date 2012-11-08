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
 * @(#)SwiftServer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc.extservice.server;

import com.sun.jbi.swiftbc.ApplicationException;
import com.sun.jbi.swiftbc.extservice.ProtocolInfo;


/**
 * Transport Protocol specific classes extends this interface to create and destory Swift Service
 * 
 * @author       S. Nageswara Rao
 *
 */
public interface SwiftServer {

	/**
	* creates SwiftService
	*
	* @param SwiftListener  The listener that receives the Swift messages from transport protocol
	* @param transportProtoolInfo Map of Information that is used during Swift Service creation
	* @throws ApplicationException
	*/
	void createSwiftService(SwiftListener SwiftListener, ProtocolInfo transportProtocolInfo) throws ApplicationException;


	/**
	* destroys SwiftService
	*
	* @param transportProtoolInfo Map of Information that is used during Swift Service creation
	* @throws ApplicationException
	*/
	void destorySwiftService(ProtocolInfo transportProtocolInfo) throws ApplicationException;

	/**
	 * Stop all the running services
	 *
	 */
	void stopAllServices() throws ApplicationException;
}
