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
 * @(#)Connection.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.descriptor.model;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.util.Util;

/**
 * Describes a JBI service connection, defined in a JBI service 
 * assembly descriptor file.
 * 
 * @author Kevan Simpson
 */
public class Connection {
	private EndpointInfo mConsumer, mProvider;
	
	/**
	 * Constructs a <code>Connection</code>.
	 * @param consumer A consuming endpoint.
	 * @param provider A provisioning endpoint.
	 */
	public Connection(EndpointInfo consumer, EndpointInfo provider) {
		mConsumer = consumer;
		mProvider = provider;
	}

	/**
	 * Returns the consumer endpoint.
	 * @return the consumer endpoint.
	 */
	public EndpointInfo getConsumer() {
		return mConsumer;
	}

	/**
	 * Returns the provider endpoint.
	 * @return the provider endpoint.
	 */
	public EndpointInfo getProvider() {
		return mProvider;
	}

	/** @see java.lang.Object#equals(java.lang.Object) */
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Connection) {
			Connection conn = (Connection) obj;
			return (Util.equals(getConsumer(), conn.getConsumer()) &&
					Util.equals(getProvider(), conn.getProvider()));
		}
		
		return false;
	}

	/** @see java.lang.Object#hashCode() */
	@Override
	public int hashCode() {
		return Util.hashCode(getConsumer()) * 7 + Util.hashCode(getProvider()) * 11 + 17;
	}

	/** @see java.lang.Object#toString() */
	@Override
	public String toString() {
		StringBuffer buff = new StringBuffer();
		buff.append("Connection(").append(getConsumer())
			.append(" --> ").append(getProvider()).append(")");
		return buff.toString();
	}
}
