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
 * @(#)Provides.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.plugins.aspects.support.model.xml;

import java.io.Serializable;

import javax.xml.namespace.QName;

import com.sun.jbi.cam.common.EqualsUtil;
import com.sun.jbi.cam.common.HashCodeUtil;

/**
 * @author graj
 *        <provides interface-name="ns1:portType1" service-name="ns1:partnerlinktype1_Facade" endpoint-name="partnerlinktyperole1_Facade"/>
        <consumes interface-name="ns2:portType1" service-name="ns2:service1" endpoint-name="port1" link-type="standard"/>
 
 */
public class Provides implements Serializable {
	private static final long serialVersionUID = 1L;
	
	QName interfaceName;
	QName serviceName;
	String endpointName;
	

	/**
	 * 
	 */
	public Provides() {
		// TODO Auto-generated constructor stub
	}

	/**
	 * @param interfaceName
	 * @param serviceName
	 * @param endpointName
	 */
	public Provides(QName interfaceName, QName serviceName, String endpointName) {
		super();
		this.interfaceName = interfaceName;
		this.serviceName = serviceName;
		this.endpointName = endpointName;
	}
	
	/**
	 * @param interfaceName
	 * @param serviceName
	 * @param endpointName
	 */
	public Provides(String interfaceName, String serviceName, String endpointName) {
		super();
		this.interfaceName = QName.valueOf(interfaceName);
		this.serviceName = QName.valueOf(serviceName);
		this.endpointName = endpointName;
	}	

	/**
	 * @return the endpointName
	 */
	public String getEndpointName() {
		return endpointName;
	}

	/**
	 * @param endpointName the endpointName to set
	 */
	public void setEndpointName(String endpointName) {
		this.endpointName = endpointName;
	}

	/**
	 * @return the interfaceName
	 */
	public QName getInterfaceName() {
		return interfaceName;
	}

	/**
	 * @param interfaceName the interfaceName to set
	 */
	public void setInterfaceName(QName interfaceName) {
		this.interfaceName = interfaceName;
	}

	/**
	 * @return the serviceName
	 */
	public QName getServiceName() {
		return serviceName;
	}

	/**
	 * @param serviceName the serviceName to set
	 */
	public void setServiceName(QName serviceName) {
		this.serviceName = serviceName;
	}
	
	public boolean equals(Object aThat) {
		// check for self-comparison
		if (this == aThat) {
			return true;
		}

		// use instanceof instead of getClass here for two reasons
		// 1. if need be, it can match any supertype, and not just one class;
		// 2. it renders an explict check for "that == null" redundant, since
		// it does the check for null already - "null instanceof [type]" always
		// returns false. (See Effective Java by Joshua Bloch.)
		if (!(aThat instanceof Provides)) {
			return false;
		}
		// Alternative to the above line :
		// if ( aThat == null || aThat.getClass() != this.getClass() ) return
		// false;

		// cast to native object is now safe
		Provides that = (Provides) aThat;

		// now a proper field-by-field evaluation can be made
		return EqualsUtil.areEqual(this.endpointName, that.endpointName)
				&& EqualsUtil.areEqual(this.interfaceName, that.interfaceName)
				&& EqualsUtil.areEqual(this.serviceName, that.serviceName);
	}

	public int hashCode() {
		int result = HashCodeUtil.SEED;
		result = HashCodeUtil.hash(result, this.interfaceName);
		result = HashCodeUtil.hash(result, this.serviceName);
		result = HashCodeUtil.hash(result, this.endpointName);
		return result;
	}
	

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
