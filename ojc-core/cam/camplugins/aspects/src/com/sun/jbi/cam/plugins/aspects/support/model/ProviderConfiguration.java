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
 * @(#)ProviderConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.plugins.aspects.support.model;

import java.io.Serializable;

import javax.xml.namespace.QName;

/**
 * @author graj
 *
 */
public class ProviderConfiguration implements Serializable {
	private static final long serialVersionUID = 1L;

	QName serviceQName;
	String portName;
	QName portTypeQName;

	/**
	 * 
	 */
	public ProviderConfiguration() {
		// TODO Auto-generated constructor stub
	}

	/**
	 * @param serviceQName
	 * @param portName
	 * @param portTypeQName
	 */
	public ProviderConfiguration(QName serviceQName, String portName, QName portTypeQName) {
		this.serviceQName = serviceQName;
		this.portName = portName;
		this.portTypeQName = portTypeQName;
	}

	/**
	 * @return the portName
	 */
	public String getPortName() {
		return portName;
	}

	/**
	 * @param portName the portName to set
	 */
	public void setPortName(String portName) {
		this.portName = portName;
	}

	/**
	 * @return the portTypeQName
	 */
	public QName getPortTypeQName() {
		return portTypeQName;
	}

	/**
	 * @param portTypeQName the portTypeQName to set
	 */
	public void setPortTypeQName(QName portTypeQName) {
		this.portTypeQName = portTypeQName;
	}

	/**
	 * @return the serviceQName
	 */
	public QName getServiceQName() {
		return serviceQName;
	}

	/**
	 * @param serviceQName the serviceQName to set
	 */
	public void setServiceQName(QName serviceQName) {
		this.serviceQName = serviceQName;
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
