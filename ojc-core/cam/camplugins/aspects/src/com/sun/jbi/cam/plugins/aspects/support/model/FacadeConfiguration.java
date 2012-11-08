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
 * @(#)FacadeConfiguration.java 
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
public class FacadeConfiguration implements Serializable {
	private static final long serialVersionUID = 1L;
	
	String targetNamespace;
	String facadeServiceName;
	String facadePortName;
	String locationURI;
	QName portTypeQName;

	/**
	 * 
	 */
	public FacadeConfiguration() {
		// TODO Auto-generated constructor stub
	}

	/**
	 * @param targetNamespace
	 * @param facadeServiceName
	 * @param facadePortName
	 * @param locationURI
	 */
	public FacadeConfiguration(String targetNamespace, String facadeServiceName, String facadePortName, String locationURI) {
		this.targetNamespace = targetNamespace;
		this.facadeServiceName = facadeServiceName;
		this.facadePortName = facadePortName;
		this.locationURI = locationURI;
	}
	
	/**
	 * @return the facadePortName
	 */
	public String getFacadePortName() {
		return facadePortName;
	}


	/**
	 * @param facadePortName the facadePortName to set
	 */
	public void setFacadePortName(String facadePortName) {
		this.facadePortName = facadePortName;
	}


	/**
	 * @return the facadeServiceQName
	 */
	public QName getFacadeServiceQName() {
		return new QName(this.targetNamespace, this.facadeServiceName);
	}


	/**
	 * @param facadeServiceQName the facadeServiceQName to set
	 */
	public void setFacadeServiceQName(QName facadeServiceQName) {
		this.targetNamespace = facadeServiceQName.getNamespaceURI();
		this.facadeServiceName = facadeServiceQName.getLocalPart();
	}


	/**
	 * @return the targetNamespace
	 */
	public String getTargetNamespace() {
		return targetNamespace;
	}


	/**
	 * @param targetNamespace the targetNamespace to set
	 */
	public void setTargetNamespace(String targetNamespace) {
		this.targetNamespace = targetNamespace;
	}


	/**
	 * @return the facadeServiceName
	 */
	public String getFacadeServiceName() {
		return facadeServiceName;
	}


	/**
	 * @param facadeServiceName the facadeServiceName to set
	 */
	public void setFacadeServiceName(String facadeServiceName) {
		this.facadeServiceName = facadeServiceName;
	}



	/**
	 * @return the locationURI
	 */
	public String getLocationURI() {
		return locationURI;
	}

	/**
	 * @param locationURI the locationURI to set
	 */
	public void setLocationURI(String locationURI) {
		this.locationURI = locationURI;
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
	 * serviceName-sn;portName-pn;http://www.soap.com;targetNameSpace-tns 
	 * @return
	 */
	public String retrieveValuesAsString() {
		StringBuffer buffer = new StringBuffer("");
		buffer.append(this.facadeServiceName+";");
		buffer.append(this.facadePortName+";");
		buffer.append(this.locationURI+";");
		buffer.append(this.targetNamespace+";");
		return buffer.toString();
		
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}


}
