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
 * @(#)PartnerConfiguration.java 
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
public class PartnerConfiguration implements Serializable {
	private static final long serialVersionUID = 1L;
	
	String partnerLinkTypeName;
	QName partnerLinkQName;
	String roleName;
	QName portTypeQName;

	/**
	 * 
	 */
	public PartnerConfiguration() {
		// TODO Auto-generated constructor stub
	}

	/**
	 * @param partnerLinkTypeName
	 * @param partnerLinkQName
	 * @param roleName
	 * @param portTypeQName
	 */
	public PartnerConfiguration(String partnerLinkTypeName, QName partnerLinkQName, String roleName, QName portTypeQName) {
		this.partnerLinkTypeName = partnerLinkTypeName;
		this.partnerLinkQName = partnerLinkQName;
		this.roleName = roleName;
		this.portTypeQName = portTypeQName;
	}

	/**
	 * @return the partnerLinkQName
	 */
	public QName getPartnerLinkQName() {
		return partnerLinkQName;
	}

	/**
	 * @param partnerLinkQName the partnerLinkQName to set
	 */
	public void setPartnerLinkQName(QName partnerLinkQName) {
		this.partnerLinkQName = partnerLinkQName;
	}

	/**
	 * @return the partnerLinkTypeName
	 */
	public String getPartnerLinkTypeName() {
		return partnerLinkTypeName;
	}

	/**
	 * @param partnerLinkTypeName the partnerLinkTypeName to set
	 */
	public void setPartnerLinkTypeName(String partnerLinkTypeName) {
		this.partnerLinkTypeName = partnerLinkTypeName;
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
	 * @return the roleName
	 */
	public String getRoleName() {
		return roleName;
	}

	/**
	 * @param roleName the roleName to set
	 */
	public void setRoleName(String roleName) {
		this.roleName = roleName;
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
