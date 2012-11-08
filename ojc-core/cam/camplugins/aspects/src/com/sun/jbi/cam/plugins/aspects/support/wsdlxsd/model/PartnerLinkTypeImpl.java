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
 * @(#)PartnerLinkTypeImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.model;

import java.io.Serializable;

import javax.xml.namespace.QName;

import com.sun.jbi.cam.plugins.aspects.common.XmlConstants;

/**
 * @author graj
 *
 */
public class PartnerLinkTypeImpl extends AbstractWSDLElement implements
		Serializable, PartnerLinkType {
	private static final long serialVersionUID = 1L;
	
	String name;
	Role role;
	boolean required = true;
	QName elementType = XmlConstants.WSDL_PARTNERLINKTYPE_QNAME_VALUE;

	/**
	 * 
	 */
	public PartnerLinkTypeImpl() {
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.model.PartnerLinkType#getName()
	 */
	public String getName() {
		// TODO Auto-generated method stub
		return this.name;
	}

	/* (non-Javadoc)
	 * @see com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.model.PartnerLinkType#getRole()
	 */
	public Role getRole() {
		// TODO Auto-generated method stub
		return this.role;
	}

	/* (non-Javadoc)
	 * @see javax.wsdl.extensions.ExtensibilityElement#getElementType()
	 */
	public QName getElementType() {
		return this.elementType;
	}

	/* (non-Javadoc)
	 * @see javax.wsdl.extensions.ExtensibilityElement#getRequired()
	 */
	public Boolean getRequired() {
		// TODO Auto-generated method stub
		return Boolean.valueOf(this.required);
	}

	public void setName(String name) {
		this.name = name;
	}

	public void setRole(Role role) {
		this.role = role;
	}

	/* (non-Javadoc)
	 * @see javax.wsdl.extensions.ExtensibilityElement#setElementType(javax.xml.namespace.QName)
	 */
	public void setElementType(QName elementType) {
		this.elementType = elementType;
	}

	/* (non-Javadoc)
	 * @see javax.wsdl.extensions.ExtensibilityElement#setRequired(java.lang.Boolean)
	 */
	public void setRequired(Boolean required) {
		this.required = Boolean.valueOf(required);
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}


}
