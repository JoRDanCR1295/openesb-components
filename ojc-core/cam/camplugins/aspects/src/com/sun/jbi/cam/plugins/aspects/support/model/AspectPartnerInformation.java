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
 * @(#)AspectPartnerInformation.java 
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
import java.util.ArrayList;
import java.util.List;

/**
 * @author graj
 *
 */
public class AspectPartnerInformation implements Serializable {
	private static final long serialVersionUID = 1L;

	List<PartnerConfiguration> partnerConfigurationList = new ArrayList<PartnerConfiguration>();
	/**
	 * 
	 */
	public AspectPartnerInformation() {
		// TODO Auto-generated constructor stub
	}

	/**
	 * @return the partnerConfigurationList
	 */
	public List<PartnerConfiguration> getPartnerConfigurationList() {
		return partnerConfigurationList;
	}

	/**
	 * @param partnerConfigurationList the partnerConfigurationList to set
	 */
	public void setPartnerConfigurationList(
			List<PartnerConfiguration> partnerConfigurationList) {
		this.partnerConfigurationList = partnerConfigurationList;
	}
	
	/**
	 * @param partnerConfiguration the partnerConfiguration to add
	 */
	public void addPartnerConfigurationList(
			PartnerConfiguration partnerConfiguration) {
		this.partnerConfigurationList.add(partnerConfiguration);
	}
	

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
