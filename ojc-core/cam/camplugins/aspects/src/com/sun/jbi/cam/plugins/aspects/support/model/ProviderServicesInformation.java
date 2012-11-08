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
 * @(#)ProviderServicesInformation.java 
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
public class ProviderServicesInformation implements Serializable {
	private static final long serialVersionUID = 1L;
	
	List<ProviderConfiguration> providerConfigurationList = new ArrayList<ProviderConfiguration>();

	/**
	 * 
	 */
	public ProviderServicesInformation() {
		// TODO Auto-generated constructor stub
	}

	/**
	 * @return the providerConfigurationList
	 */
	public List<ProviderConfiguration> getProviderConfigurationList() {
		return providerConfigurationList;
	}

	/**
	 * @param providerConfigurationList the providerConfigurationList to set
	 */
	public void setProviderConfigurationList(
			List<ProviderConfiguration> providerConfigurationList) {
		this.providerConfigurationList = providerConfigurationList;
	}
	/**
	 * @param providerConfiguration the providerConfiguration to add
	 */
	public void addProviderConfiguration(
			ProviderConfiguration providerConfiguration) {
		this.providerConfigurationList.add(providerConfiguration);
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
