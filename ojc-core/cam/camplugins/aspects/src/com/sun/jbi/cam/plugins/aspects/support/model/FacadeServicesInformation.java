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
 * @(#)FacadeServicesInformation.java 
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
public class FacadeServicesInformation implements Serializable {
	private static final long serialVersionUID = 1L;
	List<FacadeConfiguration> facadeConfigurationList = new ArrayList<FacadeConfiguration>();
	
	/**
	 * 
	 */
	public FacadeServicesInformation() {
		// TODO Auto-generated constructor stub
	}

	/**
	 * @return the facadeConfigurationList
	 */
	public List<FacadeConfiguration> getFacadeConfigurationList() {
		return facadeConfigurationList;
	}
	
	/**
	 * @return the facadeConfiguration
	 */
	public void addFacadeConfiguration(FacadeConfiguration facadeConfiguration) {
		this.facadeConfigurationList.add(facadeConfiguration);
	}
	

	/**
	 * @param facadeConfigurationList the facadeConfigurationList to set
	 */
	public void setFacadeConfigurationList(
			List<FacadeConfiguration> facadeConfigurationList) {
		this.facadeConfigurationList = facadeConfigurationList;
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
