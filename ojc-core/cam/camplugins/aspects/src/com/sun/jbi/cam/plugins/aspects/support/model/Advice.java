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
 * @(#)Advice.java 
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



/*
 * <logging>
	<config>
		<property name="level" value="WARNING" />
		<property name="rotation-policy" value="WEEKLY" />
		<property name="log-file" value="/tmp/loggingse.log" />
	</config>
</logging>
 */
/**
 * @author graj
 *
 */
public class Advice implements Serializable {
    private static final long serialVersionUID = 1L;
    AspectType aspectType;
    AdviceConfiguration adviceConfiguration;
    

	/**
	 * 
	 */
	public Advice() {
		// TODO Auto-generated constructor stub
	}
	
	

	/**
	 * @return the aspectType
	 */
	public AspectType getAspectType() {
		return aspectType;
	}



	/**
	 * @param aspectType the aspectType to set
	 */
	public void setAspectType(AspectType aspectType) {
		this.aspectType = aspectType;
	}



	/**
	 * @return the adviceConfiguration
	 */
	public AdviceConfiguration getAdviceConfiguration() {
		return adviceConfiguration;
	}



	/**
	 * @param adviceConfiguration the adviceConfiguration to set
	 */
	public void setAdviceConfiguration(AdviceConfiguration adviceConfiguration) {
		this.adviceConfiguration = adviceConfiguration;
	}
	
	public void dump() {
		System.out.println("Advice Type is: "+this.aspectType);
	}



	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
