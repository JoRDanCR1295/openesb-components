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
 * @(#)PartnerLinkType.java 
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

import javax.wsdl.extensions.ExtensibilityElement;

/**
 * @author graj
 *
 */
public interface PartnerLinkType extends Serializable, ExtensibilityElement {
	
	  /**
	   * Set the name of this PartnerLinkType.
	   *
	   * @param name the desired name
	   */
	  public void setName(String name);

	  /**
	   * Get the name of this PartnerLinkType.
	   *
	   * @return the role name
	   */
	  public String getName();
	
	  /**
	   * Set the Role for PartnerLinkType.
	   *
	   * @param role the port type associated with this PartnerLinkType
	   */
	  public void setRole(Role role);	  
	
	  /**
	   * Get the Role this is PartnerLinkType.
	   *
	   * @return the associated role
	   */
	  public Role getRole();
}
