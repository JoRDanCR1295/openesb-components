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
 * @(#)CacheAspectSEEndpointConfigurationMbean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint.mbean;

import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;

import com.sun.jbi.engine.aspect.utils.Description;

/**
 * @author Sujit Biswas
 *
 */
public interface CacheAspectSEEndpointConfigurationMbean {
	
	@Description("get max no of entries in the cache")
	public String getMaximumEntries()throws InvalidAttributeValueException,MBeanException ;
	
	@Description("set max no of entries in the cache")
	public void setMaximumEntries(String maxEntries) throws InvalidAttributeValueException,MBeanException ;
	
	@Description("set the  caching strategy   in the cache")
	public void setCachingStrategy(String cachingStrategy) throws InvalidAttributeValueException,MBeanException;
	
	@Description("get the  caching strategy   for the cache")
	public String getCachingStrategy() throws InvalidAttributeValueException,MBeanException;
	
	@Description("purge the cache")
	public void purge() throws InvalidAttributeValueException,MBeanException;
	
	

}
