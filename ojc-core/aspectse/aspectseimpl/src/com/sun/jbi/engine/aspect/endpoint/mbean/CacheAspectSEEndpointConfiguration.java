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
 * @(#)CacheAspectSEEndpointConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint.mbean;

import javax.management.AttributeChangeNotification;
import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;
import javax.management.NotCompliantMBeanException;
import javax.management.Notification;

import com.sun.jbi.engine.aspect.endpoint.support.CacheStrategyType;

/**
 * @author Sujit Biswas
 * 
 */
public class CacheAspectSEEndpointConfiguration extends
		AspectSEEndpointConfiguration implements
		CacheAspectSEEndpointConfigurationMbean {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	static final String CACHING_STRATEGY_KEY = "CachingStrategy";

	static final String MAXIMUM_ENTRIES_KEY = "MaximumEntries";

	private CacheStrategyType cacheStrategyType = CacheStrategyType.FirstInFirstOut;

	private Integer maximumEntries = new Integer(100);

	public CacheAspectSEEndpointConfiguration(String maximumEntries2, String cachingStrategy)
			throws NotCompliantMBeanException {
		super(CacheAspectSEEndpointConfigurationMbean.class);
		
		if(cachingStrategy.equals(CacheStrategyType.GenericCache.getCachingStrategy())){
			cacheStrategyType = CacheStrategyType.GenericCache;
		}
		maximumEntries = new Integer(maximumEntries2);
	}

	public String getMaximumEntries() throws InvalidAttributeValueException,
			MBeanException {
		return maximumEntries.toString();
	}

	public void setMaximumEntries(String maxEntries)
			throws InvalidAttributeValueException, MBeanException {
		if (maxEntries == null) {
			return;
		}
		String newValue = maxEntries;
		String oldValue = this.getMaximumEntries();
		this.maximumEntries = (new Integer(maxEntries)).intValue();

		// TODO persist the changes to the corresponding config file
		// save(mPropertiesFile);

		// Notify listeners of this change
		long sequenceNumber = 0L;
		String msg = "CacheAspectSEEndpoint.MaximumEntries_Attribute_changed";
		Notification notification = new AttributeChangeNotification(this,
				sequenceNumber, System.currentTimeMillis(), msg,
				MAXIMUM_ENTRIES_KEY, String.class.getName(), oldValue, newValue);
		notificationBroadcaster.sendNotification(notification);

	}

	public void setCachingStrategy(String cachingStrategy)
			throws InvalidAttributeValueException, MBeanException {
		if (cachingStrategy == null) {
			return;
		}
		String newValue = cachingStrategy;
		String oldValue = this.getCachingStrategy();
		if (true == cachingStrategy.equals(CacheStrategyType.FirstInFirstOut
				.getCachingStrategy())) {
			this.cacheStrategyType = CacheStrategyType.FirstInFirstOut;
		} else {
			this.cacheStrategyType = CacheStrategyType.GenericCache;
		}

		// TODO persist the changes to the corresponding config file
		// save(configFile);

		// Notify listeners of this change
		long sequenceNumber = 0L;
		String msg = "CacheAspectSEEndpoint.CacheStrategyType_Attribute_changed";
		Notification notification = new AttributeChangeNotification(this,
				sequenceNumber, System.currentTimeMillis(), msg,
				CACHING_STRATEGY_KEY, String.class.getName(), oldValue,
				newValue);
		notificationBroadcaster.sendNotification(notification);
	}

	public String getCachingStrategy() {
		return this.cacheStrategyType.getCachingStrategy();
	}

	public void purge() throws InvalidAttributeValueException, MBeanException {
		// TODO Auto-generated method stub
		;
	}

}
