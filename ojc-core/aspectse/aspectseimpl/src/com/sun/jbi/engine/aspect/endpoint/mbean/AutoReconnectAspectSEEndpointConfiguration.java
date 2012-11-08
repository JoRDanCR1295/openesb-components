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
 * @(#)AutoReconnectAspectSEEndpointConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint.mbean;

import com.sun.jbi.engine.aspect.endpoint.handler.AspectConstants;
import com.sun.jbi.engine.aspect.utils.Description;
import javax.management.AttributeChangeNotification;

import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;
import javax.management.NotCompliantMBeanException;
import javax.management.Notification;

public class AutoReconnectAspectSEEndpointConfiguration extends
        AspectSEEndpointConfiguration implements
        AutoReconnectAspectSEEndpointConfigurationMbean {
    
    private int timeout;
    private long rate;
    
    public AutoReconnectAspectSEEndpointConfiguration(String retryTimeout, String retryRate) throws NotCompliantMBeanException {
        super(AutoReconnectAspectSEEndpointConfigurationMbean.class);
        try {
            // TODO Auto-generated constructor stub
            timeout = Integer.parseInt(retryTimeout);
        } catch (NumberFormatException ex) {
            timeout = 10;
        }
        try {
            rate = Long.parseLong(retryRate);
        } catch (NumberFormatException ex) {
            rate = 1000;
        }
    }
    
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    
    @Description("get auto reconnect interval")
    public String getRetryRate() throws InvalidAttributeValueException, MBeanException {
        return String.valueOf(this.rate);
    }
    
    @Description("set auto reconnect interval")
    public void setRetryRate(String rate) throws InvalidAttributeValueException,
            MBeanException {
        if (rate == null) {
            return;
        }
        String newValue = rate;
        String oldValue = this.getRetryRate();
        this.rate = (new Integer(rate)).longValue();
        
        // Notify listeners of this change
        long sequenceNumber = 0L;
        String msg = "AutoReconnectAspectSEEndpoint.RetryRate_Attribute_changed";
        Notification notification = new AttributeChangeNotification(this,
                sequenceNumber, System.currentTimeMillis(), msg,
                AspectConstants.RETRY_PROP_RATE, String.class.getName(), oldValue, newValue);
        notificationBroadcaster.sendNotification(notification);
        
    }
    
    @Description("get timeout")
    public String getTimeout() throws InvalidAttributeValueException, 
            MBeanException {
        return String.valueOf(this.timeout);
    }
    
    @Description("set timeout")
    public void setTimeout(String timeout) throws InvalidAttributeValueException, 
            MBeanException {
        if (timeout == null) {
            return;
        }
        String newValue = timeout;
        String oldValue = this.getTimeout();
        this.timeout = (new Integer(timeout)).intValue();
        
        // Notify listeners of this change
        long sequenceNumber = 0L;
        String msg = "AutoReconnectAspectSEEndpoint.RetryTimeout_Attribute_changed";
        Notification notification = new AttributeChangeNotification(this,
                sequenceNumber, System.currentTimeMillis(), msg,
                AspectConstants.RETRY_PROP_TIMEOUT, String.class.getName(), oldValue, newValue);
        notificationBroadcaster.sendNotification(notification);        
    }    
}
