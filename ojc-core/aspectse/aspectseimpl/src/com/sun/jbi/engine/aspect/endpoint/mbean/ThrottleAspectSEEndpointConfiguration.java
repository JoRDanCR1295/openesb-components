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
 * @(#)ThrottleAspectSEEndpointConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint.mbean;

import com.sun.jbi.engine.aspect.endpoint.handler.AspectConstants;
import com.sun.jbi.engine.aspect.endpoint.mbean.ThrottleAspectSEEndpointConfigurationMbean;
import com.sun.jbi.engine.aspect.utils.Description;
import javax.management.AttributeChangeNotification;
import javax.management.MBeanException;
import javax.management.NotCompliantMBeanException;
import javax.management.Notification;
import javax.naming.directory.InvalidAttributeValueException;

/**
 *
 * @author Manish
 */
public class ThrottleAspectSEEndpointConfiguration extends AspectSEEndpointConfiguration implements ThrottleAspectSEEndpointConfigurationMbean  {
    
    private String throttlepollTimer, throttleRate, maxThrottleQueueSize;
    
    /** Creates a new instance of ThrottleAspectSEEndpointConfiguration */
    public ThrottleAspectSEEndpointConfiguration(String throttlepollTimer, String throttleRate, String maxThrottleQueueSize) throws NotCompliantMBeanException
    {
        super(ThrottleAspectSEEndpointConfigurationMbean.class);
        this.throttlepollTimer = throttlepollTimer;
        this.throttleRate = throttleRate;
        this.maxThrottleQueueSize = maxThrottleQueueSize;
    }

    @Description("get Throttle Thread Polling Rate")
    public String getThrottlePollTimer() throws InvalidAttributeValueException, MBeanException {
        return this.throttlepollTimer; 
    }

    @Description("set Throttle Thread Polling Rate")
    public void setThrottlePollTimer(String pollTimer) throws InvalidAttributeValueException, MBeanException {
        if (pollTimer == null)
        {
            return;
        }
        String newValue = pollTimer;
        String oldValue = this.getThrottlePollTimer();
        this.throttlepollTimer = pollTimer;
        
        // Notify listeners of this change
        long sequenceNumber = 0L;
        String msg = "ThrottleAspectSEEndpoint.ThrottleThreadPollTimer_Attribute_changed";
        Notification notification = new AttributeChangeNotification(this, sequenceNumber, System.currentTimeMillis(), msg, AspectConstants.THROTTLE_POLLING_TIMER, String.class.getName(), oldValue, newValue);
        notificationBroadcaster.sendNotification(notification);        
    }

    @Description("get Throttle Rate")
    public String getThrottleRate() throws InvalidAttributeValueException, MBeanException {
        return this.throttleRate;
    }

    @Description("set Throttle Rate")
    public void setThrottleRate(String throttleRate) throws InvalidAttributeValueException, MBeanException {
        if (throttleRate == null)
        {
            return;
        }
        String newValue = throttleRate;
        String oldValue = this.getThrottleRate();
        this.throttleRate = throttleRate;
        
        // Notify listeners of this change
        long sequenceNumber = 0L;
        String msg = "ThrottleAspectSEEndpoint.ThrottleRate_Attribute_changed";
        Notification notification = new AttributeChangeNotification(this, sequenceNumber, System.currentTimeMillis(), msg, AspectConstants.THROTTLE_RATE, String.class.getName(), oldValue, newValue);
        notificationBroadcaster.sendNotification(notification);        
    }
    
    @Description("get Max Throttle Queue Size")
    public String getMaxThrottleQueueSize() throws InvalidAttributeValueException, MBeanException {
        return this.maxThrottleQueueSize;
    }

    @Description("set Max Throttle Queue Size")
    public void setMaxThrottleQueueSize(String maxThrottleQueueSize) throws InvalidAttributeValueException, MBeanException {
        if (maxThrottleQueueSize == null)
        {
            return;
        }
        String newValue = maxThrottleQueueSize;
        String oldValue = this.getMaxThrottleQueueSize();
        this.maxThrottleQueueSize = maxThrottleQueueSize;
        
        // Notify listeners of this change
        long sequenceNumber = 0L;
        String msg = "ThrottleAspectSEEndpoint.MaxThrottleQueueSize_Attribute_changed";
        Notification notification = new AttributeChangeNotification(this, sequenceNumber, System.currentTimeMillis(), msg, AspectConstants.MAX_THROTTLE_QUEUE_SIZE, String.class.getName(), oldValue, newValue);
        notificationBroadcaster.sendNotification(notification);        
    }    
    
}
