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
 * @(#)ThrottleAspectSEEndpointConfigurationMbean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint.mbean;

import com.sun.jbi.engine.aspect.utils.Description;
import javax.management.MBeanException;
import javax.naming.directory.InvalidAttributeValueException;

/**
 *
 * @author Manish
 */
public interface ThrottleAspectSEEndpointConfigurationMbean {
    
    @Description("get Throttle Thread Polling Rate")    
    public String getThrottlePollTimer()throws InvalidAttributeValueException,MBeanException ;
	
    @Description("set Throttle Thread Polling Rate")
    public void setThrottlePollTimer(String pollTimer) throws InvalidAttributeValueException,MBeanException ;
        
    @Description("get Throttle Rate")
    public String getThrottleRate()throws InvalidAttributeValueException,MBeanException ;
	
    @Description("set Throttle Rate")
    public void setThrottleRate(String throttleRate) throws InvalidAttributeValueException,MBeanException ;
    
    @Description("get Max Throttle Queue Size")
    public String getMaxThrottleQueueSize()throws InvalidAttributeValueException,MBeanException ;
	
    @Description("set Max Throttle Queue Size")
    public void setMaxThrottleQueueSize(String maxThrottleQueueSize) throws InvalidAttributeValueException,MBeanException ;    
    
}
