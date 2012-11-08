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
 * @(#)AspectSEComponentConfigurationMBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect;

import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;

import com.sun.jbi.engine.aspect.utils.Description;


/**
 * @author Sujit Biswas
 *
 */
public interface AspectSEComponentConfigurationMBean {

    @Description("get max thread count")
    public String getMaxThreadCount()throws InvalidAttributeValueException, MBeanException;
    
    @Description("set max thread count")
    public void setMaxThreadCount(String threadCount) throws InvalidAttributeValueException, MBeanException;

    /**
     * 
     * @return 
     */
    @Description("Retrieves the Configuration Display Schema")
    public String retrieveConfigurationDisplaySchema();
    
    /**
     * 
     * @return 
     */
    @Description("Retrieves the Configuration Display Data")
    public String retrieveConfigurationDisplayData();

}
