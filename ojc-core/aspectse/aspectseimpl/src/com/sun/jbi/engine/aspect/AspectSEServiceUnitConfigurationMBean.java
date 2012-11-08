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
 * @(#)AspectSEServiceUnitConfigurationMBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect;

import com.sun.jbi.engine.aspect.utils.Description;
import com.sun.jbi.engine.aspect.utils.ParameterName;


public interface AspectSEServiceUnitConfigurationMBean {

    @Description("Set the Aspect policy for the Service Unit")
    public void setPolicy(@ParameterName("The Aspect Policy as a string")
    String somePolicy);

    @Description("Get the Aspect policy for the Service Unit")
    public String getPolicy();
    
    @Description("Get the Aspect Policy Schema")
    public String retrievePolicySchema();
    
    @Description("Get the Service Unit Folder for the Service Unit")
    public String retrieveServiceUnitFolder();
    
}
