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
 * @(#)ConfigurationService.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.services.configuration;

import java.util.Map;

/**
 *
 * @author ylee
 */
public interface ConfigurationService {
    
    // define Configuration interface
    public Map<String,Object> getConfigurationProperties(String componentName, String componentType);
    
    public void setConfigurationProperties(String componentName, String componentType, Map<String,Object> props);
    
    public void setProperties(String mbeanName, Map<String,Object> props);
    
    public void setProperty(String name, Object value);
    
    public Object getProperty(String name);
     
    // define Configuration interface
    public Map<String,Object> getSUConfigurationProperties(String componentName,
        String componentType, String serviceUnitId);
    
    public String getSchema(String componentName, String componentType);
    
    public String getXmlData(String componentName, String componentType);
    
    public String[] getAttributeNames(String componentName, String componentType);
    
}
