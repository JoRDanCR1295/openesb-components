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
 * @(#)ManagementService.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.services.management;


/**
 *
 * @author ylee
 */
public interface ManagementService {
    
    
    /**
     * get the state of the component
     */
    public String getState(String componentName, String componentType);
    
    /**
     * starts (service engine, binding component, service assembly, service unit, endpoint)
     * @param componentName name of the component
     */
    public String start(String componentName, String componentType);

    /**
     * stop ( service engine, binding component, service assembly, service unit, endpoint)
     * @param componentName name of the component
     */
    public String stop(String componentName, String componentType);

    /**
     * shutdowm ( service engine, binding component,service assembly, service unit, endpoint)
     * @param componentName name of the component
     */
    public String shutdown(String componentName, String componentType);

    /**
     * shutdowmForce (service engine, binding component,service assembly, service unit, endpoint)
     * @param componentName name of the component
     */
    public String shutdownForce(String componentName, String componentType);

    public String suspend(String componentName, String componentType);

    public String resume(String componentName, String componentType);
    
    
}
