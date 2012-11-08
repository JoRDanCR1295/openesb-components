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
 * @(#)AdministrationService.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.services.administration;

import com.sun.jbi.cam.model.management.JBIComponentStatus;
import com.sun.jbi.cam.model.management.JBIServiceAssemblyStatus;
import java.util.List;
import javax.management.ObjectName;

/**
 *
 * @author ylee
 * @author graj
 */
public interface AdministrationService {

    public void prepare(String targetName);
    
    /**
     * @return Returns the bindingComponentList.
     */
    public List<JBIComponentStatus> getBindingComponentList();

    /**
     * @return Returns the serviceAssemblyList.
     */
    public List<JBIServiceAssemblyStatus> getServiceAssemblyList();

    /**
     * @return Returns the serviceEngineList.
     */
    public List<JBIComponentStatus> getServiceEngineList();

    /**
     * @return Returns the sharedNamespacesList.
     */
    public List<JBIComponentStatus> getSharedNamespacesList();
    
    
    public String getComponentType(String name);
    
    public String installComponent(String component);
    
    public String deployServiceAssembly(String sa);
    
    // 
    public Object invoke(ObjectName objectName, String operationName);
    
    public Object invoke(ObjectName objectName, String operationName, Object[] parameters);
    
    public String getProviderUrl(String name,String type,
                                    String componentName,
                                    String componentType);    
    
    public String getProviderUrl(String componentType);    
    
    
    /**
     * Returns the jbi.xml Deployment Descriptor for a Service Assembly.
     * 
     * @param serviceAssemblyName - the name of the Service Assembly
     * @return the jbi.xml deployment descriptor of the archive
     */
    public String getServiceAssemblyDeploymentDescriptor(
            String serviceAssemblyName) ;

    /**
     * Returns the jbi.xml Deployment Descriptor for a Service Unit.
     * 
     * @param serviceAssemblyName - the name of the Service Assembly
     * @param serviceUnitName -  the name of the Service Unit
     * @return the jbi.xml deployment descriptor of the archive
     */
    public String getServiceUnitDeploymentDescriptor(
            String serviceAssemblyName, String serviceUnitName);
    
}
