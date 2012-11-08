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
 * @(#)DeploymentService.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.services.deployment;

import java.util.List;
import java.util.Properties;

/**
 *
 * @author ylee
 */
public interface DeploymentService {
    
    
       /**
     * installs component (service engine, binding component)
     * @return lifecycle object name string for service engine or binding component.
     * @param zipFilePath archive file in a zip format
     * @param params Properties Object
     */
    public String installComponent(String zipFilePath, Properties paramProps);

    /**
     * installs component (service engine, binding component)
     * @return lifecycle object name string for service engine or binding component.
     * @param zipFilePath archive file in a zip format
     */
    public String installComponent(String zipFilePath);

    /**
     * uninstalls component (service engine, binding component)
     * @param componentName name of the component
     */
    public String uninstallComponent(String componentName);

    /**
     * installs shared namespace
     * @return shared library name object name string for service engine or binding component.
     * @param zipFilePath archive file in a zip format
     */
    public String installSharedLibrary(String zipFilePath);

    /**
     * uninstalls shared library
     * @param sharedLibraryName name of the shared library
     */
   public String uninstallSharedLibrary(String sharedLibraryName);


    
    /**
     * deploys service assembly
     * @return deployment status result as a management message xml
     * @param zipFilePath fie path
     */
    public String deployServiceAssembly(String zipFilePath);

    /**
     * start service assembly
     * @param serviceAssemblyName name of the service assembly
     */
    public String startServiceAssembly(String serviceAssemblyName);

    /**
     * stop service assembly
     * @param serviceAssemblyName name of the service assembly
     */
    public String stopServiceAssembly(String serviceAssemblyName);

    /**
     * shutdown service assembly
     * @param serviceAssemblyName name of the service assembly
     */
    public String shutdownServiceAssembly(String serviceAssemblyName);


    /**
     * undeploys service assembly
     * @param serviceAssemblyName name of the service assembly
     */
    public String undeployServiceAssembly(String serviceAssemblyName);
    
    /**
     * deploy plugin to app server
     * @param pluginName - full path name
     * @param targetName - server target name
     */
    public String deployPlugin(String pluginName);
     
    /**
     * undeploy plugin from app server
     * @param pluginName
     * @param targetName - server target name     *
     */
    public String undeployPlugin(String pluginName);
    
    /**
     * get a list of plugins
     */
    public List<String> getPlugins();

    
}
