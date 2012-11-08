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
 * @(#)AdminstrationService.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.test.framework.container;

/**
 *
 * Inteface for JBI runtime administration service
 */
public interface AdministrationService {
       
    /**
     * Deploy the service assembly
     * 
     * @param zipFileName The full file path to SA artifact.
     * @return The name of the service assembly that was deployed.
     * @throws com.sun.jbi.test.framework.container.DeploymentException upon error.
     */
    public String deployServiceAssembly (String zipFileName) throws DeploymentException;
    
    /**
     * Stops the service assembly.
     * 
     * @param saName The service assembly name.
     * @throws com.sun.jbi.test.framework.container.DeploymentException upon error.
     */
    public void stopServiceAssembly (String saName) throws DeploymentException;
    
    /**
     * Deploy the service assembly.
     * 
     * @param saName The service assembly name.
     * @throws com.sun.jbi.test.framework.container.DeploymentException upon error.
     */
    public void startServiceAssembly (String saName) throws DeploymentException;
    
    /**
     * Shut down the service assembly.
     * 
     * @param saName The service assembly name.
     * @throws com.sun.jbi.test.framework.container.DeploymentException upon error.
     */    
    public void shutdownServiceAssembly (String saName) throws DeploymentException;
    
    /**
     * Deploy the service assembly.
     * 
     * @param saName The service assembly name.
     * @throws com.sun.jbi.test.framework.container.DeploymentException upon error.
     */
    public void undeployServiceAssembly (String saFilsaNameeName) throws DeploymentException;
}
