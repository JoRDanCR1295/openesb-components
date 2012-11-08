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
 * @(#)OpenESBAdministrationService.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.test.framework.container.openesb;

import javax.management.remote.JMXServiceURL;

import com.sun.jbi.ui.common.JBIAdminCommands;
import com.sun.jbi.ui.common.JBIArchive;
import com.sun.jbi.ui.common.ServiceAssemblyDD;
import com.sun.jbi.ui.client.JBIAdminCommandsClientFactory;

import com.sun.jbi.component.test.framework.container.AdministrationService;
import com.sun.jbi.component.test.framework.container.AdministrationServiceException;
import com.sun.jbi.component.test.framework.container.DeploymentException;

/**
 *
 * OpenESB administration service
 */
public class OpenESBAdministrationService implements AdministrationService {

    private JBIAdminCommands jbiCommands;
    private String targetName;
    
    public OpenESBAdministrationService (OpenESBAdminServiceConnectionSpec connSpec,
                                         String targetName)
            throws AdministrationServiceException {
        try {
            jbiCommands = JBIAdminCommandsClientFactory.getInstance(connSpec.getHost(),
                                                                    connSpec.getPort(),
                                                                    connSpec.getUsername(),
                                                                    connSpec.getPassword());
            this.targetName = targetName;
        } catch (Throwable t) {
            throw new AdministrationServiceException (t);
        }
    }
    
    public String deployServiceAssembly(String zipFileName) throws DeploymentException {
        try {
            jbiCommands.deployServiceAssembly(zipFileName, targetName);
            JBIArchive sa = new JBIArchive(zipFileName);
            return ((ServiceAssemblyDD)sa.getJbiDescriptor()).getName();
        } catch (Throwable t) {
            throw new DeploymentException (t);
        }
    }

    public void stopServiceAssembly(String saName) throws DeploymentException {
        try {
            jbiCommands.stopServiceAssembly(saName, targetName);
        } catch (Throwable t) {
            throw new DeploymentException (t);
        }
    }

    public void startServiceAssembly(String saName) throws DeploymentException {
        try {
            jbiCommands.startServiceAssembly(saName, targetName);
        } catch (Throwable t) {
            throw new DeploymentException (t);
        }
    }

    public void undeployServiceAssembly(String saName) throws DeploymentException {
        try {
            jbiCommands.undeployServiceAssembly(saName, targetName);
        } catch (Throwable t) {
            throw new DeploymentException (t);
        }
    }

    public void shutdownServiceAssembly(String saName) throws DeploymentException {
        try {
            jbiCommands.shutdownServiceAssembly(saName, targetName);
        } catch (Throwable t) {
            throw new DeploymentException (t);
        }
    }
    
    public boolean isJBIRuntimeEnabled() throws DeploymentException {
        try {
            return jbiCommands.isJBIRuntimeEnabled();
        } catch (Throwable t) {
            throw new DeploymentException (t);
        }
    }
}
