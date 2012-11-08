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
 * @(#)ServiceFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.services;

import com.sun.jbi.cam.connectors.ServerConnector;
import com.sun.jbi.cam.services.administration.AdministrationService;
import com.sun.jbi.cam.services.administration.providers.SunAdministrationService;
import com.sun.jbi.cam.services.configuration.ConfigurationService;
import com.sun.jbi.cam.services.configuration.providers.SunConfigurationService;
import com.sun.jbi.cam.services.deployment.DeploymentService;
import com.sun.jbi.cam.services.deployment.providers.SunDeploymentService;
import com.sun.jbi.cam.services.management.ManagementService;
import com.sun.jbi.cam.services.management.providers.SunManagementService;
import com.sun.jbi.cam.services.performance.PerformanceService;
import com.sun.jbi.cam.services.performance.providers.SunPerformanceService;
import com.sun.jbi.cam.services.statistics.StatisticsService;
import com.sun.jbi.cam.services.statistics.providers.SunStatisticsService;

/**
 *
 * @author ylee
 */
public class ServiceFactory {
    
    /** Creates a new instance of ServiceFactory */
    public ServiceFactory() {
    }
    
    
    public static AdministrationService getAdministrationService(ServerConnector connector,String targetName) {
       // todo - specific instance based on type
       return new SunAdministrationService(connector,targetName);
    }
    
    public static ConfigurationService getConfigurationService(ServerConnector connector,String targetName) {
       // todo - specific instance based on type
       return new SunConfigurationService(connector,targetName);
    }
    
    public static PerformanceService getPerformanceService(ServerConnector connector,String targetName) {
       // todo - specific instance based on type
       return new SunPerformanceService(connector,targetName);
    }

    public static ManagementService getManagementService(ServerConnector connector,String targetName) {
       // todo - specific instance based on type
       return new SunManagementService(connector,targetName);
    }
    
    public static StatisticsService getStatisticsService(ServerConnector connector,String targetName) {
        // todo - specific instance based on type
       return new SunStatisticsService(connector,targetName);
    }
    
   public static DeploymentService getDeploymentService(ServerConnector connector,String targetName) {
       // todo - specific instance should be based on server type
       return new SunDeploymentService(connector,targetName);
   }
   
    
}
