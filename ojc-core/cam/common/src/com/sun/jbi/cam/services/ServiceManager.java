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
 * @(#)ServiceManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.services;

import com.sun.jbi.cam.services.configuration.ConfigurationService;
import com.sun.jbi.cam.services.deployment.DeploymentService;
import com.sun.jbi.cam.services.management.ManagementService;
import com.sun.jbi.cam.services.performance.PerformanceService;
import com.sun.jbi.cam.services.administration.AdministrationService;
import com.sun.jbi.cam.services.statistics.StatisticsService;
import java.io.Serializable;
import java.util.logging.Logger;

import com.sun.jbi.cam.connectors.ServerConnector;
import com.sun.jbi.cam.connectors.LocalServerConnector;
import com.sun.jbi.cam.common.GenericConstants;

/**
 * ServiceManager - singleton - provides the list of supported services
 *  e.g Configuration, Performance, Administration, Management
 *
 * @author ylee
 */
public class ServiceManager implements Serializable {

    private String hostName;
    private String httpAdminPort;
    private String userName;
    private String password;

    private ServerConnector connector;
    
    private static ServiceManager serviceManager;

    private static Logger logger = Logger.getLogger(ServiceManager.class.getName());

    
    private ServiceManager() {
        this(null);
    }

    public static ServiceManager getInstance() {
        if ( serviceManager==null ) {
            serviceManager = new ServiceManager();
        }
        return serviceManager;
    }
    
    
    public static ServiceManager getInstance(ServerConnector serverConnector) {
        if ( serviceManager==null ) {
            serviceManager = new ServiceManager(serverConnector);
        }
        return serviceManager;
    }
    
    
    /**
     * Creates a new instance of ServiceManager
     * @param connector
     */
    private ServiceManager(ServerConnector connector) {
        if ( connector==null ) {
            this.connector = new LocalServerConnector();
        } else {
            this.connector = connector;
        }
        this.hostName = this.connector.getHostName();
        this.httpAdminPort = this.connector.getPort();
        this.userName = this.connector.getUserName();
        this.password = this.connector.getPassword();
    }
    
    
   public ConfigurationService getConfigurationService(String targetName) {
       // todo - specific instance should be based on server type
       return ServiceFactory.getConfigurationService(this.connector,targetName);
   }

   public PerformanceService getPerformanceService(String targetName) {
       // todo - specific instance should be based on server type
       return ServiceFactory.getPerformanceService(this.connector,targetName);
   }
   
   public ManagementService getManagementService(String targetName) {
       // todo - specific instance should be based on server type
       return ServiceFactory.getManagementService(this.connector,targetName);
   }
   
   public AdministrationService getAdministrationService(String targetName) {
       // todo - specific instance should be based on server type
       return ServiceFactory.getAdministrationService(this.connector,targetName);
   }
   
   public StatisticsService getStatisticsService(String targetName) {
       // todo - specific instance should be based on server type
       return ServiceFactory.getStatisticsService(this.connector,targetName);
   }
   
   public DeploymentService getDeploymentService(String targetName) {
       // todo - specific instance should be based on server type
       return ServiceFactory.getDeploymentService(this.connector,targetName);
   }
   

}
