/*
 * Copyright (c) 2007 Sun Microsystems, Inc.
 * All Rights Reserved.
 *
 * This program, and all the routines referenced herein,
 * are the proprietary properties and trade secrets of
 * Sun Microsystems.
 *
 * Except as provided for by license agreement, this
 * program shall not be duplicated, used, or disclosed
 * without  written consent signed by an officer of
 * Sun Microsystems.
 */

package com.sun.caps.management.common;

import java.io.IOException;
import java.util.Set;

import javax.management.MBeanServerConnection;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;

import com.sun.caps.management.impl.log.LogConstants;



/**
 * @author graj
 *
 */
public final class CAPSJMXObjectNames {

    /** JMX Domain name for the jbi jmx server */
    public static final String JMX_JBI_DOMAIN                   = "com.sun.jbi";

    /** JMX Domain name for the ebi jmx server */
    public static final String JMX_EBI_DOMAIN                   = "com.sun.ebi";

    /** JBI MBean Object type */
    public static final String CONTROL_TYPE_KEY                 = "ControlType";

    /** JBI MBean Object type value */
    public static final String DEPLOYER_CONTROL_TYPE_VALUE      = "Deployer";

    /** JBI MBean Object type value */
    public static final String INSTALLER_CONTROL_TYPE_VALUE     = "Installer";

    /** JBI MBean Object type value */
    public static final String LIFECYCLE_CONTROL_TYPE_VALUE     = "Lifecycle";

    /** JBI MBean Object type value */
    public static final String CONTROLLER_CONTROL_TYPE_VALUE    = "Controller";

    /** JBI MBean Object type value */
    public static final String ADMIN_SERVICE_CONTROL_TYPE_VALUE = "AdministrationService";

    /** JBI MBean Object type */
    public static final String COMPONENT_TYPE_KEY               = "ComponentType";

    /** JBI MBean Object type value */
    public static final String SYSTEM_COMPONENT_TYPE_VALUE      = "System";

    /** JBI MBean Object type value */
    public static final String INSTALL_COMPONENT_TYPE_VALUE     = "Installed";

    /** JBI MBean Object type */
    public static final String INSTALLED_TYPE_KEY               = "InstalledType";

    /** JBI MBean Object type */
    public static final String ENGINE_INSTALLED_TYPE_VALUE      = "Engine";

    /** JBI MBean Object type */
    public static final String BINDING_INSTALLED_TYPE_VALUE     = "Binding";

    /** JBI MBean Object type name */
    public static final String COMPONENT_ID_KEY                 = "ComponentName";

    /** JBI Service Names */
    /** JBI MBean Object type name */
    public static final String SERVICE_NAME_KEY                 = "ServiceName";

    /** Service Names */
    public static final String ADMIN_SERVICE                    = "AdministrationService";

    /** Service Names */
    public static final String DEPLOYMENT_SERVICE               = "DeploymentService";

    /** Service Names */
    public static final String INSTALLATION_SERVICE             = "InstallationService";

    /** Service Names */
    public static final String CONFIGURATION_SERVICE            = "ConfigurationServiceImpl";

    /** Service Names */
    public static final String JBI_ADMIN_UI_SERVICE             = "JbiAdminUiService";

    /** Service Names */
    public static final String JBI_REFERENCE_ADMIN_UI_SERVICE   = "JbiReferenceAdminUiService";

    public static final String TARGET_KEY                       = "Target";

    public static final String SERVICE_TYPE_KEY                 = "ServiceType";
    
    public static final String IDENTIFICATION_NAME_KEY          = "IdentificationName";

    // public static final String COMPONENT_LIFECYCLE_KEY =
    // "ComponentLifecycle";
    public static final String COMPONENT_LIFECYCLE_KEY          = "ComponentLifeCycle";

    /** jbi jmx domain */
    private static String      JBIJMXDOMAIN                    = null;

    /** Enterprise Business Integration jmx domain */
    private static String      EBIJMXDOMAIN                    = null;

    /** jbi reference admin object name */
    private static ObjectName  JBICOMMONCLIENTMBEAN      = null;

    static final String OBJECT_NAME_PREFIX = "com.sun.ebi:ServiceType=Status,InstallationType=";
    static final String OBJECT_NAME_SUFFIX = "," + IDENTIFICATION_NAME_KEY +"=";

    static final String COMPONENT_TYPE_BINDING = "bindingComponents";
    static final String COMPONENT_TYPE_ENGINE = "engineComponents";


    public static final String EM_DOMAIN = "EM";

    public final static String ALERT_MANAGER_MBEAN_NAME =
      EM_DOMAIN+":type=Service,name=AlertManager";

    public static final String EVENT_DB_MBEAN_NAME =
      EM_DOMAIN+":type=Service,name=EventDB";




    /**
     * gets the jbi jmx domain name
     *
     * @return domain name
     */
    public static String getJbiJmxDomain() {
        if (JBIJMXDOMAIN == null) {
            JBIJMXDOMAIN = System.getProperty("jbi.jmx.domain", JMX_JBI_DOMAIN);
        }
        return JBIJMXDOMAIN;
    }
    
    /**
     * gets the ebi jmx domain name
     *
     * @return domain name
     */
    public static String getEbiJmxDomain() {
        if (EBIJMXDOMAIN == null) {
            EBIJMXDOMAIN = System.getProperty("ebi.jmx.domain", JMX_EBI_DOMAIN);
        }
        return EBIJMXDOMAIN;
    }

    /**
     * Get the EBI Configuration MBean ObjectName
     * @param server
     * @param componentName
     * @param targetName
     * @return object name of ebi config MBean
     * @throws ManagementRemoteException
     */
    public static ObjectName getEbiConfigurationMBeanObjectName(MBeanServerConnection server, String componentName, String targetName)
    throws ManagementRemoteException {
        ObjectName objectName = null;
        Set<ObjectName> objectNameSet = null;
        String name = getEbiJmxDomain() + ":"
            +SERVICE_TYPE_KEY+"=Configuration,"
            +IDENTIFICATION_NAME_KEY + "="
            + componentName + ",*";
        try {
            ObjectName objectNameFilter = new ObjectName(name);
            if (server != null) {
                objectNameSet = server.queryNames(objectNameFilter, null);
                for (ObjectName mBeanObjectName : objectNameSet) {
                    if (mBeanObjectName != null) {
                        objectName = mBeanObjectName;
                        break;
                    }
                }
            }
        } catch (MalformedObjectNameException e) {
            throw new ManagementRemoteException(e);
        } catch (NullPointerException e) {
            throw new ManagementRemoteException(e);
        } catch (IOException e) {
            throw new ManagementRemoteException(e);
        }
        return objectName;
    }

    /**
     * Get the EBI Status MBean ObjectName
     * @param server
     * @param componentName
     * @param targetName
     * @return object name of ebi config MBean
     * @throws ManagementRemoteException
     */
    public static ObjectName getEbiStatusMBeanObjectName(MBeanServerConnection server, String componentName, String targetName)
    throws ManagementRemoteException {
        ObjectName objectName = null;
        Set<ObjectName> objectNameSet = null;
        String name = getEbiJmxDomain() + ":"
            +SERVICE_TYPE_KEY+"=Status,"
            +IDENTIFICATION_NAME_KEY + "="
            + componentName + ",*";
        try {
            ObjectName objectNameFilter = new ObjectName(name);
            if (server != null) {
                objectNameSet = server.queryNames(objectNameFilter, null);
                for (ObjectName mBeanObjectName : objectNameSet) {
                    if (mBeanObjectName != null) {
                        objectName = mBeanObjectName;
                        break;
                    }
                }
            }
        } catch (MalformedObjectNameException e) {
            throw new ManagementRemoteException(e);
        } catch (NullPointerException e) {
            throw new ManagementRemoteException(e);
        } catch (IOException e) {
            throw new ManagementRemoteException(e);
        }
        return objectName;
    }

    /**
     * Returns the ObjectName for the JBI Common Client MBean.
     *
     * @return the ObjectName for the JBI Common Client MBean.
     * @throws MalformedObjectNameException
     *             if the object name is not formatted according to jmx object
     *             name
     */
    public static ObjectName getJbiAdminUiMBeanObjectName()
            throws ManagementRemoteException {
        try {
            if (JBICOMMONCLIENTMBEAN == null) {
                String mbeanRegisteredName = getJbiJmxDomain() + ":"
                        + SERVICE_NAME_KEY + "="
                        + JBI_REFERENCE_ADMIN_UI_SERVICE + ","
                        + COMPONENT_TYPE_KEY + "="
                        + SYSTEM_COMPONENT_TYPE_VALUE;
                JBICOMMONCLIENTMBEAN = new ObjectName(
                        mbeanRegisteredName);
            }
        } catch (MalformedObjectNameException e) {
            throw new ManagementRemoteException(e);
        } catch (NullPointerException e) {
            throw new ManagementRemoteException(e);
        }
        return JBICOMMONCLIENTMBEAN;

    }
    
    /**
     * Returns the Object Name for the logger MBean.
     * 
     * @param type
     * @return the Object Name for the reference logger MBean.
     * @throws ManagementRemoteException
     *             if the object name is not formatted according to jmx object
     *             name
     */
    public static ObjectName getLogManagementServiceObjectName(String type) throws ManagementRemoteException {
        
        String logServiceMbeanName = null;

        if ( LogConstants.TYPE_JMS.equals(type) ) {
            logServiceMbeanName = "com.stc.Logging:name=LogReader,type=AppServerLogReader,logger=jms";
        } else {
            logServiceMbeanName = "com.stc.Logging:name=LogReader,type=AppServerLogReader,logger=server";
        }
        ObjectName logServiceObjectName = null;
        try {
            logServiceObjectName = new ObjectName(logServiceMbeanName);
        } catch (MalformedObjectNameException e) {
            throw new ManagementRemoteException(e);
        } catch (NullPointerException e) {
            throw new ManagementRemoteException(e);
        }
        return logServiceObjectName;
    }
    
    
    /**
     * Returns the Object Name for the JMS mbean
     * @param name  the name of the message server
     * @return the Object Name for the reference Jms MBean.
     * @throws ManagementRemoteException
     *             if the object name is not formatted according to jmx object
     *             name
     */ 
    public static ObjectName getJmsManagementServiceObjectName(String name) throws ManagementRemoteException {
        String jmsMbeanName = "com.sun.appserv:type=messaging-server-admin-mbean,jmsservertype=stcms,name=";
        ObjectName jmsObjectName = null;
        try {
            jmsObjectName = new ObjectName(jmsMbeanName+name);
        } catch (MalformedObjectNameException e) {
            throw new ManagementRemoteException(e);
        } catch (NullPointerException e) {
            throw new ManagementRemoteException(e);
        }
        return jmsObjectName;
    }
    
    /**
     * Returns the Object Name for the Classic EM Http Service
     * @return the Object Name for the Classic EM Http Service MBean.
     * @throws ManagementRemoteException
     *             if the object name is not formatted according to jmx object
     *             name
     */ 
    public static ObjectName getClassicHttpServiceObjectName() throws ManagementRemoteException {
        String mbeanName = "EM:type=adaptor,protocol=HTTP,name=HTTPService";
        ObjectName objectName = null;
        try {
            objectName = new ObjectName(mbeanName);
        } catch (MalformedObjectNameException e) {
            throw new ManagementRemoteException(e);
        } catch (NullPointerException e) {
            throw new ManagementRemoteException(e);
        }
        return objectName;
    }
    
    /**
     * Returns the Object Name for the Classic EM Http Service
     * 
     * @return the Object Name for the Classic EM Http Service MBean.
     * @throws ManagementRemoteException
     *             if the object name is not formatted according to jmx object
     *             name
     */
    public static ObjectName getBPELManagementServiceObjectName()
            throws ManagementRemoteException {
        String mbeanName = "com.sun.jbi:JbiName=server,ComponentName=sun-bpel-engine,ControlType=Custom,CustomControlName=Administration,ComponentType=Installed,InstalledType=Engine";
        // com.sun.jbi:JbiName=server,CustomControlName=Administration,ComponentName=sun-bpel-engine,ControlType=Custom,ComponentType=Installed,InstalledType=Engine
        // String mbeanName =
        // "com.sun.ebi:ServiceType=ManagementAPI,InstallationType=serviceEngines,IdentificationName=sun-bpel-engine";
        ObjectName objectName = null;
        try {
            objectName = new ObjectName(mbeanName);
        } catch (MalformedObjectNameException e) {
            throw new ManagementRemoteException(e);
        } catch (NullPointerException e) {
            throw new ManagementRemoteException(e);
        }
        return objectName;
    }

}
