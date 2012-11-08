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
 * @(#)GenericConstants.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.common;

import org.w3c.dom.Element;


/**
 *
 * @author ylee
 * @author Graj
 */
public interface GenericConstants {
    
    public static final String SUCCESS = "success";
    public static final String FAILIRE = "failure";

    public static final String COMPOSITE_APPLICATION_TYPE = "CompositeApplicationType";
    public static final String SERVICE_UNIT_TYPE = "ServiceUnit";
    public static final String PORT_MAP_VIEWER_TYPE = "PortMapViewerType";
    public static final String SERVICE_ASSEMBLY_NAME="name";
    public static final String APP_NAME = "appName";
    
    public static final String COMPONENT_NAME = "name";
    public static final String COMPONENT_TYPE = "type";
    public static final String COMPONENT_CTYPE = "ctype";
    public static final String COMPONENT_CNAME = "cname";
    public static final String COMPONENT_PNAME = "pname";
    public static final String COMPONENT_TNAME = "tname";
    public static final String COMPONENT_SUNAME = "serviceUnitName";
    public static final String COLON_SEPARATOR = ":";
    
    public static final String BC_TYPE_MBEAN_NAME = "bindingComponents";  //$NON-NLS-1$
    public static final String SE_TYPE_MBEAN_NAME = "serviceEngines";     //$NON-NLS-1$    
    public static final String SA_TYPE_MBEAN_NAME = "serviceAssemblies";  //$NON-NLS-1$  
    public static final String SU_TYPE_MBEAN_NAME = "serviceUnits";       //$NON-NLS-1$  

    public static final String BC_TYPE = "BC";  //$NON-NLS-1$
    public static final String SE_TYPE = "SE";  //$NON-NLS-1$    
    public static final String SA_TYPE = "SA";  //$NON-NLS-1$  
    public static final String SU_TYPE = "SU";  //$NON-NLS-1$      
    
    public static final String SUN_APP_SERVER = "SunAppServer";     //$NON-NLS-1$
    public static final String JBOSS_APP_SPERVER = "JBoss";         //$NON-NLS-1$
    public static final String WEBLOGIC_APP_SERVER = "WebLogic";    //$NON-NLS-1$
    public static final String WEBSPHERE_APP_SERVER = "WebSphere";  //$NON-NLS-1$

    public static final String BINDING_INSTALLED_TYPE = "Binding";
    public static final String ENGINE_INSTALLED_TYPE = "Engine";
    public static final String SVG_ELEMENT_TYPE = "SVGElementType";
    
    public static final String PORT = "port"; //$NON-NLS-1$
    public static final String HOSTNAME = "hostName"; //$NON-NLS-1$
    public static final String DOMAINNAME = "domainName"; //$NON-NLS-1$
    public static final String HTTP_ADMINISTRATION_PORT = "httpAdministrationPort"; //$NON-NLS-1$
    public static final String IIOP_PORT = "iiopPort"; //$NON-NLS-1$
    public static final String JRMP_PORT = "jrmpPort"; //$NON-NLS-1$
    public static final String HTTP_ENDPOINT_PORT = "httpEndpointPort"; //$NON-NLS-1$
    public static final String USER_NAME = "userName"; //$NON-NLS-1$
    public static final String PASSWORD = "password"; //$NON-NLS-1$
    public static final String MULTICAST_GROUP_ADDRESS = "multicastGroupAddress"; //$NON-NLS-1$
    public static final String MULTICAST_GROUP_PORT = "multicastGroupPort"; //$NON-NLS-1$
    public static final String MULTICAST_TIME_TO_LIVE = "multicastTimeToLive"; //$NON-NLS-1$
    public static final String MULTICAST_MESSAGE_SEND_INTERVAL = "multicastMessageSendInterval"; //$NON-NLS-1$
    public static final String UPDATER_REFRESH_RATE = "updaterRefreshRate"; // seconds //$NON-NLS-1$

    public static final String DEFAULT_HOST_NAME = "127.0.0.1"; //$NON-NLS-1$
    public static final String DEFAULT_DOMAIN_NAME = "domain1"; //$NON-NLS-1$
    public static final String DEFAULT_ADMIN_PORT = "4848"; //$NON-NLS-1$
    public static final String DEFAULT_HTTP_USER_PORT = "8080"; //$NON-NLS-1$
    public static final String DEFAULT_IIOP_ADMIN_PORT = "3700"; //$NON-NLS-1$
    public static final String DEFAULT_JRMP_ADMIN_PORT = "8686"; //$NON-NLS-1$
    public static final String DEFAULT_USER_NAME = "admin"; //$NON-NLS-1$
    public static final String DEFAULT_CREDENTIALS = "adminadmin"; //$NON-NLS-1$
    public static final String DEFAULT_MULTICAST_GROUP_ADDRESS = "225.1.2.3"; //$NON-NLS-1$
    public static final String DEFAULT_MULTICAST_GROUP_PORT = "5000"; //$NON-NLS-1$
    public static final String DEFAULT_MULTICAST_TIME_TO_LIVE = "10"; //$NON-NLS-1$
    public static final String DEFAULT_MULTICAST_MESSAGE_SEND_INTERVAL = "60"; // seconds //$NON-NLS-1$
    public static final String DEFAULT_UPDATER_REFRESH_RATE = "8"; // seconds //$NON-NLS-1$

    public static final String AT_SEPARATOR = "_at_"; // at separator //$NON-NLS-1$
    public static final String COMPONENT_PATH_SEPARATOR = "_";        //$NON-NLS-1$
    public static final String COMMA_SEPARATOR = ",";                 //$NON-NLS-1$
    public static final String HASH_SEPARATOR = "_s_";                //$NON-NLS-1$
    public static final String AT_SIGN_SEPARATOR = "@";               //$NON-NLS-1$
    public static final String HYPHEN_SEPARATOR = "-";               //$NON-NLS-1$

    public static final String DOMAIN_SERVER = "domain";			 //$NON-NLS-1$
    public static final String ADMIN_SERVER = "server";			     //$NON-NLS-1$
    
    // None
    public static final int INVALID_DIRECTION_KEY = 10;
    // backward
    public static final int PROVIDER_TO_CONSUMER_DIRECTION_KEY = -1;
    // forward and backward round-trip
    public static final int CONSUMER_TO_PROVIDER_AND_BACK_DIRECTION_KEY = 0;
    //  forward
    public static final int CONSUMER_TO_PROVIDER_DIRECTION_KEY = 1;

    public static final String CONSUMER_SUFFIX = "Consumer"; //$NON-NLS-1$
    
    
    public static final String PORTMAPS_KEY = "portmaps"; //$NON-NLS-1$
    public static final String PORTMAP_KEY = "portmap"; //$NON-NLS-1$
    public static final String DIRECTION_KEY = "direction"; //$NON-NLS-1$
    public static final String INBOUND_KEY = "inbound"; //$NON-NLS-1$
    public static final String OUTBOUND_KEY = "outbound"; //$NON-NLS-1$
    public static final String ENDPOINT_KEY = "endPoint"; //$NON-NLS-1$
    public static final String SERVICE_KEY = "service"; //$NON-NLS-1$

    public static final String COLON_DELIMITER = "\\:"; //$NON-NLS-1$
    public static final String DOLLAR_DELIMITER = "\\$"; //$NON-NLS-1$
    public static final String VERTICAL_LINE_DELIMITER = "\\|"; //$NON-NLS-1$
    public static final String TILDE_DELIMITER = "\\~"; //$NON-NLS-1$

    public static final String ENDPOINT_SELECTION_SESSION_KEY = "EndpointsSelected"; //$NON-NLS-1$
    public static final String ENDPOINT_MANAGER_SESSION_KEY = "EndpointManager"; //$NON-NLS-1$
    public static final String CONNECTION_METADATA_HELPER_SESSION_KEY = "ConnectionMetadataHelper"; //$NON-NLS-1$

//    public static final String ASPECT_PACKAGER_METADATA_SESSION_KEY = "AspectPackagerMetadata";
//    public static final String ASPECT_CONFIGURATION_PROPERTIES_SESSION_KEY = "AspectConfigurationProperties";
    
    public static final String GOVERNANCE_CONFIGURATION_SESSION_KEY = "GovernanceConfiguration"; //$NON-NLS-1$
    public static final String GOVERNANCE_CURRENT_SERVICE_ASSEMBLY_SESSION_KEY = "GovernanceCurrentServiceAssembly"; //$NON-NLS-1$
    public static final String GOVERNANCE_CURRENT_SERVICE_UNIT_LIST_SESSION_KEY = "GovernanceCurrentServiceUnitList"; //$NON-NLS-1$
    
    

    public static final String CONNECTION_PROPERTIES_KEY = "connection.properties"; //$NON-NLS-1$
    public static final String SERVER_INFORMATION_KEY = "SERVER_INFORMATION"; //$NON-NLS-1$

    public static final String SERVICE_UNIT_NAME_PARAMETER_KEY = "serviceUnitName"; //$NON-NLS-1$
    
    public static final String NAME_PARAMETER_KEY = "name"; //$NON-NLS-1$
    public static final String OPERATION_PARAMETER_KEY = "operation"; //$NON-NLS-1$
    public static final String INPUT_PARAMETER_KEY = "input"; //$NON-NLS-1$
    public static final String TYPE_PARAMETER_KEY = "type"; //$NON-NLS-1$
    public static final String COMPONENT_NAME_KEY = "componentName"; //$NON-NLS-1$
    public static final String SHOW_PARAMETER_KEY = "show"; //$NON-NLS-1$
    public static final String KEY_PARAMETER_KEY = "key"; //$NON-NLS-1$
    public static final String CLEAN_PARAMETER_KEY = "clean";  //$NON-NLS-1$
    public static final String FILE_NAME_PARAMETER_KEY = "fileName"; //$NON-NLS-1$
    public static final String ACTION_NAME_PARAMETER_KEY = "actionName";     //$NON-NLS-1$
    public static final String REMOVE_PARAMETER_KEY = "remove";  //$NON-NLS-1$
    public static final String CONFIGURE_PARAMETER_KEY = "configure";  //$NON-NLS-1$
    
    public static final String TYPE_PARAMETER_KEY_VALUE = "ServiceEngine"; //$NON-NLS-1$
    public static final String SHOW_CONFIG_PROPERTIES_PARAMETER_KEY_VALUE = "ConfigProperties"; //$NON-NLS-1$
    public static final String SHOW_PORTMAP_URL_PARAMETER_KEY_VALUE = "PortMapURL"; //$NON-NLS-1$

    public static final String PROVISIONING_ID = "Provider"; //$NON-NLS-1$
    public static final String CONSUMING_ID = "Consumer"; //$NON-NLS-1$

    public static final String PROVISIONING_SERVICE_NAME = "com.sun.ProvisioningService"; //$NON-NLS-1$
    public static final String PROVISIONING_SERVICE_UNIT_SUFFIX = "-ProvisioningServiceUnit"; //$NON-NLS-1$
    public static final String FQ_ENDPOINT_NAME = "FQEndPointName"; //$NON-NLS-1$

   
    /** Deployment Type  */
    public static final String DEPLOYMENT_TYPE = "service-assembly"; //$NON-NLS-1$
    /** unknown type */
    public static final String UNKNOWN_TYPE = "unknown"; //$NON-NLS-1$
    /** Binding type  */
    public static final String BINDING_TYPE = "binding-component"; //$NON-NLS-1$
    /** Engine Type */
    public static final String ENGINE_TYPE = "service-engine"; //$NON-NLS-1$
    /** Namespace Type  */
    public static final String NAMESPACE_TYPE = "shared-library"; //$NON-NLS-1$

    /** state  Loaded status.  */
    public static final String UNKNOWN_STATE = "Unknown"; //$NON-NLS-1$
    /** Installed status */
    public static final String SHUTDOWN_STATE = "Shutdown"; //$NON-NLS-1$
    /** Stopped status  */
    public static final String STOPPED_STATE = "Stopped"; //$NON-NLS-1$
    /** Started status */
    public static final String STARTED_STATE = "Started"; //$NON-NLS-1$




    public static final String SOAP_ENCODING_SCHEMA_URL = "http://schemas.xmlsoap.org/soap/encoding/"; //$NON-NLS-1$
    public static final String WSDL_SOAP_SCHEMA_URL = "http://schemas.xmlsoap.org/wsdl/soap/"; //$NON-NLS-1$
    public static final String WSDL_MODEL_SESSION_ATTRIBUTE_KEY = "WSDL_MODEL_SESSION_ATTRIBUTE"; //$NON-NLS-1$
    public static final String WSDL_MODEL_DOCUMENT_SESSION_ATTRIBUTE_KEY = "WSDL_MODEL_DOCUMENT_SESSION_ATTRIBUTE"; //$NON-NLS-1$
    public static final String RESULT_SESSION_ATTRIBUTE_KEY = "RESULT"; //$NON-NLS-1$
    public static final String SOAP_TRANSPORT_KEY = "http://schemas.xmlsoap.org/soap/http"; //$NON-NLS-1$
    public static final String HTTP_URL_PREFIX_KEY = "http://"; //$NON-NLS-1$
    public static final String FILE_URL_REFIX_KEY = "file:///"; //$NON-NLS-1$
    public static final String WSDL_VERBOSE_KEY = "javax.wsdl.verbose"; //$NON-NLS-1$
    public static final String WSDL_IMPORT_DOCUMENTS_KEY = "javax.wsdl.importDocuments"; //$NON-NLS-1$
    public static final String WSDL_SCHEMA_URL = "http://schemas.xmlsoap.org/wsdl/"; //$NON-NLS-1$


    public static final String LIST_BINDING_COMPONENTS_OPERATION_NAME = "listBindingComponents"; //$NON-NLS-1$
    public static final String LIST_SERVICE_ENGINES_OPERATION_NAME = "listServiceEngines"; //$NON-NLS-1$
    public static final String LIST_SHARED_LIBRARIES_OPERATION_NAME = "listSharedLibraries"; //$NON-NLS-1$
    public static final String LIST_SERVICE_ASSEMBLIES_OPERATION_NAME = "listServiceAssemblies"; //$NON-NLS-1$
    public static final String LIST_SHARED_LIBRARY_DEPENDENTS_OPERATION_NAME = "listSharedLibraryDependents"; //$NON-NLS-1$

    public static final String DEPLOY_SERVICE_ASSEMBLY_OPERATION_NAME = "deployServiceAssembly"; //$NON-NLS-1$
    public static final String INSTALL_COMPONENT_OPERATION_NAME = "installComponent"; //$NON-NLS-1$
    public static final String INSTALL_SHARED_LIBRARY_OPERATION_NAME = "installSharedLibrary"; //$NON-NLS-1$
    public static final String SHUTDOWN_COMPONENT_OPERATION_NAME = "shutdownComponent"; //$NON-NLS-1$
    public static final String START_COMPONENT_OPERATION_NAME = "startComponent"; //$NON-NLS-1$
    public static final String STOP_COMPONENT_OPERATION_NAME = "stopComponent"; //$NON-NLS-1$

    public static final String START_SERVICE_ASSEMBLY_OPERATION_NAME = "startServiceAssembly"; //$NON-NLS-1$
    public static final String STOP_SERVICE_ASSEMBLY_OPERATION_NAME = "stopServiceAssembly"; //$NON-NLS-1$
    public static final String SHUTDOWN_SERVICE_ASSEMBLY_OPERATION_NAME = "shutdownServiceAssembly"; //$NON-NLS-1$

    public static final String UNDEPLOY_SERVICE_ASSEMBLY_OPERATION_NAME = "undeployServiceAssembly"; //$NON-NLS-1$
    public static final String UNINSTALL_COMPONENT_OPERATION_NAME = "uninstallComponent"; //$NON-NLS-1$
    public static final String UNINSTALL_SHARED_LIBRARY_OPERATION_NAME = "uninstallSharedLibrary"; //$NON-NLS-1$

    public static final String DEPLOY_FOLDER_NAME = "deploy"; //$NON-NLS-1$
    public static final String BINDING_COMPONENTS_FOLDER_NAME = "bindingComponents"; //$NON-NLS-1$
    public static final String SERVICE_ENGINES_FOLDER_NAME = "serviceEngines"; //$NON-NLS-1$
    public static final String SHARED_LIBRARIES_FOLDER_NAME = "sharedLibraries"; //$NON-NLS-1$
    public static final String SERVICE_ASSEMBLIES_FOLDER_NAME = "serviceAssemblies"; //$NON-NLS-1$

    public static final String SUN_JBI_DOMAIN_NAME = "com.sun.jbi"; //$NON-NLS-1$
    public static final String STC_EBI_DOMAIN_NAME = "com.sun.ebi"; //$NON-NLS-1$

    public static final String EM_DOMAIN_NOTIFICATION_HANDLER = "com.sun.eManager:name=DomainNotificationHandler,ServiceType=eManagerAdministration"; //$NON-NLS-1$

    // MBean Open Type class names
    public static final String OPEN_TYPE_CLASS_VOID = "java.lang.Void"; //$NON-NLS-1$
    public static final String OPEN_TYPE_CLASS_BOOLEAN = "java.lang.Boolean"; //$NON-NLS-1$
    public static final String OPEN_TYPE_CLASS_CHARACTER = "java.lang.Character"; //$NON-NLS-1$
    public static final String OPEN_TYPE_CLASS_BYTE = "java.lang.Byte"; //$NON-NLS-1$
    public static final String OPEN_TYPE_CLASS_SHORT = "java.lang.Short"; //$NON-NLS-1$
    public static final String OPEN_TYPE_CLASS_INTEGER = "java.lang.Integer"; //$NON-NLS-1$
    public static final String OPEN_TYPE_CLASS_LONG = "java.lang.Long"; //$NON-NLS-1$
    public static final String OPEN_TYPE_CLASS_FLOAT = "java.lang.Float"; //$NON-NLS-1$
    public static final String OPEN_TYPE_CLASS_DOUBLE = "java.lang.Double"; //$NON-NLS-1$
    public static final String OPEN_TYPE_CLASS_STRING = "java.lang.String"; //$NON-NLS-1$
    public static final String OPEN_TYPE_CLASS_BIGDECIMAL = "java.math.BigDecimal"; //$NON-NLS-1$
    public static final String OPEN_TYPE_CLASS_BIGINTEGER = "java.math.BigInteger"; //$NON-NLS-1$
    public static final String OPEN_TYPE_CLASS_DATE = "java.util.Date"; //$NON-NLS-1$
    public static final String OPEN_TYPE_CLASS_OBJECTNAME = "javax.management.ObjectName"; //$NON-NLS-1$

    // MBean Server Object Name
    public static final String MBEAN_SERVER_OBJECT_NAME = "JMImplementation:type=MBeanServerDelegate"; //$NON-NLS-1$

    /////////////////////////////////////
    // Local JVM Management Object Names
    /////////////////////////////////////
    // Local JVM Management java.lang.management.ManagementFactory MXBeans Object Names
    public static final String CLASS_LOADING_MXBEAN_NAME = "java.lang:type=ClassLoading"; //$NON-NLS-1$
    public static final String COMPILATION_MXBEAN_NAME = "java.lang:type=Compilation"; //$NON-NLS-1$
    public static final String GARBAGE_COLLECTOR_MXBEAN_DOMAIN_TYPE = "java.lang:type=GarbageCollector"; //$NON-NLS-1$
    public static final String MEMORY_MANAGER_MXBEAN_DOMAIN_TYPE = "java.lang:type=MemoryManager"; //$NON-NLS-1$
    public static final String MEMORY_MXBEAN_NAME = "java.lang:type=Memory"; //$NON-NLS-1$
    public static final String MEMORY_POOL_MXBEAN_DOMAIN_TYPE = "java.lang:type=MemoryPool"; //$NON-NLS-1$
    public static final String OPERATING_SYSTEM_MXBEAN_NAME = "java.lang:type=OperatingSystem"; //$NON-NLS-1$
    public static final String RUNTIME_MXBEAN_NAME = "java.lang:type=Runtime"; //$NON-NLS-1$
    public static final String THREAD_MXBEAN_NAME = "java.lang:type=Threading"; //$NON-NLS-1$

    // Local JVM Management java.lang.management.MemoryNotificationInfo MXBeans Object Names
    public static final String MEMORY_COLLECTION_THRESHOLD_EXCEEDED = "java.management.memory.collection.threshold.exceeded"; //$NON-NLS-1$
    public static final String MEMORY_THRESHOLD_EXCEEDED = "java.management.memory.threshold.exceeded"; //$NON-NLS-1$

    /////////////////////////////////////
    // JBI Framework MBeans Object Names
    /////////////////////////////////////
    // Services
    public static final String JBI_ADMINISTRATION_SERVICE_OBJECT_NAME = "com.sun.jbi:ComponentType=System,ControlType=AdministrationService,ServiceName=AdminService"; //$NON-NLS-1$
    public static final String JBI_CONFIGURATION_SERVICE_OBJECT_NAME = "com.sun.jbi:ComponentType=System,ControlType=ConfigurationService,ServiceName=ConfigurationService"; //$NON-NLS-1$
    public static final String JBI_DEPLOYMENT_SERVICE_OBJECT_NAME = "com.sun.jbi:ComponentType=System,ControlType=DeploymentService,ServiceName=DeploymentService"; //$NON-NLS-1$
    public static final String JBI_INSTALLATION_SERVICE_OBJECT_NAME = "com.sun.jbi:ComponentType=System,ControlType=InstallationService,ServiceName=InstallationService"; //$NON-NLS-1$
    public static final String JBI_MESSAGE_SERVICE_OBJECT_NAME = "com.sun.jbi:ComponentType=System,ControlType=MessageService,ServiceName=MessageService"; //$NON-NLS-1$
    public static final String JBI_LOGGING_SERVICE_OBJECT_NAME = "com.sun.jbi:ComponentType=System,ControlType=LoggingService,ServiceName=LoggingService"; //$NON-NLS-1$

    public static final String JBI_FRAMEWORK_STATISTICS_OBJECT_NAME = "com.sun.jbi:ComponentType=System,ControlType=Statistics,ServiceName=Framework"; //$NON-NLS-1$

    // Heartbeat
    public static final String JBI_HEART_BEAT_ADMIN_SERVICE_OBJECT_NAME = "com.sun.jbi:ComponentType=System,ControlType=HeartBeat,ServiceName=AdminService"; //$NON-NLS-1$

    // Configuration
    public static final String JBI_ADMINISTRATION_SERVICE_CONFIGURATION_OBJECT_NAME = "com.sun.jbi:ComponentType=System,ControlType=Configuration,ServiceName=AdminService"; //$NON-NLS-1$
    public static final String JBI_CONFIGURATION_SERVICE_SYSTEM_CONFIG_OBJECT_NAME = "com.sun.jbi:ComponentType=System,ControlType=Configuration,ServiceName=ConfigurationService"; //$NON-NLS-1$
    public static final String JBI_DEPLOYMENT_SERVICE_SYSTEM_CONFIG_OBJECT_NAME = "com.sun.jbi:ComponentType=System,ControlType=Configuration,ServiceName=DeploymentService"; //$NON-NLS-1$
    public static final String JBI_INSTALLATION_SERVICE_SYSTEM_CONFIG_OBJECT_NAME = "com.sun.jbi:ComponentType=System,ControlType=Configuration,ServiceName=InstallationService"; //$NON-NLS-1$
    public static final String JBI_LOGGING_SERVICE_SYSTEM_CONFIG_OBJECT_NAME = "com.sun.jbi:ComponentType=System,ControlType=Configuration,ServiceName=LoggingService"; //$NON-NLS-1$
    public static final String JBI_MESSAGE_SERVICE_SYSTEM_CONFIG_OBJECT_NAME = "com.sun.jbi:ComponentType=System,ControlType=Configuration,ServiceName=MessageService"; //$NON-NLS-1$

    // Lifecycle
    public static final String JBI_ADMINISTRATION_SERVICE_LIFECYCLE_OBJECT_NAME = "com.sun.jbi:ComponentType=System,ControlType=Lifecycle,ServiceName=AdminService"; //$NON-NLS-1$
    public static final String JBI_CONFIGURATION_SERVICE_LIFECYCLE_OBJECT_NAME = "com.sun.jbi:ComponentType=System,ControlType=Lifecycle,ServiceName=ConfigurationService"; //$NON-NLS-1$
    public static final String JBI_DEPLOYMENT_SERVICE_LIFECYCLE_OBJECT_NAME = "com.sun.jbi:ComponentType=System,ControlType=Lifecycle,ServiceName=DeploymentService"; //$NON-NLS-1$
    public static final String JBI_INSTALLATION_SERVICE_LIFECYCLE_OBJECT_NAME = "com.sun.jbi:ComponentType=System,ControlType=Lifecycle,ServiceName=InstallationService"; //$NON-NLS-1$
    public static final String JBI_LOGGING_SERVICE_LIFECYCLE_OBJECT_NAME = "com.sun.jbi:ComponentType=System,ControlType=Lifecycle,ServiceName=LoggingService"; //$NON-NLS-1$
    public static final String JBI_MESSAGE_SERVICE_LIFECYCLE_OBJECT_NAME = "com.sun.jbi:ComponentType=System,ControlType=Lifecycle,ServiceName=MessageService"; //$NON-NLS-1$

    // urls
    public static final String SERVICE_ASSEMBLY_VIEWER_URL = "/faces/manager/framework/renderer/SAViewer.jsp"; //$NON-NLS-1$
    public static final String SERVICE_UNIT_URL = "/faces/manager/framework/generic/tabsFrame.jsp?"; //$NON-NLS-1$
    public static final String BLANK_SERVICE_UNIT_URL = "/faces/manager/framework/renderer/blank.jsp"; //$NON-NLS-1$
    public static final String SERVICE_ASSEMBLY_FIREFOX_URL = "/faces/manager/framework/renderer/SAViewerFirefox.jsp"; //$NON-NLS-1$
    public static final String SERVICE_ASSEMBLY_IE_URL = "/faces/manager/framework/renderer/SAViewerIE.jsp"; //$NON-NLS-1$
    public static final String SERVICE_ASSEMBLY_CONTROL_URL = "/faces/manager/framework/renderer/SAControl.jsp"; //$NON-NLS-1$
    public static final String SERVICE_ASSEMBLY_TEXTVIEWER_URL = "/faces/manager/framework/renderer/SAJBITextFrame.jsp"; //$NON-NLS-1$
    public static final String SERVICE_ASSEMBLY_SPLITVIEWER_URL = "/faces/manager/framework/renderer/SASplitViewer.jsp"; //$NON-NLS-1$



}
