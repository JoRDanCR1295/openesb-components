/*
 * Copyright 2007 Sun Microsystems, Inc. All rights reserved.
 */

package com.sun.em.common;

/**
 * EM constants and properties.
 * 
 * @author  Yoke Lee
 */
public interface EMProperties {
    
    // DOMAINS
    public static final String EM_DOMAIN = "EM";
    public  static final String EGATE_DOMAIN = "eGate";
    public static final String SEEBEYOND_DOMAIN = "SeeBeyond";
    
    // MBEANS
    public static final String NAMING_SERVICE_MBEAN_NAME = EM_DOMAIN+":type=naming,name=rmiregistry";
    public static final String RMI_SERVICE_MBEAN_NAME = EM_DOMAIN+":type=adaptor,protocol=RMI,name=RMIService";
    public static final String HTTP_ADAPTOR_MBEAN_NAME = EM_DOMAIN+":type=adaptor,protocol=HTTP,name=HTTPService";
    public static final String XSLT_PROCESSOR_NAME = EM_DOMAIN+":type=processor,name=XSLTProcessor";
    public static final String EM_TIMER_MBEAN_NAME = EM_DOMAIN+":type=service,name=TimerService";
    public static final String EM_ALERTS_TIMER_MBEAN_NAME = EM_DOMAIN+":type=service,name=AlertsTimerService";
    public static final String PROXY_MBEAN_NAME = EM_DOMAIN+":type=service,name=ProxyService";
    public static final String MIRROR_MANAGER_MBEAN_NAME = EM_DOMAIN+":type=service,name=MirrorManager";
    public static final String MBEAN_SERVER_DELEGATE_NAME="JMImplementation:type=MBeanServerDelegate";
    public static final String MEJB_MBEAN_NAME="com.sun.appserv:j2eeType=J2EEApplication,name=MEjbApp,J2EEServer=server,category=runtime";
    public static final String META_DATA_MANAGER_MBEAN_NAME="MetaDataManager";
    public static final String TREEBUILDER_MBEAN_NAME = EM_DOMAIN+":type=service,name=TreeBuilder";
    public static final String ALERT_MANAGER_MBEAN_NAME = EM_DOMAIN+":type=Service,name=AlertManager";
    public static final String EVENT_DB_MBEAN_NAME = EM_DOMAIN+":type=Service,name=EventDB";
    public static final String ADD_NOTIFICATION_LISTENER_OPERATION="addNotification";
    public static final String REMOVE_NOTIFICATION_LISTENER_OPERATION="removeNotification";
    public static final String GET_NOTIFICATION_INFO_OPERATION="getNotificationInfo";
    public static final String SAVE_EVENT_OPERATION="saveEvent";
    public static final String HEART_BEAT_SERVICE_MBEAN_NAME = EM_DOMAIN+":type=Service,name=HeartBeatService";
    
    
    // MISC
    public static final String JNDI_INITIAL_CONTEXT_FACTORY = "com.sun.jndi.rmi.registry.RegistryContextFactory";
    public static final String MANAGEMENT_AGENT_SERVER_NAME = "ICAN Enterprise Monitor Management Agent";
    public static final String RMI_CONNECTOR_SERVER_TYPE = "rmi";
    public static final String IIOP_CONNECTOR_SERVER_TYPE = "iiop";
    public static final String SOAP_CONNECTOR_SERVER_TYPE = "soap";
    public static final String HESSIAN_CONNECTOR_SERVER_TYPE = "hessian";
    public static final String BURLAP_CONNECTOR_SERVER_TYPE = "burlap";
    public static final String LOCAL_CONNECTOR_SERVER_TYPE = "local";
    public static final String RMI_CONNECTOR_SERVER_NAME = "/jndi/rmi";
    public static final String IIOP_CONNECTOR_SERVER_NAME = "/jndi/ior";
    public static final String STUB_CONNECTOR_SERVER_NAME = "/stub";
    public static final String SOAP_CONNECTOR_SERVER_NAME = "/EMServices/services/jmx";
    public static final String BURLAP_CONNECTOR_SERVER_NAME = "/EMServices/burlap";
    public static final String HESSIAN_CONNECTOR_SERVER_NAME = "/EMServices/hessian";
    public static final String LOCAL_CONNECTOR_SERVER_NAME = "/id/local";
    public static final String LOCAL_HOST_NAME = "localhost";
    public static final String MBEAN_TYPE_PROPERTY = "type";
    public static final String MBEAN_NAME_PROPERTY = "name";
    public static final String MSG_SERVER_TYPE = "MsgServer";
    public static final String APP_SERVER_TYPE = "AppServer";
    public static final String SERVERS = "Servers";
    public static final String EM_51X_DEPLOYER = "deployment51x";
    public static final String EM_51X_USER_MANAGEMENT ="usermanagement51x";
    public static final String LINK_TYPE = ".LINK";
    public static final String HEARTBEAT_TYPE = "HeartBeatType";
    public static final String PING_NOTIFICATION_TYPE = "PingNotificationType";
    
    // APP SERVER TYPES
    public static final String RTS_APP_SERVER_TYPE = "RTS";
    public static final String SUN_APP_SERVER_TYPE = "SunOne";
    public static final String WLS_APP_SERVER_TYPE = "WebLogic";
    public static final String WAS_APP_SERVER_TYPE = "WebSphere";
    public static final String JBOSS_APP_SERVER_TYPE = "JBoss";
    public static final String GERANIMO_APP_SERVER_TYPE = "Geranimo";
    public static final String OAS_APP_SERVER_TYPE = "Oracle";
    public static final String JONAS_APP_SERVER_TYPE = "Jonas";
    
    public static final String DOMAIN = "domain";
    public static final String JMS51X = "jms51x";
    public static final String CM51X = "cm51x";
    public static final String IS51X = "is51x";
    public static final String FILE51XEWAY = "file51xEway";
    public static final String COLLAB51X = "collab51x";
    public static final String JCE_COLLAB51X = "jce.JavaCollaborationDefinition";
    public static final String PROJECT51X = "project51x";
    public static final String PROJECTDEPLOYMENT51X = "ProjectDeployment51x";
    public static final String GENERIC_RTMONITOR = "genericRTMonitor";
    public static final String MESSAGE_SERVICE_TOPIC = "messageService.Topic";
    public static final String MESSAGE_SERVICE_QUEUE = "messageService.Queue";
    public static final String MESSAGE_SERVICE_TOPIC_LINK = "messageService.Topic.LINK";
    public static final String MESSAGE_SERVICE_QUEUE_LINK = "messageService.Queue.LINK";
    public static final String PNG = "png";
    public static final String GRAPHICSRENDERER_PATH = "/graphicsRenderer/config/";
    public static final String BLANK_IMG = "Blank.png";
    
    public static final String SYSTEM_NAME = "System";
    public static final String DOMAIN_NAME = "Domain";
    public static final String HOST_NAME_AND_PORT = "HostAndPort";
    public static final String COMPONENT_NAME = "Component";
    public static final String COMPONENT_TYPE = "ComponentType";
    public static final String RESTART_REQUIRED = "RestartRequired";
    
    //ATTRIBUTES
    public static final String STATUS_ATTRIBUTE = "Status";
    
    // OPERATIONS
    public static final String GET_STATUS_OPERATION = "getStatus";
    public static final String IS_RESTART_REQUIRED_OPERATION = "restartRequired";
    public static final String RESOLVE_OBJECT_NAME_OPERATION = "resolveObjectName";
    public static final String NOTIFY_OPERATION = "notify";
    public static final String REGISTER_OPERATION = "register";
    public static final String UNREGISTER_OPERATION = "unregister";
    public static final String ADD_NODES_OPERATION = "addNodes";
    public static final String REMOVE_NODES_OPERATION = "removeNodes";
    public static final String GET_META_DATA_OBJECT_OPERATION = "getMetaDataObject";
    public static final String GET_META_DATA_OBJECT_AS_STRING_OPERATION = "getMetaDataObjectAsString";
    public static final String STORE_META_DATA_OBJECT_OPERATION = "storeMetaDataObject";
    public static final String SEND_EVENT_OPERATION = "sendEvent";
    public static final String GET_SVG_OPERATION = "getSVG";
    public static final String GET_METADATA_OPERATION = "getMetaData";
    public static final String GET_EVENT_REPOSITORY_OPERATION = "getEventRepository";
    public static final String STORE_EVENT_OPERATION = "storeEvent";
    public static final String QUERY_EVENT_OPERATION = "queryEvent";
    public static final String QUERY_MAP_EVENT_OPERATION = "queryMapEvent";
    public static final String UPDATE_EVENT_STATUS_OPERATION = "updateEventObservationalState";
    public static final String DELETE_EVENTS_OPERATION = "deleteEvents";
    public static final String START_OPERATION = "start";
    public static final String RESTART_OPERATION = "restart";
    public static final String STOP_OPERATION = "stop";
    public static final String GET_OBJECT_NAME_OPERATION="getObjectName";
    public static final String GET_COMPONENTS_LIST_OPERATION = "retrieveComponentsList";
    public static final String CREATE_MIRROR_MANAGER_OPERATION = "createMirrorManager";
    public static final String REMOVE_MIRROR_MANAGER_OPERATION = "removeMirrorManager";
    public static final String GET_PROPERTIES_OPERATION = "getProperties";
    public static final String IS_STARTABLE_OPERATION = "isStartable";
    public static final String IS_RESTARTABLE_OPERATION = "isRestartable";
    public static final String IS_STOPPABLE_OPERATION = "isStoppable";
    public static final String POST_REGISTRATION_TASK = "doPostRegistrationTask";
    public static final String START_SOAP_CONNECTOR_SERVER_OPERATION = "startSoapConnectorServer";
    public static final String STOP_SOAP_CONNECTOR_SERVER_OPERATION = "stopSoapConnectorServer";
    public static final String ADD_SERVER_OPERATION = "addServer";
    public static final String REMOVE_SERVER_OPERATION = "removeServer";
    public static final String ADD_NOTIFICATION = "addNotification";
    public static final String REMOVE_NOTIFICATION = "removeNotification";
    public static final String IS_EWAY_NODE_OPERATION = "isEwayNode";
    public static final String GET_LINK_OBJECT_NAMES = "getLinkObjectNames";
    
    public static final String SJS51X = "sjs51x";
    public static final String WAS51X = "was51x";
    public static final String JBOSS51X = "jboss51x";
    public static final String WEBLGC51X = "wl51x";
    public static final String HESSIAN_URL_PATH = "/EMRemoteServices/hessian";
    
}
