/*
 *
 * Copyright 2007 Sun Microsystems, Inc. All rights reserved.
 */

package com.sun.em.common;

/**
 *
 * @author ylee
 */
public interface GenericConstants {

    public static final String SUCCESS = "success";
    public static final String FAILURE = "failed";
    
    // component types
    public static final String SA_TYPE = "SA";
    public static final String SU_TYPE = "SU";
    public static final String BC_TYPE = "BC";
    public static final String SE_TYPE = "SE";
    
    // frames
    public static final String WORKSPACE_FRAME = "workspaceFrame"; 
    public static final String TREE_FRAME = "treeFrame";
    public static final String BANNER_FRAME = "BannerFrame";

    // server types
    public static final String SJSAS_SERVER_TYPE = "SJSAS";
    public static final String NCSG_SERVER_TYPE = "NCSG";
    public static final String WEBLOGIC_SERVER_TYPE = "WEBLOGIC";
    public static final String WEBSPHERE_SERVER_TYPE = "WEBSPHERE";
    public static final String JBOSS_SERVER_TYPE = "JBOSS";
    public static final String ORACLE_SERVER_TYPE = "ORACLE";
    
    // separators/delimiters
    public static final String COLON_SEPARATOR = ":";
    public static final String HYPHEN_SEPARATOR = "-";
    public static final String BAR_SEPARATOR = "|";
    
    public static final String SERVICE_MANAGER_FACTORY_ATTRIBUTE = "SERVICE_MANAGER_FACTORY";
    public static final String UNKNOWN = "unknown";
    public static final String SERVICE_NOT_FOUND = UNKNOWN;
    public static final String COMPONENT = "component";
    public static final String USER = "user";
    public static final String USER_NAME = "username";
    public static final String PASSWORD = "password";
    public static final String PORT = "port";
    public static final String LOGICAL_NAME = "logicalName";
    public static final String HOST_NAME = "hostName";
    public static final String REPOSITORY_NAME = "repName";
    public static final String DEFAULT = "default";
    public static final String SERVICE_IDENTIFIER = "serviceIdentifier";
    public static final String REFRESH = "refresh";
    public static final String UI_REFRESH_RATE= "uiRefreshRate";
    public static final String DISABLE_UI_AUTO_REFRESH= "disableUIAutoRefresh";
    public static final String KEEP_EM_ALIVE = "keepEMAlive";
    public static final String RELOAD = "reload";
    public static final String SERVICE_CLEANER_SLEEPING_DEFAULT_TIME = "60000";
    public static final String SERVICE_CLEANER_SLEEPING_KEY = "serviceCleanerSleppingKey";
    public static final String SERVICE_CLEANER_OUTDATING_INTERVAL = "300000";
    public static final String SERVICE_CLEANER_OUTDATING_INTERVAL_KEY = "serviceCleanerOutdatingIntervalKey";
    public static final String SESSION_ID = "sessionId";
    public static final String UP = "Up";
    public static final String DOWN = "Down";
    public static final String UNKNOWN_STATE = "Unknown";
    public static final String NOT_DEPLOYED = "NotDeployed";
    public static final String BLANK = "Blank";
    public static final String EMPTY = "Empty";
    public static final String GIF_EXTENSION = ".gif";
    public static final String PNG_EXTENSION = ".png";
    public static final String STATE = "State";
    public static final String SINCE = "Since";    
    public static final String ALL = "*";
    public static final String COMPONENT_SEPARATOR = "|";
    public static final String ALERTS = "alerts";
    public static final String LAST_PROCESSED_ALERT_INDEX = "lastAlertIndex";
    public static final String UPDATER_TIMER = "updaterTimer";
    public static final String UPDATER_TIMER_FAST = "1";
    public static final String UPDATER_TIMER_SLOW = "0";
    public static final String OBJECT = "object";
    public static final String BLANK_PAGE = "blankPage";
    public static final String NOT_AVAILABLE = "not available";
    public static final String FAILURE_REASON = "failureReason";
    public static final String COMPONENT_TYPE = "componentType";
    public static final String CURRENT_COMPONENT="currentComponent";
    public static final String ACTION = "action";
    public static final String STATUS = "Status";

    public final String APPLICATION_DISPLAY = "applicationDisplay";
    public final String LOGOUT = "logout";
    public final String DIRTY_CHECKER = "dirtyChecker";
    public final String USER_CONTEXT = "userContext";
    public final String SRE_MONITOR = "eManager";
    public final String OPERATION = "op";
    public static final String COMPONENT_NAME = "componentName";
    public static final String SYSTEM_TYPE = "systemType";
    public static final String E45X = "e45x";
    public static final String E50X = "e50x";
    public static final String E51X = "e51x";
    public static final String GENERIC_COMPONENT_ALERT_MANAGER="genericComponentAlertManager";
    public static final String ROUTING_MANAGER = "routingObject";
    public static final String APPLICATIONS_MANAGER = "management51x";
    public static final String GENERIC_TREE_MANAGER="genericTreeManager";
    public static final String GENERIC_ALERT_MANAGER ="genericAlertManager";
    public static final String GENERIC_LOGGER_MANAGER ="genericLoggerManager";
    public static final String CALLBACK_URL="callbackURL";
    public static final String GENERIC_RT_MONITOR = "genericRTMonitor";
    public static final String DEFAULT_BANNER_MANAGER ="defaultBannerManager";
    public static final String UPDATER_DISCRIMINANT = COMPONENT_SEPARATOR + "@updater";
    public static final String LOGGER_DISCRIMINANT = COMPONENT_SEPARATOR + "@logger";
    public static final String EM_NODE ="EMNode";
    public static final String HTTP_SERVER_REQUEST ="request";
    public static final String REQUEST_CODE="reqCode";
    public static final String CM_SVG = "cmSVG";
    public static final String BUTTON_DISABLED = "disabled";
    
    public static final String CURRRENT_NODE = "currentNode";
    public static final String CURRENT_HOSTNAME = "currentHostname";
    public static final String CURRENT_SCHEMA =  "currentSchema";
    public static final String CURRENT_CB = "currentCB";    
    public static final String NO_CB_COMPONENT = "noCBComponent";
    public static final String CB_COMPONENT = "cbComponent";    
    public static final String CONTAINER_PATH = "containerPath";
    public static final String COMPONENT_PATH = "componentPath";
    public static final String ADD_SERVER = "addServer";
    public static final String CONSUMPTION = "consumption";
    public static final String JMS = "JMS";
    public static final String ROLES = "roles";
    public static final String ROLE = "role";
    public static final String ROLES_KEY = "com.stc.emanager.sentinel.client.filter.roles";
    public static final String ACCESS_MANAGER = "accesManager";
    public static final String USER_PREFERENCES = "userPreferences";
 
    public static final String SERVERS_NODE = "Servers";
    public static final String HTTP_REQUEST = "httpRequest";
    public static final String ROOT_USER = "E51X_ROOT";
    public static final String ADMIN_ROLE = "Manager";
    public static final String ROW = "row";
    public static final String POP_UP_MENU = "popmenu";
    public static final String CANCEL = "popup.menu.cancel.server";
    public static final String REMOVE = "popup.menu.remove.server";
    public static final String HIDE = "popup.menu.hide.server";
    public static final String SHOW_ALL = "popup.menu.showall.server";
    public static final String USER_MANAGER_ROLE = "User Management";

    
    public static final String DIRTY = "dirty_flag";
    public static final String SAVE_ON_EXIT = "saveOnExit";
    public static final String RESTART_REQUIRED = "restartRequired";
    public final String SOURCE= "src";

    public final String APP_MSG_TITLE= "appMsgTitle";
    public final String APP_MSG_BODY= "appMsgContent";
    public final String APP_CONSTRAIN= "app_constrain";
    public final String CAN_NOT_DELETE_LAST_ADMIN= "last_admin_constrain";
    public final String CAN_NOT_EDIT_LAST_ADMIN_ROLE= "edit_last_admin_constrain";
    public final String CALLER_URL= "callerUrl";
    
    public final String TIMEOUT_LOGOUT = "logoutOnTimeOut";
    public static final String FRAME_TARGET = "frameTarget";
    public static final String IS_STARTABLE = "isStartable";
    public static final String IS_RESTARTABLE = "isRestartable";
    public static final String IS_STOPPABLE = "isStoppable";       
    
    public static String NA = "NA";

    
}
