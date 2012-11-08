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
 * @(#)EnginePropertyConstants.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow;

public interface EnginePropertyConstants {
    
    static final String DATASOURCE_JNDI = "DataSourceJNDIName";  
    static final String DATASOURCE_TYPE = "DataSourceType";
    static final String DATABASE_SCHEMA = "DatabaseSchema";
    
    //A flag to turn on/off testing mode, if it is in testing mode, the schema is recreated every time when workflow engine is started,
    //if the testing mode is off, the schema if existing, will not be recreated.
    static final String TEST_MODE = "TestModeEnabled";
//    static final String PERSISTENCE_ENABLED = "PersistenceEnabled";
    static final String MAXIMUM_THREADCOUNT = "MaxThreadCount";
    
    
    static final String DATASOURCE_JNDI_DEFAULT = "jdbc/__workflow";  
    static final String DATASOURCE_TYPE_DEFAULT = "Derby";
    //A flag to turn on/off testing mode, if it is in testing mode, the schema is recreated every time when workflow engine is started,
    //if the testing mode is off, the schema if existing, will not be recreated.
    static final Boolean TEST_MODE_DEFAULT = Boolean.FALSE;
//    static final Boolean PERSISTENCE_ENABLED_DEFAULT = Boolean.TRUE;
    static final Integer MAXIMUM_THREADCOUNT_DEFAULT = 10;
    
//    static final Boolean LOG_SQL = Boolean.FALSE;
    //The default directory to create index
    static final String  INDEX_DIR_PROP_DEFAULT = "java.io.tmpdir";
    static final String  INDEX_DIR_PROP = "IndexDirProperty";
    
    
    static final  Boolean UPDATE_INDEX_ON_START_DEFAULT = Boolean.FALSE;
    static final  String UPDATE_INDEX_ON_START = "UpdateIndexOnStart";
    
    static final String TASKCOMMON_INTERFACE_QNAME = "http://jbi.com.sun/wfse/wsdl/TaskCommon";
    static final String TASKCOMMON_INTERACE = "TaskCommonPortType";
    static final String TASKCOMMON_SERVICE = "TaskCommonService";
    
    //LDAP configuration
    static final String UID_ATTR = "UidAttr";
    static final String MANAGER_ATTR = "ManagerAttr";
    static final String EMAIL_ATTR = "EmailAttr";
    static final String LOGIN_TYPE = "LoginType";
    static final String LOGIN_DN = "LoginDN";
    static final String PWD = "LoginPassword";
    static final String IS_SSL = "SSL";
    static final String LDAP_HOST = "LdapHost";
    static final String LDAP_PORT = "LdapPort";
    static final String LDAPS_PORT = "LdapSPort";
    static final String USER_FILTER = "UserFilter";
    static final String GROUP_FILTER = "GroupFilter";
    
    static final String BASE_DN = "BaseDN";
    static final String SCOPE_TYPE = "ScopeType";    
    static final String USE_LDAP = "UseLDAP";
    
    static final String UID_ATTR_DEFAULT = "uid";
    static final String MANAGER_ATTR_DEFAULT = "manager";
    static final String EMAIL_ATTR_DEFAULT = "mail";    
    static final String LOGIN_TYPE_DEFAULT = "none";
    static final Boolean IS_SSL_DEFAULT = Boolean.FALSE;
    static final Boolean USE_LDAP_DEFAULT = Boolean.FALSE;
    static final String LDAP_HOST_DEFAULT = "localhost";
    static final Integer LDAP_PORT_DEFAULT = 389;
    static final Integer LDAPS_PORT_DEFAULT = 636;
    static final String USER_FILTER_DEFAULT = "(uid=%s)";
    static final String GROUP_FILTER_DEFAULT = "(cn=%s)";
    
    static final String BASE_DN_DEFAULT = "dc=example,dc=com";
    static final String SCOPE_TYPE_DEFAULT = "sub";    
    
    

}
