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
 * @(#)$Id: LDAPConfig.java,v 1.4 2010/02/15 19:24:07 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.process;

import java.util.Properties;
import java.util.Set;
import java.util.logging.Logger;

import javax.naming.directory.DirContext;

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.qos.config.Property;
import com.sun.jbi.engine.workflow.EnginePropertyConstants;
import com.sun.jbi.engine.workflow.util.I18n;
import com.sun.jbi.engine.workflow.util.LDAPSearchUtil;



public class LDAPConfig {
    
    public static final String UID_ATTR = "UidAttr";
    public static final String MANAGER_ATTR = "ManagerAttr";
    public static final String EMAIL_ATTR = "EmailAttr";
    public static final String LOGIN_TYPE = "LoginType";
    public static final String LOGIN_DN = "LoginDN";
    public static final String PWD = "LoginPassword";
    public static final String IS_SSL = "SSL";
    public static final String LDAP_HOST = "LdapHost";
    public static final String LDAP_PORT = "LdapPort";
    public static final String LDAPS_PORT = "LdapSPort";
    public static final String USER_FILTER = "UserFilter";
    public static final String GROUP_FILTER = "GroupFilter";
    
    public static final String BASE_DN = "BaseDN";
    public static final String SCOPE_TYPE = "ScopeType";    
    public static final String USE_LDAP = "UseLDAP";
    
    private Properties mProps = new Properties ();
    
    private static final Logger LOGGER = Logger.getLogger(LDAPConfig.class.getName());
    
    public LDAPConfig()  {
        mProps.put (UID_ATTR, "uid");
        mProps.put(MANAGER_ATTR, "manager");        
        mProps.put(EMAIL_ATTR, "mail");
        mProps.put(LOGIN_TYPE, "none");
        mProps.put(IS_SSL, false);
        mProps.put(LDAP_HOST, "localHost");
        mProps.put(LDAP_PORT, "389");
        mProps.put(USER_FILTER, "(uid=%s)");
        mProps.put(GROUP_FILTER, "(cn=%s)");
        mProps.put(BASE_DN, "dc=example,dc=com");
        mProps.put(SCOPE_TYPE, "sub");
        // TODO Auto-generated constructor stub
        try {            
            LDAPSearchUtil.getLdapContext(mProps.getProperty(LOGIN_TYPE) ,
                mProps.getProperty(LOGIN_DN), 
                mProps.getProperty(PWD), 
               (Boolean) mProps.get(IS_SSL),
                mProps.getProperty(LDAP_HOST),
                mProps.getProperty(LDAP_PORT), 
                mProps.getProperty(LDAPS_PORT));
        }catch (Exception e) {
            LOGGER.warning(I18n.loc(
                    "WLM-6087: Can not create Ldap connection,  Ldap config : {0}", this.toString()
                    ));                 
        }
    }
    
    public LDAPConfig (ComponentConfig config) {
        Set<Property> propertySet = config.propertySet();
        for (Property prop : propertySet) {
            if (prop.getValue() != null) {
                mProps.put(prop.getName(), prop.getValue());
            }
        }      
        // TODO Auto-generated constructor stub
        try {
            LDAPSearchUtil.getLdapContext(mProps.getProperty(LOGIN_TYPE) ,
                mProps.getProperty(LOGIN_DN), 
                mProps.getProperty(PWD), 
               Boolean.valueOf((String)mProps.get(IS_SSL)),
                mProps.getProperty(LDAP_HOST),
                mProps.getProperty(LDAP_PORT), 
                mProps.getProperty(LDAPS_PORT));
        }catch (Exception e) {
            LOGGER.warning(I18n.loc(
                    "WLM-6087: Can not create Ldap connection,  Ldap config : {0}", this.toString()
                    ));                 
        }        
    }
    
    public DirContext getLDAPContext () {
        
       DirContext context = null;
        try {
            context =LDAPSearchUtil.getLdapContext(mProps.getProperty(LOGIN_TYPE) ,
                    mProps.getProperty(LOGIN_DN), 
                    mProps.getProperty(PWD), 
                    Boolean.valueOf((String)mProps.get(IS_SSL)),
                    mProps.getProperty(LDAP_HOST),
                    mProps.getProperty(LDAP_PORT), 
                    mProps.getProperty(LDAPS_PORT));
            }catch (Exception e) {
                LOGGER.warning(I18n.loc(
                        "WLM-6087: Can not create Ldap connection,  Ldap config : {0}", this.toString()
                        ));                 
            }
            return context;
    }
    
    public String getUIDAttName () {
        return mProps.getProperty(UID_ATTR);
    }
    
    public String getManagerAttName () {
        return mProps.getProperty(MANAGER_ATTR);
    }
    
    public String getUserFilter () {
        return mProps.getProperty(USER_FILTER);
    }
    
    public String getGroupFilter () {
        return mProps.getProperty(GROUP_FILTER);
    }
    public String getBaseDN () {
        return mProps.getProperty(BASE_DN);
    }
    
    public String getScope () {
        return mProps.getProperty(SCOPE_TYPE);
    }
    
    public String getEmailAttName () {
        return mProps.getProperty(EMAIL_ATTR);
    }

    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        // TODO Auto-generated method stub
        return mProps.toString();
    }
    
    
}
