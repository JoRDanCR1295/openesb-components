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
 * @(#)$Id: WLMSEInstallerConfiguration.java,v 1.5 2010/02/15 19:22:46 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow;


import javax.management.InvalidAttributeValueException;
import javax.management.NotCompliantMBeanException;
import javax.management.StandardMBean;

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.qos.config.Property;


public class WLMSEInstallerConfiguration extends StandardMBean implements
        WLMSEInstallerConfigurationMBean {

    private ComponentConfig mProp = null;

    private Boolean testModeEnabled = null;

    private String dataSourceType = null;

    private Boolean persistenceEnabled = null;

    private String dataSourceJNDIName = null;

    private Integer maxThreadCount = null;
    
    //INDEX root directory system property
    private String indexDirProperty = null;
    
    //update the index on start
    private Boolean updateIndexOnStart = null;
    
    //LDAP Configuration
    private String uidAttr = null;
    private String managerAttr = null;
    private String emailAttr = null;
    private String loginType = null;
    private String loginDN = null;
    private String loginPassword = null;
    private Boolean sSL = null;
    private Boolean useLDAP = null;
    private String ldapHost = null;
    private Integer ldapPort = null;
    private Integer ldapSPort = null;
    private String userFilter = null;
    private String groupFilter = null;
    private String baseDN = null;
    private String scopeType = null;    
    
    

    /**
     * default constructor
     * 
     * @throws NotCompliantMBeanException
     *             not compliant MBean exception
     */
    public WLMSEInstallerConfiguration() throws NotCompliantMBeanException {
        super(WLMSEInstallerConfigurationMBean.class);
    }

    public Boolean getTestModeEnabled() {
        if (testModeEnabled == null) {
            testModeEnabled = EnginePropertyConstants.TEST_MODE_DEFAULT;
        }

        return testModeEnabled;
    }

    public void setTestModeEnabled(Boolean bflag)
            throws InvalidAttributeValueException {
        testModeEnabled = bflag;

        if (testModeEnabled == null) {
            testModeEnabled = EnginePropertyConstants.TEST_MODE_DEFAULT;
        }

    }

    public String getDataSourceType() {
        if (dataSourceType == null) {
            dataSourceType = EnginePropertyConstants.DATASOURCE_TYPE_DEFAULT;
        }

        return dataSourceType;
    }

    public void setDataSourceType(String databaseType) {
        if (databaseType == null) {
            databaseType = EnginePropertyConstants.DATASOURCE_TYPE_DEFAULT;
        }

        dataSourceType = databaseType;
    }

    public String getDataSourceJNDIName() {
        if (dataSourceJNDIName == null) {
            dataSourceJNDIName = EnginePropertyConstants.DATASOURCE_JNDI_DEFAULT;
        }

        return dataSourceJNDIName;
    }

//    public Boolean getPersistenceEnabled() {
//        if (persistenceEnabled == null) {
//            persistenceEnabled = EnginePropertyConstants.PERSISTENCE_ENABLED_DEFAULT;
//        }
//
//        return persistenceEnabled;
//    }

    public void setDataSourceJNDIName(String jndiName)
            throws InvalidAttributeValueException {
        if (jndiName == null) {
            jndiName = EnginePropertyConstants.DATASOURCE_JNDI_DEFAULT;
        }

        dataSourceJNDIName = jndiName;
    }

//    public void setPersistenceEnabled(Boolean bflag) {
//
//        persistenceEnabled = bflag;
//
//        if (persistenceEnabled == null) {
//            persistenceEnabled = EnginePropertyConstants.PERSISTENCE_ENABLED_DEFAULT;
//        }
//    }

    /**
     * getter for Max Thread Count Does not allow for null value settings.
     * Instead returns FACTORYDEFAULT value.
     * 
     * @return Max Thread Count
     */
    public Integer getMaxThreadCount() {
        if (maxThreadCount == null) {
            maxThreadCount = EnginePropertyConstants.MAXIMUM_THREADCOUNT_DEFAULT;
        }
        return maxThreadCount;
    }

    /**
     * setter for Max Thread Count Does not allow for null value settings.
     * Instead returns FACTORYDEFAULT value.
     * 
     * @param userInputValue
     *            value of number of threads
     */
    public void setMaxThreadCount(Integer userInputValue) {
        if (userInputValue < 1) {
            userInputValue = EnginePropertyConstants.MAXIMUM_THREADCOUNT_DEFAULT;
        }
        maxThreadCount = userInputValue;
    }

    public void setValues(ComponentConfig props) {
        mProp = props;
        dataSourceJNDIName = mProp.getProperty(
                EnginePropertyConstants.DATASOURCE_JNDI).getValue();
//        persistenceEnabled = Boolean.valueOf(mProp.getProperty(
//                EnginePropertyConstants.PERSISTENCE_ENABLED).getValue());
        try {
            maxThreadCount = Integer.parseInt(mProp.getProperty(
                EnginePropertyConstants.MAXIMUM_THREADCOUNT).getValue());
        }catch(NumberFormatException numex) {
            maxThreadCount = 1;
        }
        testModeEnabled = Boolean.valueOf(mProp.getProperty(EnginePropertyConstants.TEST_MODE)
                .getValue());
        dataSourceType = mProp.getProperty(
                EnginePropertyConstants.DATASOURCE_TYPE).getValue();
        uidAttr = mProp.getProperty(EnginePropertyConstants.UID_ATTR).getValue();
        managerAttr = mProp.getProperty(EnginePropertyConstants.MANAGER_ATTR).getValue();
        emailAttr =  mProp.getProperty(EnginePropertyConstants.EMAIL_ATTR).getValue();
        loginType = mProp.getProperty(EnginePropertyConstants.LOGIN_TYPE).getValue();
        Property loginDNProp = mProp.getProperty(EnginePropertyConstants.LOGIN_DN);
        if (loginDNProp != null) {
            loginDN = loginDNProp.getValue();
        }
        Property loginPasswordProp = mProp.getProperty(EnginePropertyConstants.PWD);
        if (loginPasswordProp != null) {
            loginPassword = loginPasswordProp.getValue();
        }
        
        sSL = Boolean.valueOf(mProp.getProperty(EnginePropertyConstants.IS_SSL).getValue());
        useLDAP= Boolean.valueOf(mProp.getProperty(EnginePropertyConstants.USE_LDAP).getValue());
        ldapHost = mProp.getProperty(EnginePropertyConstants.LDAP_HOST).getValue();
        ldapPort = Integer.valueOf(mProp.getProperty(EnginePropertyConstants.LDAP_PORT).getValue());
        ldapSPort =  Integer.valueOf(mProp.getProperty(EnginePropertyConstants.LDAPS_PORT).getValue());
        userFilter = mProp.getProperty(EnginePropertyConstants.USER_FILTER).getValue();
        groupFilter = mProp.getProperty(EnginePropertyConstants.GROUP_FILTER).getValue();
        baseDN = mProp.getProperty(EnginePropertyConstants.BASE_DN).getValue();
        scopeType = mProp.getProperty(EnginePropertyConstants.SCOPE_TYPE).getValue();        
        indexDirProperty = mProp.getProperty(EnginePropertyConstants.INDEX_DIR_PROP).getValue();
        updateIndexOnStart = Boolean.valueOf(mProp.getProperty(EnginePropertyConstants.UPDATE_INDEX_ON_START).getValue());
    }

    public ComponentConfig getValues() {
        mProp.getProperty(EnginePropertyConstants.DATASOURCE_JNDI).setValue(
                dataSourceJNDIName);
//        mProp.getProperty(EnginePropertyConstants.PERSISTENCE_ENABLED)
//                .setValue(persistenceEnabled.toString());
        mProp.getProperty(EnginePropertyConstants.MAXIMUM_THREADCOUNT)
                .setValue(maxThreadCount.toString());
        mProp.getProperty(EnginePropertyConstants.TEST_MODE)
                .setValue(testModeEnabled.toString());
        mProp.getProperty(EnginePropertyConstants.DATASOURCE_TYPE).setValue(
                dataSourceType);
        mProp.getProperty(EnginePropertyConstants.UID_ATTR).setValue(uidAttr);
        mProp.getProperty(EnginePropertyConstants.MANAGER_ATTR).setValue(managerAttr);
        mProp.getProperty(EnginePropertyConstants.EMAIL_ATTR).setValue(emailAttr);
        mProp.getProperty(EnginePropertyConstants.LOGIN_TYPE).setValue(loginType);        
        if (loginDN != null) {
            mProp.getProperty(EnginePropertyConstants.LOGIN_DN).setValue(loginDN);    
        }
        if (loginPassword != null) {
            mProp.getProperty(EnginePropertyConstants.PWD).setValue(loginPassword);    
        }
        mProp.getProperty(EnginePropertyConstants.IS_SSL).setValue(sSL.toString());    
        mProp.getProperty(EnginePropertyConstants.USE_LDAP).setValue(useLDAP.toString());    
        mProp.getProperty(EnginePropertyConstants.LDAP_HOST).setValue(ldapHost);    
        mProp.getProperty(EnginePropertyConstants.LDAP_PORT).setValue(ldapPort.toString());  
        mProp.getProperty(EnginePropertyConstants.LDAPS_PORT).setValue(ldapSPort.toString());         
        mProp.getProperty(EnginePropertyConstants.USER_FILTER).setValue(userFilter);     
        mProp.getProperty(EnginePropertyConstants.GROUP_FILTER).setValue(groupFilter);   
        mProp.getProperty(EnginePropertyConstants.BASE_DN).setValue(baseDN);   
        mProp.getProperty(EnginePropertyConstants.SCOPE_TYPE).setValue(scopeType);   
        mProp.getProperty(EnginePropertyConstants.INDEX_DIR_PROP).setValue(indexDirProperty);
        mProp.getProperty(EnginePropertyConstants.UPDATE_INDEX_ON_START).setValue(updateIndexOnStart.toString());
         
        return mProp;
    }
    
    /**
     * @return the baseDN
     */
    public String getBaseDN() {
        if (baseDN == null) {
            baseDN = EnginePropertyConstants.BASE_DN_DEFAULT;
        }

        return baseDN;
    }


    /**
     * @param baseDN the baseDN to set
     */
    public void setBaseDN(String dn) {
        this.baseDN = dn;

        if (baseDN == null) {
            baseDN = EnginePropertyConstants.BASE_DN_DEFAULT;
        }
    }


    /**
     * @return the emailAttr
     */
    public String getEmailAttr() {
        if (emailAttr == null) {
            emailAttr = EnginePropertyConstants.EMAIL_ATTR_DEFAULT;
        }

        return emailAttr;
    }


    /**
     * @param emailAttr the emailAttr to set
     */
    public void setEmailAttr(String emAtt) {
       this.emailAttr = emAtt;

        if (emailAttr == null) {
            emailAttr = EnginePropertyConstants.EMAIL_ATTR_DEFAULT;
        }
    }


    /**
     * @return the groupFilter
     */
    public String getGroupFilter() {
        if (groupFilter == null) {
            groupFilter = EnginePropertyConstants.GROUP_FILTER_DEFAULT;
        }

        return groupFilter;
    }


    /**
     * @param groupFilter the groupFilter to set
     */
    public void setGroupFilter(String grFilter) {
        this.groupFilter= grFilter;

         if (groupFilter == null) {
             groupFilter = EnginePropertyConstants.GROUP_FILTER;
         }
    }


    /**
     * @return the isSSL
     */
    public Boolean getSSL() {
        if (sSL == null) {
            sSL = EnginePropertyConstants.IS_SSL_DEFAULT;
        }

        return sSL;
    }


    /**
     * @param sSL the isSSL to set
     */
    public void setSSL(Boolean isS) {
        this.sSL= isS;

         if (sSL == null) {
             sSL = EnginePropertyConstants.IS_SSL_DEFAULT;
         }
    }


    /**
     * @return the ldapHost
     */
    public String getLdapHost() {
        if (ldapHost == null) {
            ldapHost = EnginePropertyConstants.LDAP_HOST_DEFAULT;
        }

        return ldapHost;
    }


    /**
     * @param ldapHost the ldapHost to set
     */
    public void setLdapHost(String ldHost) {
        this.ldapHost= ldHost;

         if (ldapHost == null) {
             ldapHost = EnginePropertyConstants.LDAP_HOST_DEFAULT;
         }
    }


    /**
     * @return the ldapPort
     */
    public Integer getLdapPort() {
        if (ldapPort == null) {
            ldapPort = EnginePropertyConstants.LDAP_PORT_DEFAULT;
        }
        return ldapPort;
    }


    /**
     * @param ldapPort the ldapPort to set
     */
    public void setLdapPort(Integer ldPort) {
        this.ldapPort= ldPort;

         if (ldapPort == null) {
             ldapPort = EnginePropertyConstants.LDAP_PORT_DEFAULT;
         }
    }


    /**
     * @return the ldapSPort
     */
    public Integer getLdapSPort() {
        if (ldapSPort == null) {
            ldapSPort = EnginePropertyConstants.LDAPS_PORT_DEFAULT;
        }
        return ldapSPort;
    }


    /**
     * @param ldapSPort the ldapSPort to set
     */
    public void setLdapSPort(Integer ldSPort) {
        this.ldapSPort= ldSPort;

         if (ldapSPort == null) {
             ldapSPort = EnginePropertyConstants.LDAPS_PORT_DEFAULT;
         }
    }


    /**
     * @return the loginDN
     */
    public String getLoginDN() {
        return loginDN;
    }


    /**
     * @param loginDN the loginDN to set
     */
    public void setLoginDN(String loDN) {
        this.loginDN= loDN;
    }


    /**
     * @return the loginPassword
     */
    public String getLoginPassword() {
        return loginPassword;
    }


    /**
     * @param loginPassword the loginPassword to set
     */
    public void setLoginPassword(String lopwd) {
        this.loginPassword= lopwd;
    }


    /**
     * @return the loginType
     */
    public String getLoginType() {
        if (loginType == null) {
            loginType = EnginePropertyConstants.LOGIN_TYPE_DEFAULT;
        }
        return loginType;
    }


    /**
     * @param loginType the loginType to set
     */
    public void setLoginType(String loType) {
        this.loginType= loType;

         if (loginType == null) {
             loginType = EnginePropertyConstants.LOGIN_TYPE_DEFAULT;
         }
    }


    /**
     * @return the managerAttr
     */
    public String getManagerAttr() {
        if (managerAttr == null) {
            managerAttr = EnginePropertyConstants.MANAGER_ATTR_DEFAULT;
        }
        return managerAttr;
    }


    /**
     * @param managerAttr the managerAttr to set
     */
    public void setManagerAttr(String manAttr) {
        this.managerAttr= manAttr;

         if (managerAttr == null) {
             managerAttr = EnginePropertyConstants.MANAGER_ATTR_DEFAULT;
         }
    }


    /**
     * @return the scopeType
     */
    public String getScopeType() {
        if (scopeType == null) {
            scopeType = EnginePropertyConstants.SCOPE_TYPE_DEFAULT;
        }
        return scopeType;
    }


    /**
     * @param scopeType the scopeType to set
     */
    public void setScopeType(String scType) {
        this.scopeType= scType;

         if (scopeType == null) {
             scopeType = EnginePropertyConstants.SCOPE_TYPE_DEFAULT;
         }
    }


    /**
     * @return the uidAttr
     */
    public String getUidAttr() {
        if (uidAttr == null) {
            uidAttr = EnginePropertyConstants.UID_ATTR_DEFAULT;
        }
        return uidAttr;
    }


    /**
     * @param uidAttr the uidAttr to set
     */
    public void setUidAttr(String uiA) {
        this.uidAttr= uiA;

         if (uidAttr == null) {
             uidAttr = EnginePropertyConstants.UID_ATTR_DEFAULT;
         }
    }


    /**
     * @return the userFilter
     */
    public String getUserFilter() {
        if (userFilter == null) {
            userFilter = EnginePropertyConstants.USER_FILTER;
        }
        return userFilter;
    }


    /**
     * @param userFilter the userFilter to set
     */
    public void setUserFilter(String usFilter) {
        this.userFilter= usFilter;

         if (userFilter == null) {
             userFilter = EnginePropertyConstants.USER_FILTER_DEFAULT;
         }
    }

    public void setUseLDAP(Boolean ldapUsed) {
        // TODO Auto-generated method stub
        this.useLDAP= ldapUsed;

        if (useLDAP == null) {
            useLDAP = EnginePropertyConstants.USE_LDAP_DEFAULT;
        }        
    }

    public Boolean getUseLDAP() {
        // TODO Auto-generated method stub
        if (useLDAP == null) {
            useLDAP = EnginePropertyConstants.USE_LDAP_DEFAULT;
        }

        return useLDAP;
    }

    public String getIndexDirProperty() {
        if (indexDirProperty  == null) {
            indexDirProperty = EnginePropertyConstants.INDEX_DIR_PROP_DEFAULT;
        }
        return indexDirProperty;
    }

    public void setIndexDirProperty(String indexDirProperty) {
        this.indexDirProperty = indexDirProperty;
        if (indexDirProperty  == null) {
            indexDirProperty = EnginePropertyConstants.INDEX_DIR_PROP_DEFAULT;
        }        
    }

    public Boolean getUpdateIndexOnStart() {
        if (updateIndexOnStart == null) {
            updateIndexOnStart = EnginePropertyConstants.UPDATE_INDEX_ON_START_DEFAULT;
        }
        return updateIndexOnStart;
    }

    public void setUpdateIndexOnStart(Boolean updateIndexOnStart) {
        this.updateIndexOnStart = updateIndexOnStart;
    }    
   
    
    
}
