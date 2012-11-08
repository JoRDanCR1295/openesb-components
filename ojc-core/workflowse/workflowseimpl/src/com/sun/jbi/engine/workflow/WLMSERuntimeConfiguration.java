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
 * @(#)WLMERuntimeConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow;

/*
 * WLMERuntimeConfiguration.java
 *
 * Created on August 29, 2005, 3:38 PM
 *
 * To change this template, choose Tools | Options and locate the template under
 * the Source Creation and Management node. Right-click the template and choose
 * Open. You can then make changes to the template in the Source Editor.
 */

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.management.AttributeChangeNotification;
import javax.management.InvalidAttributeValueException;
import javax.management.ListenerNotFoundException;
import javax.management.MBeanException;
import javax.management.MBeanNotificationInfo;
import javax.management.NotCompliantMBeanException;
import javax.management.Notification;
import javax.management.NotificationBroadcasterSupport;
import javax.management.NotificationEmitter;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.qos.config.ConfigPersistence;
import com.sun.jbi.common.qos.config.Property;
import com.sun.jbi.engine.workflow.util.I18n;

/**
 * Implenentation of WLMSERuntimeConfigurationMBean
 *
 * @author Sun Microsystems
 */
public class WLMSERuntimeConfiguration implements WLMSERuntimeConfigurationMBean,
    NotificationEmitter {

    
    private ComponentConfig mProp = null;
    
    private Boolean testModeEnabled = null;
    private String dataSourceType = null;
    private Boolean persistenceEnabled = null;
    private String dataSourceJNDIName = null;
    private Integer maxThreadCount;
    private String workspaceRoot = null;
    
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
    
    //INDEX root directory system property
    private String indexDirProperty = null;    
    //update the index on start
    private Boolean updateIndexOnStart = null;
    
    
    private NotificationBroadcasterSupport broadcasterSupport = new NotificationBroadcasterSupport();

    /**
     * Creates a new WLMERuntimeConfiguration object.
     *
     * @param propFile property File name where to read and write configuration values.
     *
     * @throws NotCompliantMBeanException throws exception
     */
    public WLMSERuntimeConfiguration(ComponentContext ctx)
        throws NotCompliantMBeanException, JBIException {
        mProp =  ComponentConfig.parse(ctx.getInstallRoot());        
        workspaceRoot = ctx.getWorkspaceRoot();
        restore();
    }
    
   
    public ComponentConfig getConfiguration() {
    	return this.mProp;
    }
    
    public Boolean getTestModeEnabled() {
    	 if (testModeEnabled == null) {
    		 testModeEnabled = EnginePropertyConstants.TEST_MODE_DEFAULT;
         }

         return testModeEnabled;
    }
    
    public void setTestModeEnabled(Boolean bflag) throws InvalidAttributeValueException, MBeanException {
        Boolean oldValue = getTestModeEnabled();
        testModeEnabled = bflag;
         if (testModeEnabled == null) {
             testModeEnabled = EnginePropertyConstants.TEST_MODE_DEFAULT;
         }
         saveAndNotifyListners("Test_Mode Attribute changed",
                 EnginePropertyConstants.TEST_MODE, oldValue != null ? oldValue.toString() : null, testModeEnabled.toString());
    }
    
    public String getDataSourceType() {
    	 if (dataSourceType == null) {
    		 dataSourceType = EnginePropertyConstants.DATASOURCE_TYPE_DEFAULT;
         }

         return dataSourceType;
    }
    
    public void setDataSourceType(String databaseType) throws MBeanException {
        String oldValue = getDataSourceType();
    	if (databaseType == null) {
    		databaseType = EnginePropertyConstants.DATASOURCE_TYPE_DEFAULT;
        }
        dataSourceType = databaseType;
        saveAndNotifyListners("DataSource_Type Attribute changed", EnginePropertyConstants.DATASOURCE_TYPE, oldValue, databaseType);
    	
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

   
   
    public void setDataSourceJNDIName(String jndiName) throws InvalidAttributeValueException, MBeanException {
        String oldValue = getDataSourceJNDIName();
        if (jndiName == null) {
        	jndiName = EnginePropertyConstants.DATASOURCE_JNDI_DEFAULT;
        }
        dataSourceJNDIName = jndiName;
        saveAndNotifyListners("DataSource_JNDI Attribute changed", EnginePropertyConstants.DATASOURCE_JNDI, oldValue, jndiName);
    }

    
//    public void setPersistenceEnabled(Boolean bflag) throws MBeanException {
//
//        Boolean oldValue = getPersistenceEnabled();
//        persistenceEnabled = bflag;
//
//        if (persistenceEnabled == null) {
//            persistenceEnabled = EnginePropertyConstants.PERSISTENCE_ENABLED_DEFAULT;
//        }
//        saveAndNotifyListners("Persistence_Enabled Attribute changed",
//                EnginePropertyConstants.PERSISTENCE_ENABLED, oldValue != null ? oldValue.toString() : null, persistenceEnabled.toString());
//    }

   
    /**
     * getter for Max Thread Count
     * Does not allow for null value settings.  Instead returns FACTORYDEFAULT value.
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
     * setter for Max Thread Count Does not allow for null value settings. Instead returns
     * FACTORYDEFAULT value.
     * 
     * @param userInputValue value of number of threads
     * @throws MBeanException 
     */
    public void setMaxThreadCount(Integer userInputValue) throws MBeanException {
        Integer oldValue = getMaxThreadCount();
        int maxValue = 1;
       
            maxValue = userInputValue;
            if (maxValue < 1) {
                userInputValue = EnginePropertyConstants.MAXIMUM_THREADCOUNT_DEFAULT;
            }
        
        maxThreadCount = userInputValue;
        saveAndNotifyListners("Maximum_ThreadCount Attribute changed", EnginePropertyConstants.MAXIMUM_THREADCOUNT, oldValue.toString(),
                maxThreadCount.toString());

    }
    
    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public MBeanNotificationInfo[] getNotificationInfo() {
        return new MBeanNotificationInfo[] {
            new MBeanNotificationInfo(
                new String[] {AttributeChangeNotification.ATTRIBUTE_CHANGE},
                AttributeChangeNotification.class.getName(), 
                   I18n.loc("WLM-5000: WLMSE Runtime Configuration Changed")
            )
        };
    }

    /**
     * DOCUMENT ME!
     *
     * @param listener DOCUMENT ME!
     * @param filter DOCUMENT ME!
     * @param handback DOCUMENT ME!
     */
    public void addNotificationListener(
        NotificationListener listener, NotificationFilter filter, Object handback
    ) {
        broadcasterSupport.addNotificationListener(listener, filter, handback);
    }

    /**
     * DOCUMENT ME!
     *
     * @param listener DOCUMENT ME!
     *
     * @throws ListenerNotFoundException DOCUMENT ME!
     */
    public void removeNotificationListener(NotificationListener listener)
        throws ListenerNotFoundException {
        broadcasterSupport.removeNotificationListener(listener);
    }

    /**
     * DOCUMENT ME!
     *
     * @param listener DOCUMENT ME!
     * @param filter DOCUMENT ME!
     * @param handback DOCUMENT ME!
     *
     * @throws ListenerNotFoundException DOCUMENT ME!
     */
    public void removeNotificationListener(
        NotificationListener listener, NotificationFilter filter, Object handback
    ) throws ListenerNotFoundException {
        broadcasterSupport.removeNotificationListener(listener, filter, handback);
    }

   
    public void restore() throws JBIException {
            ConfigPersistence.loadConfig(mProp, workspaceRoot);
            dataSourceJNDIName = mProp.getProperty(EnginePropertyConstants.DATASOURCE_JNDI).getValue();
//            persistenceEnabled = Boolean.valueOf(mProp.getProperty(EnginePropertyConstants.PERSISTENCE_ENABLED).getValue());
            maxThreadCount = Integer.valueOf(mProp.getProperty(EnginePropertyConstants.MAXIMUM_THREADCOUNT).getValue());
            testModeEnabled =  Boolean.valueOf(mProp.getProperty(EnginePropertyConstants.TEST_MODE).getValue());
            dataSourceType  =  mProp.getProperty(EnginePropertyConstants.DATASOURCE_TYPE).getValue();
            
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

   
    public void save() throws JBIException {
        mProp.getProperty(EnginePropertyConstants.DATASOURCE_JNDI).setValue(dataSourceJNDIName);
//        mProp.getProperty(EnginePropertyConstants.PERSISTENCE_ENABLED).setValue (persistenceEnabled.toString()) ;
        mProp.getProperty(EnginePropertyConstants.MAXIMUM_THREADCOUNT).setValue(maxThreadCount.toString());
        mProp.getProperty(EnginePropertyConstants.TEST_MODE).setValue(testModeEnabled.toString());
        mProp.getProperty(EnginePropertyConstants.DATASOURCE_TYPE).setValue(dataSourceType);
        
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

        try {
            ConfigPersistence.persistConfig(mProp, workspaceRoot);
        }catch (Exception e) {
            // TODO: handle exception
            throw new RuntimeException (e);
        }
     
    }
    
    private void saveAndNotifyListners(String msgId, String attrName, String oldValue, String newValue) throws MBeanException {
        //save the configuration to properties file
        try {
            save();
        } catch (JBIException e) {
            throw new MBeanException (e); 
        }

        // Notify listeners of this change
        long seqNo = 0;
        String attrType = String.class.getName();
        Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msgId, attrName,
                attrType, oldValue, newValue);
        broadcasterSupport.sendNotification(notif);
    }

    public ComponentConfig getProperties() {
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
    public void setBaseDN(String dn) throws InvalidAttributeValueException, MBeanException {
        String oldValue = getBaseDN();
        this.baseDN = dn;

        if (baseDN == null) {
            baseDN = EnginePropertyConstants.BASE_DN_DEFAULT;
        }
        saveAndNotifyListners("BASE_DN Attribute changed",
                EnginePropertyConstants.BASE_DN, oldValue != null ? oldValue.toString() : null, baseDN);

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
    public void setEmailAttr(String emAtt) throws InvalidAttributeValueException, MBeanException {
        String oldValue = getEmailAttr();
       this.emailAttr = emAtt;

        if (emailAttr == null) {
            emailAttr = EnginePropertyConstants.EMAIL_ATTR_DEFAULT;
        }
        saveAndNotifyListners("Email Attribute changed",
                EnginePropertyConstants.EMAIL_ATTR, oldValue != null ? oldValue.toString() : null, emailAttr);
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
    public void setGroupFilter(String grFilter) throws InvalidAttributeValueException, MBeanException {
        String oldValue = getGroupFilter();
        this.groupFilter= grFilter;

         if (groupFilter == null) {
             groupFilter = EnginePropertyConstants.GROUP_FILTER;
         }
         saveAndNotifyListners("Group Filter changed",
                 EnginePropertyConstants.GROUP_FILTER, oldValue != null ? oldValue.toString() : null, groupFilter);
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
     * @param isSSL the isSSL to set
     */
    public void setSSL(Boolean isS) throws InvalidAttributeValueException, MBeanException {
        Boolean oldValue = getSSL();
        this.sSL= isS;

         if (sSL == null) {
             sSL = EnginePropertyConstants.IS_SSL_DEFAULT;
         }
         saveAndNotifyListners("IS SSL changed",
                 EnginePropertyConstants.IS_SSL, oldValue != null ? oldValue.toString() : null, sSL.toString());
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
    public void setLdapHost(String ldHost) throws InvalidAttributeValueException, MBeanException {
        String oldValue = getLdapHost();
        this.ldapHost= ldHost;

         if (ldapHost == null) {
             ldapHost = EnginePropertyConstants.LDAP_HOST_DEFAULT;
         }
         saveAndNotifyListners("LDAP Host changed",
                 EnginePropertyConstants.LDAP_HOST, oldValue != null ? oldValue.toString() : null, ldapHost);
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
    public void setLdapPort(Integer ldPort) throws InvalidAttributeValueException, MBeanException {
        Integer oldValue = getLdapPort();
        this.ldapPort= ldPort;

         if (ldapPort == null) {
             ldapPort = EnginePropertyConstants.LDAP_PORT_DEFAULT;
         }
         saveAndNotifyListners("LDAP Port changed",
                 EnginePropertyConstants.LDAP_PORT, oldValue != null ? oldValue.toString() : null, ldapPort.toString());
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
    public void setLdapSPort(Integer ldSPort) throws InvalidAttributeValueException, MBeanException {
        Integer oldValue = getLdapSPort();
        this.ldapSPort= ldSPort;

         if (ldapSPort == null) {
             ldapSPort = EnginePropertyConstants.LDAPS_PORT_DEFAULT;
         }
         saveAndNotifyListners("LDAPS Port changed",
                 EnginePropertyConstants.LDAPS_PORT, oldValue != null ? oldValue.toString() : null, ldapSPort.toString());

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
    public void setLoginDN(String loDN) throws InvalidAttributeValueException, MBeanException {
        String oldValue = getLoginDN();
        this.loginDN= loDN;


         saveAndNotifyListners("Login DN changed",
                 EnginePropertyConstants.LOGIN_DN, oldValue != null ? oldValue.toString() : null, loginDN);
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
    public void setLoginPassword(String lopwd) throws InvalidAttributeValueException, MBeanException {
        String oldValue = getLoginPassword();
        this.loginPassword= lopwd;


         saveAndNotifyListners("Login Password changed",
                 EnginePropertyConstants.PWD, oldValue != null ? oldValue.toString() : null, loginPassword);
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
    public void setLoginType(String loType) throws InvalidAttributeValueException, MBeanException {
        String oldValue = getLoginType();
        this.loginType= loType;

         if (loginType == null) {
             loginType = EnginePropertyConstants.LOGIN_TYPE_DEFAULT;
         }
         saveAndNotifyListners("Login Type changed",
                 EnginePropertyConstants.LOGIN_TYPE, oldValue != null ? oldValue.toString() : null, loginType);
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
    public void setManagerAttr(String manAttr) throws InvalidAttributeValueException, MBeanException {
        String oldValue = getManagerAttr();
        this.managerAttr= manAttr;

         if (managerAttr == null) {
             managerAttr = EnginePropertyConstants.MANAGER_ATTR_DEFAULT;
         }
         saveAndNotifyListners("Manager Attribute changed",
                 EnginePropertyConstants.MANAGER_ATTR, oldValue != null ? oldValue.toString() : null, managerAttr);
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
    public void setScopeType(String scType) throws InvalidAttributeValueException, MBeanException {
        String oldValue = getScopeType();
        this.scopeType= scType;

         if (scopeType == null) {
             scopeType = EnginePropertyConstants.SCOPE_TYPE_DEFAULT;
         }
         saveAndNotifyListners("Scope Type changed",
                 EnginePropertyConstants.SCOPE_TYPE, oldValue != null ? oldValue.toString() : null, scopeType);
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
    public void setUidAttr(String uiA) throws InvalidAttributeValueException, MBeanException {
        String oldValue = getUidAttr();
        this.uidAttr= uiA;

         if (uidAttr == null) {
             uidAttr = EnginePropertyConstants.UID_ATTR_DEFAULT;
         }
         saveAndNotifyListners("UID Attribute changed",
                 EnginePropertyConstants.UID_ATTR, oldValue != null ? oldValue.toString() : null, uidAttr);
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
    public void setUserFilter(String usFilter) throws InvalidAttributeValueException, MBeanException {
        String oldValue = getUserFilter();
        this.userFilter= usFilter;

         if (userFilter == null) {
             userFilter = EnginePropertyConstants.USER_FILTER_DEFAULT;
         }
         saveAndNotifyListners("User Filter changed",
                 EnginePropertyConstants.USER_FILTER, oldValue != null ? oldValue.toString() : null, userFilter);
    }
    
    public void setUseLDAP(Boolean ldapUsed) throws InvalidAttributeValueException, MBeanException {
        // TODO Auto-generated method stub
        Boolean oldValue = getUseLDAP ();
        this.useLDAP= ldapUsed;

        if (useLDAP == null) {
            useLDAP = EnginePropertyConstants.USE_LDAP_DEFAULT;
        }        
        saveAndNotifyListners("Use LDAP changed",
                EnginePropertyConstants.USE_LDAP, oldValue != null ? oldValue.toString() : null, useLDAP.toString());        
    }

    public Boolean getUseLDAP()  {
        // TODO Auto-generated method stub
        if (useLDAP == null) {
            useLDAP = EnginePropertyConstants.USE_LDAP_DEFAULT;
        }

        return useLDAP;
    }


    public String getIndexDirProperty() {
        // TODO Auto-generated method stub
        if (indexDirProperty == null) {
            indexDirProperty = EnginePropertyConstants.INDEX_DIR_PROP_DEFAULT;
        }
        return indexDirProperty;
    }


    public Boolean getUpdateIndexOnStart() {
        // TODO Auto-generated method stub
        if (updateIndexOnStart == null) {
            updateIndexOnStart = EnginePropertyConstants.UPDATE_INDEX_ON_START_DEFAULT;            
        }
        return updateIndexOnStart;
    }


    public void setIndexDirProperty(String indexDirProperty) throws InvalidAttributeValueException, MBeanException {
        // TODO Auto-generated method stub
        String oldValue = getIndexDirProperty();
        this.indexDirProperty = indexDirProperty != null ? indexDirProperty : EnginePropertyConstants.INDEX_DIR_PROP_DEFAULT;
        saveAndNotifyListners("Index Dir Property changed",
                EnginePropertyConstants.INDEX_DIR_PROP, oldValue != null ? oldValue.toString() : null, indexDirProperty);
        
    }


    public void setUpdateIndexOnStart(Boolean updateIndexOnStart) throws InvalidAttributeValueException, MBeanException {
        // TODO Auto-generated method stub
        Boolean oldValue = getUpdateIndexOnStart();
        this.updateIndexOnStart = updateIndexOnStart != null? updateIndexOnStart : EnginePropertyConstants.UPDATE_INDEX_ON_START_DEFAULT;
        saveAndNotifyListners("Update Index on start changed",
                EnginePropertyConstants.UPDATE_INDEX_ON_START, oldValue != null ? oldValue.toString() : null, updateIndexOnStart.toString());
      
    } 
}
