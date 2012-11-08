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
 * @(#)$Id: WLMSEConfigurationMBean.java,v 1.1 2010/02/15 19:25:08 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.base;

import javax.management.MBeanException;


public interface WLMSEConfigurationMBean {
    
    /**
     * Getter for test mode. In test mode database schema and table
     * are recreated after every restart of wlmse.
     *
     * @return true if test mode is enable, false otherwise.
     */
    public Boolean getTestModeEnabled();

    /**
     * Setter for test mode. In test mode database schema and table
     * are recreated after every restart of wlmse.
     *
     * @param bflag if test mode is enable, false otherwise.
     * @throws InvalidAttributeValueException throws this exception if attribute value is invalid
     */
    public void setTestModeEnabled(Boolean bflag) throws MBeanException  ;

    
    /**
     * Get the JNDI Name to be used to connect to database
     *
     * @return JNDI Name to be used when connecting to database
     */
    public String getDataSourceJNDIName();

    
    /**
     * Set the Database JNDI Name to be used for
     * connection to task persistence database.
     *
     * @param jndiName jndi Name to be used to connect to database
     *
     * @throws InvalidAttributeValueException throws this exception if attribute value is invalid
     * 
     */
    public void setDataSourceJNDIName(String port)  throws MBeanException ;

    /**
     * Get the Name of the database where tasks are stored
     *
     * @return name of the database.
     */
    public String getDataSourceType();

    
    /**
     * Set the Name of the database where tasks are stored
     *
     * @parama databaseType name of the database.
     */
    public void setDataSourceType(String databaseType) throws MBeanException ;

//    /**
//     * Check if persistence is enabled
//     *
//     * @return true if persistence is on, false otherwise
//     */
//    public Boolean getPersistenceEnabled();

    
//    /**
//     * set the persistence feature, if false then in memory
//     * TaskManager will be used.
//     *
//     * @param bflag true if persistence is on, false otherwise
//     *
//     * @throws InvalidAttributeValueException throws exception if value is wrong.
//     * 
//     */
//    public void setPersistenceEnabled(Boolean bflag) throws MBeanException ;
    
    /**
     * Set the maximum thread count to be used.
     *
     * @param count is Maximum Thread Count
     *
     * @throws InvalidAttributeValueException
     * 
     */
    public void setMaxThreadCount(Integer count) throws MBeanException ;
    
    /**
     * Set the maximum thread count to be used.
     *
     * @return Maximum Thread Count
     */
    public Integer getMaxThreadCount();   
    
    /**
     * @return the baseDN
     */
    public String getBaseDN() ;


    /**
     * @param baseDN the baseDN to set
     */
    public void setBaseDN(String dn) throws MBeanException  ;


    /**
     * @return the emailAttr
     */
    public String getEmailAttr() ;


    /**
     * @param emailAttr the emailAttr to set
     */
    public void setEmailAttr(String emAtt) throws MBeanException ;


    /**
     * @return the groupFilter
     */
    public String getGroupFilter() ;


    /**
     * @param groupFilter the groupFilter to set
     */
    public void setGroupFilter(String grFilter)  throws MBeanException ;

    /**
     * @return the isSSL
     */
    public Boolean getSSL() ;

    /**
     * @param isSSL the isSSL to set
     */
    public void setSSL(Boolean isS) throws MBeanException  ;
    
    /**
     * @return the useLDAP
     */
    public Boolean getUseLDAP() ;

    /**
     * @param useLDAP  the isSSL to set
     */
    public void setUseLDAP(Boolean ldapUsed) throws MBeanException ;    
    
    /**
     * @return the ldapHost
     */
    public String getLdapHost() ;

    /**
     * @param ldapHost the ldapHost to set
     */
    public void setLdapHost(String ldHost) throws MBeanException ;

    /**
     * @return the ldapPort
     */
    public Integer getLdapPort() ;

    /**
     * @param ldapPort the ldapPort to set
     */
    public void setLdapPort(Integer ldPort) throws MBeanException ;
    /**
     * @return the ldapSPort
     */
    public Integer getLdapSPort() ;

    /**
     * @param ldapSPort the ldapSPort to set
     */
    public void setLdapSPort(Integer ldSPort) throws MBeanException ;
    /**
     * @return the loginDN
     */
    public String getLoginDN() ;

    /**
     * @param loginDN the loginDN to set
     */
    public void setLoginDN(String loDN) throws MBeanException ;
    /**
     * @return the loginPassword
     */
    public String getLoginPassword() ;
    /**
     * @param loginPassword the loginPassword to set
     */
    public void setLoginPassword(String lopwd) throws MBeanException ;
    /**
     * @return the loginType
     */
    public String getLoginType() ;
    /**
     * @param loginType the loginType to set
     */
    public void setLoginType(String loType) throws MBeanException ;
    /**
     * @return the managerAttr
     */
    public String getManagerAttr() ;
    /**
     * @param managerAttr the managerAttr to set
     */
    public void setManagerAttr(String manAttr) throws MBeanException ;
    /**
     * @return the scopeType
     */
    public String getScopeType() ;
    /**
     * @param scopeType the scopeType to set
     */
    public void setScopeType(String scType) throws MBeanException ;
    /**
     * @return the uidAttr
     */
    public String getUidAttr() ;
    /**
     * @param uidAttr the uidAttr to set
     */
    public void setUidAttr(String uiA) throws MBeanException ;
    /**
     * @return the userFilter
     */
    public String getUserFilter() ;
    /**
     * @param userFilter the userFilter to set
     */
    public void setUserFilter(String usFilter) throws MBeanException ;
    
    /**
     * 
     * @return The system property indicating the root directory for index, default java.io.tmpdir
     */
    public String getIndexDirProperty();
    
    /**
     * 
     * @param indexDirProperty
     */
    public void setIndexDirProperty(String indexDirProperty) throws MBeanException ;
    
    /**
     * 
     * @return Whether or not the index will be recreated at WLMSE start
     */
    public Boolean getUpdateIndexOnStart() ;
    
    /**
     * 
     * @param updateIndexOnStart
     */
    public void setUpdateIndexOnStart(Boolean updateIndexOnStart) throws MBeanException ;    

}
