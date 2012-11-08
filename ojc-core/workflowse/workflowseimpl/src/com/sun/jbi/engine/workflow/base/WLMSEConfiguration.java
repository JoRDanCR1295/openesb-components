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
 * @(#)$Id: WLMSEConfiguration.java,v 1.1 2010/02/15 19:25:07 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.base;

import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;
import javax.management.MBeanException;

import com.sun.jbi.common.qos.config.AbstractConfigMBean;
import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.component.toolkit.lifecycle.impl.PollerConfig;
import com.sun.jbi.engine.workflow.EnginePropertyConstants;

public class WLMSEConfiguration extends PollerConfig  implements
        WLMSEConfigurationMBean {

    public WLMSEConfiguration(ComponentContext ctx, ComponentConfig config) throws DeploymentException {
        super(ctx, config);
        // TODO Auto-generated constructor stub
    }

    public String getBaseDN() {
        // TODO Auto-generated method stub
        return getConfig().getProperty(EnginePropertyConstants.BASE_DN).getValue();
    }

    public String getDataSourceJNDIName() {
        // TODO Auto-generated method stub
        return getConfig().getProperty(EnginePropertyConstants.DATASOURCE_JNDI).getValue();
    }

    public String getDataSourceType() {
        // TODO Auto-generated method stub
        return getConfig().getProperty(EnginePropertyConstants.DATASOURCE_TYPE).getValue();
    }

    public String getEmailAttr() {
        // TODO Auto-generated method stub
        return getConfig().getProperty(EnginePropertyConstants.EMAIL_ATTR).getValue();
    }

    public String getGroupFilter() {
        // TODO Auto-generated method stub
        return  getConfig().getProperty(EnginePropertyConstants.GROUP_FILTER).getValue();
    }

    public String getIndexDirProperty() {
        // TODO Auto-generated method stub
        return   getConfig().getProperty(EnginePropertyConstants.INDEX_DIR_PROP).getValue();
    }

    public String getLdapHost() {
        // TODO Auto-generated method stub
        return getConfig().getProperty(EnginePropertyConstants.LDAP_HOST).getValue();
    }

    public Integer getLdapPort() {
        // TODO Auto-generated method stub
        return  Integer.valueOf(getConfig().getProperty(EnginePropertyConstants.LDAP_PORT).getValue());
    }

    public Integer getLdapSPort() {
        // TODO Auto-generated method stub
        return Integer.valueOf(getConfig().getProperty(EnginePropertyConstants.LDAPS_PORT).getValue());
    }

    public String getLoginDN() {
        // TODO Auto-generated method stub
        return  getConfig().getProperty(EnginePropertyConstants.LOGIN_DN).getValue();
    }

    public String getLoginPassword() {
        // TODO Auto-generated method stub
        return getConfig().getProperty(EnginePropertyConstants.PWD).getValue();
    }

    public String getLoginType() {
        // TODO Auto-generated method stub
        return  getConfig().getProperty(EnginePropertyConstants.LOGIN_TYPE).getValue();
    }

    public String getManagerAttr() {
        // TODO Auto-generated method stub
        return   getConfig().getProperty(EnginePropertyConstants.MANAGER_ATTR).getValue();
    }

    public Integer getMaxThreadCount() {
        // TODO Auto-generated method stub
        return  Integer.valueOf(getConfig().getProperty(EnginePropertyConstants.MAXIMUM_THREADCOUNT).getValue());
    }

//    public Boolean getPersistenceEnabled() {
//        // TODO Auto-generated method stub
//        return Boolean.valueOf(getConfig().getProperty(EnginePropertyConstants.PERSISTENCE_ENABLED).getValue());
//    }

    public Boolean getSSL() {
        // TODO Auto-generated method stub
        return Boolean.valueOf(getConfig().getProperty(EnginePropertyConstants.IS_SSL).getValue());
    }

    public String getScopeType() {
        // TODO Auto-generated method stub
        return getConfig().getProperty(EnginePropertyConstants.SCOPE_TYPE).getValue();
    }

    public Boolean getTestModeEnabled() {
        // TODO Auto-generated method stub
        return Boolean.valueOf(getConfig().getProperty(EnginePropertyConstants.TEST_MODE).getValue());
    }

    public String getUidAttr() {
        // TODO Auto-generated method stub
        return getConfig().getProperty(EnginePropertyConstants.UID_ATTR).getValue();
    }

    public Boolean getUpdateIndexOnStart() {
        // TODO Auto-generated method stub
        return Boolean.valueOf(getConfig().getProperty(EnginePropertyConstants.UPDATE_INDEX_ON_START).getValue());
    }

    public Boolean getUseLDAP() {
        // TODO Auto-generated method stub
        return Boolean.valueOf(getConfig().getProperty(EnginePropertyConstants.USE_LDAP).getValue());
    }

    public String getUserFilter() {
        // TODO Auto-generated method stub
        return getConfig().getProperty(EnginePropertyConstants.USER_FILTER).getValue();
    }

    public void setBaseDN(String dn)  throws MBeanException {
        // TODO Auto-generated method stub
            getConfig().getProperty(EnginePropertyConstants.BASE_DN).setValue(dn); 
            persistConfiguration();
    }

    public void setDataSourceJNDIName(String jndi)   throws MBeanException {
        // TODO Auto-generated method stub
        getConfig().getProperty(EnginePropertyConstants.DATASOURCE_JNDI).setValue(jndi); 
        persistConfiguration();
    }

    public void setDataSourceType(String databaseType)  throws MBeanException {
        // TODO Auto-generated method stub
        getConfig().getProperty(EnginePropertyConstants.DATASOURCE_TYPE).setValue(databaseType); 
        persistConfiguration();
    }

    public void setEmailAttr(String emAtt)  throws MBeanException {
        // TODO Auto-generated method stub
        getConfig().getProperty(EnginePropertyConstants.EMAIL_ATTR).setValue(emAtt); 
        persistConfiguration();
    }

    public void setGroupFilter(String grFilter)  throws MBeanException {
        // TODO Auto-generated method stub
        getConfig().getProperty(EnginePropertyConstants.GROUP_FILTER).setValue(grFilter); 
        persistConfiguration();
    }

    public void setIndexDirProperty(String indexDirProperty)  throws MBeanException {
        // TODO Auto-generated method stub
        getConfig().getProperty(EnginePropertyConstants.INDEX_DIR_PROP).setValue(indexDirProperty); 
        persistConfiguration();
    }

    public void setLdapHost(String ldHost)  throws MBeanException {
        // TODO Auto-generated method stub
        getConfig().getProperty(EnginePropertyConstants.LDAP_HOST).setValue(ldHost); 
        persistConfiguration();
    }

    public void setLdapPort(Integer ldPort)  throws MBeanException {
        // TODO Auto-generated method stub
        getConfig().getProperty(EnginePropertyConstants.LDAP_PORT).setValue(ldPort.toString()); 
        persistConfiguration();
    }

    public void setLdapSPort(Integer ldSPort)  throws MBeanException {
        // TODO Auto-generated method stub
        getConfig().getProperty(EnginePropertyConstants.LDAPS_PORT).setValue(ldSPort.toString()); 
        persistConfiguration();
    }

    public void setLoginDN(String loDN)  throws MBeanException {
        // TODO Auto-generated method stub
        getConfig().getProperty(EnginePropertyConstants.LOGIN_DN).setValue(loDN); 
        persistConfiguration();
    }

    public void setLoginPassword(String lopwd)  throws MBeanException {
        // TODO Auto-generated method stub
        getConfig().getProperty(EnginePropertyConstants.PWD).setValue(lopwd); 
        persistConfiguration();
    }

    public void setLoginType(String loType)  throws MBeanException {
        // TODO Auto-generated method stub
        getConfig().getProperty(EnginePropertyConstants.LOGIN_TYPE).setValue(loType); 
        persistConfiguration();
    }

    public void setManagerAttr(String manAttr)  throws MBeanException {
        // TODO Auto-generated method stub
        getConfig().getProperty(EnginePropertyConstants.MANAGER_ATTR).setValue(manAttr); 
        persistConfiguration();
    }

    public void setMaxThreadCount(Integer count)  throws MBeanException {
        // TODO Auto-generated method stub
        getConfig().getProperty(EnginePropertyConstants.MAXIMUM_THREADCOUNT).setValue(count.toString()); 
        persistConfiguration();
    }

//    public void setPersistenceEnabled(Boolean bflag)  throws MBeanException {
//        // TODO Auto-generated method stub
//        getConfig().getProperty(EnginePropertyConstants.PERSISTENCE_ENABLED).setValue(bflag.toString()); 
//        persistConfiguration();
//    }

    public void setSSL(Boolean isS)  throws MBeanException {
        // TODO Auto-generated method stub
        getConfig().getProperty(EnginePropertyConstants.IS_SSL).setValue(isS.toString()); 
        persistConfiguration();
    }

    public void setScopeType(String scType) throws MBeanException {
        // TODO Auto-generated method stub
        getConfig().getProperty(EnginePropertyConstants.SCOPE_TYPE).setValue(scType); 
        persistConfiguration();
    }

    public void setTestModeEnabled(Boolean bflag)  throws MBeanException {
        // TODO Auto-generated method stub
        getConfig().getProperty(EnginePropertyConstants.TEST_MODE).setValue(bflag.toString()); 
        persistConfiguration();
    }

    public void setUidAttr(String uiA) throws MBeanException {
        // TODO Auto-generated method stub
        getConfig().getProperty(EnginePropertyConstants.UID_ATTR).setValue(uiA); 
        persistConfiguration();
    }

    public void setUpdateIndexOnStart(Boolean updateIndexOnStart) throws MBeanException {
        // TODO Auto-generated method stub
        getConfig().getProperty(EnginePropertyConstants.UPDATE_INDEX_ON_START).setValue(updateIndexOnStart.toString()); 
        persistConfiguration();
    }

    public void setUseLDAP(Boolean ldapUsed) throws MBeanException {
        // TODO Auto-generated method stub
        getConfig().getProperty(EnginePropertyConstants.USE_LDAP).setValue(ldapUsed.toString()); 
        persistConfiguration();
    }

    public void setUserFilter(String usFilter) throws MBeanException {
        // TODO Auto-generated method stub
        getConfig().getProperty(EnginePropertyConstants.USER_FILTER).setValue(usFilter); 
        persistConfiguration();
    }

}
