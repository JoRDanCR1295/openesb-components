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
 * @(#)SchedulerConfiguration.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.schedulerbc;

import com.sun.jbi.common.qos.config.AppConfig;
import com.sun.jbi.common.qos.config.AppVar;
import com.sun.jbi.common.util.Util;
import java.util.logging.Logger;
import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.qos.config.Property;
import com.sun.jbi.common.qos.config.RuntimeConfiguration;
import com.sun.jbi.common.util.Base64;
import java.io.UnsupportedEncodingException;
import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeType;

/**
 * Default implementation of {@link SchedulerConfigurationMBean}.
 * 
 * @author sunsoabi_edwong
 */
public class SchedulerConfiguration extends RuntimeConfiguration
        implements SchedulerConfigurationMBean {
    
    private Logger logger = null;

    public SchedulerConfiguration(ComponentContext ctx, ComponentConfig config)
            throws DeploymentException {
        super(ctx, config);
        
        logger = Util.getLogger(ctx, getClass().getName());
    }

    public Integer getPollerCount() {
        return Integer.valueOf(getConfig().getProperty(POLLER_COUNT_PROPERTY)
                .getValue());
    }

    public void setPollerCount(Integer count) {
        int val = (count == null) ? DEFAULT_POLLER_COUNT : count.intValue();
        setCompConfig(POLLER_COUNT_PROPERTY, String.valueOf(val));
    }

    public Integer getQuartzThreadCount() {
        return Integer.valueOf(getConfig().getProperty(
                QUARTZ_THREAD_COUNT_PROPERTY).getValue());
    }

    public void setQuartzThreadCount(Integer count) {
        int val = (count == null) ? DEFAULT_QUARTZ_THREAD_COUNT
                : count.intValue();
        setCompConfig(QUARTZ_THREAD_COUNT_PROPERTY, String.valueOf(val));
    }

    public Boolean getUseQuartzRAMJobStore() {
        return Boolean.valueOf(getConfig().getProperty(
                USE_QUARTZ_RAM_JOBSTORE_PROPERTY).getValue());
    }

    public void setUseQuartzRAMJobStore(Boolean use) {
        Boolean val = (null == use) ? DEFAULT_USE_QUARTZ_RAM_JOBSTORE
                : use;
        setCompConfig(USE_QUARTZ_RAM_JOBSTORE_PROPERTY, val.toString());
    }

    public String getQuartzPersistentJobStoreJNDIURL() {
        return getConfig().getProperty(
                QUARTZ_PERSISTENT_JOBSTORE_JNDI_URL_PROPERTY).getValue();
    }

    public void setQuartzPersistentJobStoreJNDIURL(String url) {
        String val = ((null == url) || DEFAULT_BAD_JNDI_URL.equals(url))
                ? "" : url;                                             //NOI18N
        setCompConfig(QUARTZ_PERSISTENT_JOBSTORE_JNDI_URL_PROPERTY, val);
    }

    private void warnCannotPersist(String what, String name,
            MBeanException mbe) {
        I18n.warning(log(), "SCHEDBC-6024: Cannot persist {0} {1}!",    //NOI18N
                mbe, what, name);
    }
    
    private void setCompConfig(String configName, String value) {
        if (!Util.equals(getConfig().getProperty(configName).getValue(),
                value)) {
            try {
                getConfig().getProperty(configName).setValue(value);
                persistConfiguration();
                I18n.config(log(), "SCHEDBC-4001: "                     //NOI18N
                        + "Set Component Configuration {0}: {1}",       //NOI18N
                        configName, value);
            } catch (MBeanException mbe) {
                warnCannotPersist("Component Configuration",            //NOI18N
                        configName, mbe);
            }
        }
    }
    
    private void setAppConfig(String appConfigName, String propName,
            String propVal) {
        AppConfig appCfg = getConfig().getAppConfig(appConfigName);
        if (appCfg != null) {
            Property prop = appCfg.getProperty(propName);
            if (prop != null) {
                if (!Util.equals(prop.getValue(), propVal)) {
                    try {
                        prop.setValue(propVal);
                        persistApplicationConfig();
                        I18n.config(log(), "SCHEDBC-4002: Set "         //NOI18N
                                + "Application Configuration {0}: {1}", //NOI18N
                                propName, propVal);
                    } catch (MBeanException mbe) {
                         warnCannotPersist("Application Configuration", //NOI18N
                                 appConfigName + "::" + propName, mbe); //NOI18N
                    }
                }
            }
        }
    }
    
    private String getAppConfig(String appConfigName, String propName) {
        AppConfig appCfg = getConfig().getAppConfig(appConfigName);
        if (appCfg != null) {
            Property prop = appCfg.getProperty(propName);
            if (prop != null) {
                return prop.getValue();
            }
        }
        return null;
    }

    public void setStartDate(String appConfigName, String startDate) {
        setAppConfig(appConfigName, START_DATE, startDate);
    }

    public String getStartDate(String appConfigName) {
        return getAppConfig(appConfigName, START_DATE);
    }

    public void setEndDate(String appConfigName, String endDate) {
        setAppConfig(appConfigName, END_DATE, endDate);
    }

    public String getEndDate(String appConfigName) {
        return getAppConfig(appConfigName, END_DATE);
    }

    public void setTimeZone(String appConfigName, String timeZone) {
        setAppConfig(appConfigName, TIME_ZONE, timeZone);
    }

    public String getTimeZone(String appConfigName) {
        return getAppConfig(appConfigName, TIME_ZONE);
    }
    
    public String getAppVar(String appVarName) {
        AppVar appVar = getConfig().getAppVar(appVarName);
        if (appVar != null) {
            if (AppVar.VarType.Password.equals(getAppVarType(appVarName))) {
                return elucidate(appVar.getValue());
            }
            return appVar.getValue();
        }
        return null;
    }
    
    public AppVar.VarType getAppVarType(String appVarName) {
        AppVar appVar = getConfig().getAppVar(appVarName);
        if (appVar != null) {
            return appVar.getType();
        }
        return null;
    }
    
    @Override
    protected AppVar validateAppVar(String name, CompositeData appVar)
            throws InvalidAttributeValueException {
        CompositeType rowType = appVar.getCompositeType();
        if (rowType.keySet().size() != 3) {
            throw new InvalidAttributeValueException(I18n.warning(log(),
                    "SCHEDBC-6020: Invalid Application Variable "       //NOI18N
                    + "Item Size: {0}",                                 //NOI18N
                    String.valueOf(rowType.keySet().size())));
        }
        
        if (!appVar.containsKey(APPLICATION_VARIABLES_NAME)) {
            throw new InvalidAttributeValueException(I18n.warning(log(),
                    "SCHEDBC-6021: Invalid Application Variable "       //NOI18N
                    + "- Missing Name Field: {0}", name));              //NOI18N
        } 
        
        String value = (String) appVar.get(APPLICATION_VARIABLES_VALUE);
        String type = (String) appVar.get(APPLICATION_VARIABLES_TYPE);
        
        if (value == null) {
            throw new InvalidAttributeValueException(I18n.warning(log(),
                    "SCHEDBC-6022: Invalid Application Variable "       //NOI18N
                    + "- Missing Value Field: {0}", name));             //NOI18N
        }
        
        if (type == null) {
            throw new InvalidAttributeValueException(I18n.warning(log(),
                    "SCHEDBC-6023: Invalid Application Variable "       //NOI18N
                    + "- Missing Type Field: {0}", name));              //NOI18N
        }
        
        AppVar.VarType enType = AppVar.VarType.toVarType(type);
        if (AppVar.VarType.Password.equals(enType)) {
            value = obfuscate(value);
            I18n.config(log(), "SCHEDBC-4019: Obfuscated "              //NOI18N
                    + "Password Application Variable {0}",              //NOI18N
                    name);
        }
        
        return new AppVar(name, value, enType);
    }
    
    private String obfuscate(String s) {
        try {
            s = Base64.encode(s, "UTF-8");                              //NOI18N
        } catch (UnsupportedEncodingException ex) {
            // ignore because hardcoded UTF-8 is valid
        }
        return s;
    }
    
    private String elucidate(String s) {
        try {
            s = Base64.decode(s, "UTF-8");                              //NOI18N
        } catch (UnsupportedEncodingException usee) {
            // ignore because hardcode UTF-8 is valid
        }
        return s;
    }
    
    public void setAppVar(String appVarName, String appVarValue) {
        AppVar appVar = getConfig().getAppVar(appVarName);
        if (appVar != null) {
            if (AppVar.VarType.Password.equals(getAppVarType(appVarName))) {
                appVarValue = obfuscate(appVarValue);
                I18n.config(log(), "SCHEDBC-4019: Obfuscated "          //NOI18N
                        + "Password Application Variable {0}",          //NOI18N
                        appVarName);
            }
            if (!Util.equals(appVar.getValue(), appVarValue)) {
                try {
                    appVar.setValue(appVarValue);
                    persistApplicationConfig();
                    I18n.config(log(), "SCHEDBC-4003: "                 //NOI18N
                            + "Set Application Variable {0}: {1}",      //NOI18N
                            appVarName, appVarValue);
                } catch (MBeanException mbe) {
                     warnCannotPersist("Application Variable",          //NOI18N
                             appVarName, mbe);
                }
            }
        }
    }

    @Override
    protected Logger log() {
        return logger;
    }

}
