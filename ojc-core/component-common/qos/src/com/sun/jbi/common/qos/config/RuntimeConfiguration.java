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
 * @(#)RuntimeConfiguration.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.config;

import java.util.Iterator;
import java.util.logging.Level;

import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;
import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeDataSupport;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.SimpleType;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.TabularDataSupport;
import javax.management.openmbean.TabularType;
import javax.xml.XMLConstants;
import javax.xml.namespace.QName;

import com.sun.jbi.common.qos.I18n;
import com.sun.jbi.common.qos.config.AppVar.VarType;

/**
 * Default implementation of {@link RuntimeConfigurationMBean}.
 * 
 * @author Kevan Simpson
 */
public class RuntimeConfiguration extends AbstractConfigMBean 
                                  implements RuntimeConfigurationMBean {
    private TabularType mAppConfigTabularType, mAppVarTabularType;
    private CompositeType mAppConfigRowType, mAppVarRowType;
    
    public RuntimeConfiguration(ComponentContext ctx) throws DeploymentException {
        this(ctx, null);
    }
    
    public RuntimeConfiguration(ComponentContext ctx, 
                                ComponentConfig config) throws DeploymentException {
        super(ctx, config);
        
        initAppConfig(config.getAppConfigDefs());
        initAppVars();
    }

    /** @see com.sun.jbi.common.qos.config.RuntimeConfigurationMBean#addApplicationConfiguration(java.lang.String, javax.management.openmbean.CompositeData) */
    public void addApplicationConfiguration(String name, CompositeData appConfig)
            throws MBeanException {
        if (getConfig().getAppConfig(name) != null) {
            throw mbeanError(null, I18n.loc(
                             "QOS-6030: Add Application Configuration Failed: Configuration {0} already exists!", 
                             name));
        }
        
        // verify correct number of fields
        CompositeType rowType = appConfig.getCompositeType();
        Property[] configProps = getConfig().getAppConfigDefs();
        if (configProps.length != rowType.keySet().size()) {
            throw mbeanError(null, I18n.loc(
                             "QOS-6031: Add Application Configuration Failed: Configuration should have {0} fields, not {1}!", 
                             String.valueOf(configProps.length), String.valueOf(rowType.keySet().size())));
        }
        
        if (!appConfig.containsKey(APPLICATION_CONFIGURATION_NAME)) {
            throw mbeanError(null, I18n.loc(
                             "QOS-6032: Add Application Configuration Failed: Configuration missing {0} key!", 
                             RuntimeConfigurationMBean.APPLICATION_CONFIGURATION_NAME));
        }

        try {
            AppConfig config = validateAppConfig(name, appConfig);
            getConfig().putAppConfig(config);
            if (log().isLoggable(Level.CONFIG)) {
                log().config(I18n.loc(
                        "QOS-4001: Successfully added new Application Configuration: {0}", 
                        config.getName()));
            }
        }
        catch (InvalidAttributeValueException iave) {
            throw mbeanError(iave, I18n.loc(
                    "QOS-6033: Add Application Configuration Failed: {0} value is invalid!", 
                    name));
        }

        persistApplicationConfig();
    }

    /** @see com.sun.jbi.common.qos.config.RuntimeConfigurationMBean#addApplicationVariable(java.lang.String, javax.management.openmbean.CompositeData) */
    public void addApplicationVariable(String name, CompositeData appVar)
            throws MBeanException {
        if (appVar == null) return;
        
        if (getConfig().getAppVar(name) != null) {
            throw mbeanError(null, I18n.loc(
                    "QOS-6034: Add Application Variable Failed: Variable {0} already exists!", 
                    name));
        }
        
        try {
            AppVar var = validateAppVar(name, appVar);
            getConfig().putAppVar(var);
            if (log().isLoggable(Level.CONFIG)) {
                log().config(I18n.loc(
                        "QOS-4002: Successfully added new Application Variable: {0} - Value = {1}", 
                        var.getName(), var.getValue()));
            }
        }
        catch (InvalidAttributeValueException iave) {
            throw mbeanError(iave, I18n.loc(
                    "QOS-6035: Add Application Variable Failed: {0} value is invalid!", 
                    name));
        }
        
        persistApplicationConfig();
    }

    /** @see com.sun.jbi.common.qos.config.RuntimeConfigurationMBean#deleteApplicationConfiguration(java.lang.String) */
    public void deleteApplicationConfiguration(String name)
            throws MBeanException {
        if (getConfig().getAppConfig(name) == null) {
            throw mbeanError(null, I18n.loc(
                    "QOS-6036: Delete Application Configuration Failed: Configuration {0} does not exist!", 
                    name));
        }
        
        getConfig().removeAppConfig(name);
        if (log().isLoggable(Level.CONFIG)) {
            log().config(I18n.loc(
                    "QOS-4003: Successfully deleted Application Configuration: {0}", 
                    name));
        }
        
        persistApplicationConfig();
    }

    /** @see com.sun.jbi.common.qos.config.RuntimeConfigurationMBean#deleteApplicationVariable(java.lang.String) */
    public void deleteApplicationVariable(String name) throws MBeanException {
        if (getConfig().getAppVar(name) == null) {
            throw mbeanError(null, I18n.loc(
                    "QOS-6037: Application Variable Delete Failed: Variable {0} does not exist!", 
                    name));
        }
        
        getConfig().removeAppVar(name);
        if (log().isLoggable(Level.CONFIG)) {
            log().config(I18n.loc(
                    "QOS-4004: Successfully deleted Application Variable: {0}", name));
        }
        
        persistApplicationConfig();
    }

    /** @see com.sun.jbi.common.qos.config.RuntimeConfigurationMBean#getApplicationConfigurations() */
    public TabularData getApplicationConfigurations() {
        TabularData tabularData = new TabularDataSupport(mAppConfigTabularType);
        Property[] props = getConfig().getAppConfigDefs();
        // only try to add AppConfigs if they're configured
        if (props != null && props.length > 0) {
            String[] rowNames = getNames(props);
            
            for (Iterator<String> iter = getConfig().appConfigNames(); iter.hasNext();) {
                String name = iter.next();
                AppConfig ac = getConfig().getAppConfig(name);
                try {
                    Object[] vals = getValues(props, ac);
                    // TODO log values with name at Finer
                    tabularData.put(new CompositeDataSupport(mAppConfigRowType,
                                                             rowNames,
                                                             vals));
                }
                catch (OpenDataException ode) {
                    // TODO can we throw MBeanException here?
                    throw new RuntimeException(ode);
                }
            }
        }
        
        return tabularData;
    }

    /** @see com.sun.jbi.common.qos.config.RuntimeConfigurationMBean#getApplicationVariables() */
    public TabularData getApplicationVariables() {
        try {
            TabularData data = new TabularDataSupport(mAppVarTabularType);
            for (Iterator<String> iter = getConfig().appVarNames(); iter.hasNext();) {
                String name = iter.next();
                AppVar var = getConfig().getAppVar(name);
                Object[] values = { name,
                                    (var.getType() == VarType.Password)
                                            ? "********" : var.getValue(),
                                    String.valueOf(var.getType()) };
                // TODO log values with name at Finer
                data.put(new CompositeDataSupport(mAppVarRowType,
                                                  new String[] { APPLICATION_VARIABLES_NAME,
                                                                 APPLICATION_VARIABLES_VALUE,
                                                                 APPLICATION_VARIABLES_TYPE },
                                                  values));
            }
            
            return data;
        }
        catch (OpenDataException ode) {
            // TODO can we throw MBeanException here?
            throw new RuntimeException(ode);
        }
    }

    /** @see com.sun.jbi.common.qos.config.RuntimeConfigurationMBean#queryApplicationConfigurationType() */
    public CompositeType queryApplicationConfigurationType() {
        return mAppConfigRowType;
    }

    /** @see com.sun.jbi.common.qos.config.RuntimeConfigurationMBean#setApplicationConfiguration(java.lang.String, javax.management.openmbean.CompositeData) */
    public void setApplicationConfiguration(String name, CompositeData appConfig)
            throws MBeanException {
        if (getConfig().getAppConfig(name) == null) {
            throw mbeanError(null, I18n.loc(
                    "QOS-6038: Set Application Configuration Failed: Configuration {0} does not exist!", 
                    name));
        }
        
        // verify correct number of fields
        CompositeType rowType = appConfig.getCompositeType();
        Property[] configProps = getConfig().getAppConfigDefs();
        if (configProps.length != rowType.keySet().size()) {
            throw mbeanError(null, I18n.loc(
                    "QOS-6039: Set Application Configuration Failed: Configuration should have {0} fields, not {1}!", 
                    String.valueOf(configProps.length), String.valueOf(rowType.keySet().size())));
        }
        
        if (!appConfig.containsKey(APPLICATION_CONFIGURATION_NAME)) {
            throw mbeanError(null, I18n.loc( 
                    "QOS-6040: Set Application Configuration Failed: Configuration missing {0} key!", 
                    RuntimeConfigurationMBean.APPLICATION_CONFIGURATION_NAME));
        }

        try {
            AppConfig config = validateAppConfig(name, appConfig);
            getConfig().putAppConfig(config);
            if (log().isLoggable(Level.CONFIG)) {
                log().config(I18n.loc(
                        "QOS-4005: Successfully set Application Configuration: {0}", 
                        config.getName()));
            }
        }
        catch (InvalidAttributeValueException iave) {
            throw mbeanError(iave, I18n.loc(
                    "QOS-6041: Set Application Configuration Failed: {0} value is invalid!", 
                    name));
        }

        persistApplicationConfig();
    }

    /** @see com.sun.jbi.common.qos.config.RuntimeConfigurationMBean#setApplicationVariable(java.lang.String, javax.management.openmbean.CompositeData) */
    public void setApplicationVariable(String name, CompositeData appVar)
            throws MBeanException {
        if (appVar == null) return;
        
        if (getConfig().getAppVar(name) == null) {
            throw mbeanError(null, I18n.loc(
                    "QOS-6042: Set Application Variable Failed: Variable {0} does not exist!", 
                    name));
        }
        
        try {
            AppVar var = validateAppVar(name, appVar);
            getConfig().putAppVar(var);
            if (log().isLoggable(Level.CONFIG)) {
                log().config(I18n.loc(
                        "QOS-4006: Successfully set Application Variable: {0} - Value = {1}", 
                        var.getName(), var.getValue()));
            }
        }
        catch (InvalidAttributeValueException iave) {
            throw mbeanError(iave, I18n.loc(
                    "QOS-6043: Set Application Variable Failed: {0} value is invalid!", 
                    name));
        }
        
        persistApplicationConfig();
    }

    protected AppConfig validateAppConfig(String name, CompositeData data) throws InvalidAttributeValueException {
        Property[] props = getConfig().getAppConfigDefs();
        AppConfig config = new AppConfig(name, props);
        for (Property metadataProp : props) {
            // does CompositeData have this property?
            if (!data.containsKey(metadataProp.getName())) {
                throw invalidAttr("QOS-6025: Application Configuration is missing field: {0}", 
                                  metadataProp.getName());
            }
            Object value = data.get(metadataProp.getName());
            if (metadataProp.isRequired() && value == null) {
                throw invalidAttr("QOS-6026: Application Configuration is missing value for field: {0}", 
                                  metadataProp.getName());
            }
            
            // TODO validate value against Property's Constraints
            
            // add the Property instance, if missing, which holds values
            Property p = config.getProperty(metadataProp.getName());
            if (p == null) {
                config.addProperty(new Property(metadataProp));
            }
            // set the value of the newly added Property
            if (metadataProp.isRepeating()) {
                try {
                    Object[] strs = (Object[]) value;
                    for (Object s : strs) {
                        p.addValue(String.valueOf(s));
                    }
                }
                catch (Exception e) {
                    log().log(Level.WARNING, e.getMessage(), e);
                    throw invalidAttr("QOS-6044: Application Configuration {0} - Value Invalid: {1}", 
                                      metadataProp.getName(), e.getMessage());
                }
            }
            else {
                p.setValue(String.valueOf(value));
            }
        }
        
        return config;
    }
    
    protected AppVar validateAppVar(String name, CompositeData appVar) throws InvalidAttributeValueException {
        CompositeType rowType = appVar.getCompositeType();
        if (rowType.keySet().size() != 3) {
            throw invalidAttr(
                    "QOS-6021: Invalid Application Variable Item Size: {0}", 
                    String.valueOf(rowType.keySet().size()));
        }
        
        if (!appVar.containsKey(APPLICATION_VARIABLES_NAME)) {
            throw invalidAttr(
                    "QOS-6022: Invalid Application Variable - Missing Name Field: {0}", 
                    name);
        } 
        
        String value = (String) appVar.get(APPLICATION_VARIABLES_VALUE);
        String type = (String) appVar.get(APPLICATION_VARIABLES_TYPE);
        
        // TODO do we need to check for NULL values here...not 'required' in schema
        if (value == null) {
            throw invalidAttr(
                    "QOS-6023: Invalid Application Variable - Missing Value Field: {0}", 
                    name);
        }
        
        if (type == null) {
            throw invalidAttr(
                    "QOS-6024: Invalid Application Variable - Missing Type Field: {0}", 
                    name);
        }
        
        return new AppVar(name, value, VarType.toVarType(type));
    }
    
    protected OpenType convertType(QName type) throws DeploymentException {
        if (type == null) {
            throw deployError(null, I18n.loc(
                    "QOS-6045: Failed to convert NULL Property type!"));
        }
        
        if (type.getNamespaceURI() != null && 
            type.getNamespaceURI().equals(XMLConstants.W3C_XML_SCHEMA_NS_URI)) {
            // we only support XSD types: int, positiveInteger, boolean, string
            String name = type.getLocalPart();
            if (name.equals("string")) {
                return SimpleType.STRING;
            }
            else if (name.equals("int")) {
                return SimpleType.INTEGER;
            }
            else if (name.equals("boolean")) {
                return SimpleType.BOOLEAN;
            }
            else if (name.equals("positiveInteger")) {
                return SimpleType.BIGINTEGER;
            }
        }

        throw deployError(null, I18n.loc(
                "QOS-6046: Component Configuration does not support the type: {0}", 
                type));
    }
    
    protected void initAppConfig(Property[] props) throws DeploymentException {
        try {
            int len = (props == null) ? 0 : props.length;
            if (len > 0) {  // meaning AppConfig exists...
                String[] attrNames = new String[len];
                String[] attrDesc = new String[len];
                OpenType[] attrTypes = new OpenType[len];
                
                for (int i = 0; i < len; i++) {
                    attrNames[i] = attrDesc[i] = props[i].getName();
                    attrTypes[i] = convertType(props[i].getType());
                }
                
                mAppConfigRowType = new CompositeType("ApplicationConfigurationObject",
                                                      "Application Configuration Composite Data",
                                                      attrNames, attrDesc, attrTypes);
                mAppConfigTabularType = new TabularType("ApplicationConfigurationObjectList",
                                                        "List of Application Configuration Objects",
                                                        mAppConfigRowType,
                                                        new String[] { APPLICATION_CONFIGURATION_NAME });
            }
        }
        catch (OpenDataException ode) {
            throw deployError(ode, I18n.loc(
                    "QOS-6047: Failed to initialize Application Configuration: {0}", 
                    ode.getMessage()));
        }
    }

    protected void initAppVars() throws DeploymentException {
        try {
            mAppVarRowType = new CompositeType(
                    "ApplicationVariables",
                    "Application Variable Composite Data",
                    new String[] { APPLICATION_VARIABLES_NAME, 
                                   APPLICATION_VARIABLES_VALUE, 
                                   APPLICATION_VARIABLES_TYPE },
                    new String[] { "Application Variable Name",
                                   "Application Variable Value",
                                   "Application Variable Type" },
                    new OpenType[] { SimpleType.STRING,
                                     SimpleType.STRING,
                                     SimpleType.STRING });
            mAppVarTabularType = new TabularType("ApplicationVariableList",
                                                 "List of Application Variables",
                                                 mAppVarRowType,
                                                 new String[] { APPLICATION_VARIABLES_NAME });
        }
        catch (OpenDataException ode) {
            throw deployError(ode, I18n.loc(
                    "QOS-6048: Failed to initialize Application Variables: {0}", 
                    ode.getMessage()));
        }
    }

    private String[] getNames(Property[] props) {
        int len = props.length;
        String[] names = new String[len];
        for (int i = 0; i < len; i++) {
            names[i] = props[i].getName();
        }
        return names;
    }
    private Object[] getValues(Property[] props, AppConfig ac) {
        int len = props.length;
        Object[] vals = new Object[len];
        for (int i = 0; i < len; i++) {
            Property p = props[i];
            if (p.isRepeating()) {
                int ct = p.count();
                Object[] strs = new Object[ct];
                for (int j = 0; j < ct; j++) {
                    strs[j] = ac.getProperty(p.getName()).getTypedValueAt(j);
                }
                vals[i] = strs;
            }
            else {
                vals[i] = ac.getProperty(p.getName()).getTypedValue();
            }
        }
        return vals;
    }
}
