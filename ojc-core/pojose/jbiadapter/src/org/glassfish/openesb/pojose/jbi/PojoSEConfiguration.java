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
 * @(#)PojoSEConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.pojose.jbi;

import com.sun.jbi.common.qos.config.AbstractConfigMBean;
import com.sun.jbi.common.qos.config.ComponentConfig;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;
import javax.management.AttributeChangeNotification;
import javax.management.InvalidAttributeValueException;
import javax.management.ListenerNotFoundException;
import javax.management.MBeanException;
import javax.management.MBeanNotificationInfo;
import javax.management.Notification;
import javax.management.NotificationBroadcasterSupport;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeDataSupport;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.SimpleType;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.TabularDataSupport;
import javax.management.openmbean.TabularType;

/**
 *
 * @author gpatil
 */
public class PojoSEConfiguration  extends AbstractConfigMBean
                        implements PojoSEConfigurationMBean {

    /*
     * 201-230
     */
    private static final Logger logger = Logger.getLogger(PojoSEConfiguration.class.getName());
    
    private ComponentContext ctx;
    
    private NotificationBroadcasterSupport broadcasterSupport = new NotificationBroadcasterSupport();    
    
    // Configuration file name for environment variables
    private static final String PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME = "ApplicationVariables.properties";
    
    // Application variables row fields
    private static final String APPLICATION_VARIABLES_ROW_KEY = "name";
    private static final String APPLICATION_VARIABLES_VALUE_FIELD = "value";
    private static final String APPLICATION_VARIABLES_TYPE_FIELD = "type";
    
    // Global application configurations
    private Map mAppVarMap;
    private CompositeType mAppVarRowType = null;
    private TabularType mAppVarTabularType = null;
    
    public PojoSEConfiguration(ComponentContext ctx, ComponentConfig config) 
                                    throws DeploymentException {
        super(ctx, config);
        
        this.ctx = ctx;
        
        try {
            mAppVarMap = loadApplicationVariablesConfig(ctx.getWorkspaceRoot());
            mAppVarRowType = createApplicationVariableCompositeType();
            mAppVarTabularType = createApplicationVariableTabularType();
        } catch (Exception e) {
            String msg = I18n.loc("POJOSE-7523: Failed during mbean initialization {0}", e);
            logger.severe(msg);
            throw new DeploymentException(msg, e);
        }
    }

    public Integer getCoreThreadPoolSize() {
        String tc = getConfig().getProperty(CORE_THREAD_POOL_SIZE).getValue();
        Integer ret = new Integer(DEFAULT_CORE_THREAD_POOL_SIZE);
        if (tc != null){
            try {
                ret = Integer.valueOf(tc);
            } catch (Exception ex){
                ret = new Integer(DEFAULT_CORE_THREAD_POOL_SIZE);
            }
        }
        return ret;
    }

    public Integer getMaxThreadPoolSize() {
        String tc = getConfig().getProperty(MAX_THREAD_POOL_SIZE).getValue();
        Integer ret = new Integer(DEFAULT_MAX_THREAD_POOL_SIZE);
        if (tc != null){
            try {
                ret = Integer.valueOf(tc);
            } catch (Exception ex){
                ret = new Integer(DEFAULT_MAX_THREAD_POOL_SIZE);
            }
        }
        return ret;
    }

    public void setCoreThreadPoolSize(Integer cz) {
        int val = (cz == null) ? DEFAULT_CORE_THREAD_POOL_SIZE : cz.intValue();
        String ov = getConfig().getProperty(CORE_THREAD_POOL_SIZE).getValue();
        getConfig().getProperty(CORE_THREAD_POOL_SIZE).setValue(String.valueOf(val));        
        persistCfg();
        String msg = I18n.loc("POJOSE-4550: Changed core thread pool size from {0} to {1}", ov, val);
        Logger.getLogger(PojoSEConfiguration.class.getName()).config(msg);
        fireCfgEvent(msg, CORE_THREAD_POOL_SIZE, Integer.class.getName(), ov, cz);
    }

    public void setMaxThreadPoolSize(Integer mz) {
        int val = (mz == null) ? DEFAULT_MAX_THREAD_POOL_SIZE : mz.intValue();
        String ov = getConfig().getProperty(MAX_THREAD_POOL_SIZE).getValue();
        getConfig().getProperty(MAX_THREAD_POOL_SIZE).setValue(String.valueOf(val));        
        persistCfg();
        String msg = I18n.loc("POJOSE-4551: Changed max thread pool size from {0} to {1}", ov, val);
        Logger.getLogger(PojoSEConfiguration.class.getName()).config(msg);
        fireCfgEvent(msg, CORE_THREAD_POOL_SIZE, Integer.class.getName(), ov, mz);
    }

    /**
     * 
     * Returns ThreadPoolBlockingQueueSize. Note ThreadPoolBlockingQueueSize can
     * only be set during bootstrap process. Hence the setter is in bootstrap
     * config MBean.
     * 
     * @return Integer ThreadPoolBlockingQueueSize
     */
    public Integer getThreadPoolBlockingQueueSize() {
        String tc = getConfig().getProperty(THREAD_POOL_BLOCKING_QUEUE_SIZE).getValue();
        Integer ret = new Integer(DEFAULT_THREAD_POOL_BLOCKING_QUEUE_SIZE);
        if (tc != null){
            try {
                ret = Integer.valueOf(tc);
            } catch (Exception ex){
                ret = new Integer(DEFAULT_THREAD_POOL_BLOCKING_QUEUE_SIZE);
            }
        }
        return ret;
    }
    
    public void setThreadPoolBlockingQueueSize(Integer cz) {
        int val = (cz == null) ? DEFAULT_THREAD_POOL_BLOCKING_QUEUE_SIZE : cz.intValue();
        String ov = getConfig().getProperty(THREAD_POOL_BLOCKING_QUEUE_SIZE).getValue();
        getConfig().getProperty(THREAD_POOL_BLOCKING_QUEUE_SIZE).setValue(String.valueOf(val));
        persistCfg();
        String msg = I18n.loc("POJOSE-4552: Changed blocking queue size from {0} to {1}", ov, val);
        Logger.getLogger(PojoSEConfiguration.class.getName()).config(msg);
        //fireCfgEvent(msg, CORE_THREAD_POOL_SIZE, Integer.class.getName(), ov, mz);
    }    

    private synchronized void fireCfgEvent(String msg, String attrName, String attrType,
            Object oldVal, Object newVal) {
        long seqNo = 0;
        Notification notif = new AttributeChangeNotification(this, seqNo,
                System.currentTimeMillis(), msg, attrName,
                attrType, oldVal, newVal);
        broadcasterSupport.sendNotification(notif);
    }

    protected void persistCfg(){
        try {
            this.persistConfiguration();
        } catch (Exception ex){
            String msg = I18n.loc("POJOSE-7521: Exception while persisting configuration changes. {0}", ex);
            Logger.getLogger(PojoSEConfiguration.class.getName()).severe(msg);
        }
    }
    // Support notifying about config changes
    public MBeanNotificationInfo[] getNotificationInfo(){
        return new MBeanNotificationInfo[] {new MBeanNotificationInfo(new String[] {AttributeChangeNotification.ATTRIBUTE_CHANGE}, AttributeChangeNotification.class.getName(), "Attribute changed")};
    }

    public void addNotificationListener(NotificationListener listener, NotificationFilter filter, Object handback){
        broadcasterSupport.addNotificationListener(listener, filter, handback);
    }

    public void removeNotificationListener(NotificationListener listener) throws ListenerNotFoundException {
        broadcasterSupport.removeNotificationListener(listener);
    }

    public void removeNotificationListener(NotificationListener listener, NotificationFilter filter, Object handback) throws ListenerNotFoundException{
        broadcasterSupport.removeNotificationListener(listener, filter, handback);
    }
    
    private Map loadApplicationVariablesConfig(String workspaceRoot) throws MBeanException {
        Properties persistedConfig = new Properties();
        Map appVarMap = new HashMap();
        
        File appVarPersistFileName = new File(workspaceRoot, PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME);
        if (!appVarPersistFileName.exists()) {
            return appVarMap;
        }
        
        try {
            InputStream is = new FileInputStream(appVarPersistFileName);
            persistedConfig.load(is);
            is.close();
            
            // load the persisted environment variable configurations in the map
            for (Enumeration e = persistedConfig.propertyNames(); e.hasMoreElements(); ) {
                String name = (String) e.nextElement();
                String metadata = persistedConfig.getProperty(name);
                int startIndex = metadata.indexOf("{");
                String value = (startIndex == 0)? null : metadata.substring(0, startIndex);
                String type = metadata.substring(startIndex + 1, metadata.length() -1);
                appVarMap.put(name, new String[] {value, type});
            }
        } catch (Exception ex) {
            String msg = I18n.loc("POJOSE-7539: Failed to load application variables {0} {1}", PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME, ex);
            logger.severe(msg);
            throw new MBeanException(ex, msg);
        }
        
        return appVarMap;
    }
    
    private CompositeType createApplicationVariableCompositeType() throws OpenDataException {
        if (mAppVarRowType != null) {
            return mAppVarRowType;
        }
        
        String[] appVarRowAttrNames = { APPLICATION_VARIABLES_ROW_KEY, APPLICATION_VARIABLES_VALUE_FIELD, APPLICATION_VARIABLES_TYPE_FIELD };
        String[] appVarRowAttrDesc = { "Application Variable Name", "Application Variable Value", "Application Variable Type" };
        OpenType[] appVarRowAttrTypes = { SimpleType.STRING, SimpleType.STRING, SimpleType.STRING };
        
        mAppVarRowType = new CompositeType("ApplicationVariables",
                                           "Application Variable Composite Data",
                                           appVarRowAttrNames,
                                           appVarRowAttrDesc,
                                           appVarRowAttrTypes);
        
        return mAppVarRowType;
    }
    
    private TabularType createApplicationVariableTabularType() throws OpenDataException {
        if (mAppVarTabularType != null) {
            return mAppVarTabularType;
        }
        
        if (mAppVarRowType == null) {
            mAppVarRowType = createApplicationVariableCompositeType();
        }
        
        mAppVarTabularType = new TabularType("ApplicationVariableList",
                                             "List of Application Variables",
                                             mAppVarRowType,
                                             new String[] { APPLICATION_VARIABLES_ROW_KEY } );
        
        return mAppVarTabularType;
    }

    /**
     * This operation adds a new application variable. If a variable with the same name 
     * already exists, the operation fails.
     * 
     * @param name - name of the application variable
     * @param appVar - this is the application variable compoiste
     * @throws MBeanException if an error occurs in adding the application variable to the 
     *         component. 
     */
    public void addApplicationVariable(String name, CompositeData appVar) throws InvalidAttributeValueException, MBeanException {
        if (mAppVarMap.containsKey(name)) {
            String msg = I18n.loc("POJOSE-7524: Application variable name already exists {0}", name);
            logger.severe(msg);
            throw new MBeanException(new Exception(msg));
        }
        
        CompositeType rowType = appVar.getCompositeType();
        if (rowType.keySet().size() != 3) {
            String msg = I18n.loc("POJOSE-7525: Invalid item size for app variable {0}", rowType.keySet().size());
            logger.severe(msg);
            throw new InvalidAttributeValueException(msg);
        }
        
        if (!appVar.containsKey(APPLICATION_VARIABLES_ROW_KEY)) {
            String msg = I18n.loc("POJOSE-7526: Invalid key for composite data for app variable {0}", name);
            logger.severe(msg);
            throw new InvalidAttributeValueException(msg);
        } 
        
        String appVarValue = (String)appVar.get(APPLICATION_VARIABLES_VALUE_FIELD);
        String appVarType = (String)appVar.get(APPLICATION_VARIABLES_TYPE_FIELD);
        
        if (appVarValue == null) {
            String msg = I18n.loc("POJOSE-7527: Invalid app variable composite data no value field {0}", name);
            logger.severe(msg);
            throw new InvalidAttributeValueException(msg);
        }
        
        if (appVarType == null) {
            String msg = I18n.loc("POJOSE-7528: Invalid app variable composite data no type field", name);
            logger.severe(msg);
            throw new InvalidAttributeValueException(msg);
        }
        
        mAppVarMap.put(name, new String[] { appVarValue, appVarType });
        
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.loc("POJOSE-7529: New application variable added {0} {1}", name, appVarValue);
            logger.finest(msg);
        }
        
        persistApplicationVariablesConfig();
    }
    
    /**
     * This operation sets an application variable. If a variable does not exist with 
     * the same name, its an error.
     * 
     * @param name - name of the application variable
     * @param appVar - this is the application variable compoiste to be updated.
     * @throws MBeanException if one or more application variables cannot be deleted
     */
    public void setApplicationVariable(String name, CompositeData appVar) throws InvalidAttributeValueException, MBeanException {
        if (!mAppVarMap.containsKey(name)) {
            String msg = I18n.loc("POJOSE-7530: Application variable does not exist for set {0}", name);
            logger.severe(msg);
            throw new MBeanException(new Exception(msg));
        }
        
        CompositeType rowType = appVar.getCompositeType();
        if (rowType.keySet().size() != 3) {
            String msg = I18n.loc("POJOSE-7531: Invalid item size for app variable", rowType.keySet().size());
            logger.severe(msg);
            throw new InvalidAttributeValueException(msg);
        }
        
        if (!appVar.containsKey(APPLICATION_VARIABLES_ROW_KEY)) {
            String msg = I18n.loc("POJOSE-7526: Invalid key for composite data for app variable {0}", name);
            logger.severe(msg);
            throw new InvalidAttributeValueException(msg);
        } 
        
        String appVarValue = (String)appVar.get(APPLICATION_VARIABLES_VALUE_FIELD);
        String appVarType = (String)appVar.get(APPLICATION_VARIABLES_TYPE_FIELD);
        
        if (appVarValue == null) {
            String msg = I18n.loc("POJOSE-7527: Invalid app variable composite data no value field {0}", name);
            logger.severe(msg);
            throw new InvalidAttributeValueException(msg);
        }
        
        if (appVarType == null) {
            String msg = I18n.loc("POJOSE-7534: Invalid app variable composite data no type field {0}", name);
            logger.severe(msg);
            throw new InvalidAttributeValueException(msg);
        }
        
        mAppVarMap.put(name, new String[] { appVarValue, appVarType });
        
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.loc("POJOSE-7535: Application variable updated {0} {1}", name, appVarValue );
            logger.finest(msg);
        }
    
        // persist the application variable properties
        persistApplicationVariablesConfig();
    }

    /**
     * This operation deletes an application variable, if a variable with the specified name does
     * not exist, it's an error.
     *
     * @param name - name of the application variable
     * @throws MBeanException on errors.
     */
     public void deleteApplicationVariable(String name) throws MBeanException {
        if (!mAppVarMap.containsKey(name)) {
            String msg = I18n.loc("POJOSE-7536: Application variable does not exist for delete {0}", name);
            logger.severe(msg);
            throw new MBeanException(new Exception(msg));
        }
        
        mAppVarMap.remove(name);
        
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.loc("POJOSE-7537: Application variable deleted {0}", name);
            logger.finest(msg);
        }
        
        persistApplicationVariablesConfig();
    }

    public TabularData getApplicationVariables() {
        TabularData tabularData = new TabularDataSupport(mAppVarTabularType);
        for (Iterator iter = mAppVarMap.keySet().iterator(); iter.hasNext(); ) { 
            String name = (String) iter.next();
            String[] metadata = (String[]) mAppVarMap.get(name);
            String value = metadata [0];
            String type = metadata [1];
            Object[] data = ("PASSWORD".equalsIgnoreCase(type) && value != null && !"".equals(value))? 
                            new Object[] { name, "*******", type } : new Object[] {name, value, type};
            try {
                CompositeData rowData = new CompositeDataSupport(mAppVarRowType,
                                                                 new String[] { APPLICATION_VARIABLES_ROW_KEY , 
                                                                                APPLICATION_VARIABLES_VALUE_FIELD , 
                                                                                APPLICATION_VARIABLES_TYPE_FIELD },
                                                                 data);
                
                tabularData.put(rowData);
            } catch (OpenDataException e) {
                String msg = I18n.loc("POJOSE-7538: Unable to construct composite data for app variable", e);
                logger.severe(msg);
                throw new RuntimeException(msg);
            }
        }
        
        return tabularData;
    }

    public Map retrieveApplicationVariablesMap() {
        return mAppVarMap;
    }
    
    public void updateApplicationVariablesMap(Map appVarMap) throws MBeanException {
        mAppVarMap = appVarMap;
        persistApplicationVariablesConfig();
    }
    
    private void persistApplicationVariablesConfig() throws MBeanException {
        // Persist the changed configuration        
        try {
            File appVarPersistFileName = new File(ctx.getWorkspaceRoot(), PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME);
            OutputStream os = new FileOutputStream(appVarPersistFileName);
            for (Iterator iter = mAppVarMap.keySet().iterator(); iter.hasNext(); ) {
               String key = (String) iter.next();
               String[] metadata = (String[]) mAppVarMap.get(key);
               String value = metadata[0];
               String type = metadata[1];
               String prop = (value != null)? key + "=" + value + "{" + type + "}\n" : key + "={" + type + "}\n";
               os.write(prop.getBytes());
            } 
            os.close();
        } catch (Exception ex) {
            String msg = I18n.loc("POJOSE-7540: Failed to persist application variables {0} {1}", PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME, ex);
            logger.severe(msg);
            throw new MBeanException(ex, msg); 
        } 
    }
}

