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
 * @(#)IEPSEBootstrap.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep;

import java.io.File;
import java.lang.management.ManagementFactory;
import java.lang.reflect.Method;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.Bootstrap;
import javax.jbi.component.InstallationContext;
import javax.jbi.component.ComponentContext;
import javax.jbi.management.MBeanNames;
import javax.jbi.JBIException;
import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.StandardMBean;
import javax.management.ObjectName;
import javax.management.MBeanServer;

import com.sun.jbi.engine.iep.core.runtime.util.NDC;
import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;
import com.sun.jbi.internationalization.Messages;

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.qos.config.ConfigPersistence;

/**
 * IEPSEBootstrap.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class IEPSEBootstrap implements Bootstrap, IEPConfig {

    private static final Messages mMessages = Messages.getMessages(IEPSEBootstrap.class);
    private static Logger mLogger = Messages.getLogger(IEPSEBootstrap.class);
    private InstallationContext mContext;
    private ObjectName mInstallerExtName;
    private IEPSEInstallerConfiguration mInstallerConfigMBean;
    private boolean mIsClustered = false;

    public IEPSEBootstrap() {
        mIsClustered = Boolean.getBoolean(GF_IS_CLUSTERED);
        //If there is no GlassFish Cluster, then check if the user has
        //setup IEP in the High-Availability and fail-over mode
        if (mIsClustered == false) {
        	mIsClustered = Boolean.getBoolean(IEP_IS_CLUSTERED);
        }
    }

    /**
     * Initializes the installation environment for a component. This method is
     * expected to save any information from the installation context that may
     * be needed by other methods
     *
     * @param installContext the context containing information from the install
     * command and from the component's installation ZIP file.
     *
     * @throws JBIException when there is an error requiring that the
     * installation be terminated.
     */
    public void init(InstallationContext installContext) throws JBIException {
        try {
            NDC.enter("Application", "IEPSE", "Task", "IEPSEBootstrap.init");
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSEBootstrap.Calling_init_method"));
            }
            mContext = installContext;
            Messages.registerContext(mContext.getContext());
            // acquire JBI Logger after registering context
            mLogger = Messages.getLogger(IEPSEBootstrap.class);
            try {
                
                // Read default properties from jbi.xml using QOS library
            	ComponentConfig compConfig = ComponentConfig.parse(installContext.getInstallRoot());    

                // create installer config. mbean and initialize it with default properties
                mInstallerConfigMBean = new IEPSEInstallerConfiguration();
                mInstallerConfigMBean.setValues(compConfig);

                if (!mIsClustered) {
                	
                	//Make sure that we are running in DAS. Automatic creation of connection pools
                	//and JDBC resources required by IEP are only supported when installing IEP
                	//in DAS. It is not supported when installing in a GlassFish standalone instance
                	
                    /*
                     * https://openesb.atlassian.net/browse/ESBCOMP-114
                     * 
                     * Check if GF classes are available. If not, do not try to create 
                     * datasources.
                     */
                    try {
                        Class proxyFactoryClass = Class.forName("com.sun.appserv.management.client.ProxyFactory");
                        Class[] par=new Class[1];
                        par[0]=MBeanServer.class;
                        Method mthd = proxyFactoryClass.getMethod("getInstance",par);

                        boolean isDAS = true;

                        try {
                            MBeanServer mbeanserver = ManagementFactory.getPlatformMBeanServer();
                            mthd.invoke(proxyFactoryClass, mbeanserver);
                        } catch (Exception e) {
                            isDAS = false;
                        }

                        if (isDAS) {
                            //Create the Non-XA connection pool and JNDI JDBC resource.
                            String dataSrcClassName = "org.apache.derby.jdbc.ClientDataSource";
                            String resourceType = "javax.sql.DataSource";
                            createPoolandResource("iepseDerbyPoolNonXA", "jdbc/iepseDerbyNonXA", dataSrcClassName, resourceType, false);

                            //Create the XA connection pool and JNDI JDBC resource.
                            dataSrcClassName = "org.apache.derby.jdbc.ClientXADataSource";
                            resourceType = "javax.sql.XADataSource";
                            createPoolandResource("iepseDerbyPoolXA", "jdbc/iepseDerbyXA", dataSrcClassName, resourceType, true);
                        }
                    } catch (ClassNotFoundException cnfe) {
                        // Unable to create datasources, the user have to do it
                        // manually
                    }
                }

                // publish installer config. mbean for user to configure at installation time.
                MBeanServer mbeanServer = mContext.getContext().getMBeanServer();
                MBeanNames mbeanNames = mContext.getContext().getMBeanNames();
                mInstallerExtName = mbeanNames.createCustomComponentMBeanName(MBeanNames.BOOTSTRAP_EXTENSION);
                if (!mbeanServer.isRegistered(mInstallerExtName)) {
                    StandardMBean stdBean = new StandardMBean(mInstallerConfigMBean, IEPSEInstallerConfigurationMBean.class);
                    mbeanServer.registerMBean(stdBean, mInstallerExtName);
                }
            } catch (Exception ex) {
                throw new JBIException(mMessages.getString("IEPSEBootstrap.Caught_exception_while_creating_Installatoin_Configuration_MBean_failed_to_init_iepse_component"), ex);
            }

            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSEBootstrap.init_method_has_been_called"));
            }
        } finally {
            NDC.exit("Application", "IEPSE", "Task", "IEPSEBootstrap.init");
        }
    }

    /**
     * Obtains the optional installer configuration MBean ObjectName. If none is
     * provided by this component, this method must return null.
     *
     * @return a JMX object name which represents the MBean registered by the
     * init() method. If none was registered, returns null
     */
    public ObjectName getExtensionMBeanName() {
        return mInstallerExtName;
    }

    /**
     * Called at the beginning of installation of a component to perform any
     * special installation tasks required by the component
     *
     * @throws JBIException when there is an error requiring that the
     * installation be terminated.
     */
    public void onInstall() throws JBIException {
        try {
            NDC.enter("Application", "IEPSE", "Task", "IEPSEBootstrap.onInstall");
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSEBootstrap.Calling_onInstall_method"));
            }
            
            //Save the bootstrap information and the backup the configuration in updateConfig.properties
            
            ComponentContext ctx = mContext.getContext();
            String workspaceRoot = ctx.getWorkspaceRoot();
            
            File upatedConfigFile = new File(workspaceRoot + File.separator + PROP_UPDATED_CONFIG_FILE);
            if (!mInstallerConfigMBean.save(upatedConfigFile)) {
                throw new JBIException(mMessages.getString("IEPSEBootstrap.Caught_exception_while_storing_updated_configuration_properties_file"));
            }
            
            File bootstrapFile = new File(workspaceRoot + File.separator + PROP_BOOTSTRAP_FILE);
            Properties bootstrapProps = new Properties();
            bootstrapProps.setProperty(PROP_NEW_INSTALL, "true");
            if (!PropertyUtil.store(bootstrapProps, bootstrapFile)) {
                throw new JBIException(mMessages.getString("IEPSEBootstrap.Caught_exception_while_storing_bootstrap_properties_file"));
            }
            
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSEBootstrap.onInstall_method_has_been_called"));
            }
             
            //Get the all properties from the installer Config. MBean. Note that at this point
            //the user may have overridden the defaults that were read from the jbi.xml
            ComponentConfig compConfig = mInstallerConfigMBean.getComponentConfig();
            ConfigPersistence.persistConfig(compConfig, workspaceRoot);
        } finally {
            NDC.exit("Application", "IEPSE", "Task", "IEPSEBootstrap.onInstall");
        }
    }

    /**
     * Called at the beginning of uninstallation of a component to perform any
     * special uninstallation tasks required by the component.
     *
     * @throws JBIException when there is an error requiring that
     * the uninstallation be terminated.
     */
    public void onUninstall() throws JBIException {
        try {
            NDC.enter("Application", "IEPSE", "Task", "IEPSEBootstrap.onUninstall");
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSEBootstrap.Calling_onUninstall_method"));
                mLogger.log(Level.FINE, mMessages.getString("IEPSEBootstrap.onUninstall_method_has_been_called"));
            }
        } finally {
            NDC.exit("Application", "IEPSE", "Task", "IEPSEBootstrap.onUninstall");
        }
    }

    /**
     * Cleans up any resources allocated by the bootstrap implementation,
     * including performing deregistration of the extension MBean, if applicable.
     * This method must be called after the onInstall() or onUninstall() method
     * is called, whether it succeeds or fails. It must be called after init()
     * is called, if init() fails by throwing an exception.
     *
     * @throws JBIException - if the bootstrap cannot clean up allocated resources}
     */
    public void cleanUp() throws JBIException {
        try {
            NDC.enter("Application", "IEPSE", "Task", "IEPSEBootstrap.cleanUp");
            if (mInstallerExtName != null) {
                ComponentContext ctx = mContext.getContext();
                if (ctx != null) {
                    if (ctx.getMBeanServer().isRegistered(mInstallerExtName)) {
                        ctx.getMBeanServer().unregisterMBean(mInstallerExtName);
                        if (mLogger.isLoggable(Level.FINE)) {
                            mLogger.log(Level.FINE, mMessages.getString("IEPSEBootstrap.Unregistered_MBean", mInstallerExtName));
                        }
                    }
                }
            }
        } catch (Exception ex) {
            throw new JBIException(mMessages.getString("IEPSEBootstrap.Failed_to_unregister_MBean", new Object[]{mInstallerExtName, ex.getMessage()}), ex);
        } finally {
            NDC.exit("Application", "IEPSE", "Task", "IEPSEBootstrap.cleanUp");
        }
    }

    /**
     * This method creates a connection pool and a jdbc resource pointing to that pool
     * if the pool is not already created.
     * Use non public, non published apis to lookup and create the pools and resources.
     * Hence it is liable to change as and when the api's change.
     *
     * @param connPoolName, String value for the connection pool name.
     * @param jndiResName, String value for the Jdbc jndi resource name.
     * @param dataSrcClassName, String value of the actual datasource class name.
     * @param resourceType, String value of the resource type. ex: javax.sql.DataSource
     * @param isXA, boolean value indicating if this is a XA connection
     * @throws Exception
     */
    private void createPoolandResource(String connPoolName, String jndiResName, String dataSrcClassName, String resourceType, boolean isXA) throws Exception {
        MBeanServer mBeanServer = mContext.getContext().getMBeanServer();
        ObjectName configObjName = ObjectName.getInstance("com.sun.appserv:type=resources,category=config");

        final String sFalse = "false";
        final String sTrue = "true";
        final String sZero = "0";

        Object retRes = null;

        try {
            retRes = mBeanServer.invoke(configObjName, "getJdbcResourceByJndiName", new Object[]{jndiResName}, new String[]{String.class.getName()});
        } catch (Exception resourceExec) {
        // if the JDBC Resource is not present then the above call throws an exception
        // Do nothing as we have to create the pool and then the resource
        }

        // if the JDBC Resource exists then return. The assumption is that JDBC resource
        // can exist only for a valid connection pool.
        if (retRes != null) {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSEBootstrap.JDBC_Resource_is_present", jndiResName));
            }
            return;
        }
//        if (mIsClustered) {
//            String msg = mMessages.getString("IEPSEBootstrap.JDBC_Resources_must_be_defined_before_installing_IEPSE_in_clustered_mode", new Object[]{"jdbc/iepseDBNonXA", "jdbc/iepseDBXA"});
//            mLogger.log(Level.SEVERE, msg);
//            throw new Exception(msg);
//        }
        // check to see if the connextion pool is present
        Object retPool = null;
        try {
            retPool = mBeanServer.invoke(configObjName, "getJdbcConnectionPoolByName", new Object[]{connPoolName}, new String[]{String.class.getName()});
        } catch (Exception poolExec) {
        // if the connPoolName is not there the above call throws an exception
        // Do Nothing as we have to create the pool and the resource
        }

        if (retPool == null) {
            // Create the connection pool and the JDBC JNDI resource.
            AttributeList attrList = new AttributeList();

            // attributes of the General Tab in the admin console
            attrList.add(new Attribute("name", connPoolName));
            attrList.add(new Attribute("datasource-classname", dataSrcClassName));
            attrList.add(new Attribute("res-type", resourceType));

            attrList.add(new Attribute("steady-pool-size", "8"));
            attrList.add(new Attribute("max-pool-size", "32"));
            attrList.add(new Attribute("pool-resize-quantity", "2"));
            attrList.add(new Attribute("idle-timeout-in-seconds", "300"));
            attrList.add(new Attribute("max-wait-time-in-millis", "60000"));

            attrList.add(new Attribute("is-connection-validation-required", sFalse));
            attrList.add(new Attribute("connection-validation-method", "auto-commit"));
            attrList.add(new Attribute("fail_all_connections", sFalse));
            // String allowComponentCallers = (isXA) ? sTrue : sFalse;
            attrList.add(new Attribute("allow-non-component-callers", sTrue));

            attrList.add(new Attribute("non-transactional-connections", sFalse));
            attrList.add(new Attribute("is-isolation-level-guaranteed", sFalse));

            // attributes of the Advanced tab in the admin console
            // attrList.add(new Attribute("statement-timeout-in-seconds",
            // "-1"));
            attrList.add(new Attribute("wrap-jdbc-objects", sFalse));
            attrList.add(new Attribute("validate-atmost-once-period-in-seconds", sZero));
            attrList.add(new Attribute("connection-leak-timeout-in-seconds", sZero));
            attrList.add(new Attribute("connection-leak-reclaim", sFalse));
            attrList.add(new Attribute("connection-creation-retry-attempts", sZero));
            attrList.add(new Attribute("connection-creation-retry-interval-in-seconds", "10"));
            attrList.add(new Attribute("lazy-connection-enlistment", sFalse));
            attrList.add(new Attribute("lazy-connection-association", sFalse));
            attrList.add(new Attribute("associate-with-thread", sFalse));
            attrList.add(new Attribute("match-connections", sFalse));
            attrList.add(new Attribute("max-connection-usage-count", sZero));

            // addition properties tab in the admin console
            Properties props = new Properties();
            props.setProperty("PortNumber", "1527");
            props.setProperty("Password", "iepseDB");
            props.setProperty("User", "iepseDB");
            props.setProperty("serverName", "localhost");
            props.setProperty("DatabaseName", "iepseDB");
            props.setProperty("connectionAttributes", ";create=true");

            // create the connection pool
            mBeanServer.invoke(configObjName, "createJdbcConnectionPool", new Object[]{attrList, props, "domain"}, new String[]{javax.management.AttributeList.class.getName(), java.util.Properties.class.getName(), String.class.getName()});
        } else {
            // the connection pool exits, log it.
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSEBootstrap.Connection_is_present", connPoolName));
            }
        }

        AttributeList attrList2 = new AttributeList();
        attrList2.add(new Attribute("jndi-name", jndiResName));
        attrList2.add(new Attribute("pool-name", connPoolName));
        attrList2.add(new Attribute("object-type", "user"));
        attrList2.add(new Attribute("enabled", sTrue));

        // create the JNDI JDBC resource
        mBeanServer.invoke(configObjName, "createJdbcResource", new Object[]{attrList2, new Properties(), "server"}, new String[]{javax.management.AttributeList.class.getName(), java.util.Properties.class.getName(), String.class.getName()});
    }
}
