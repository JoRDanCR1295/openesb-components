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
 * @(#)ConnectionConfigurationImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.common.tale.core.connection.impl;

import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.naming.InitialContext;

import com.sun.jbi.common.tale.core.connection.ConnectionConfiguration;
import com.sun.jbi.common.tale.core.util.TaleConfigurationException;
import com.sun.jbi.common.tale.core.util.I18n;

/** 
 * @author Sun Microsystems.
 *
 */
public class ConnectionConfigurationImpl implements ConnectionConfiguration {
	
    private static final Logger LOGGER = Logger.getLogger(ConnectionConfigurationImpl.class.getName());
    private static final String RESOURCES_MBEAN_OBJ_NAME = "com.sun.appserv:type=resources,category=config";
    private static final String ALE_POOL_NAME = "alesePool";
    private static final String DB_NAME = "aleseDB"; // default name of the ALE SE tablespace on Derby DB
    private static final String USR_NAME = "alese_user";
    private static final String PASSWORD = "alese_user";
    private static final String SERVER_NAME = "localhost"; //default is taken as the localhost where Derby DB is running
    private static final String PORT_NUM = "1527"; // Derby DB port number (taken as the default.

    private ComponentContext mComponentContext;
    private Properties mProperties;

    public ConnectionConfigurationImpl(ComponentContext componentContext, Properties properties) {
        mComponentContext = componentContext;
        mProperties = properties;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.engine.ale.core.connection.ConnectionConfiguration#checkJDBCResource()
     */
    public boolean checkJDBCResource() {
        String jndiResName = mProperties.getProperty(DatabaseJNDIName);
	    
        //Check if the JDBC Resource is already created
        MBeanServer mBeanServer = mComponentContext.getMBeanServer();		
        Object retRes = null;
		
        try {
        	ObjectName configObjName = ObjectName.getInstance(RESOURCES_MBEAN_OBJ_NAME);
            retRes = mBeanServer.invoke(configObjName,
                    "getJdbcResourceByJndiName", 
                    new Object[] { jndiResName }, 
                    new String[] { String.class.getName() });
        } catch (Exception resourceExec) {
            // if the JDBC Resource is not present then the above call throws an exception			
        }
        // if the JDBC Resource exists then return. The assumption is that JDBC resource
        // can exist only for a valid connection pool.
        if (retRes != null) {
            if (LOGGER.isLoggable(Level.FINE)) {
                LOGGER.log(Level.FINE, I18n.loc("TALE-3001: JDBC Resource {0} exists hence is not created.", 
                        jndiResName));
            }
            return true;
        }
	    return false;
	}
	/* (non-Javadoc)
	 * @see com.sun.jbi.engine.ale.core.connection.ConnectionConfiguration#createConnPoolAndResource()
	 */
    public void createConnPoolAndResource() {
        if (checkJDBCResource()) {
            return;
        }
		try {
			String jndiResName = mProperties.getProperty(DatabaseJNDIName);
			
			//Create the connection pool and JNDI JDBC resource.
			String dataSrcClassName = "org.apache.derby.jdbc.ClientDataSource";
			String resourceType = "javax.sql.DataSource";
			createPoolandResource(ALE_POOL_NAME, jndiResName, dataSrcClassName, resourceType);
			
		} catch (Exception configExep) {
			String message = I18n.loc("TALE-7009: Exception thrown while creating ConnectionConfiguration. Exception Detals : {0} ", configExep.getMessage());
			LOGGER.log(Level.SEVERE, message);            		
			throw new TaleConfigurationException(message);
		}
	}
	
	/* (non-Javadoc)
	 * @see com.sun.jbi.engine.ale.core.connection.ConnectionConfiguration#getNamingContext()
	 */
	public InitialContext getNamingContext() {
		return mComponentContext.getNamingContext();
	}
	
	/**
	 * This method creates a connection pool and a jdbc resource pointing to that pool
	 * if the pool is not already created.
	 * Uses non public, non published apis to lookup and create the pools and resources.
	 * Hence it is liable to change as and when the api's change.
	 * 
	 * @param connPoolName, String value for the connection pool name.
	 * @param jndiResName, String value for the Jdbc jndi resource name.
	 * @param dataSrcClassName, String value of the actual datasource class name.
	 * @param resourceType, String value of the resource type. ex: javax.sql.DataSource
	 * @throws Exception
	 */
    private void createPoolandResource(String connPoolName, String jndiResName,
            String dataSrcClassName, String resourceType)
            throws Exception {

        MBeanServer mBeanServer = mComponentContext.getMBeanServer();
        ObjectName configObjName = ObjectName.getInstance(RESOURCES_MBEAN_OBJ_NAME);

        final String sFalse = "false";
        final String sTrue = "true";
        final String sZero = "0";
        
        // check to see if the connection pool is present
        Object retPool = null;
        try {
            retPool = mBeanServer.invoke(configObjName,
                    "getJdbcConnectionPoolByName", 
                    new Object[] { connPoolName },
                    new String[] { String.class.getName() });
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
            attrList.add(new Attribute("allow-non-component-callers", sTrue));
            attrList.add(new Attribute("non-transactional-connections", sFalse));
            attrList.add(new Attribute("is-isolation-level-guaranteed", sFalse));

            // attributes of the Advanced tab in the admin console
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
            props.setProperty("PortNumber", PORT_NUM);
            props.setProperty("DatabaseName", DB_NAME);
            props.setProperty("User", USR_NAME);
            props.setProperty("Password", PASSWORD);
            props.setProperty("serverName", SERVER_NAME);
            props.setProperty("connectionAttributes", ";create=true");
            
            // create the connection pool
            mBeanServer.invoke(configObjName, "createJdbcConnectionPool",
                    new Object[] { attrList, props, "domain" }, new String[] {
            		        javax.management.AttributeList.class.getName(),
            		        java.util.Properties.class.getName(),
            		        String.class.getName() });
        } else {
            // the connection pool exits, log it.
        	if (LOGGER.isLoggable(Level.FINE)) {
        		LOGGER.log(Level.FINE, I18n.loc("TALE-3002: Connection pool {0} exists hence is not created.", 
                        connPoolName));
            }			
        }

        AttributeList attrList2 = new AttributeList();
        attrList2.add(new Attribute("jndi-name", jndiResName));
        attrList2.add(new Attribute("pool-name", connPoolName));
        attrList2.add(new Attribute("object-type", "user"));
        attrList2.add(new Attribute("enabled", sTrue));

        // create the JNDI JDBC resource
        mBeanServer.invoke(configObjName,
                "createJdbcResource", 
                new Object[] { attrList2, new Properties(), "server" }, 
                new String[] { javax.management.AttributeList.class.getName(),
                    java.util.Properties.class.getName(),
                    String.class.getName() });

    }

    public Properties getConnectionProperties() {
        return mProperties;
    }
}
