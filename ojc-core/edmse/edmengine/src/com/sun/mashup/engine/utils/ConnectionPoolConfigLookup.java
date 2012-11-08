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
 * @(#)I18N.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.mashup.engine.utils;

import java.util.Map;
import java.util.HashMap;
import java.util.Properties;

import java.io.IOException;
import java.net.MalformedURLException;

import javax.management.MBeanServerConnection;
import javax.management.ObjectName;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;

/**
 *
 * @author admin
 */
public class ConnectionPoolConfigLookup {

    private static MBeanServerConnection mbeanServer = null; // MBeans

    public static void initMBeanServer(MBeanServerConnection mbeanServerConn) {
        mbeanServer = mbeanServerConn;
    }

    public static MBeanServerConnection getConnection() {
        return mbeanServer;
    }

    private static String getPoolName(String jndiResName) {
        String poolName = null;
        if (jndiResName != null && !"".equalsIgnoreCase(jndiResName)) {
            try {
                String objectNameStr = "amx:j2eeType=X-JDBCResourceConfig,name=" + jndiResName;
                ObjectName mbeanObj = new ObjectName(objectNameStr);
                poolName = (String) mbeanServer.getAttribute(mbeanObj, "PoolName");
            } catch (Throwable e) {
                e.printStackTrace();
            }
        }
        return poolName;
    }

    public static Map getConnectionPoolProperties(String jndiResName) throws Exception {
        if (jndiResName == null || "".equalsIgnoreCase(jndiResName)) {
            return new HashMap();
        }
        String objectNameStr = "amx:j2eeType=X-JDBCConnectionPoolConfig,name=" + getPoolName(jndiResName);
        ObjectName mbeanObj = new ObjectName(objectNameStr);
        Map props = (Map) mbeanServer.getAttribute(mbeanObj, "Properties");
        return props;
    }

    public static void main(String[] args) {
//        Properties jmxProps = new Properties();
//        jmxProps.put("JmxHost", "localhost");
//        jmxProps.put("JmxPort", "8686");
//        jmxProps.put("JmxUser", "admin");
//        jmxProps.put("JmxPassword", "adminadmin");
//        ConnectionPoolConfigLookup.initMBeanServer(jmxProps);

    }
}
