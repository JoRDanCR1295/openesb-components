package com.sun.jbi.cam.plugin.etlse.jmx;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.HashMap;
import java.util.Set;
import java.io.InputStream;
import java.util.Properties;
import javax.management.MBeanServerConnection;
import javax.management.ObjectInstance;
import javax.management.ObjectName;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.management.AttributeNotFoundException;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanException;
import javax.management.MalformedObjectNameException;
import javax.management.ReflectionException;
import com.sun.jbi.internationalization.Messages;

/**
 * A utility class which provides access to eTL MBean, which exposes methods
 * that this web application calls to provide the functionality.
 * 
 * @author Sujit Biswas
 */
public class MBeanUtil {

    public static final String ETL_MBEAN_LIST = "etl_mbean_list";
    private static final String JMX_HOST = "JmxHostName";
    private static final String JMX_USER = "JmxUserName";
    private static final String JMX_PWD = "JmxPassword";
    private static final String JMX_PORT = "JmxPort";
    private static transient final Logger mLogger = Logger.getLogger(MBeanUtil.class.getName());
   private static final Messages mMessages = Messages.getMessages(MBeanUtil.class);
    private static MBeanServerConnection mbeanServer = null; // MBeans
    // container

    

    static {
        initMBeanServer();
    }

    public static ObjectName getETLMBeanObjectName(String collabName) throws Exception {


        String etlMbeanObjectNameStr = "ETL Monitoring:type=service,collab=" + collabName;
        ObjectName etlRuntimeMBean = new ObjectName(etlMbeanObjectNameStr);

        Set etlMBeans = mbeanServer.queryMBeans(etlRuntimeMBean, null);
        ObjectName etlMbeanObjectName = null;
        if (etlMBeans.size() == 1) {
            ObjectInstance etlObjIn = (ObjectInstance) etlMBeans.iterator().next();
            etlMbeanObjectName = etlObjIn.getObjectName();
        }



        return etlMbeanObjectName;
    }

    private static String getServerJMXPort() {
        String jmxPortToReturn = "-1";
        try {
            String objectNameStr = "com.sun.jbi:ServiceName=ESBAdministrationService,ComponentType=System";
            ObjectName mbeanObj = new ObjectName(objectNameStr);
            jmxPortToReturn = (String)mbeanServer.getAttribute(mbeanObj, "JmxRmiPort");
            mLogger.log(Level.INFO,mMessages.getString("ETLSE-I0174.JMXPort")+ jmxPortToReturn);
        } catch (MBeanException ex) {
            jmxPortToReturn = "-1";
            Logger.getLogger(MBeanUtil.class.getName()).log(Level.SEVERE, null, ex);
        } catch (AttributeNotFoundException ex) {
            jmxPortToReturn = "-1";
            Logger.getLogger(MBeanUtil.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InstanceNotFoundException ex) {
            jmxPortToReturn = "-1";
            Logger.getLogger(MBeanUtil.class.getName()).log(Level.SEVERE, null, ex);
        } catch (ReflectionException ex) {
            jmxPortToReturn = "-1";
            Logger.getLogger(MBeanUtil.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IOException ex) {
            jmxPortToReturn = "-1";
            Logger.getLogger(MBeanUtil.class.getName()).log(Level.SEVERE, null, ex);
        } catch (MalformedObjectNameException ex) {
            jmxPortToReturn = "-1";
            Logger.getLogger(MBeanUtil.class.getName()).log(Level.SEVERE, null, ex);
        } catch (NullPointerException ex) {
            jmxPortToReturn = "-1";
            Logger.getLogger(MBeanUtil.class.getName()).log(Level.SEVERE, null, ex);
        }
        return jmxPortToReturn;
    }

    private static void initMBeanServer() {
        String jmxPort = null; //Default
        String jmxUserName = null;
        String jmxPwd = null;
        String jmxHostName = "localhost"; //Default
        try {
            HashMap<String, String[]> env = new HashMap<String, String[]>();

            InputStream connectionIn = MBeanUtil.class.getResourceAsStream("etlsemonitor.properties");
            Properties connectionProp = new Properties();
            connectionProp.load(connectionIn);
            mLogger.log(Level.INFO,mMessages.getString("ETLSE-I0171.Read_the_propertiesfile"));
            connectionIn.close();
            jmxHostName = connectionProp.getProperty(JMX_HOST, "localhost");
            mLogger.log(Level.INFO,mMessages.getString("ETLSE-I0172.JMXHost")+ jmxHostName);
            if (jmxPwd != null) {
                jmxUserName = connectionProp.getProperty(JMX_USER, "admin");
                mLogger.log(Level.INFO,mMessages.getString("ETLSE-I0173.JMXPAssword")+ jmxPwd);
            } else {
                jmxUserName = connectionProp.getProperty(JMX_USER, "admin");
                jmxPwd = connectionProp.getProperty(JMX_PWD, "adminadmin");
            }

            jmxPort = connectionProp.getProperty(JMX_PORT);
            mLogger.log(Level.INFO,mMessages.getString("ETLSE-I0174.JMXPort")+ jmxPort);
            /*if (jmxPort.equals("-1")) {
                jmxPort = "8686";
            }*/

            String[] credentials = new String[]{jmxUserName, jmxPwd};
            env.put(JMXConnector.CREDENTIALS, credentials);
            JMXServiceURL url = new JMXServiceURL("service:jmx:rmi:///jndi/rmi://" + jmxHostName +
                    ":" + jmxPort + "/jmxrmi");
            mLogger.log(Level.INFO,mMessages.getString("ETLSE-I0175.JMXServiceURL")+ url.toString());
            JMXConnector jmxc = JMXConnectorFactory.connect(url, env);
            mbeanServer = jmxc.getMBeanServerConnection();
        } catch (MalformedURLException e) {
            mLogger.log(Level.INFO, e.getMessage());
        } catch (IOException e) {
            mLogger.log(Level.INFO, e.getMessage());
        }
    }

    public static MBeanServerConnection getMBeanServer() {
        return mbeanServer;
    }

    public static Set getETLEngineMbeanList() throws Exception {
        String etlMbeanObjectNameStr = "ETL Monitoring:type=service";
        ObjectName etlRuntimeMBean = new ObjectName(etlMbeanObjectNameStr + ",*");

        Set etlMBeans = mbeanServer.queryMBeans(etlRuntimeMBean, null);
        mLogger.log(Level.INFO,mMessages.getString("ETLSE-I0176.Error"), etlMBeans);
        return etlMBeans;

    }
}