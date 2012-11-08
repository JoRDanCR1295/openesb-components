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
 * @(#)JMSBindingDeployerTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc;

import junit.framework.*;

import java.net.URL;
import java.text.MessageFormat;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.ResourceBundle;
import java.util.logging.Logger;
import java.util.logging.Level;
import javax.jbi.JBIException;
import javax.jbi.management.DeploymentException;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.management.*;

import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.management.message.JBITaskMessageBuilder;
import com.sun.jbi.management.message.DefaultJBITaskMessageBuilder;
import com.sun.jbi.jmsbc.jms.ChannelManager;
import com.sun.jbi.jmsbc.jms.Channel;
import com.sun.jbi.jmsbc.mbeans.JMSBCRuntimeConfiguration;
import com.sun.jbi.jmsbc.mbeans.RuntimeConfiguration;
import com.sun.jbi.jmsbc.util.JMSBCContext;

import org.jmock.core.*;
import org.jmock.*;

/**
 *
 * Unit tests for JMSBindingDeployer
 */
public class JMSBindingDeployerTest extends org.jmock.cglib.MockObjectTestCase {

    // Proxies and concretes
    private ComponentContext componentContext = null;
    private MBeanServer mbeanServer = null;
    private ChannelManager channelManager = null;
    private DeliveryChannel deliveryChannel = null;
    private JMSBCRuntimeConfiguration runtimeConfiguration = null;
    private StatusProviderHelper statusProviderHelper = null;
    private MessagingChannel messagingChannel;
    
    private HashMap suMap = null;
    
    // Mocks
    private Mock componentContextMock = null;
    private Mock mbeanServerMock = null;
    private Mock channelManagerMock = null;
    private Mock deliveryChannelMock = null;
    private Mock runtimeConfigurationMock = null;
    private Mock messagingChannelMock = null;
    
    
    public JMSBindingDeployerTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        componentContextMock = mock(ComponentContext.class);
        componentContext = (ComponentContext)componentContextMock.proxy();
        componentContextMock.stubs().method("getComponentName").will(returnValue("JMS-BC"));
        componentContextMock.stubs().method("getLogger").will(returnValue(Logger.getLogger(JMSBindingDeployerTest.class.getName(), null)));
        
        mbeanServerMock = mock(MBeanServer.class);
        mbeanServer = (MBeanServer)mbeanServerMock.proxy();
        mbeanServerMock.stubs().method("queryNames").will(returnValue(null));
        
        componentContextMock.stubs().method("getMBeanServer").will(returnValue(mbeanServer));
        
        channelManagerMock = mock(ChannelManager.class);
        channelManager = (ChannelManager)channelManagerMock.proxy();
        
        deliveryChannelMock = mock(DeliveryChannel.class);
        deliveryChannel = (DeliveryChannel)deliveryChannelMock.proxy();
        
        messagingChannelMock= mock(MessagingChannel.class);
        messagingChannelMock.stubs().method("installServiceQualities");
        messagingChannelMock.stubs().method("uninstallServiceQualities");
        messagingChannel = (MessagingChannel)messagingChannelMock.proxy();
        
        
        URL url = getClass().getClassLoader().getResource("com/sun/jbi/jmsbc/testdir/config.properties");
        String testDir = url.getPath();
        testDir = testDir.substring(0, testDir.lastIndexOf('/'));
//        runtimeConfiguration = new JMSBCRuntimeConfiguration("test/com/sun/jbi/jmsbc/testdir", null);
      runtimeConfiguration = new JMSBCRuntimeConfiguration(testDir, null);
        
        statusProviderHelper = 
                new StatusProviderHelper("JMSBC", 
                                         "Binding",
                                         "JMS Binding Component",
                                         mbeanServer);
        
        suMap = new HashMap();
        JMSBCContext.getRef().setChannel(messagingChannel);
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(JMSBindingDeployerTest.class);
        
        return suite;
    }

    /**
     * Test of deploy method, of class com.sun.jbi.jmsbc.JMSBindingDeployer.
     */
    public void testDeploy() throws Exception {
        System.out.println("deploy");
        
        String serviceUnitID = "SERVICE_UNIT_ID";
        String asaFilePath = "/bogusASAFilePath";
        
        Mock serviceUnitMock = mock(ServiceUnit.class);
        serviceUnitMock.expects(once()).method("deploy");
        ServiceUnit serviceUnit = (ServiceUnit)serviceUnitMock.proxy();
        suMap.put(serviceUnitID, serviceUnit);
        
        JMSBindingDeployer instance = 
                new JMSBindingDeployer(componentContext,
                                       statusProviderHelper,
                                       channelManager,
                                       suMap,
                                       null,
                                       runtimeConfiguration);        
        String result = instance.deploy(serviceUnitID, asaFilePath);
        System.out.println(result);
        assertTrue(result != null);        
    }

    /**
     * Test of shutdown method, of class com.sun.jbi.jmsbc.JMSBindingDeployer.
     */
    public void testShutdown() throws Exception {
        System.out.println("shutdown");

        String serviceUnitID = "SERVICE_UNIT_ID";
        
        Mock serviceUnitMock = mock(ServiceUnit.class);
        serviceUnitMock.expects(once()).method("shutdown");
        ServiceUnit serviceUnit = (ServiceUnit)serviceUnitMock.proxy();
        suMap.put(serviceUnitID, serviceUnit);
        JMSBindingDeployer instance = 
                new JMSBindingDeployer(componentContext,
                                       statusProviderHelper,
                                       channelManager,
                                       suMap,
                                       null,
                                       runtimeConfiguration);
        instance.shutDown(serviceUnitID);
    }

    /**
     * Test of init method, of class com.sun.jbi.jmsbc.JMSBindingDeployer.
     */
    public void testInit() throws Exception {
        System.out.println("init");
        
        String serviceUnitID = "SERVICE_UNIT_ID";
        String suPath = "." + java.io.File.separator;
        
        JMSBindingDeployer instance = 
                new JMSBindingDeployer(componentContext,
                                       statusProviderHelper,
                                       channelManager,
                                       suMap,
                                       null,
                                       runtimeConfiguration);
        
        instance.init(serviceUnitID, suPath);
        
        ServiceUnit su = (ServiceUnit)suMap.get(serviceUnitID);
        
        assertTrue(su.getServiceUnitId().equals(serviceUnitID));
    }

    /**
     * Test of start method, of class com.sun.jbi.jmsbc.JMSBindingDeployer.
     */
    public void testStart() throws Exception {
        System.out.println("start");
        
        String serviceUnitID = "SERVICE_UNIT_ID";
        
        Mock serviceUnitMock = mock(ServiceUnit.class);
        serviceUnitMock.expects(once()).method("start");
        ServiceUnit serviceUnit = (ServiceUnit)serviceUnitMock.proxy();
        suMap.put(serviceUnitID, serviceUnit);
        JMSBindingDeployer instance = 
                new JMSBindingDeployer(componentContext,
                                       statusProviderHelper,
                                       channelManager,
                                       suMap,
                                       null,
                                       runtimeConfiguration);
        
        instance.start(serviceUnitID);        
    }

    /**
     * Test of stop method, of class com.sun.jbi.jmsbc.JMSBindingDeployer.
     */
    public void testStop() throws Exception {
        System.out.println("stop");
        
        String serviceUnitID = "SERVICE_UNIT_ID";
        
        Mock serviceUnitMock = mock(ServiceUnit.class);
        serviceUnitMock.expects(once()).method("stop");
        ServiceUnit serviceUnit = (ServiceUnit)serviceUnitMock.proxy();
        suMap.put(serviceUnitID, serviceUnit);
        JMSBindingDeployer instance = 
                new JMSBindingDeployer(componentContext,
                                       statusProviderHelper,
                                       channelManager,
                                       suMap,
                                       null,
                                       runtimeConfiguration);

        instance.stop(serviceUnitID);        
    }

    /**
     * Test of undeploy method, of class com.sun.jbi.jmsbc.JMSBindingDeployer.
     */
    public void testUndeploy() throws Exception {
        System.out.println("undeploy");

        String serviceUnitID = "SERVICE_UNIT_ID";
        String asaFilePath = "/bogusASAFilePath";
        
        Mock serviceUnitMock = mock(ServiceUnit.class);
        serviceUnitMock.stubs().method("shutdown");
        ServiceUnit serviceUnit = (ServiceUnit)serviceUnitMock.proxy();
        suMap.put(serviceUnitID, serviceUnit);
        
        componentContextMock.stubs().method("getComponentName")
                                    .will(returnValue("JMS Binding Component"));
        JMSBindingDeployer instance = 
                new JMSBindingDeployer(componentContext,
                                       statusProviderHelper,
                                       channelManager,
                                       suMap,
                                       null,
                                       runtimeConfiguration);

        String result = instance.undeploy(serviceUnitID, asaFilePath);
        assertTrue(result != null);
    }

    /**
     * Test of getServiceUnits method, of class com.sun.jbi.jmsbc.JMSBindingDeployer.
     */
    public void testGetServiceUnits() {
        System.out.println("getServiceUnits");
        
        String serviceUnitID1 = "SERVICE_UNIT_ID1";
        String serviceUnitID2 = "SERVICE_UNIT_ID2";
        String suPath = "." + java.io.File.separator;

        try {
            JMSBindingDeployer instance = 
                    new JMSBindingDeployer(componentContext,
                                           statusProviderHelper,
                                           channelManager,
                                           suMap,
                                           null,
                                           runtimeConfiguration);

            instance.init(serviceUnitID1, suPath);
            instance.init(serviceUnitID2, suPath);

            Collection result = instance.getServiceUnits();
            
            assertTrue(result.size() == 2);
            
            java.util.Iterator iter = result.iterator();
            while (iter.hasNext()) {
                ServiceUnit su = (ServiceUnit)iter.next();
                assertTrue(su.getServiceUnitId()==serviceUnitID1 ||
                           su.getServiceUnitId()==serviceUnitID2);
            }
        } catch (Exception ex) {
            fail ("Test testGetServiceUnits failed: " + ex);
        }
    }
    
}
