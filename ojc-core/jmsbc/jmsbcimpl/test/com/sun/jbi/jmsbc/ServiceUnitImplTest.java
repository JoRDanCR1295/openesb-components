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
 * @(#)ServiceUnitImplTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc;

import junit.framework.*;
import java.io.File;
import java.net.URL;
import java.text.MessageFormat;
import java.util.Collection;
import java.util.Collections;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.HashMap;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.logging.Logger;
import java.util.logging.Level;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import javax.management.*;

import com.sun.jbi.common.qos.messaging.BaseMessagingChannel;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusReporting;
import com.sun.jbi.jmsbc.EndpointImpl;
import com.sun.jbi.jmsbc.Endpoint.EndpointState;
import com.sun.jbi.jmsbc.Endpoint.EndpointType;
//import com.sun.jbi.jmsbc.packaging.ConfigurationValidator;
import com.sun.jbi.jmsbc.packaging.EndpointConfiguration;
import com.sun.jbi.jmsbc.packaging.EndpointConfigurationFactory;
import com.sun.jbi.jmsbc.packaging.InvalidConfigurationException;
import com.sun.jbi.jmsbc.packaging.WSDLConfigurations;
import com.sun.jbi.jmsbc.util.JMSBCContext;
import com.sun.jbi.jmsbc.extensions.JMSOperation;
import com.sun.jbi.jmsbc.jms.Channel;
import com.sun.jbi.jmsbc.jms.ChannelManager;
import com.sun.jbi.jmsbc.mbeans.JMSBCRuntimeConfiguration;
import com.sun.jbi.jmsbc.mbeans.RuntimeConfiguration;

import org.jmock.core.*;
import org.jmock.*;

/**
 *
 * Unit test(s) from ServiceUnitImpl class
 */
public class ServiceUnitImplTest extends org.jmock.cglib.MockObjectTestCase {
    
    // proxies and concretes
    private String suid = null;
    private ComponentContext context = null;
    private StatusProviderHelper statusProviderHelper = null;
    private ChannelManager jmsChannelMgr = null;
    private DeliveryChannel deliveryChannel = null;
    private JMSBCRuntimeConfiguration runtimeConfiguration = null;
    private MBeanServer mbeanServer = null;
    private MessagingChannel messagingChannel;
    
    // mocks
    private Mock contextMock = null;
    private Mock statusProviderHelperMock = null;
    private Mock jmsChannelMgrMock = null;
    private Mock inboundProcessorMgrMock = null;
    private Mock deliveryChannelMock = null;
    private Mock runtimeConfigurationMock = null;
    private Mock mbeanServerMock = null;
    private Mock messagingChannelMock = null;
    private Map endpoints = new HashMap();
    
    
    public ServiceUnitImplTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        suid = "A-SERVICE-UNIT-ID";
        
        contextMock = mock(ComponentContext.class);
        contextMock.stubs().method("getDeliveryChannel");
        contextMock.stubs().method("getComponentName");
        contextMock.stubs().method("activateEndpoint");
        contextMock.stubs().method("getLogger").will(returnValue(Logger.getLogger(ServiceUnitImplTest.class.getName(), null)));
        context = (ComponentContext)contextMock.proxy();
                
        jmsChannelMgrMock = mock(ChannelManager.class);
        jmsChannelMgr = (ChannelManager)jmsChannelMgrMock.proxy();
        
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
                
        mbeanServerMock = mock(MBeanServer.class);
        mbeanServer = (MBeanServer)mbeanServerMock.proxy();
        mbeanServerMock.stubs().method("queryNames").will(returnValue(null));
        contextMock.stubs().method("getMBeanServer").will(returnValue(mbeanServer));
        
        statusProviderHelperMock = mock(StatusProviderHelper.class,
                                        new Class []  {String.class,
                                                       String.class,
                                                       String.class,
                                                       MBeanServer.class},
                                        new Object [] {"JMSBC", 
                                                       "Binding",
                                                       "JMS Binding Component",
                                                       mbeanServer});
        statusProviderHelper = (StatusProviderHelper)statusProviderHelperMock.proxy();      
        
        EndpointImpl endpoint = new EndpointImpl();
        endpoint.setServiceName(new QName("MySerivceName"));
        endpoint.setEndpointName("MyEndpointName");
        endpoint.setEndpointType(Endpoint.EndpointType.OUTBOUND);
        
        endpoints.put(getEndpointKey(endpoint), endpoint);   
        
        JMSBCContext.getRef().setChannel(messagingChannel);
        
        
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(ServiceUnitImplTest.class);
        
        return suite;
    }

    /**
     * Test of getServiceUnitId method, of class com.sun.jbi.jmsbc.ServiceUnitImpl.
     */
    public void testGetServiceUnitId() {
        System.out.println("getServiceUnitId");
        
        String serviceUnitRootPath = "."  + File.separator;
        ServiceUnitImpl instance = new ServiceUnitImpl(suid,
                                                       serviceUnitRootPath,
                                                       context,
                                                       statusProviderHelper,
                                                       jmsChannelMgr,
                                                       null,
                                                       runtimeConfiguration,
                                                       null);
        
        String result = instance.getServiceUnitId();
        assertEquals(suid, result);        
    }

    /**
     * Test of init method, of class com.sun.jbi.jmsbc.ServiceUnitImpl.
     */
    public void testInit() throws Exception {
        System.out.println("init");
        
        String serviceUnitRootPath = "."  + File.separator;

        /*
        String serviceUnitRootPath = "test"  + File.separator +
                                     "com"   + File.separator +
                                     "sun"   + File.separator +
                                     "jbi"   + File.separator +
                                     "jmsbc" + File.separator +
                                     "wsdls";
         */
        
        ServiceUnitImpl instance = new ServiceUnitImpl(suid,
                                                       serviceUnitRootPath,
                                                       context,
                                                       statusProviderHelper,
                                                       jmsChannelMgr,
                                                       null,
                                                       runtimeConfiguration,
                                                       null);
        instance.setEndpoints(endpoints);
        instance.init(serviceUnitRootPath);
    }

    /**
     * Test of start method, of class com.sun.jbi.jmsbc.ServiceUnitImpl.
     */
    public void testStart() throws Exception {
        System.out.println("start");
        
        String serviceUnitRootPath = "."  + File.separator;
        ServiceUnitImpl instance = new ServiceUnitImpl(suid,
                                                       serviceUnitRootPath,
                                                       context,
                                                       statusProviderHelper,
                                                       jmsChannelMgr,
                                                       null,
                                                       runtimeConfiguration,
                                                       null);
        
        instance.start();
    }

    /**
     * Test of stop method, of class com.sun.jbi.jmsbc.ServiceUnitImpl.
     */
    public void testStop() throws Exception {
        System.out.println("stop");
        
        String serviceUnitRootPath = "."  + File.separator;
        ServiceUnitImpl instance = new ServiceUnitImpl(suid,
                                                       serviceUnitRootPath,
                                                       context,
                                                       statusProviderHelper,
                                                       jmsChannelMgr,
                                                       null,
                                                       runtimeConfiguration,
                                                       null);
        
        instance.stop();        
    }

    /**
     * Test of shutdown method, of class com.sun.jbi.jmsbc.ServiceUnitImpl.
     */
    public void testShutdown() throws Exception {
        System.out.println("shutdown");
        
        String serviceUnitRootPath = "."  + File.separator;
        ServiceUnitImpl instance = new ServiceUnitImpl(suid,
                                                       serviceUnitRootPath,
                                                       context,
                                                       statusProviderHelper,
                                                       jmsChannelMgr,
                                                       null,
                                                       runtimeConfiguration,
                                                       null);
        
        instance.shutdown();        
    }
 
    private String getEndpointKey(Endpoint endpoint) {
        return getEndpointKey (endpoint.getServiceName().toString(),
                               endpoint.getEndpointName(),
                               endpoint.getEndpointType());
    }

    private String getEndpointKey(String serviceName, 
                                  String endpointName,
                                  int endpointType) {
        return suid + 
               serviceName + 
               endpointName +
               EndpointImpl.endpointTypeToString(endpointType);               
    }
    
}
