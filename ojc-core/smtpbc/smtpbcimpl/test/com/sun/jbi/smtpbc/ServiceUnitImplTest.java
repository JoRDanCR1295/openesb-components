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

package com.sun.jbi.smtpbc;


import junit.framework.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Collection;
import java.util.HashSet;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.component.ComponentContext;
import javax.xml.namespace.QName;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import javax.management.MBeanServer;
import com.sun.jbi.smtpbc.extensions.MailTo;
import com.sun.jbi.smtpbc.extensions.SMTPAddress;
import com.sun.jbi.smtpbc.mbeans.RuntimeConfiguration;
import org.jmock.*;

/**
 *
 * @author rchen
 */
public class ServiceUnitImplTest extends MockObjectTestCase {
    
    ServiceUnitImpl instance = null;
    
    Mock componentContext = mock(ComponentContext.class);
    Mock deliveryChannel = mock(DeliveryChannel.class);
    
    StatusProviderHelper statusHelper = null;
    InboundReceiver inboundReceiver = null;
    Endpoint endpoint = null;
    List endpoints = new ArrayList();
    MessageStore mMessageStore = null;
    String serviceUnitRootPath = null;
    
    public ServiceUnitImplTest(final String testName) {
        super(testName);
    }

    @Override
    protected void setUp() throws Exception {
        statusHelper = new StatusProviderHelper("shortName", "componentType", "componentName", (MBeanServer)mock(MBeanServer.class).proxy());
        endpoint = new EndpointImpl();
        endpoint.setServiceName(new QName("MySerivceName"));
        endpoint.setEndpointName("MyEndpointName");
        endpoints.add(endpoint);
        mMessageStore= new MessageStore();
        serviceUnitRootPath = "test/com/sun/jbi/smtpbc/packaging/descriptors";
        final MailTo mailTo = new MailTo("mailto:afung@seebeyond.com");
        final SMTPAddress smtpAddress = new SMTPAddress();
        smtpAddress.setLocation(mailTo);
        smtpAddress.setSMTPServer("localhost");
        endpoint.setSMTPAddress(smtpAddress);
        final Collection  mEndpointChangeListeners = new HashSet();  
        String workspaceRoot = "test/com/sun/jbi/smtpbc/packaging";
        RuntimeConfiguration runtimeConfig = new RuntimeConfiguration(workspaceRoot);
                  
          instance = new ServiceUnitImpl("TestId",serviceUnitRootPath,
                                       (ComponentContext)componentContext.proxy(),
                                       statusHelper,
                                       mEndpointChangeListeners,runtimeConfig);
    }

    @Override
	protected void tearDown() throws Exception {
    }

    /**
     * Test of getServiceUnitId method, of class com.sun.jbi.smtpbc.ServiceUnitImpl.
     */
    public void testGetServiceUnitId() {
        System.out.println("getServiceUnitId");
         
        
        final String expResult = "TestId";
        final String result = instance.getServiceUnitId();
        Assert.assertEquals(expResult, result);
        
       
    }

    /**
     * Test of init method, of class com.sun.jbi.smtpbc.ServiceUnitImpl.
     */
    public void testInit() throws Exception {
        System.out.println("init");
        
        instance.init(serviceUnitRootPath);
        
       
    }

    /**
     * Test of start method, of class com.sun.jbi.smtpbc.ServiceUnitImpl.
     */
    public void testStart() throws Exception {
        System.out.println("start");
        
       
        
        instance.start();
        
        // TODO review the generated test code and remove the default call to fail.
       
    }

    /**
     * Test of stop method, of class com.sun.jbi.smtpbc.ServiceUnitImpl.
     */
    public void testStop() throws Exception {
        System.out.println("stop");
        
      
        
        instance.stop();
        
       
    }

    /**
     * Test of shutdown method, of class com.sun.jbi.smtpbc.ServiceUnitImpl.
     */
    public void testShutdown() throws Exception {
        System.out.println("shutdown");
        
      
        
        instance.shutdown();
        
      
    }

    /**
     * Test of getEndpoints method, of class com.sun.jbi.smtpbc.ServiceUnitImpl.
     */
    public void testGetEndpoints() {
        System.out.println("getEndpoints");
            
        final Collection result = instance.getEndpoints();
        Assert.assertEquals(0, result.size());
        
        
    }

    /**
     * Test of activateEndpoint method, of class com.sun.jbi.smtpbc.ServiceUnitImpl.
     */
    public void testActivateEndpoint() throws Exception {
        System.out.println("activateEndpoint");
        
        endpoint.setEndpointType(Endpoint.EndpointType.INBOUND);
        
        instance.activateEndpoint(endpoint);
        
      
    }

    /**
     * Test of deactivateEndpoint method, of class com.sun.jbi.smtpbc.ServiceUnitImpl.
     */
    public void testDeactivateEndpoint() throws Exception {
        System.out.println("deactivateEndpoint");
        
        
           endpoint.setEndpointType(Endpoint.EndpointType.INBOUND);
        instance.deactivateEndpoint(endpoint);
        
      
    }
    
}
