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

package com.sun.jbi.httpsoapbc;

import junit.framework.*;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.logging.Logger;
import java.util.Map;
import javax.jbi.JBIException;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.management.descriptor.SUDescriptorSupport;
import java.io.File;
import java.util.Collection;
import java.util.Iterator;
import javax.wsdl.Definition;
import javax.wsdl.factory.WSDLFactory;
import com.sun.jbi.httpsoapbc.configuration.RuntimeConfigurationMBean;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.wsdlvalidator.ValidatingWSDLReader;
import com.sun.jbi.wsdlvalidator.factory.ValidatingWSDLFactory;
import com.sun.jbi.httpsoapbc.validator.HttpSoapValidatorRegistry;
import com.sun.jbi.management.descriptor.EndpointIdentifier;
import javax.jbi.component.ComponentContext;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import org.jmock.*;

/**
 *
 */
public class ServiceUnitImplTest extends MockObjectTestCase  {
    
    public ServiceUnitImplTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    /**
     * Test of getServiceUnitId method, of class com.sun.jbi.httpsoapbc.ServiceUnitImpl.
     */
    public void testGetServiceUnitId() {
        System.out.println("getServiceUnitId");
        
        String id = "someId";
        Mock context = mock(javax.jbi.component.ComponentContext.class);
        context.expects(atLeastOnce()).method("getComponentName").will(returnValue(id));
        ServiceUnitImpl instance = new ServiceUnitImpl(id, (javax.jbi.component.ComponentContext) context.proxy(), null, null, null);
        
        String expResult = id;
        String result = instance.getServiceUnitId();
        assertEquals(expResult, result);
    }

    /**
     * Test of init method, of class com.sun.jbi.httpsoapbc.ServiceUnitImpl.
     */
//    public void testInit() throws Exception {
//        System.out.println("init");
//        
//        Mock runtimeConfig = mock(RuntimeConfigurationMBean.class);
//        String serviceUnitRootPath = "";
//        ServiceUnitImpl instance = null;
//        Map endpoints = new HashMap();
//        Mock endpoint = mock(Endpoint.class);
//        HashSet endpointListeners = new HashSet();
//        Mock listener = mock(EndpointChangeListener.class);
//        listener.expects(atLeastOnce()).method("endpointInitialized").withAnyArguments().isVoid();
//        endpointListeners.add(listener.proxy());
//
//        String id = "someId";
//        Mock context = mock(javax.jbi.component.ComponentContext.class);
//        context.expects(atLeastOnce()).method("getComponentName").will(returnValue(id));
//        
//        instance = new ServiceUnitImpl(null, 
//                (javax.jbi.component.ComponentContext) context.proxy(),
//                (RuntimeConfigurationMBean) runtimeConfig.proxy(),
//                endpointListeners, null, endpoints);
//        
//        // Run our test
//        try{ 
//            instance.init(serviceUnitRootPath);
//            fail("Call to init() method should have failed, but it didn't");
//        } catch (Exception ex) {
//            fail("Exception thrown when calling start() when no exception should have been thrown");
//        }
//        
//    }

    /**
     * Test of start method, of class com.sun.jbi.httpsoapbc.ServiceUnitImpl.
     */
    public void testSuccessfulStart() throws Exception {
        
        // Create our Mock objects
        Mock runtimeConfig = mock(RuntimeConfigurationMBean.class);
        Map endpoints = new HashMap();
        Mock endpoint = mock(Endpoint.class);
        Mock wsdls = mock(Map.class);


        endpoints.put("http://localhost/test,service1,port1", endpoint.proxy());
        
        HashSet endpointListeners = new HashSet();
        Mock listener = mock(EndpointChangeListener.class);
        listener.expects(exactly(1)).method("endpointActivated").withAnyArguments().isVoid();
        endpointListeners.add(listener.proxy());

        String id = "someId";
        Mock context = mock(javax.jbi.component.ComponentContext.class);
        context.expects(atLeastOnce()).method("getComponentName").will(returnValue(id));
        
        ServiceUnitImpl instance = new ServiceUnitImpl(null, 
                (javax.jbi.component.ComponentContext) context.proxy(),
                (RuntimeConfigurationMBean) runtimeConfig.proxy(),
                endpointListeners, null, endpoints);
        
        // Run our test
        try{ 
            instance.start();
            
            // Verify our interactions
            endpoint.verify();
            listener.verify();
        } catch (Exception ex) {
            fail("Exception thrown when calling start() when no exception should have been thrown");
        }
    }

    /**
     * Test of start method, of class com.sun.jbi.httpsoapbc.ServiceUnitImpl.
     */
    public void testFailedStart() throws Exception {
        
        String errorMsg = "errorMsg";
        
        // Create our Mock objects
        Mock runtimeConfig = mock(RuntimeConfigurationMBean.class);
        Map endpoints = new HashMap();
        Mock endpoint = mock(Endpoint.class);
        endpoints.put("http://localhost/test,service1,port1", endpoint.proxy());
        
        HashSet endpointListeners = new HashSet();
       
        String id = "someId";
        Mock context = mock(javax.jbi.component.ComponentContext.class);
        context.expects(atLeastOnce()).method("getComponentName").will(returnValue(id));
        
        ServiceUnitImpl instance = new ServiceUnitImpl(null, 
                (javax.jbi.component.ComponentContext) context.proxy(),
                (RuntimeConfigurationMBean) runtimeConfig.proxy(),
                endpointListeners, null, endpoints);
        
        // Run our test
        try {
            instance.start();
        } catch (Exception ex) {
            // Verify our interactions
            endpoint.verify();
        }
    }
    
    /**
     * Test of stop method, of class com.sun.jbi.httpsoapbc.ServiceUnitImpl.
     */
    public void testSuccessfulStop() throws Exception {
        // Create our Mock objects
        Mock runtimeConfig = mock(RuntimeConfigurationMBean.class);
        Map endpoints = new HashMap();
        Mock endpoint = mock(Endpoint.class);
        endpoints.put("http://localhost/test,service1,port1", endpoint.proxy());
        
        HashSet endpointListeners = new HashSet();
        Mock listener = mock(EndpointChangeListener.class);
        listener.expects(exactly(1)).method("endpointDeactivated").withAnyArguments().isVoid();
        endpointListeners.add(listener.proxy());
        
        String id = "someId";
        Mock context = mock(javax.jbi.component.ComponentContext.class);
        context.expects(atLeastOnce()).method("getComponentName").will(returnValue(id));
        
        ServiceUnitImpl instance = new ServiceUnitImpl(null, 
                (javax.jbi.component.ComponentContext) context.proxy(),
                (RuntimeConfigurationMBean) runtimeConfig.proxy(),
                endpointListeners, null, endpoints);
        
        // Run our test
        try{ 
            instance.stop();
            
            // Verify our interactions
            endpoint.verify();
            listener.verify();
        } catch (Exception ex) {
            fail("Exception thrown when calling stop() when no exception should have been thrown");
        };
    }

   /**
     * Test of start method, of class com.sun.jbi.httpsoapbc.ServiceUnitImpl.
     */
    public void testFailedStop() throws Exception {
        
        String errorMsg = "errorMsg";
        
        // Create our Mock objects
        Mock runtimeConfig = mock(RuntimeConfigurationMBean.class);
        Map endpoints = new HashMap();
        Mock endpoint = mock(Endpoint.class);
        endpoints.put("http://localhost/test,service1,port1", endpoint.proxy());
        
        HashSet endpointListeners = new HashSet();
       
        String id = "someId";
        Mock context = mock(javax.jbi.component.ComponentContext.class);
        context.expects(atLeastOnce()).method("getComponentName").will(returnValue(id));
        
        ServiceUnitImpl instance = new ServiceUnitImpl(null, 
                (javax.jbi.component.ComponentContext) context.proxy(),
                (RuntimeConfigurationMBean) runtimeConfig.proxy(),
                endpointListeners, null, endpoints);
        
        // Run our test
        try {
            instance.stop();
        } catch (Exception ex) {
            // Verify our interactions
            endpoint.verify();
        }
    }
    
    /**
     * Test of shutdown method, of class com.sun.jbi.httpsoapbc.ServiceUnitImpl.
     */
    public void testSuccessfulShutdown() throws Exception {
        // Create our Mock objects
        Mock runtimeConfig = mock(RuntimeConfigurationMBean.class);
        Mock channel = mock(com.sun.jbi.common.qos.messaging.MessagingChannel.class);
        Map endpoints = new HashMap();
        Mock endpoint = mock(Endpoint.class);
        endpoint.expects(exactly(1)).method("deactivate").withAnyArguments().isVoid();
        endpoints.put("http://localhost/test,service1,port1", endpoint.proxy());
        
        HashSet endpointListeners = new HashSet();
        Mock listener = mock(EndpointChangeListener.class);
        listener.expects(exactly(1)).method("endpointShutdown").withAnyArguments().isVoid();
        endpointListeners.add(listener.proxy());
        
        String id = "someId";
        Mock context = mock(javax.jbi.component.ComponentContext.class);
        context.expects(atLeastOnce()).method("getComponentName").will(returnValue(id));
        channel.expects(atLeastOnce()).method("uninstallServiceQualities").withAnyArguments().isVoid();
        
        ServiceUnitImpl instance = new ServiceUnitImpl(null, 
                (javax.jbi.component.ComponentContext) context.proxy(),
                (RuntimeConfigurationMBean) runtimeConfig.proxy(),
                endpointListeners, null, endpoints, (MessagingChannel)channel.proxy());
        
        // Run our test
        try{ 
            instance.shutdown();
            
            // Verify our interactions
            endpoint.verify();
            listener.verify();
        } catch (Exception ex) {
            fail("Exception thrown when calling stop() when no exception should have been thrown");
        };
    }

    // removing invalid test case. When the SU is undeployed, it's entry in the SU map will be removed, otherwise, we will end up with a memory leak
    /**
     * Test of start method, of class com.sun.jbi.httpsoapbc.ServiceUnitImpl.
     */
//    public void testFailedShutdown() throws Exception {
//        
//        String errorMsg = "errorMsg";
//        
//        // Create our Mock objects
//        Mock runtimeConfig = mock(RuntimeConfigurationMBean.class);
//        Map endpoints = new HashMap();
//        Mock endpoint = mock(Endpoint.class);
//        //endpoint.expects(exactly(1)).method("deactivate").withAnyArguments().will(throwException(new JBIException(errorMsg)));
//        endpoints.put("http://localhost/test,service1,port1", endpoint.proxy());
//        
//        HashSet endpointListeners = new HashSet();
//       
//        String id = "someId";
//        Mock context = mock(javax.jbi.component.ComponentContext.class);
//        context.expects(atLeastOnce()).method("getComponentName").will(returnValue(id));
//        
//        ServiceUnitImpl instance = new ServiceUnitImpl(null, 
//                (javax.jbi.component.ComponentContext) context.proxy(),
//                (RuntimeConfigurationMBean) runtimeConfig.proxy(),
//                endpointListeners, null, endpoints);
//        
//        // Run our test
//        try {
//            instance.shutdown();
//            fail("Call to shutdown() method should have failed, but it didn't");
//        } catch (Exception ex) {
//            // Verify our interactions
//            endpoint.verify();
//        }
//    }
    
    /**
     * Test of getEndpoints method, of class com.sun.jbi.httpsoapbc.ServiceUnitImpl.
     */
    public void testGetEndpoints() {
        Mock runtimeConfig = mock(RuntimeConfigurationMBean.class);
        Map endpoints = new HashMap();
        Mock endpoint = mock(Endpoint.class);
        endpoints.put("http://localhost/test,service1,port1", endpoint.proxy());
                
        String id = "someId";
        Mock context = mock(javax.jbi.component.ComponentContext.class);
        context.expects(atLeastOnce()).method("getComponentName").will(returnValue(id));
        
        ServiceUnitImpl instance = new ServiceUnitImpl(null,
                (javax.jbi.component.ComponentContext) context.proxy(),
                (RuntimeConfigurationMBean) runtimeConfig.proxy(),
                null, null, endpoints);
        
        Map expResult = endpoints;
        Map result = instance.getEndpoints();
        assertEquals(1, result.size());
        Iterator it = result.values().iterator();
        int counter = 0;
        while (it.hasNext()) {
            Endpoint endpt = (Endpoint)it.next();
            assertEquals(endpoint.proxy(), endpt);
            break;
        }
    }
    
}
