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
 * @(#)JMSBindingBootstrapTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc;

import junit.framework.*;
import java.io.FileOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Properties;
import java.util.ResourceBundle;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.text.MessageFormat;
import javax.jbi.JBIException;
import javax.jbi.component.Bootstrap;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.InstallationContext;
import javax.jbi.management.MBeanNames;
import javax.management.MBeanInfo;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.StandardMBean;
import javax.management.InstanceNotFoundException;
import javax.management.JMException;
import org.w3c.dom.DocumentFragment;
import com.sun.jbi.configuration.ConfigPersistence;

import org.jmock.core.*;
import org.jmock.*;

/**
 *
 * Unit tests for JMSBindingBootstrap class
 */
public class JMSBindingBootstrapTest extends org.jmock.cglib.MockObjectTestCase {
    
    // Proxies and concrete classes
    private InstallationContext installationContext = null;
    private ComponentContext componentContext = null;
    private MBeanServer mbeanServer = null;
    private MBeanNames mbeanNames = null;
    private ObjectName mbeanObjName = null;
    private Logger logger = null;
    
    // Mocks
    private Mock installationContextMock = null;
    private Mock componentContextMock = null;
    private Mock mbeanServerMock = null;
    private Mock mbeanNamesMock = null;
    private Mock loggerMock = null;
    
    public JMSBindingBootstrapTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {    
        mbeanObjName = new ObjectName("SUN-domain",
                                      "Akey",
                                      "Avalue");
        
        mbeanServerMock = mock(MBeanServer.class);
        mbeanServerMock.stubs().method("isRegistered")
                               .will(returnValue(false));
        mbeanServerMock.stubs().method("registerMBean");
        mbeanServer = (MBeanServer)mbeanServerMock.proxy();
        
        mbeanNamesMock = mock(MBeanNames.class);
        mbeanNamesMock.expects(once())
                      .method("createCustomComponentMBeanName")
                      .with(eq(MBeanNames.BOOTSTRAP_EXTENSION))
                      .will(returnValue(mbeanObjName));
        
        mbeanNames = (MBeanNames)mbeanNamesMock.proxy();
        
        //loggerMock = mock(Logger.class);
        //loggerMock.stubs().method("log");
        logger = logger.getLogger("JMSBindingBootstrapTest");
        
        componentContextMock = mock(ComponentContext.class);
        componentContextMock.expects(atLeastOnce())
                            .method("getMBeanServer")
                            .will(returnValue(mbeanServer));
        componentContextMock.expects(once())
                            .method("getMBeanNames")
                            .will(returnValue(mbeanNames));        
        componentContextMock.stubs().method("getLogger").will(returnValue(logger));
        componentContext = (ComponentContext)componentContextMock.proxy();
        
        installationContextMock = mock(InstallationContext.class);
        installationContextMock.expects(atLeastOnce())
                               .method("getContext")
                               .will(returnValue(componentContext));
        installationContextMock.stubs()
                               .method("getInstallationDescriptorExtension")
                               .will(returnValue(null));
        installationContext = (InstallationContext)installationContextMock.proxy();
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(JMSBindingBootstrapTest.class);
        
        return suite;
    }

    /**
     * Test of init and getExtensionMBeanName methods, of class com.sun.jbi.jmsbc.JMSBindingBootstrap.
     */
    public void testInitAndGetExtensionMBeanName() {
        System.out.println("Test init and getExtensionMBeanName");
        
        try {
            JMSBindingBootstrap instance = new JMSBindingBootstrap();
            instance.init(installationContext);

            ObjectName result = instance.getExtensionMBeanName();

            assertEquals(mbeanObjName, result);
        } catch (Exception ex) {
            fail ("Test init and getExtensionMBeanName failed: " + ex);
        }
    }

    /**
     * Test of onInstall method, of class com.sun.jbi.jmsbc.JMSBindingBootstrap.
     */
    public void testOnInstall() throws Exception {
        System.out.println("onInstall");

        componentContextMock.expects(once())
                            .method("getWorkspaceRoot")
                            .will(returnValue("." + File.separator));     

        MBeanInfo mbeanInfo = new MBeanInfo(JMSBindingBootstrap.class.toString(),
                                            "MBean for JMS Binding",
                                            null, null, null, null);
        
        mbeanServerMock.stubs().method("getMBeanInfo").will(returnValue(mbeanInfo));
        
        JMSBindingBootstrap instance = new JMSBindingBootstrap();
        instance.init(installationContext);
        
        instance.onInstall();        
    }

    /**
     * Test of onUninstall method, of class com.sun.jbi.jmsbc.JMSBindingBootstrap.
     */
    public void testOnUninstall() throws Exception {
        System.out.println("onUninstall");
        
        JMSBindingBootstrap instance = new JMSBindingBootstrap();
        instance.init(installationContext);
        instance.onUninstall();
    }

    /**
     * Test of cleanUp method, of class com.sun.jbi.jmsbc.JMSBindingBootstrap.
     */
    public void testCleanUp() throws Exception {
        System.out.println("cleanUp");

        MBeanInfo mbeanInfo = new MBeanInfo(JMSBindingBootstrap.class.toString(),
                                            "MBean for JMS Binding",
                                            null, null, null, null);
        
        mbeanServerMock.stubs().method("isRegistered").will(returnValue(true));        
        mbeanServerMock.expects(once()).method("unregisterMBean")
                                       .with(eq(mbeanObjName));
        
        JMSBindingBootstrap instance = new JMSBindingBootstrap();
        instance.init(installationContext);
        mbeanServerMock.stubs().method("isRegistered")
                               .will(returnValue(true));
        
        instance.cleanUp();        
    }
    
}
