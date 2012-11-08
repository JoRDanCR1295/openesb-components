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
 * @(#)ExecBindingBootstrapTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.execbc.bootstrap;

import junit.framework.*;
import com.sun.jbi.configuration.ConfigPersistence;
import com.sun.jbi.execbc.bootstrap.ExecBindingBootstrap;
import com.sun.jbi.internationalization.Messages;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Hashtable;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Properties;
import javax.jbi.component.Bootstrap;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.InstallationContext;
import javax.jbi.JBIException;
import javax.jbi.management.MBeanNames;
import javax.management.JMException;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanRegistrationException;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.StandardMBean;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.NodeList;
import org.jmock.*;

/**
 *
 * @author sweng
 */
public class ExecBindingBootstrapTest extends org.jmock.cglib.MockObjectTestCase {
    ExecBindingBootstrap instance = null;
    ObjectName objectName = null;
    
    Mock context = mock(InstallationContext.class);
    Mock componentContext = mock(ComponentContext.class);
    Mock mbServer = mock(MBeanServer.class);
    Mock mbNames = mock(MBeanNames.class);
    
    public ExecBindingBootstrapTest(String testName) {
        super(testName);
        instance = new ExecBindingBootstrap();
    }

    protected void setUp() throws Exception {
        Hashtable table = new Hashtable();
        table.put("someKey", "someValue");
        objectName = new ObjectName("someObjectName", table);
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(ExecBindingBootstrapTest.class);
        
        return suite;
    }

    /**
     * Test of init method, of class com.sun.jbi.execbc.bootstrap.ExecBindingBootstrap.
     */
    public void testInit() throws Exception {
        System.out.println("Testing init");
        
        // 1. testing the success scenario
        context.expects(atLeastOnce()).method("getContext").will(returnValue(componentContext.proxy()));
        componentContext.expects(atLeastOnce()).method("getLogger").with(eq(ExecBindingBootstrap.class.getName()), eq("com.sun.jbi.execbc.bootstrap.messages.Bundle")).will(returnValue(Logger.getLogger(ExecBindingBootstrap.class.getName(), "com.sun.jbi.execbc.bootstrap.messages.Bundle")));
        componentContext.expects(atLeastOnce()).method("getLogger").with(eq("com.sun.jbi.execbc.bootstrap.InstallerExt"), eq("com.sun.jbi.execbc.bootstrap.messages.Bundle")).will(returnValue(Logger.getLogger("com.sun.jbi.execbc.bootstrap.InstallerExt", "com.sun.jbi.execbc.bootstrap.messages.Bundle")));
        componentContext.expects(atLeastOnce()).method("getMBeanServer").will(returnValue(mbServer.proxy()));
        componentContext.expects(atLeastOnce()).method("getMBeanNames").will(returnValue(mbNames.proxy()));
        mbNames.expects(atLeastOnce()).method("createCustomComponentMBeanName").with(eq(MBeanNames.BOOTSTRAP_EXTENSION)).will(returnValue(objectName));
        mbServer.expects(atLeastOnce()).method("isRegistered").with(eq(objectName)).will(returnValue(false));
        mbServer.expects(atLeastOnce()).method("registerMBean").with(isA(StandardMBean.class), eq(objectName));
        try {
            instance.init((InstallationContext)context.proxy());
            System.out.println("Successfully tested init for the scenario where no exception is expected.");
        } catch (Exception e) {
            fail("Failed to test init for the scenario where no exception is expected due to: " + e.getMessage());
        }
        mbServer.verify();
        mbNames.verify();
        context.verify();
        
        // 2. testing the failure scenario
        mbServer.expects(once()).method("registerMBean").with(isA(StandardMBean.class), eq(objectName)).will(throwException(new MBeanRegistrationException(new Exception(), "someException")));
        try {
            instance.init((InstallationContext)context.proxy());
            fail("Failed to test init when an exception should be caught - a MBeanRegistrationException is raised.");
        } catch (Exception e) {
            System.out.println("Successfully tested init when an exception is raised.");
        }
        mbServer.verify();
        mbNames.verify();
        context.verify();
    }

    /**
     * Test of onInstall method, of class com.sun.jbi.execbc.bootstrap.ExecBindingBootstrap.
     */
//    public void testOnInstall() throws Exception {
//        System.out.println("Testing onInstall");
//        
//        Mock documentFragment = mock(DocumentFragment.class);
//        Mock nodelist = mock(NodeList.class);
//        nodelist.expects(atLeastOnce()).method("getLength").will(returnValue(0));
//        documentFragment.expects(atLeastOnce()).method("getChildNodes").will(returnValue(nodelist.proxy()));
//        context.expects(once()).method("getContext").will(returnValue(componentContext.proxy()));
//        context.expects(once()).method("getInstallationDescriptorExtension").will(returnValue(documentFragment.proxy()));
//        componentContext.expects(atLeastOnce()).method("getMBeanServer").will(returnValue(mbServer.proxy()));
//        componentContext.expects(once()).method("getWorkspaceRoot").will(returnValue("test/com/sun/jbi/execbc/testDir"));
//        mbServer.expects(once()).method("getMBeanInfo").will(returnValue(null));
//        instance.setInstallationContext((InstallationContext)context.proxy());
//        try {
//            instance.onInstall();
//            System.out.println("Successfully tested onInstall.");
//        } catch (Exception e) {
//            fail("Failed to test onInstall due to: " + e.getMessage());
//        }
//        context.verify();
//        componentContext.verify();
//    }
    
    /**
     * Test of cleanUp method, of class com.sun.jbi.execbc.bootstrap.ExecBindingBootstrap.
     */
    public void testCleanUp() throws Exception {
        System.out.println("Testing cleanUp");
        
        context.expects(once()).method("getContext").will(returnValue(componentContext.proxy()));
        componentContext.expects(atLeastOnce()).method("getMBeanServer").will(returnValue(mbServer.proxy()));
        mbServer.expects(atLeastOnce()).method("isRegistered").with(eq(objectName)).will(returnValue(true));
        mbServer.expects(atLeastOnce()).method("unregisterMBean").with(eq(objectName));
        instance.setInstallationContext((InstallationContext)context.proxy());
        instance.setMBeanObjectName(objectName);
        
        try {
            instance.cleanUp();
            System.out.println("Successfully tested cleanUp.");
        } catch (Exception e) {
            fail("Failed to test cleanUp due to: " + e.getMessage());
        }
        context.verify();
        componentContext.verify();
    }
}
