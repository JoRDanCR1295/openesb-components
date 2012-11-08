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
 * @(#)JMSXARecoveryTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.recovery;

import java.io.File;
import java.util.Properties;

import javax.jbi.component.ComponentContext;
import javax.resource.spi.ManagedConnectionFactory;
import javax.transaction.xa.XAResource;

import org.jmock.Mock;

import com.sun.jbi.component.jbiext.KeyStoreUtilClient;
import com.sun.jbi.jmsbc.mbeans.JMSBCRuntimeConfiguration;
import com.sun.jbi.jmsbc.mbeans.RuntimeConfiguration;

/**
 *
 * Unit test for JMSXARecovery
 */
public class JMSXARecoveryTest extends org.jmock.cglib.MockObjectTestCase {
    private String testDir = null;
    private Properties persisterProps = null;
    private JMSConnectionInfoFilePersister persister = null;
    private JMSConnectionInfoRecord [] recs = null;
    
    private Mock runtimeConfigMock;
    private RuntimeConfiguration runtimeConfig;
    
    public JMSXARecoveryTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {        
        String baseDir = getClass().getClassLoader().getResource("com/sun/jbi/jmsbc/testdir/config.properties").getPath();
        baseDir = baseDir.substring(0, baseDir.lastIndexOf('/'));
        runtimeConfigMock = mock(JMSBCRuntimeConfiguration.class,
                                    new Class [] {String.class, KeyStoreUtilClient.class},
                                    new Object[] {baseDir, null});
        runtimeConfig = (RuntimeConfiguration)runtimeConfigMock.proxy();
        
        String testDir = getClass().getResource("JMSXARecoveryTest.class").getPath();
        testDir = testDir.substring(0, testDir.lastIndexOf('/'));
//        testDir = new File("test" + File.separator + 
//                           "com" + File.separator + 
//                           "sun" + File.separator + 
//                           "jbi" + File.separator + 
//                           "jmsbc" + File.separator + 
//                           "recovery").getAbsolutePath();        
        persisterProps = new Properties();
        persisterProps.setProperty(JMSConnectionInfoFilePersister.FILE_PERSISTER_PROPS_DIR,
                                   testDir);
        persister = new JMSConnectionInfoFilePersister();
        persister.initialize(persisterProps);
        
        // Add connection(s) to persistence
        recs = new JMSConnectionInfoRecord[4];
        recs[0] = new JMSConnectionInfoRecord("mq://hostA:7676", "admin", "admin", null);        
        recs[1] = new JMSConnectionInfoRecord("mq://hostB:7676", "admin", "admin", "");        
        recs[2] = new JMSConnectionInfoRecord("mq://hostC:7676", "admin", "admin", "");        
        recs[3] = new JMSConnectionInfoRecord("mq://hostD:7676", "admin", "admin", null);        
        persister.persist(recs);
    }

    protected void tearDown() throws Exception {
        persister.close();
    }

    /**
     * Test of recover method, of class com.sun.jbi.jmsbc.recovery.JMSXARecovery.
     */
    public void testRecoverSuccess() throws Exception {
        System.out.println("recover succeeds");
                                
        ConnectionInfoRecord [] recsToRecover = persister.retrieve();
        
        Mock mcfMock = mock (ManagedConnectionFactory.class);
        ManagedConnectionFactory mcf = (ManagedConnectionFactory)mcfMock.proxy();
        
        Mock helperMock = mock (JMSXARecoveryHelper.class,
                                new Class [] {},
                                new Object[] {});
        
        for (int i=0; i < recsToRecover.length; i++) {
            helperMock.expects(once()).method("getXAResource").with(eq(recsToRecover[i]));
        }
        JMSXARecoveryHelper helper = (JMSXARecoveryHelper)helperMock.proxy();
        
        Mock componentContextMock = mock(RecoverableComponentContext.class);
        componentContextMock.expects(atLeastOnce()).method("registerXAResource");        
        ComponentContext componentContext = (ComponentContext)componentContextMock.proxy();
        
        JMSXARecovery instance = new JMSXARecovery(componentContext, persister, helper);
        instance.recover();
                
        // Should have recover all records added by test and any records
        // that existed in persistence
        assertTrue(persister.retrieve().length==0);
    }

    /**
     * Test of recover method, of class com.sun.jbi.jmsbc.recovery.JMSXARecovery.
     */
    public void testRecoverFail() throws Exception {
        System.out.println("recover fails, skip resource");

        ConnectionInfoRecord [] recsToRecover = persister.retrieve();
        int initLength = recsToRecover.length;
        Mock mcfMock = mock (ManagedConnectionFactory.class);
        ManagedConnectionFactory mcf = (ManagedConnectionFactory)mcfMock.proxy();

        Mock helperMock = mock (JMSXARecoveryHelper.class,
                                new Class [] {},
                                new Object[] {});
        JMSXARecoveryHelper helper = (JMSXARecoveryHelper)helperMock.proxy();
        
        Mock componentContextMock = mock(NonRecoverableComponentContext.class);
        ComponentContext componentContext = (ComponentContext)componentContextMock.proxy();
        
        JMSXARecovery instance = new JMSXARecovery(componentContext, persister, helper);
        instance.recover();
                
        // No records should have been recovered
        assertTrue(persister.retrieve().length==initLength);
    }
    
    
    public interface RecoverableComponentContext extends javax.jbi.component.ComponentContext {
        public void registerXAResource(XAResource xar);
    }

    public interface NonRecoverableComponentContext extends javax.jbi.component.ComponentContext {
    }
    
    private String getConnectionString(JMSConnectionInfoRecord rec) {
        String connectionURL = rec.getConnectionURL();
        String username = rec.getUsername();
        String password = rec.getPassword();
        String overloadedConnURL = connectionURL;
        if (username != null && password != null) {
            overloadedConnURL += "?user=" + username + "&password=" + password;
        }
        return overloadedConnURL;
    }    
}
