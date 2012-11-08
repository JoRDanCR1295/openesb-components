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
 * @(#)JMSJndiConnectionInfoRecordTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.recovery;

import java.util.Properties;
import junit.framework.TestCase;

/**
 *
 * Unit test for JMSConnectionInfoFilePersister
 */
public class JMSJndiConnectionInfoRecordTest extends TestCase {
    
    private JMSJndiConnectionInfoRecord rec1;
    private JMSJndiConnectionInfoRecord rec1same;
    private JMSJndiConnectionInfoRecord rec2;
    private JMSJndiConnectionInfoRecord rec2same;
    private Properties jndiEnvProps1;
    private Properties jndiEnvProps2;
    public JMSJndiConnectionInfoRecordTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        super.setUp();
        jndiEnvProps1 = new Properties();
        jndiEnvProps1.setProperty("jndiSys1-Ent1", "jndiSys1-Val1");
        jndiEnvProps1.setProperty("jndiSys1-Ent2", "jndiSys1-Val2");
        jndiEnvProps2 = new Properties();
        jndiEnvProps2.setProperty("jndiSys2-Ent1", "jndiSys2-Val1");
        jndiEnvProps2.setProperty("jndiSys2-Ent2", "jndiSys2-Val2");
        rec1 = new JMSJndiConnectionInfoRecord("jndi://", "guest", "guest", "", "MyTCF",
                                                "tcp://somehost", "MyInitialContext", "mySecPrincipal",
                                                "mySecCredentials", jndiEnvProps1);
        rec1same = new JMSJndiConnectionInfoRecord("jndi://", "guest", "guest", "", "MyTCF",
                                                 "tcp://somehost", "MyInitialContext", "mySecPrincipal",
                                                 "mySecCredentials", jndiEnvProps1);
        rec2 = new JMSJndiConnectionInfoRecord("jndi://", "guest", "guest", "", "MyQCF",
                                                "tcp://somehost", "MyInitialContext", "mySecPrincipal",
                                                "mySecCredentials", jndiEnvProps2);
        rec2same = new JMSJndiConnectionInfoRecord("jndi://", "guest", "guest", "", "MyQCF",
                                                "tcp://somehost", "MyInitialContext", "mySecPrincipal",
                                                "mySecCredentials", jndiEnvProps2);
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    public void testGetConnectionFactoryName() {
        System.out.println("getConnectionFactoryName");
        String expResult = "MyTCF";
        String result = rec1.getConnectionFactoryName();
        assertEquals(expResult, result);
    } /* Test of getConnectionFactoryName method, of class JMSJndiConnectionInfoRecord. */

    public void testGetProviderURL() {
        System.out.println("getProviderURL");
        String expResult = "tcp://somehost";
        String result = rec2.getProviderURL();
        assertEquals(expResult, result);
    } /* Test of getProviderURL method, of class JMSJndiConnectionInfoRecord. */

    public void testGetInitialContextFactory() {
        System.out.println("getInitialContextFactory");
        String expResult = "MyInitialContext";
        String result = rec2.getInitialContextFactory();
        assertEquals(expResult, result);
    } /* Test of getInitialContextFactory method, of class JMSJndiConnectionInfoRecord. */

    public void testGetSecurityPrincipal() {
        System.out.println("getSecurityPrincipal");
        String expResult = "mySecPrincipal";
        String result = rec1.getSecurityPrincipal();
        assertEquals(expResult, result);
    } /* Test of getSecurityPrincipal method, of class JMSJndiConnectionInfoRecord. */

    public void testGetSecurityCredentials() {
        System.out.println("getSecurityCredentials");
        String expResult = "mySecCredentials";
        String result = rec1.getSecurityCredentials();
        assertEquals(expResult, result);
    } /* Test of getSecurityCredentials method, of class JMSJndiConnectionInfoRecord. */

    public void testGetJndiEnv() {
        System.out.println("getJndiEnv");
        JMSJndiConnectionInfoRecord instance = new JMSJndiConnectionInfoRecord();
        Properties expResult = jndiEnvProps1;
        Properties result = rec1.getJndiEnv();
        assertEquals(expResult, result);
    } /* Test of getJndiEnv method, of class JMSJndiConnectionInfoRecord. */

    public void testSetConnectionFactoryName() {
        System.out.println("setConnectionFactoryName");
        String connectionFactoryName = "TheTCF";
        JMSJndiConnectionInfoRecord instance = new JMSJndiConnectionInfoRecord();
        instance.setConnectionFactoryName(connectionFactoryName);
        assertEquals(connectionFactoryName,instance.getConnectionFactoryName());
    } /* Test of setConnectionFactoryName method, of class JMSJndiConnectionInfoRecord. */

    public void testSetProviderURL() {
        System.out.println("setProviderURL");
        String providerURL = "foo://bar";
        JMSJndiConnectionInfoRecord instance = new JMSJndiConnectionInfoRecord();
        instance.setProviderURL(providerURL);
        assertEquals(providerURL,instance.getProviderURL());
    } /* Test of setProviderURL method, of class JMSJndiConnectionInfoRecord. */

    public void testSetInitialContextFactory() {
        System.out.println("setInitialContextFactory");
        String initialContextFactory = "foo.bar.SomeContextClass";
        JMSJndiConnectionInfoRecord instance = new JMSJndiConnectionInfoRecord();
        instance.setInitialContextFactory(initialContextFactory);
        assertEquals(initialContextFactory,instance.getInitialContextFactory());
    } /* Test of setInitialContextFactory method, of class JMSJndiConnectionInfoRecord. */

    public void testSetSecurityPrincipal() {
        System.out.println("setSecurityPrincipal");
        String securityPrincipal = "ThePrincipal";
        JMSJndiConnectionInfoRecord instance = new JMSJndiConnectionInfoRecord();
        instance.setSecurityPrincipal(securityPrincipal);
        assertEquals(securityPrincipal,instance.getSecurityPrincipal());
    } /* Test of setSecurityPrincipal method, of class JMSJndiConnectionInfoRecord. */

    public void testSetSecurityCredentials() {
        System.out.println("setSecurityCredentials");
        String securityCredentials = "TheCredentials";
        JMSJndiConnectionInfoRecord instance = new JMSJndiConnectionInfoRecord();
        instance.setSecurityCredentials(securityCredentials);
        assertEquals(securityCredentials,instance.getSecurityCredentials());
    } /* Test of setSecurityCredentials method, of class JMSJndiConnectionInfoRecord. */

    public void testSetJndiEnv() {
        System.out.println("setJndiEnv");
        Properties jndiEnv = jndiEnvProps2;
        JMSJndiConnectionInfoRecord instance = new JMSJndiConnectionInfoRecord();
        instance.setJndiEnv(jndiEnv);
        assertEquals(jndiEnv,instance.getJndiEnv());
    } /* Test of setJndiEnv method, of class JMSJndiConnectionInfoRecord. */

    public void testEquals() {
        System.out.println("equals");
        ConnectionInfoRecord other = rec1;
        JMSJndiConnectionInfoRecord instance = rec1same;
        boolean expResult = true;
        boolean result = instance.equals(other);
        assertEquals(expResult, result);
    } /* Test of equals method, of class JMSJndiConnectionInfoRecord. */

    public void testHashCode() {
        System.out.println("hashCode");
        ConnectionInfoRecord other = rec2;
        JMSJndiConnectionInfoRecord instance = rec2same;
        int expResult = other.hashCode();
        int result = instance.hashCode();
        assertEquals(expResult, result);
    } /* Test of hashCode method, of class JMSJndiConnectionInfoRecord. */

    public void testHashCodeNotEqual() {
        System.out.println("hashCodeNotEqual");
        ConnectionInfoRecord other = rec1;
        JMSJndiConnectionInfoRecord instance = rec2;
        int expResult = other.hashCode();
        int result = instance.hashCode();
        assert(expResult != result);
    } /* Test of hashCode method, of class JMSJndiConnectionInfoRecord. */
    
}
