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
 * @(#)FtpTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ftpbc.ftp;

import java.util.Properties;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/*
 * JUnit based test.
 *
 * 
 * 
 * 
 * @author Harry Liu (harry.liu@sun.com)
 *
 * Copyright 2006 Sun Microsystems, Inc. All Rights Reserved.
 */
public class FtpTest extends TestCase {
    FtpInterface instance;

    public FtpTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        super.setUp();
        instance = new FtpInterface();
        Properties props = new Properties();
        props.put("FTP/Host Name", FtpTprops.FTP_TEST_HOST);
        props.put("FTP/Directory Listing Style", "UNIX");
        props.put("FTP/User Name", "anonymous");
        props.put("FTP/Password", "abc@yahoo.com");
        instance.initialize(props);
    }
    
    protected void tearDown() throws Exception {
        try {
            instance.reset();
            if ( instance.getClient() != null )
                instance.getClient().close();
        } catch (Exception ex) {
            ;
        }
        instance = null;
        super.tearDown();
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(FtpTest.class);
        
        return suite;
    }
    
    /**
     * Test of initialize method, of class com.sun.jbi.ftpbc.ftp.FtpInterface.
     */
    public void testInitialize() throws Exception {
        System.out.println("initialize");
        
        Properties p = null;
        try {
            instance.initialize(p);
            fail("Exception is expected - null input Properties");
        } catch (Exception ex) {
            ;
        }
        /*
        p = new Properties();
        try {
            instance.initialize(p);
        } catch (Exception ex) {
            fail("No exception is expected even the input Properties is empty - default values are used");
        }
        */
    }
    /*    
     * Test of getConfiguration method, of class com.sun.jbi.ftpbc.ftp.FtpInterface.
     */
    public void testGetConfiguration() throws Exception {
        System.out.println("getConfiguration");
        
        assertNotNull(instance.getConfiguration());
        
    }
    
    /**
     * Test of setConfiguration method, of class com.sun.jbi.ftpbc.ftp.FtpInterface.
     */
    public void testSetConfiguration() throws Exception {
        System.out.println("setConfiguration");
        
        FtpFileConfiguration cfg = null;
        
        instance.setConfiguration(cfg);
        assertEquals(cfg, instance.getConfiguration());
        
        cfg = new FtpFileConfiguration(null);
        
        instance.setConfiguration(cfg);
        assertEquals(cfg, instance.getConfiguration());
    }
    
    /**
     * Test of getClient method, of class com.sun.jbi.ftpbc.ftp.FtpInterface.
     */
    public void testGetClient() throws Exception {
        System.out.println("getClient");
        
        assertNotNull(instance.getClient());
        
    }
    
    /**
     * Test of getProvider method, of class com.sun.jbi.ftpbc.ftp.FtpInterface.
     */
    public void testGetProvider() throws Exception {
        System.out.println("getProvider");
        
        assertNotNull(instance.getProvider());
        
    }
    
    /**
     * Test of setClient method, of class com.sun.jbi.ftpbc.ftp.FtpInterface.
     */
    public void testSetClient() throws Exception {
        System.out.println("setClient");
        
        FtpFileClient client = null;
        
        instance.setClient(client);
        assertEquals(client, instance.getClient());
        
        client = new FtpFileClientImpl();
        client.initialize(instance);
        
        instance.setClient(client);
        assertEquals(client, instance.getClient());
        
    }
    
    /**
     * Test of setProvider method, of class com.sun.jbi.ftpbc.ftp.FtpInterface.
     */
    public void testSetProvider() throws Exception {
        System.out.println("setProvider");
        
        FtpFileProvider provider = null;
        
        instance.setProvider(provider);
        assertEquals(provider, instance.getProvider());
        
        provider = new FtpFileProviderImpl();
        provider.setDirListingStyle("UNIX");
        
        instance.setProvider(provider);
        assertEquals(provider, instance.getProvider());
        
    }
    
    /**
     * Test of getStateManager method, of class com.sun.jbi.ftpbc.ftp.FtpInterface.
     */
    public void testGetStateManager() throws Exception {
        System.out.println("getStateManager");
        
        assertNotNull(instance.getStateManager());
        
    }
    
    /**
     * Test of getState method, of class com.sun.jbi.ftpbc.ftp.FtpInterface.
     */
    public void testGetState() throws Exception {
        System.out.println("getState");
        
        assertNotNull(instance.getState());
        
    }
    
    /**
     * Test of isStateChanged method, of class com.sun.jbi.ftpbc.ftp.FtpInterface.
     */
    public void testIsStateChanged() throws Exception {
        System.out.println("isStateChanged");
        
        boolean expResult = false;
        boolean result = instance.isStateChanged();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setStateChanged method, of class com.sun.jbi.ftpbc.ftp.FtpInterface.
     */
    public void testSetStateChanged() throws Exception {
        System.out.println("setStateChanged");
        
        boolean newStateChanged = true;
        
        instance.setStateChanged(newStateChanged);
        boolean result = instance.isStateChanged();
        assertEquals(newStateChanged, result);
        
    }
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
