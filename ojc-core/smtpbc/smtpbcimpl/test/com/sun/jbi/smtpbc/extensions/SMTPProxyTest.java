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
 * @(#)SMTPProxyTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc.extensions;

import junit.framework.*;

import javax.xml.namespace.QName;

/**
 *
 * @author rchen
 */
public class SMTPProxyTest extends TestCase {
    SMTPProxy instance = null;
    
    public SMTPProxyTest(final String testName) {
        super(testName);
    }
    
    @Override
	protected void setUp() throws Exception {
        instance = new SMTPProxy();
    }
    
    @Override
	protected void tearDown() throws Exception {
    }
    
    /**
     * Test of getElementType method, of class com.sun.jbi.smtpbc.extensions.SMTPProxy.
     */
    public void testGetElementType() {
        System.out.println("getElementType");
        
        final QName expResult = new QName(SMTPConstants.NS_URI_SMTP, SMTPProxy.ELEM_PROXY);
        final QName result = instance.getElementType();
        Assert.assertEquals(expResult, result);
        
        
    }
    
    /**
     * Test of setElementType method, of class com.sun.jbi.smtpbc.extensions.SMTPProxy.
     */
    public void testSetElementType() {
        System.out.println("setElementType");
        
        final QName elementType = null;
        instance.setElementType(elementType);
        
        final QName expResult = new QName(SMTPConstants.NS_URI_SMTP, SMTPProxy.ELEM_PROXY);
        final QName result = instance.getElementType();
        Assert.assertEquals(expResult, result);
    }
    
    /**
     * Test of getRequired method, of class com.sun.jbi.smtpbc.extensions.SMTPProxy.
     */
    public void testSetGetRequired() {
        System.out.println("getRequired");
        final Boolean required = Boolean.TRUE;
        instance.setRequired(required);
        
        final Boolean expResult =Boolean.TRUE;
        final Boolean result = instance.getRequired();
        Assert.assertEquals(expResult, result);
        
        
    }
    
    
    
    /**
     * Test of getHost method, of class com.sun.jbi.smtpbc.extensions.SMTPProxy.
     */
    public void testSetGetHost() {
        System.out.println("getHost");
        
        final String host = "myhost";
        instance.setHost(host);
        final String expResult = "myhost";
        final String result = instance.getHost();
        Assert.assertEquals(expResult, result);
        
        
    }
    
    
    /**
     * Test of getPort method, of class com.sun.jbi.smtpbc.extensions.SMTPProxy.
     */
    public void testsetGetPort() {
        System.out.println("getPort");
        final Integer port = Integer.valueOf(1234);
        instance.setPort(port);
        
        final Integer expResult =Integer.valueOf(1234);
        final Integer result = instance.getPort();
        Assert.assertEquals(expResult, result);
        
        
    }
    
    
    
    /**
     * Test of getUserName method, of class com.sun.jbi.smtpbc.extensions.SMTPProxy.
     */
    public void testSetGetUserName() {
        System.out.println("getUserName");
        
        final String userName = "abcdUsername";
        instance.setUserName(userName);
        final String expResult = "abcdUsername";
        final String result = instance.getUserName();
        Assert.assertEquals(expResult, result);
        
        
    }
    
    
    
    /**
     * Test of getPassword method, of class com.sun.jbi.smtpbc.extensions.SMTPProxy.
     */
    public void testSetGetPassword() {
        System.out.println("getPassword");
        
        final String password = "mypassword";
        instance.setPassword(password);
        final String expResult = "mypassword";
        final String result = instance.getPassword();
        Assert.assertEquals(expResult, result);
        
        
    }
    
    
    
    /**
     * Test of getSessionAuthentication method, of class com.sun.jbi.smtpbc.extensions.SMTPProxy.
     */
    public void testSetGetSessionAuthentication() {
        System.out.println("getSessionAuthentication");
        
        final Boolean sessionAuthentication = Boolean.TRUE;
        instance.setSessionAuthentication(sessionAuthentication);
        
        final Boolean expResult = Boolean.TRUE;
        final Boolean result = instance.getSessionAuthentication();
        Assert.assertEquals(expResult, result);
        
        
    }
    
    
    
    /**
     * Test of toString method, of class com.sun.jbi.smtpbc.extensions.SMTPProxy.
     */
    public void testToString() {
        System.out.println("toString");
        
        final String endWithString = "\nSMTP Element Type " + instance.getElementType() + ":"+
                "\nRequired=" + instance.getRequired(); ;
        final String result = instance.toString();
        final Boolean expectResult = Boolean.TRUE;
        final Boolean testResult = new Boolean(result.endsWith(endWithString));
        Assert.assertEquals(expectResult, testResult);
    }
    
}
