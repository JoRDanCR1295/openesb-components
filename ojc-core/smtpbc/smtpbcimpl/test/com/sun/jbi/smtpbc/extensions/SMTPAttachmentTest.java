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
 * @(#)SMTPAttachmentTest.java 
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
public class SMTPAttachmentTest extends TestCase {
    SMTPAttachment instance = null;
    public SMTPAttachmentTest(final String testName) {
        super(testName);
    }
    
    @Override
	protected void setUp() throws Exception {
        instance = new SMTPAttachment();
    }
    
    @Override
	protected void tearDown() throws Exception {
    }
    
    /**
     * Test of getElementType method, of class com.sun.jbi.smtpbc.extensions.SMTPAttachment.
     */
    public void testGetElementType() {
        System.out.println("getElementType");
        
        final QName expResult = new QName(SMTPConstants.NS_URI_SMTP, SMTPAttachment.ELEM_ATTACHMENT);
        
        final QName result = instance.getElementType();
        Assert.assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }
    
    /**
     * Test of setElementType method, of class com.sun.jbi.smtpbc.extensions.SMTPAttachment.
     */
    public void testSetElementType() {
        System.out.println("setElementType");
        
        final QName elementType = null;
        
        
        instance.setElementType(elementType);
        final QName expResult = new QName(SMTPConstants.NS_URI_SMTP, SMTPAttachment.ELEM_ATTACHMENT);
        
        final QName result = instance.getElementType();
        Assert.assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }
    
    /**
     * Test of getRequired method, of class com.sun.jbi.smtpbc.extensions.SMTPAttachment.
     */
    public void testSetGetRequired() {
        System.out.println("setRequired and getRequired");
        
        
        final Boolean required = Boolean.TRUE;
        instance.setRequired(required);
        
        final Boolean expResult = Boolean.TRUE;
        final Boolean result = instance.getRequired();
        Assert.assertEquals(expResult, result);
        
    }
    
    
    /**
     * Test of getContentType method, of class com.sun.jbi.smtpbc.extensions.SMTPAttachment.
     */
    public void testSetGetContentType() {
        System.out.println("Testing setContentTypegetContentType");
        
        
        final String contentType = "text/html";
        instance.setContentType(contentType);
        
        final String expResult = "text/html";
        final String result = instance.getContentType();
        Assert.assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }
    
    
    /**
     * Test of getName method, of class com.sun.jbi.smtpbc.extensions.SMTPAttachment.
     */
    public void testSetGetName() {
        System.out.println("Testing setName getName");
        
        
        final String name = "attachmentname";
        instance.setName(name);
        final String expResult = "attachmentname";
        final String result = instance.getName();
        Assert.assertEquals(expResult, result);
        
    }
    
    
    
    /**
     * Test of getContent method, of class com.sun.jbi.smtpbc.extensions.SMTPAttachment.
     */
    public void testSetGetContent() {
        System.out.println("Testing setContent and getContent");
        
        
        final String content = "what is the content";
        instance.setContent(content);
        final String expResult = "what is the content";
        final String result = instance.getContent();
        Assert.assertEquals(expResult, result);
        
        
    }
    
    
    
    /**
     * Test of toString method, of class com.sun.jbi.smtpbc.extensions.SMTPAttachment.
     */
    public void testToString() {
        System.out.println("Testing toString");
        
        
        
        final String endWithString = "\nSMTP operation (" + instance.getElementType() + "):"+"\nRequired=" + instance.getRequired();
        final String result = instance.toString();
        final Boolean expectResult = Boolean.TRUE;
        final Boolean testResult = new Boolean(result.endsWith(endWithString));
        System.out.println(result);
        Assert.assertEquals(expectResult, testResult);
        
        
    }
    
}
