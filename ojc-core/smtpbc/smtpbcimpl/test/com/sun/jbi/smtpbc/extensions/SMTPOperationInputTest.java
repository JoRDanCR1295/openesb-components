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
 * @(#)SMTPOperationInputTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc.extensions;

import junit.framework.*;

import javax.xml.namespace.QName;
import com.ibm.wsdl.Constants;

/**
 *
 * @author rchen
 */
public class SMTPOperationInputTest extends TestCase {
    SMTPOperationInput instance = null;
    
    public SMTPOperationInputTest(final String testName) {
        super(testName);
    }

    @Override
	protected void setUp() throws Exception {
        instance = new SMTPOperationInput();
    }

    @Override
	protected void tearDown() throws Exception {
    }

    /**
     * Test of getElementType method, of class com.sun.jbi.smtpbc.extensions.SMTPOperationInput.
     */
    public void testGetElementType() {
        System.out.println("getElementType");
     
        final QName expResult = new QName(SMTPConstants.NS_URI_SMTP, Constants.ELEM_INPUT);
        final QName result = instance.getElementType();
        Assert.assertEquals(expResult, result);
        
    }

    /**
     * Test of setElementType method, of class com.sun.jbi.smtpbc.extensions.SMTPOperationInput.
     */
    public void testSetElementType() {
        System.out.println("setElementType");
        
        final QName elementType = null;
        instance.setElementType(elementType);
        //test SetElementType no op
        final QName expResult = new QName(SMTPConstants.NS_URI_SMTP, Constants.ELEM_INPUT);
        final QName result = instance.getElementType();
        Assert.assertEquals(expResult, result);
    }

    /**
     * Test of getRequired method, of class com.sun.jbi.smtpbc.extensions.SMTPOperationInput.
     */
    public void testSetGetRequired() {
        System.out.println("setRequired getRequired");
        final Boolean required = Boolean.TRUE;
        instance.setRequired(required);
        final Boolean expResult = Boolean.TRUE;;
        final Boolean result = instance.getRequired();
        Assert.assertEquals(expResult, result);
        
      
    }

   
    /**
     * Test of getMessage method, of class com.sun.jbi.smtpbc.extensions.SMTPOperationInput.
     */
    public void testSetGetMessage() {
        System.out.println("setMessage getMessage");
        
        final String message = "SMTPOperationInputMessage";
        instance.setMessage(message);
         
        final String expResult = "SMTPOperationInputMessage";
        final String result = instance.getMessage();
        Assert.assertEquals(expResult, result);
     
    }

 
    /**
     * Test of getSubject method, of class com.sun.jbi.smtpbc.extensions.SMTPOperationInput.
     */
    public void testSetGetSubject() {
        System.out.println("setSubjectgetSubject");
         final String subject = "test subject";
       
        instance.setSubject(subject);
         
        final String expResult = "test subject";
        final String result = instance.getSubject();
        Assert.assertEquals(expResult, result);
        
        
    }

   

    /**
     * Test of getFrom method, of class com.sun.jbi.smtpbc.extensions.SMTPOperationInput.
     */
    public void testSetGetFrom() {
        System.out.println("setFrom getFrom");
        
        final String from = "test From";
        instance.setFrom(from);
        final String expResult = "test From";
        final String result = instance.getFrom();
        Assert.assertEquals(expResult, result);
        
      
    }

    

    /**
     * Test of getCharset method, of class com.sun.jbi.smtpbc.extensions.SMTPOperationInput.
     */
    public void testSetGetCharset() {
        System.out.println("getCharset");
        
        final String charset = "ASCII";
        instance.setCharset(charset);
        final String expResult = "ASCII";
        final String result = instance.getCharset();
        Assert.assertEquals(expResult, result);
       
    }

   

    /**
     * Test of getSMTPAttachment method, of class com.sun.jbi.smtpbc.extensions.SMTPOperationInput.
     */
    public void testSetGetSMTPAttachment() {
        System.out.println("getSMTPAttachment");
        final SMTPAttachment[] attachments = new SMTPAttachment[1];
        attachments[0] = new SMTPAttachment();
        instance.setSMTPAttachment(attachments);        
       
        final SMTPAttachment[] result = instance.getSMTPAttachment();
        Assert.assertSame(attachments, result);
        
      
    }

   
    /**
     * Test of toString method, of class com.sun.jbi.smtpbc.extensions.SMTPOperationInput.
     */
    public void testToString() {
        System.out.println("toString");
         
        final String endWithString = "\nSMTP operation Input (" + instance.getElementType() + "):"+
                "\nRequired=" + instance.getRequired()+
                "\nMessage=" + instance.getMessage()+
                "\nSubject=" + instance.getSubject()+
                "\nCharset=" + instance.getCharset();
        final String result = instance.toString();
        final Boolean expectResult = Boolean.TRUE;
        final Boolean testResult = new Boolean(result.endsWith(endWithString));
        Assert.assertEquals(expectResult, testResult);
    }
    
}
