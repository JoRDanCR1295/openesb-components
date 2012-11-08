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
 * @(#)EmailClientAgentTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc.extservice;

import junit.framework.*;
import javax.activation.DataHandler;

import java.util.Properties;
import java.io.FileInputStream;
import java.io.File;

/**
 *
 * @author Vishnuvardhan P.R
 */
public class EmailClientAgentTest extends TestCase {
    
    
    private EmailConfiguration emailConfiguration = null;
    private EmailMessage emailMessage = null;
    private EmailClientAgent instance = null;
    Properties testProperties = null;
    private String smtpServerHost = null;
    private String toEmailId = null;
    private String ccEmailId = null;
    private String bccEmailId = null;
    
    private final String PROPERTIES_FILE = "SMTPBCTestConfigure.properties";

    
    public EmailClientAgentTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        instance = new EmailClientAgent();
        emailConfiguration = new EmailConfiguration();
        emailMessage = new EmailMessage();
        
        testProperties = new Properties();
        testProperties.load(this.getClass().getClassLoader().getResourceAsStream(PROPERTIES_FILE));
        smtpServerHost = testProperties.getProperty("emailServerHost");
        toEmailId = testProperties.getProperty("toEmailId"); 
        ccEmailId = testProperties.getProperty("ccEmailId");
        bccEmailId = testProperties.getProperty("bccEmailId");
        
        emailConfiguration.setHostSend(smtpServerHost);
    }
    

    /**
     * Test of sendMessage method, of class com.sun.jbi.smtpbc.extservice.EmailClientAgent.
     */
    public void testSendMessage() throws Exception {
        System.out.println("Testing testSendMessage");
        
        emailMessage.getFrom().setAddress("junitTest@localhost.com");
        emailMessage.setMsgText("Testing of method testSendMessage");
        emailMessage.setSubject("Subject for testSendMessage");
        emailMessage.addTo(new EmailAddress(toEmailId,"SMTPBC JUnitTest")) ;
        boolean result = instance.sendMessage(emailConfiguration, emailMessage);
        assertEquals(true, result);
    }

    /**
     * Test of sendMessage method, of class com.sun.jbi.smtpbc.extservice.EmailClientAgent.
     */
    public void testSendMessageWithCC() throws Exception {
        System.out.println("Testing testSendMessageWithCC");
        
        emailMessage.getFrom().setAddress("junitTest@localhost.com");
        emailMessage.setMsgText("Testing of method testSendMessage");
        emailMessage.setSubject("Subject for testSendMessageWithCc");
        emailMessage.addTo(new EmailAddress(toEmailId,"SMTPBC JUnitTest")) ;        
        emailMessage.addCc(new EmailAddress(ccEmailId,"SMTPBC JUnitTest")) ;       
        boolean result = instance.sendMessage(emailConfiguration, emailMessage);
        assertEquals(true, result);
    }    
    
    /**
     * Test of sendMessage method, of class com.sun.jbi.smtpbc.extservice.EmailClientAgent.
     */
    public void testSendMessageWithBcc() throws Exception {
        System.out.println("Testing testSendMessageWithBcc");
        
        emailMessage.getFrom().setAddress("junitTest@localhost.com");
        emailMessage.setMsgText("Testing of method testSendMessage");
        emailMessage.setSubject("Subject for testSendMessageWithBcc");
        emailMessage.addTo(new EmailAddress(toEmailId,"SMTPBC JUnitTest")) ;        
        emailMessage.addBcc(new EmailAddress(bccEmailId,"SMTPBC JUnitTest")) ;
        boolean result = instance.sendMessage(emailConfiguration, emailMessage);
        assertEquals(true, result);
    }        
    
    /**
     * Test of sendMessage method, of class com.sun.jbi.smtpbc.extservice.EmailClientAgent.
     */
    public void testSendMessageWithToCCBCC() throws Exception {
        System.out.println("Testing testSendMessageWithToCCBCC");
        
        emailMessage.getFrom().setAddress("junitTest@localhost.com");
        emailMessage.setMsgText("Testing of method testSendMessage");
        emailMessage.setSubject("Subject for testSendMessageWithToCCBCC");
        emailMessage.addTo(new EmailAddress(toEmailId,"SMTPBC JUnitTest")) ;
        emailMessage.addCc(new EmailAddress(ccEmailId,"SMTPBC JUnitTest"));
        emailMessage.addBcc(new EmailAddress(bccEmailId,"SMTPBC JUnitTest"));
        boolean result = instance.sendMessage(emailConfiguration, emailMessage);
        assertEquals(true, result);
    }    
    
    /**
     * Test of sendMessage method, of class com.sun.jbi.smtpbc.extservice.EmailClientAgent.
     */
    public void testSendMessageForMultipleTo() throws Exception {
        System.out.println("Testing testSendMessageForMultipleTo");
        
        emailMessage.getFrom().setAddress("junitTest@localhost.com");
        emailMessage.setMsgText("Testing of method testSendMessage");
        emailMessage.setSubject("Subject for testSendMessageForMultipleTo");
        emailMessage.addTo(new EmailAddress(toEmailId,"SMTPBC JUnitTest")) ;
        emailMessage.addTo(new EmailAddress(toEmailId,"SMTPBC JUnitTest")) ;
        boolean result = instance.sendMessage(emailConfiguration, emailMessage);
        assertEquals(true, result);
    }    

    /**
     * Test of sendFailureNotification method, of class com.sun.jbi.smtpbc.extservice.EmailClientAgent.
     */
   /* public void testSendFailureNotification() {
        System.out.println("Testing testSendFailureNotification");
        
        emailMessage.getFrom().setAddress("mailwayi@boreas.stc.com");
        emailMessage.setMsgText("Testing of method sendFailureNotification");
        emailMessage.setSubject("Subject for testSendFailureNotification");
        emailMessage.addTo(new EmailAddress("unknownUser@unknownHost.com","Invalid User")) ;
        try {
            instance.sendMessage(emailConfiguration, emailMessage);
            fail("testSendFailureNotification failed. Expected EmailApplicationException but not occurred.");
        } catch (EmailApplicationException ex) {
            instance.sendFailureNotification(emailConfiguration, emailMessage);
        }



        
        //assertEquals(true,result);
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }
    */
}
