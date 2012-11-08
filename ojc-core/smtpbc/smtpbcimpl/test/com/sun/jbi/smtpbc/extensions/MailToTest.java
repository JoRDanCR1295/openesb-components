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
 * @(#)MailToTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/***************************************************************************
 *
 *          Copyright (c) 2005, SeeBeyond Technology Corporation,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ***************************************************************************/
package com.sun.jbi.smtpbc.extensions;

import java.net.URISyntaxException;
import junit.framework.*;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

/**
 *
 *
 * @author       Alexander Fung
 * @version      
 *
 */
public class MailToTest extends TestCase {
    
    private MailTo[] mTests;
    
    public MailToTest(final String testName) {
        super(testName);
    }

    @Override
	protected void setUp() throws Exception {

        // Positive test cases
        mTests = new MailTo[6];
        mTests[0] = new MailTo("mailto:afung@seebeyond.com");
        mTests[1] = new MailTo("mailto:afung@seebeyond.com%2Cnpiega@seebeyond.com");
        mTests[2] = new MailTo("mailto:%22Alexander%20Fung%22%20%3Cafung@seebeyond.com%3E");
        mTests[3] = new MailTo("mailto:Wilt%20.%20(the%20stilt)%20Chamberlain@NBA.us");
        mTests[4] = new MailTo("mailto:afung@seebeyond.com?body=hello%20world&cc=npiega@seebeyond.com");
        mTests[5] = new MailTo("mailto:afung@seebeyond.com?body=hello%20world&cc=npiega@seebeyond.com&bcc=rchen@seebeyond.com");

        // Negative test cases
        /*
        mTests[2] = new MailTo("mailto:afung@seebeyond.com,npiega@seebeyond.com");
        mTests[3] = new MailTo("mailto:afung%40seebeyond.com");
        */
    }

    @Override
	protected void tearDown() throws Exception {
    }

    public static Test suite() {
        final TestSuite suite = new TestSuite(MailToTest.class);
        
        return suite;
    }

    /**
     * Test of getProtocol method of
     * class com.sun.jbi.smtpbc.extensions.MailTo.
     */
    public void testGetProtocol() {
        
        Assert.assertEquals("mailto", mTests[0].getProtocol());
        Assert.assertEquals("mailto", mTests[1].getProtocol());
        Assert.assertEquals("mailto", mTests[2].getProtocol());
        Assert.assertEquals("mailto", mTests[5].getProtocol());
   
    }

    /**
     * Test of getMailbox method of class com.sun.jbi.smtpbc.extensions.MailTo.
     */
    public void testGetMailbox() {
        Collection mailboxes = mTests[0].getMailbox();
        Assert.assertEquals(1, mailboxes.size());
        Iterator it = mailboxes.iterator();
        Assert.assertEquals("afung@seebeyond.com",
                     ((Mailbox)it.next()).getNormalizedAddressSpec());

        mailboxes = mTests[1].getMailbox();
        Assert.assertEquals(2, mailboxes.size());
        it = mailboxes.iterator();
        Assert.assertEquals("afung@seebeyond.com",
                     ((Mailbox)it.next()).getNormalizedAddressSpec());
        Assert.assertEquals("npiega@seebeyond.com",
                     ((Mailbox)it.next()).getNormalizedAddressSpec());

        mailboxes = mTests[2].getMailbox();
        Assert.assertEquals(1, mailboxes.size());
        it = mailboxes.iterator();
        Mailbox box = (Mailbox)it.next();
        Assert.assertEquals("afung@seebeyond.com",
                     box.getNormalizedAddressSpec());
        Assert.assertEquals("\"Alexander Fung\"", box.getPhrase());

        mailboxes = mTests[3].getMailbox();
        Assert.assertEquals(1, mailboxes.size());
        it = mailboxes.iterator();
        box = (Mailbox)it.next();
        Assert.assertEquals("Wilt.Chamberlain@NBA.us",
                     box.getNormalizedAddressSpec());
        
        mailboxes = mTests[5].getMailbox();
        Assert.assertEquals(1, mailboxes.size());
        mailboxes = mTests[5].getCCMailbox();
        Assert.assertEquals(1, mailboxes.size());
         mailboxes = mTests[5].getBCCMailbox();
        Assert.assertEquals(1, mailboxes.size());
    }

    /**
     * Test of addMailbox method of class com.sun.jbi.smtpbc.extensions.MailTo.
     */
    public void testAddMailbox() {
        try {
            mTests[0].addMailbox(new Mailbox("npiega@seebeyond.com"));
            final Collection mailboxes = mTests[0].getMailbox();
            Assert.assertEquals(2, mailboxes.size());
            final Iterator it = mailboxes.iterator();
            Assert.assertEquals("afung@seebeyond.com",
                     ((Mailbox)it.next()).getNormalizedAddressSpec());
            Assert.assertEquals("npiega@seebeyond.com",
                         ((Mailbox)it.next()).getNormalizedAddressSpec());
            Assert.assertEquals("mailto:afung@seebeyond.com%2Cnpiega@seebeyond.com",
                         mTests[0].marshal());
        } catch (final Exception ex) {
            ex.printStackTrace();
            Assert.fail(ex.getMessage());
        }
    }

    /**
     * Test of addHeader method, of class com.sun.jbi.smtpbc.extensions.MailTo.
     */
    public void testAddHeader() {
        mTests[4].addHeader("body", "some%20random%2Cmessage");
        Assert.assertEquals("some%20random%2Cmessage", mTests[4].getHeader("body"));
        mTests[4].addHeader("randomHeader", "some message");
        Assert.assertEquals("some message", mTests[4].getHeader("randomHeader"));

    }

    /**
     * Test of getHeader method of class com.sun.jbi.smtpbc.extensions.MailTo.
     */
    public void testGetHeader() {
        Assert.assertEquals("hello world", mTests[4].getHeader("body"));
        Assert.assertEquals("npiega@seebeyond.com", mTests[4].getHeader("cc"));
    }

    /**
     * Test of getHeaders method of class com.sun.jbi.smtpbc.extensions.MailTo.
     */
    public void testGetHeaders() {
        final Map headers = mTests[4].getHeaders();
        Assert.assertEquals(2, headers.size());
        Assert.assertEquals("hello world", (String)headers.get("body"));
        Assert.assertEquals("npiega@seebeyond.com", (String)headers.get("cc"));
    }

    /**
     * Test of toString method, of class com.sun.jbi.smtpbc.extensions.MailTo.
     */
    public void testMarshal() {
        
        try {
            Assert.assertEquals("mailto:afung@seebeyond.com",
                         mTests[0].marshal());
            Assert.assertEquals("mailto:afung@seebeyond.com%2Cnpiega@seebeyond.com",
                         mTests[1].marshal());
            Assert.assertEquals("mailto:%22Alexander%20Fung%22%20%3Cafung@seebeyond.com%3E",
                         mTests[2].marshal());
        } catch (final Exception ex) {
            Assert.fail(ex.getMessage());
        }
    }
    
    /**
     * Test of parse method, of class com.sun.jbi.smtpbc.extensions.MailTo.
     */
    public void testUnmarshal() {
        try {
            MailTo mailTo = new MailTo("mailto:vinay.s@sun.com,vishnu@sun.com");
            Collection mailBoxCollection =  mailTo.getMailbox();
            Iterator it = mailBoxCollection.iterator() ;
            while(it.hasNext()){
                Mailbox mailBox = (Mailbox)it.next();
                System.out.println("Mailbox = " + mailBox.getAddressSpec());
            }
        } catch (URISyntaxException ex) {
            ex.printStackTrace();
        }

    }
    
}
