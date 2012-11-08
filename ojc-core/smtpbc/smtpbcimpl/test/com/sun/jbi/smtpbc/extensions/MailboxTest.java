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
 * @(#)MailboxTest.java 
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

import junit.framework.*;

/**
 *
 *
 * @author       Alexander Fung
 * @version      
 *
 */
public class MailboxTest extends TestCase {
    
    private Mailbox[] mTests;

    public MailboxTest(final String testName) {
        super(testName);
    }

    @Override
	protected void setUp() throws Exception {
        // Create positive test cases
        mTests = new Mailbox[7];
        mTests[0] = new Mailbox("abc@stc.com");
        mTests[1] = new Mailbox("ABC <abc@stc.com>");
        mTests[2] = new Mailbox("ABC <@msn.com: abc@stc.com>");
        mTests[3] = new Mailbox("Wilt . Chamberlain@NBA.us");
        mTests[4] = new Mailbox("Wilt . (the Stilt) Chamberlain@NBA.us");
        mTests[5] = new Mailbox("Wilt . (the (stilt) Stilt) Chamberlain (the stilt)@NBA.us");

        // Create negative test cases
        try {
            mTests[6] = new Mailbox("ABC%20%3Cabc%40stc.com%3E");
        } catch (final InvalidMailboxException ex) {
        }
    }

    @Override
	protected void tearDown() throws Exception {
    }

    public static Test suite() {
        final TestSuite suite = new TestSuite(MailboxTest.class);
        
        return suite;
    }

    /**
     * Test of getAddressSpec method of
     * class com.sun.jbi.smtpbc.extensions.Mailbox.
     */
    public void testGetAddressSpec() {
        
        Assert.assertEquals("abc@stc.com", mTests[0].getAddressSpec());
        Assert.assertEquals("abc@stc.com", mTests[1].getAddressSpec());
        Assert.assertEquals("abc@stc.com", mTests[2].getAddressSpec());
        Assert.assertEquals("Wilt . Chamberlain@NBA.us",
                     mTests[3].getAddressSpec());
        Assert.assertEquals("Wilt . (the Stilt) Chamberlain@NBA.us",
                     mTests[4].getAddressSpec());
    }

    /**
     * Test of getNormalizedAddressSpec method of
     * class com.sun.jbi.smtpbc.extensions.Mailbox.
     */
    public void testGetNormalizedAddressSpec() {
        
        Assert.assertEquals("abc@stc.com", mTests[0].getNormalizedAddressSpec());
        Assert.assertEquals("abc@stc.com", mTests[1].getNormalizedAddressSpec());
        Assert.assertEquals("abc@stc.com", mTests[2].getNormalizedAddressSpec());
        Assert.assertEquals("Wilt.Chamberlain@NBA.us",
                     mTests[3].getNormalizedAddressSpec());
        Assert.assertEquals("Wilt.Chamberlain@NBA.us",
                     mTests[4].getNormalizedAddressSpec());
        Assert.assertEquals("Wilt.Chamberlain@NBA.us",
                     mTests[5].getNormalizedAddressSpec());
    }

    /**
     * Test of getLocalPart method of
     * class com.sun.jbi.smtpbc.extensions.Mailbox.
     */
    public void testGetLocalPart() {
        
        Assert.assertEquals("abc", mTests[0].getLocalPart());
        Assert.assertEquals("abc", mTests[1].getLocalPart());
        Assert.assertEquals("abc", mTests[2].getLocalPart());
        Assert.assertEquals("Wilt . Chamberlain", mTests[3].getLocalPart());
        Assert.assertEquals("Wilt . (the Stilt) Chamberlain",
                     mTests[4].getLocalPart());
    }

    /**
     * Test of getNormalizedLocalPart method of
     * class com.sun.jbi.smtpbc.extensions.Mailbox.
     */
    public void testGetNormalizedLocalPart() {
        
        Assert.assertEquals("abc", mTests[0].getNormalizedLocalPart());
        Assert.assertEquals("abc", mTests[1].getNormalizedLocalPart());
        Assert.assertEquals("abc", mTests[2].getNormalizedLocalPart());
        Assert.assertEquals("Wilt.Chamberlain",
                     mTests[3].getNormalizedLocalPart());
        Assert.assertEquals("Wilt.Chamberlain",
                     mTests[4].getNormalizedLocalPart());
    }

    /**
     * Test of getDomain method of class com.sun.jbi.smtpbc.extensions.Mailbox.
     */
    public void testGetDomain() {
        
        Assert.assertEquals("stc.com", mTests[0].getDomain());
        Assert.assertEquals("stc.com", mTests[1].getDomain());
        Assert.assertEquals("stc.com", mTests[2].getDomain());
        Assert.assertEquals("NBA.us", mTests[3].getDomain());
        Assert.assertEquals("NBA.us", mTests[4].getDomain());
    }

    /**
     * Test of getNormalizedDomain method of class
     * com.sun.jbi.smtpbc.extensions.Mailbox.
     */
    public void testGetNormalizedDomain() {
        
        Assert.assertEquals("stc.com", mTests[0].getNormalizedDomain());
        Assert.assertEquals("stc.com", mTests[1].getNormalizedDomain());
        Assert.assertEquals("stc.com", mTests[2].getNormalizedDomain());
        Assert.assertEquals("NBA.us", mTests[3].getNormalizedDomain());
        Assert.assertEquals("NBA.us", mTests[4].getNormalizedDomain());
    }

    /**
     * Test of getPhrase method of class com.sun.jbi.smtpbc.extensions.Mailbox.
     */
    public void testGetPhrase() {
        
        Assert.assertEquals("", mTests[0].getPhrase());
        Assert.assertEquals("ABC", mTests[1].getPhrase());
        Assert.assertEquals("ABC", mTests[2].getPhrase());
        Assert.assertEquals("", mTests[3].getPhrase());
        Assert.assertEquals("", mTests[4].getPhrase());
    }

    /**
     * Test of getRoute method, of class com.sun.jbi.smtpbc.extensions.Mailbox.
     */
    public void testGetRoute() {
        
        
        Assert.assertEquals("", mTests[0].getRoute());
        Assert.assertEquals("", mTests[1].getRoute());
        Assert.assertEquals("@msn.com:", mTests[2].getRoute());
        Assert.assertEquals("", mTests[3].getRoute());
        Assert.assertEquals("", mTests[4].getRoute());
    }

    /**
     * Test of unmarshal method, of class com.sun.jbi.smtpbc.extensions.Mailbox.
     */
    public void testUnmarshal() {
        
    }

    /**
     * Test of marshal method, of class com.sun.jbi.smtpbc.extensions.Mailbox.
     */
    public void testMarshal() {
        
        try {
            Assert.assertEquals("abc@stc.com", mTests[0].marshal());
            Assert.assertEquals("ABC <abc@stc.com>", mTests[1].marshal());
            Assert.assertEquals("ABC <@msn.com: abc@stc.com>", mTests[2].marshal());
            Assert.assertEquals("Wilt . Chamberlain@NBA.us", mTests[3].marshal());
            Assert.assertEquals("Wilt . (the Stilt) Chamberlain@NBA.us",
                         mTests[4].marshal());
        } catch (final Exception ex) {
            Assert.fail(ex.getMessage());
        }
    }
    
    public void testInvalildEmailids(){
        try {
            Mailbox mailbox = new Mailbox("user@@.com");
            Assert.fail();
        } catch (InvalidMailboxException ex) {

        }
    }
    
}
