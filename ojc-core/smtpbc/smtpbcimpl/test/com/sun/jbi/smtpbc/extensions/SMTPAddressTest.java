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
 * @(#)SMTPAddressTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc.extensions;

import junit.framework.*;

/**
 *
 * @author rchen
 */
public class SMTPAddressTest extends TestCase {
    
    private SMTPAddress[] mTests;
    
    /** Creates a new instance of SMTPAddressTest */
    public SMTPAddressTest(final String testName) {
        super(testName);
    }
    @Override
	protected void setUp() throws Exception {
        mTests = new SMTPAddress[1];
        mTests[0] = new  SMTPAddress();
        mTests[0].setSMTPServer("breas.stc.com");
    }
    
    @Override
	protected void tearDown() throws Exception {
    }
    
    public static Test suite() {
        final TestSuite suite = new TestSuite(SMTPAddressTest.class);
        
        return suite;
    }
    public void testGetElementType() {
    Assert.assertNotNull(mTests[0].getElementType());
    Assert.assertSame(SMTPAddress.QNAME_ADDRESS,mTests[0].getElementType());
    Assert.assertEquals(SMTPAddress.QNAME_ADDRESS,mTests[0].getElementType());
    Assert.assertEquals("breas.stc.com",mTests[0].getSMTPServer());
    }
}
