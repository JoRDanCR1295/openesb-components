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
 * @(#)SMTPOperationOutputTest.java 
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
public class SMTPOperationOutputTest extends TestCase {
    SMTPOperationOutput instance = null;
    
    public SMTPOperationOutputTest(final String testName) {
        super(testName);
    }

    @Override
	protected void setUp() throws Exception {
        instance = new SMTPOperationOutput();
    }

    @Override
	protected void tearDown() throws Exception {
    }

    /**
     * Test of getElementType method, of class com.sun.jbi.smtpbc.extensions.SMTPOperationOutput.
     */
    public void testGetElementType() {
        System.out.println("getElementType");
               
        final QName expResult =  new QName(SMTPConstants.NS_URI_SMTP, Constants.ELEM_OUTPUT);;
        final QName result = instance.getElementType();
        Assert.assertEquals(expResult, result);
        
    }

    /**
     * Test of setElementType method, of class com.sun.jbi.smtpbc.extensions.SMTPOperationOutput.
     */
    public void testSetElementType() {
        System.out.println("setElementType");
        
        final QName elementType = null;
        
        instance.setElementType(elementType);
        
        final QName expResult =  new QName(SMTPConstants.NS_URI_SMTP, Constants.ELEM_OUTPUT);;
        final QName result = instance.getElementType();
        Assert.assertEquals(expResult, result);
        
    }

    /**
     * Test of getRequired method, of class com.sun.jbi.smtpbc.extensions.SMTPOperationOutput.
     */
    public void testSetGetRequired() {
        System.out.println("getRequired");
        final Boolean required = Boolean.TRUE;
        instance.setRequired(required);
        
        final Boolean expResult = Boolean.TRUE;
        final Boolean result = instance.getRequired();
        Assert.assertEquals(expResult, result);
        
        
    }


    /**
     * Test of toString method, of class com.sun.jbi.smtpbc.extensions.SMTPOperationOutput.
     */
    public void testToString() {
        System.out.println("toString");
        
        final String endWithString = "\nSMTP operation (" + instance.getElementType() + "):"+
                "\nRequired=" + instance.getRequired();
        final String result = instance.toString();
        final Boolean expectResult = Boolean.TRUE;
        final Boolean testResult = new Boolean(result.endsWith(endWithString));
        Assert.assertEquals(expectResult, testResult);
    }
    
}
