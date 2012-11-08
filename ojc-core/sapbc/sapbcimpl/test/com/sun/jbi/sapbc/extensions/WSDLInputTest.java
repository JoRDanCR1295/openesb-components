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
 * @(#)WSDLInputTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc.extensions;

import com.sun.wsdl.model.extensions.ExtensibilityElement;
import com.sun.wsdl.model.extensions.impl.ExtensibilityElementImpl;
import junit.framework.*;

/**
 *
 * @author sweng
 */
public class WSDLInputTest extends TestCase {
    WSDLInput instance = new WSDLInput();
    ExtensibilityElement e = new ExtensibilityElementImpl();
    
    public WSDLInputTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(WSDLInputTest.class);
        
        return suite;
    }

    /**
     * Test of setSAPMessage and getSAPMessage method, of class com.sun.jbi.filebc.extensions.WSDLInput.
     */
    public void testSetGetSAPMessage() {
        System.out.println("Testing setFileRead and getFileRead");
        
        SAPMessage sapMessage = null;
        try {
            sapMessage = new SAPMessage(e);
        } catch (Exception ex) {
            ex.printStackTrace();
            String errMsg = "testSetGetSAPMessage: Exception Thrown ["+ex.getClass().getName()+"] Message ["+ex.getMessage()+"]";
            fail(errMsg);
        }
        instance.setSAPMessage(sapMessage);
        SAPMessage result = instance.getSAPMessage();
        assertEquals(sapMessage, result);
        
        System.out.println("Successfully tested setFileRead and getFileRead");
    }
 
}
