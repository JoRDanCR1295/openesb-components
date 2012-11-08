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
 * @(#)ExecInputTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.execbc.extensions;

import junit.framework.*;
import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

import com.sun.jbi.execbc.extensions.ExecInput;
import com.sun.jbi.execbc.extensions.ExecMessage;

/**
 *
 * @author sweng
 */
public class ExecInputTest extends TestCase {
    ExecInput instance = new ExecInput();
    
    public ExecInputTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(ExecInputTest.class);
        
        return suite;
    }

    /**
     * Test of setFileMessage and getFileMessage method, of class com.sun.jbi.execbc.extensions.ExecInput.
     */
    public void testSetGetFileMessage() {
        System.out.println("Testing setFileRead and getFileRead");
        
        ExecMessage execMessage = new ExecMessage();
        instance.setExecMessage(execMessage);
        ExecMessage result = instance.getExecMessage();
        assertEquals(execMessage, result);
        
        System.out.println("Successfully tested setFileRead and getFileRead");
    }
 
}
