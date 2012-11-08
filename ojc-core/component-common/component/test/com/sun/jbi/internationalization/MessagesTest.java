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
 * @(#)MessagesTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.internationalization;

import junit.framework.*;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.text.MessageFormat;
import java.util.Locale;
import java.util.logging.Logger;
import java.util.logging.Level;
import com.sun.jbi.internationalization.test.TestClass;
import com.sun.jbi.internationalization.test2.Test2Class;
import javax.jbi.component.ComponentContext;

/**
 *
 * @author Sun Microsystems
 */
public class MessagesTest extends TestCase {
    
    public MessagesTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(MessagesTest.class);
        
        return suite;
    }

    /**
     * Test of getLogger method, of class com.sun.jbi.internationalization.Messages.
     */
    public void testGetLogger() {
        Logger logger = Messages.getLogger(TestClass.class);
        assertEquals(TestClass.class.getName(), logger.getName());
    }

    /**
     * Test of getLogger method, of class com.sun.jbi.internationalization.Messages.
     */
    public void testGetLoggerNoInternationalization() {
        Logger logger = Messages.getLogger(Test2Class.class);
        assertEquals(Test2Class.class.getName(), logger.getName());
    }

   
    /**
     * Test of getMessages method, of class com.sun.jbi.internationalization.Messages.
     */
    public void testGetMessages() {
        Messages result = Messages.getMessages(TestClass.class);
        assertNotNull(result);
    }

    /**
     * Test of getBundleName method, of class com.sun.jbi.internationalization.Messages.
     */
    public void testGetBundleName() {       
        String expResult = "com.sun.jbi.internationalization.test.messages.Bundle";
        String result = Messages.getBundleName(TestClass.class);
        assertEquals(expResult, result);
        
        Messages msgs = Messages.getMessages(TestClass.class);
        assertEquals(expResult, msgs.getBundleName());
    }

    /**
     * Test of getBundle method, of class com.sun.jbi.internationalization.Messages.
     */
    public void testGetBundle() {        
        Messages instance = Messages.getMessages(TestClass.class);
        ResourceBundle result = instance.getBundle();
        assertNotNull(result);

    }

    /**
     * Test of getString method, of class com.sun.jbi.internationalization.Messages.
     */
    public void testGetString() {
         Messages instance = Messages.getMessages(TestClass.class);
     
        String key = "TestProperty";        
        String expResult = "TestValue";
        String result = instance.getString(key);
        assertEquals(expResult, result);
    }
    
    public void testGetStringWithArgument() {
        Messages instance = Messages.getMessages(TestClass.class);  
        String key = "TestPropertyWithArgument";
        String argument = "arg1";
        String expResult = "TestValue " + argument;
        String result = instance.getString(key, argument);
        assertEquals(expResult, result);
    }
    
    public void testGetStringWithMultipleArguments() {
        Messages instance = Messages.getMessages(TestClass.class);        
        String key = "TestPropertyWithMultipleArguments";
        String argument1 = "arg1";
        String argument2 = "arg2";
        String expResult = "TestValue " + argument1 + " " + argument2;
        String result = instance.getString(key, new Object[] {argument1, argument2});
        assertEquals(expResult, result);
    }
    
}
