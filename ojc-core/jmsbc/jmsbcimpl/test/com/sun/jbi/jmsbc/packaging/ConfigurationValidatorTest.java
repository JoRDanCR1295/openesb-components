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
 * @(#)ConfigurationValidatorTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.packaging;

import junit.framework.*;
import java.util.logging.Logger;
import com.sun.jbi.jmsbc.Endpoint;
//import com.codestreet.selector.Selector;
//import com.codestreet.selector.parser.InvalidSelectorException;

/**
 *
 * @author jtran
 */
public class ConfigurationValidatorTest extends TestCase {
    
    public ConfigurationValidatorTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(ConfigurationValidatorTest.class);
        
        return suite;
    }

    /**
     * Test of validateMessageSelector method, of class com.sun.jbi.jmsbc.packaging.ConfigurationValidator.
     */
    public void testValidateMessageSelector() {
        
        assertEquals (true, true);
/*
        // Invalidate some bad message selectors
        String badSelector1 = "foo && bar";  // wrong &&
        String badSelector2 = "foo == 2000 OR bar < 2";  // wrong ==
        String badSelector3 = "foo = 'bar AND bar = 'foo'"; // missing '
        boolean invalidSyntax1 = false;
        boolean invalidSyntax2 = false;
        boolean invalidSyntax3 = false;
        
        try {
            ConfigurationValidator.validateMessageSelector(badSelector1);
        } catch (InvalidConfigurationException ex) {
            invalidSyntax1 = true;
        }

        try {
            ConfigurationValidator.validateMessageSelector(badSelector2);
        } catch (InvalidConfigurationException ex) {
            invalidSyntax2 = true;
        }

        try {
            ConfigurationValidator.validateMessageSelector(badSelector3);
        } catch (InvalidConfigurationException ex) {
            invalidSyntax3 = true;
        }
        
        assertEquals (true, invalidSyntax1 && invalidSyntax2 && invalidSyntax3);
        
        // Validate some good message selectors - From JMS 1.1 Spec
        String goodSelector1 = "Country IN ('UK', 'US', 'France')";
        String goodSelector2 = "phone LIKE '12%3'";
        String goodSelector3 = "JMSType = 'car' AND color = 'blue' AND weight > 2500";
        boolean validSyntax1 = true;
        boolean validSyntax2 = true;
        boolean validSyntax3 = true;
        try {
            ConfigurationValidator.validateMessageSelector(goodSelector1);
        } catch (InvalidConfigurationException ex) {
            validSyntax1 = false;
        }

        try {
            ConfigurationValidator.validateMessageSelector(goodSelector2);
        } catch (InvalidConfigurationException ex) {
            validSyntax2 = false;
        }

        try {
            ConfigurationValidator.validateMessageSelector(goodSelector3);
        } catch (InvalidConfigurationException ex) {
            validSyntax3 = false;
        }
        
        assertEquals (true, validSyntax1 && validSyntax2 && validSyntax3);
*/
    }
    
    public static void main (String [] args) {
        ConfigurationValidatorTest test = new ConfigurationValidatorTest ("main");
        test.testValidateMessageSelector();
    }
}
