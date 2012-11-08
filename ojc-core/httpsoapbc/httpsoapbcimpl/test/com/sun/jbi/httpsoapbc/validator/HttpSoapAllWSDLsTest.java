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
 * @(#)HttpSoapAllWSDLsTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.validator;

import junit.framework.*;
import java.io.File;
import java.io.FileFilter;
import java.io.PrintWriter;
import java.io.StringWriter;
import com.sun.jbi.wsdlvalidator.ValidatingWSDLReader;
import com.sun.jbi.wsdlvalidator.ValidationException;
import com.sun.jbi.httpsoapbc.configuration.RuntimeConfiguration;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.factory.WSDLFactory;

/**
 *
 * @author afung
 */
public class HttpSoapAllWSDLsTest extends TestCase {
    
    public HttpSoapAllWSDLsTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(HttpSoapAllWSDLsTest.class);
        
        return suite;
    }

    /**
     * Test of validate method, of class com.sun.jbi.httpsoapbc.validator.impl.HttpSoapValidator.
     */
    public void testValidateAllWSDLs() throws Exception {

        // Grab all our WSDL files to test
        File[] validWSDLs =
            new File("test/com/sun/jbi/httpsoapbc/validator/data/valid").listFiles(new FileFilter() {
                    public boolean accept(File pathname) {
                        return pathname.getName().endsWith(".wsdl");
                    }});
        File[] invalidWSDLs =
            new File("test/com/sun/jbi/httpsoapbc/validator/data/invalid").listFiles(new FileFilter() {
                    public boolean accept(File pathname) {
                        return pathname.getName().endsWith(".wsdl");
                    }});

        // Set up our reader
        WSDLFactory wsdlFactory = WSDLFactory.newInstance("com.sun.jbi.wsdlvalidator.factory.ValidatingWSDLFactory");
        ValidatingWSDLReader reader = (ValidatingWSDLReader)wsdlFactory.newWSDLReader();
        reader.setValidatorRegistry(new HttpSoapValidatorRegistry(new RuntimeConfiguration("test/com/sun/jbi/httpsoapbc/validator/data/config", null), true));
        
        // Test all valid WSDLs first
        if (validWSDLs != null) {
            for (int ii = 0; ii < validWSDLs.length; ii++) {
                try {
                    Definition def = reader.readWSDL(validWSDLs[ii].getAbsolutePath());
                } catch (Throwable th) {
                    StringWriter sw = new StringWriter();
                    PrintWriter pw = new PrintWriter(sw);
                    th.printStackTrace(pw);
                    fail(validWSDLs[ii].getAbsolutePath()+ " failed to validate when it should have.  Exception message: " + sw.toString());
                }
            }
        }
        
        // Test all invalid WSDLs
        if (invalidWSDLs != null) {
            for (int ii = 0; ii < invalidWSDLs.length; ii++) {
               try {
                   Definition def = reader.readWSDL(invalidWSDLs[ii].getAbsolutePath());
                   fail(invalidWSDLs[ii].getAbsolutePath()+" was valid when it should have failed.");
               } catch (WSDLException ex) {
                   Throwable th = ex.getTargetException();
                   if (th == null || !(th instanceof ValidationException)) {
                       StringWriter sw = new StringWriter();
                       PrintWriter pw = new PrintWriter(sw);
                       th.printStackTrace(pw);
                       fail(invalidWSDLs[ii].getAbsolutePath() + " failed with unexpected error.  Exception message: " + sw.toString());
                   }
               } catch (Throwable th) {
                   StringWriter sw = new StringWriter();
                   PrintWriter pw = new PrintWriter(sw);
                   th.printStackTrace(pw);
                   fail(invalidWSDLs[ii].getAbsolutePath() + " failed with unexpected error.  Exception message: " + sw.toString());
               }
            }
        }
    }
}
