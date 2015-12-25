/**
 * 
 */
package com.sun.jbi.httpsoapbc.soap12.validator;

import java.io.File;
import java.io.FileFilter;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URL;

import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.factory.WSDLFactory;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import com.sun.jbi.httpsoapbc.configuration.RuntimeConfiguration;
import com.sun.jbi.httpsoapbc.validator.HttpSoapValidatorRegistry;
import com.sun.jbi.wsdlvalidator.ValidatingWSDLReader;
import com.sun.jbi.wsdlvalidator.ValidationException;

/**
 * @author Sujit Biswas
 *
 */
public class Soap12WSDLTest extends TestCase {
    
    public Soap12WSDLTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(Soap12WSDLTest.class);
        
        return suite;
    }

    
    public void testValidateAllWSDLs() throws Exception {
	
	
	URL url = Soap12WSDLTest.class.getResource("config.properties");
	
	File f  = new File(url.getFile());
	
	

        // Grab all our WSDL files to test
        File[] validWSDLs =
            f.getParentFile().listFiles(new FileFilter() {
                    public boolean accept(File pathname) {
                        return pathname.getName().endsWith(".wsdl");
                    }});
        File[] invalidWSDLs =
           f.getParentFile().listFiles(new FileFilter() {
                    public boolean accept(File pathname) {
                        return pathname.getName().endsWith("*invalid*.wsdl");
                    }});

        // Set up our reader
        WSDLFactory wsdlFactory = WSDLFactory.newInstance("com.sun.jbi.wsdlvalidator.factory.ValidatingWSDLFactory");
        ValidatingWSDLReader reader = (ValidatingWSDLReader)wsdlFactory.newWSDLReader();
        
        RuntimeConfiguration rtc = new RuntimeConfiguration(f.getParent(), null); 
        
        rtc.setHttpDefaultPort(9080);
        
        reader.setValidatorRegistry(new HttpSoapValidatorRegistry(rtc, true));
        
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
