/*
 * ServiceEngineTest.java
 */

package enginetest;

import com.sun.jbi.sample.component.test.SOAPBindingTestClient;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Properties;

/**
 * The test method in this testcase uses the SOAPBindingTestClient to send the 
 * input document to the echo service provided by service engine via soap binding
 * component and receives the output document which will be placed in test results
 * directory under the same package as this test case.
 * @see com.sun.jbi.sample.component.test.SOAPBindingTestClinet
 *  @author chikkala
 */
public class ServiceEngineTest {
    
    public ServiceEngineTest(String testName) {
        // super(testName);
    }
    
    public void testServiceEngine() throws Exception {
        SOAPBindingTestClient soapClient = new SOAPBindingTestClient();
        Properties testProps = soapClient.loadTestProperties(this.getClass());
        soapClient.testService(testProps);
    }
    
    public static void main(String[] args) {
        try {
            new ServiceEngineTest("ServiceEngineTest").testServiceEngine();
        } catch(Exception ex) {
            ex.printStackTrace();
        }
    }
    
}
