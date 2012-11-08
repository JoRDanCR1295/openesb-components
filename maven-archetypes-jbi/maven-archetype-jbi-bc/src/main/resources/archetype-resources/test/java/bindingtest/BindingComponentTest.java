/*
 * ServiceEngineTest.java
 */
package bindingtest;

import com.sun.jbi.sample.component.test.JMXBindingTestClient;
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
 * The test method in this testcase uses the JMXBindingTestClient to send the 
 * input document to the echo service provided by service engine via the binding
 * component by sending the input to the binding using jmx interface.  The output
 * received from the service invocation will be placed in test results directory
 * under the same package as this test case.
 * @see com.sun.jbi.sample.component.test.JMXBindingTestClinet
 *
 *  @author chikkala
 */
public class BindingComponentTest {
    
    public BindingComponentTest(String testName) {
        // super(testName);
    }
    
    public void testBindingComponent() throws Exception {
        
        JMXBindingTestClient testClient = new JMXBindingTestClient();
        Properties testProps = testClient.loadTestProperties(this.getClass());
        testClient.testService(testProps);
    }
    
    public static void main(String[] args) {
        try {
            new BindingComponentTest("BindingComponentTest").testBindingComponent();
        } catch(Exception ex) {
            ex.printStackTrace();
        }
    }
    
}
