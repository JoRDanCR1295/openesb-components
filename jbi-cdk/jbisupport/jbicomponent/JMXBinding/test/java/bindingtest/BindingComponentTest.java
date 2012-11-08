/*
 * BindingComponentTest.java
 */
package bindingtest;
import com.sun.jbi.sample.component.test.JBIComponentTestClient;
import com.sun.jbi.sample.component.test.JMXBindingTestClient;
import java.util.Properties;
//__JUNIT__ import junit.framework.TestCase;

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
public class BindingComponentTest 
//__JUNIT__        extends TestCase         
{
    
    public BindingComponentTest(String testName) {
//__JUNIT__        super(testName);
    }
        
    public void test1() throws Exception {
        String testPropertiesPath = "test1.properties";
        JBIComponentTestClient testClient = new JMXBindingTestClient();
        Properties testProps = testClient.loadTestProperties(this.getClass(), testPropertiesPath);
        testClient.testService(testProps);
    }
    
    public void test2() throws Exception {
        String testPropertiesPath = "test2.properties";
        JBIComponentTestClient testClient = new JMXBindingTestClient();
        Properties testProps = testClient.loadTestProperties(this.getClass(), testPropertiesPath);
        testClient.testService(testProps);
    }
    
    public static void main(String[] args) {
        try {
            BindingComponentTest compTest = new BindingComponentTest("BindingComponentTest");
            compTest.test1();
            compTest.test2();
        } catch(Exception ex) {
            ex.printStackTrace();
        }
    }
    
}
