/*
 * EngineJUnitTest.java
 */
package ${package};

import junit.framework.TestCase;

/**
 * JUnit based test that test the component runtime.
 * 
 * @author chikkala
 */
public class EngineJUnitTest extends TestCase {
    
    public EngineJUnitTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }
    
    public void testEngineRuntime() throws Exception {
        javax.jbi.component.Component runtime = new MyEngineRuntime();
        javax.jbi.component.ComponentLifeCycle lifecycle = runtime.getLifeCycle();
        assertNotNull("Lifecycle reference returned by the Component can not be null", lifecycle);
    }
}
