 /*
  * MyEngineInstaller.java
  */
package ${package};

import com.sun.jbi.sample.component.common.BootstrapImpl;

/**
 * This class extends the BootstrapImpl that implements the
 * javax.jbi.component.Bootstrap for a component installation contract.
 *
 * Since MyEngine SE sample does not have any installation specific tasks to
 * implement, this implmenation is just a marker implementation to specify a
 * specific class representing the MyEngine SE in its installation descriptor.
 *
 * Add any installation specific tasks such as initializing configuration
 * and creating resources specific to MyEngine SE by overriding base class
 * methods here.
 *
 * @see javax.jbi.Bootstrap
 * @see com.sun.jbi.sample.component.common.BootstrapImpl
 * @author chikkala
 */
public class MyEngineInstaller extends BootstrapImpl {
    
    /** Constructor to create the MyEngineInstaller. */
    public MyEngineInstaller() {
    }
}
