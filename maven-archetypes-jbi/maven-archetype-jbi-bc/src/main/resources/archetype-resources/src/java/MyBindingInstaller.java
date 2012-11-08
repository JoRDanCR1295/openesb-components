 /*
  * MyBindingInstaller.java
  */
package ${package};

import com.sun.jbi.sample.component.common.BootstrapImpl;

/**
 * This class extends the BootstrapImpl that implements the 
 * javax.jbi.component.Bootstrap for a component installation contract.
 *
 * Since MyBinding BC sample does not have any installation specific tasks to 
 * implement, this implmenation is just a marker implementation to specify a 
 * specific class representing MyBinding BC in its installation descriptor.
 *
 * Add any installation specific tasks such as initializing configuration
 * and creating resources specific to MyBinding BC by overriding base class 
 * methods here.
 *
 * @see javax.jbi.Bootstrap
 * @see com.sun.jbi.sample.component.common.BootstrapImpl
 * @author chikkala
 */
public class MyBindingInstaller extends BootstrapImpl {
    
    /** Constructor to creatre the ComponentInstaller. */
    public MyBindingInstaller() {
    }
}
