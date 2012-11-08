/*
 * ProviderSEInstaller.java
 */
package xacmlse;

import com.sun.jbi.sample.component.common.ComponentInstaller;

/**
 * This class extends the ComponentInstaller that implements the
 * javax.jbi.component.Bootstrap for a component installation contract.
 *
 * Add any installation specific tasks such as initializing configuration
 * mbean and creating resources specific to component by overriding base class
 * methods here if there is any component specific installation tasks are needed.
 *
 * @see javax.jbi.Bootstrap
 * @see com.sun.jbi.sample.component.common.ComponentInstaller
 * @author chikkala
 */
public class ProviderSEInstaller extends ComponentInstaller {
    
    /** Constructor to create the MyEngineInstaller. */
    public ProviderSEInstaller() {
    }
}
