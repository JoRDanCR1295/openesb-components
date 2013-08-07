 /*
 * JMXBindingInstaller.java
 */
package net.openesb.component.BindingComponent-archetype;

import net.openesb.component.BindingComponent-archetype.common.ComponentInstaller;

/**
 * This class extends the ComponentInstaller that implements the
 * javax.jbi.component.Bootstrap for a component installation contract.
 *
 * If the default implemenation of the javax.jbi.component.Bootstrap in
 * com.sun.jbi.sample.component.common.ComponentInstaller is not sufficient for
 * this components install, uninstall and upgrade requirements, override the
 * default implementation here to add component specific installation and
 * upgrade tasks such as initializing configuration mbean and creating resources
 * specific to component or upgrade workspace and service units during upgrading
 * the component.
 *
 * @see javax.jbi.Bootstrap
 * @see com.sun.jbi.sample.component.common.ComponentInstaller
 * @author chikkala
 */
public class JMXBindingInstaller extends ComponentInstaller {

    /**
     * Constructor to create the ComponentInstaller.
     */
    public JMXBindingInstaller() {
    }
}
