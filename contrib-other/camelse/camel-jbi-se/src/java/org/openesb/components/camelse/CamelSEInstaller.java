/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 *
 * Copyright 2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
/*
  * CamelSEInstaller.java
  */
package org.openesb.components.camelse;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.management.MBeanNames;
import javax.management.NotCompliantMBeanException;
import javax.management.ObjectName;
import javax.management.StandardMBean;
import org.openesb.components.camelse.CamelSEConfigMBean.CamelSEConfigMBeanImpl;
import org.openesb.components.camelse.common.ComponentInstaller;
import javax.jbi.JBIException;
import javax.jbi.component.InstallationContext;

/**
 * This class extends the ComponentInstaller that implements the
 * javax.jbi.component.Bootstrap for a component installation contract.
 *
 * If the default implemenation of the javax.jbi.component.Bootstrap 
 * in com.sun.jbi.sample.component.common.ComponentInstaller is not sufficient
 * for this components install, uninstall and upgrade requirements, override the
 * default implementation here to add component specific installation and upgrade
 * tasks such as initializing configuration mbean and creating resources specific
 * to component or upgrade workspace and service units during upgrading the component.
 *
 * @see javax.jbi.Bootstrap
 * @see com.sun.jbi.sample.component.common.ComponentInstaller
 * @author chikkala
 */
public class CamelSEInstaller extends ComponentInstaller {
    
    /** Constructor to create the MyEngineInstaller. */
    public CamelSEInstaller() {
    }

    @Override
    protected void doInit() throws JBIException {
        super.doInit();
////        InstallationContext ctx = this.getInstallationContext();
////        List classPaths = ctx.getClassPathElements();
////        System.out.println("***** BEGIN CAMEL SE CLASS PATH ELEMENTS ******");
////        for ( Object path : classPaths ) {
////            System.out.println("CAMEL SE:ClassPathEl: " + path);
////        }
////        System.out.println("***** END CAMEL SE CLASS PATH ELEMENTS ******");
    }

    @Override
    protected StandardMBean createExtensionMBean() {
        try {
            InstallationContext ctx = this.getInstallationContext();
            CamelSEConfigMBeanImpl impl = new CamelSEConfigMBeanImpl(ctx.getInstallRoot());
            StandardMBean configBean = new StandardMBean(impl, CamelSEConfigMBean.class);
            return configBean;
        } catch (NotCompliantMBeanException ex) {
            Logger.getLogger(CamelSEInstaller.class.getName()).log(Level.SEVERE, null, ex);
            return null;
        }        
    }

    @Override
    protected ObjectName createExtensionMBeanName() {
        MBeanNames mbeanNames = this.getInstallationContext().getContext().getMBeanNames();
        return mbeanNames.createCustomComponentMBeanName(mbeanNames.COMPONENT_LIFE_CYCLE_EXTENSION);
    }
    
}
