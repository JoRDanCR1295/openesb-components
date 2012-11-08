/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)PojoSEBootstrap.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.pojose.jbi;

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.qos.config.ConfigPersistence;
import com.sun.jbi.common.util.MBeanHelper;
import com.sun.jbi.common.util.Util;
import javax.jbi.JBIException;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.component.Bootstrap;
import javax.jbi.component.InstallationContext;
import javax.jbi.management.DeploymentException;
import javax.jbi.management.MBeanNames;
import javax.management.ObjectName;

/**
 * Bootstrap implementation for POJOService Engine.
 * 
 * @author Girish Patil
 */
public class PojoSEBootstrap implements Bootstrap {
    private Logger logger;
    private MBeanHelper mbnHelper;
    private String wspRoot;
    private ComponentConfig ccfg;
    private Object mbean;

    public PojoSEBootstrap() {
    }

    public void init(InstallationContext ic) throws JBIException {
        logger = Util.getLogger(ic.getContext(),
                this.getClass().getName());
        if (logger.isLoggable(Level.FINE)) {
            String msg = I18n.lf("POJOSE-1513: Bootstrap init called.");//NOI18N
            logger.finest(msg);
        }

        try {
            mbnHelper = new MBeanHelper(ic.getContext());
            wspRoot = ic.getContext().getWorkspaceRoot();
            // parse component descriptor
            ccfg = ComponentConfig.parse(ic.getInstallRoot());
            // pass config to avoid duplicate initialization from descriptor by AcceptConfig
            mbean = new PojoSEConfiguration(ic.getContext(), ccfg);

            // register MBean
            mbnHelper.registerMBean(MBeanNames.BOOTSTRAP_EXTENSION,
                    mbean,
                    true); // force unregister, in case prior attempt failed
        } catch (DeploymentException de) {
            throw error(de, "POJOSE-7507: Failed to parse component descriptor: {0}", de.getMessage());
        } catch (JBIException je) {
            throw error(je, "POJOSE-7508: Failed to initialize component bootstrap: {0}", je.getMessage());
        } catch (Exception e) {
            throw error(e, "POJOSE-7509: Failed to register component bootstrap: {0}", e.getMessage());
        }
    }

    public void onInstall() throws JBIException {
        ConfigPersistence.persistConfig(ccfg, this.wspRoot);
        ConfigPersistence.persistApplicationConfig(ccfg, wspRoot);
    }

    public void onUninstall() throws JBIException {
        // nop
    }

    public void cleanUp() throws JBIException {
        if (this.mbnHelper != null) {
            this.mbnHelper.unregisterMBean(MBeanNames.BOOTSTRAP_EXTENSION);
        }
    }

    public ObjectName getExtensionMBeanName() {
        return mbnHelper.getObjectName(MBeanNames.BOOTSTRAP_EXTENSION);
    }

    protected Logger log() {
        return ((logger == null) ? Logger.getLogger(this.getClass().getName())
                : logger);
    }

    protected JBIException error(Exception e, String msg, Object... params) {
        String err = I18n.loc(msg, params);
        if (e == null) {
            log().warning(err);
            return new JBIException(err);
        } else {
            log().log(Level.WARNING, err, e);
            return new JBIException(err, e);
        }
    }    
}
