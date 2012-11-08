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
 * @(#)InstallerExt.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc.bootstrap;

import java.util.logging.Logger;
import com.sun.jbi.internationalization.Messages;
import java.util.logging.Level;
import com.sun.jbi.common.qos.config.ComponentConfig;


/**
 * Installer Extension MBean, allow configuration to be changed before installation
 */
public class InstallerExt implements InstallerExtMBean {
    private static final Messages mMessages = Messages.getMessages(InstallerExt.class);

    // Attribute names - Should match RuntimeConfiguration 
    public static final String CONFIG_THREADS = "Threads";

    // Default values in the absence of configuration settings
    private static final String DEFAULT_THREADS = "10";

	public static final String CONFIG_CLUSTER_DATABASE_JNDINAME = "ClusterDatabaseJNDIName";

	private static final String DEFAULT_CLUSTER_DATABASE_JNDINAME = "jdbc/__defaultDS";
    private String mThreads;
    private Logger mLogger;
    private String mClusterJNDIName;

    private ComponentConfig mConfigProps;

    /** Creates a new instance of InstallerExt */
    public InstallerExt() {
        mLogger = Messages.getLogger(InstallerExt.class);
    }

    /**
     *
     * @return 
     */
    //@Override
    public String getThreads() {
        mLogger.log(Level.INFO,mMessages.getString("DBBC_R00224.Get_threads"), mThreads);
        return mThreads;
    }

    /**
     *
     * @param val 
     */
    //@Override
    public void setThreads(final String val) {
        mLogger.log(Level.INFO,mMessages.getString("DBBC_R00225.Set_threads"), val);
        mThreads = val;
    }

    /* 
     * @return
     */
    public String getClusterDatabaseJNDIName() {
        return mClusterJNDIName;
    }

    /* 
     * @param val
     */
    public void setClusterDatabaseJNDIName(String val) {
        if (val == null) {
            val = DEFAULT_CLUSTER_DATABASE_JNDINAME;
        }
        mClusterJNDIName = val;
    }
    public void setInitialConfigurations(ComponentConfig props) {
        this.mConfigProps = props;
        
        mThreads = props.getProperty(CONFIG_THREADS).getValue();
        mClusterJNDIName = props.getProperty(CONFIG_CLUSTER_DATABASE_JNDINAME).getValue();
    }

    public ComponentConfig getInstallationConfigurationProperties() {
        mConfigProps.getProperty(CONFIG_THREADS).setValue(mThreads);
        mConfigProps.getProperty(CONFIG_CLUSTER_DATABASE_JNDINAME).setValue(mClusterJNDIName);
        
        return mConfigProps;
    }
}
