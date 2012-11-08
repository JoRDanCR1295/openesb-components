/* *************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/

package com.sun.jbi.dcombc.mbeans;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;

/**
 * Installer Extension MBean, allow configuration to be changed before installation
 * 
 * @author Chandrakanth Belde
 */
public class InstallerExt implements InstallerExtMBean {
	/**
	 *
	 */
    private static final Messages mMessages = Messages.getMessages(InstallerExt.class);

    private static final Logger mLogger = Messages.getLogger(InstallerExt.class);

    private String mThreads;
	
	/**
     * Creates a new instance of InstallerExt
     */
    public InstallerExt() {
    }

    /**
     * Gets the number of threads
     *
	 * @return String Number of threads
     */
    public String getThreads() {
        mLogger.log(Level.INFO, "InstallerExt.GETTHREADS_CALLED", new Object[] { mThreads });

        return mThreads;
    }

    /**
     * Sets the number of threads
     *
	 * @param val String Number of threads
     */
    public void setThreads(String val) {
        mLogger.log(Level.INFO, "InstallerExt.SETTHREADS_CALLED", new Object[] { val });

        mThreads = val;
    }
}
