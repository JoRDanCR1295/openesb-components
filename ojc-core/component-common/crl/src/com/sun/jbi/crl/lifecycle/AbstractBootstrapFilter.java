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
 * @(#)AbstractBootstrapFilter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.lifecycle;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.InstallationContext;
import javax.management.ObjectName;

import com.sun.jbi.crl.util.I18n;
import com.sun.jbi.crl.util.LogUtil;

/**
 * Provides a base implementation of a {@link BootstrapFilter}, 
 * which provides a unique {@link Logger} via the component's
 * {@link ComponentContext}, if available. 
 * 
 * @author Kevan Simpson
 */
public abstract class AbstractBootstrapFilter implements BootstrapFilter {
    private BootstrapFilterChain mChain;
    private Logger mLogger;
    
    public AbstractBootstrapFilter(BootstrapFilterChain chain) {
        mChain = chain;
    }
    
    public BootstrapFilterChain getChain() {
        return mChain;
    }
    
    /** @see com.sun.jbi.crl.lifecycle.BootstrapFilter#cleanUp() */
    public void cleanUp() throws JBIException {
    }

    /** @see com.sun.jbi.crl.lifecycle.BootstrapFilter#getExtensionMBeanName() */
    protected ObjectName getExtensionMBeanName() {
        // TODO is this method necessary? it's only a shortcut
        return getChain().getExtensionMBeanName();
    }

    /** 
     * This implementation initializes a {@link ComponentContext}-aware {@link Logger},
     * accessible via {@link #log()}, for this {@link BootstrapFilter} implementation.
     * 
     * @see com.sun.jbi.crl.lifecycle.BootstrapFilter#init(javax.jbi.component.InstallationContext)
     * @see #log()
     */
    public void init(InstallationContext installContext) throws JBIException {
    	mLogger = LogUtil.getLogger(installContext.getContext(), this.getClass().getName());
    }

    /** @see com.sun.jbi.crl.lifecycle.BootstrapFilter#onInstall() */
    public void onInstall() throws JBIException {
    }

    /** @see com.sun.jbi.crl.lifecycle.BootstrapFilter#onUninstall() */
    public void onUninstall() throws JBIException {
    }
    
    /** @see java.lang.Object#toString() */
	public String toString() {
		return this.getClass().getName();
	}

	/** Fetches a non-null {@link Logger} for this bootstrap filter. */ 
    protected Logger log() {
    	return ((mLogger == null) ? Logger.getLogger(this.getClass().getName()) 
    							  : mLogger);
    }
    protected JBIException error(Exception e, String msg, Object... params) {
        String err = I18n.loc(msg, params);
        if (e == null) {
            log().warning(err);
            return new JBIException(err);
        }
        else {
            log().log(Level.WARNING, err, e);
            return new JBIException(err, e);
        }
    }
}
