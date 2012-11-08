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
 * @(#)BootstrapFilter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.lifecycle;

import javax.jbi.JBIException;
import javax.jbi.component.InstallationContext;

/**
 * Defines interface for JBI Bootstrap filters, derived from 
 * the Core J2EE Intercepting Filter pattern.
 * <p>
 * The absence of a mutator for the managing filter chain implies
 * filters should be defined with a constructor that accepts a
 * filter chain.
 * 
 * @author Kevan Simpson
 */
public interface BootstrapFilter {
    /** 
     * Fetches reference to bootstrap filter chain of which this filter
     * is a member.
     * @return the filter chain.
     */
    public BootstrapFilterChain getChain();
    
    /** @see javax.jbi.component.Bootstrap#cleanUp() */
    public void cleanUp() throws JBIException;
//
//    /** @see javax.jbi.component.Bootstrap#getExtensionMBeanName() */
//    public ObjectName getExtensionMBeanName();

    /** @see javax.jbi.component.Bootstrap#init(javax.jbi.component.InstallationContext) */
    public void init(InstallationContext installContext) throws JBIException;

    /** @see javax.jbi.component.Bootstrap#onInstall() */
    public void onInstall() throws JBIException;
    
    /** @see javax.jbi.component.Bootstrap#onUninstall() */
    public void onUninstall() throws JBIException;
}
