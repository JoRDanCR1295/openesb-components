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
 * @(#)ProcessorFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.proc;

import javax.jbi.JBIException;

import com.sun.jbi.crl.mep.Callback;

/**
 * Lightweight factory for {@link Processor} instances.
 * 
 * @author Kevan Simpson
 */
public interface ProcessorFactory {
    // TODO there should be some ConfigurationException class, don't use JBIException
    
    /**
     * Fetches (or creates) a {@link Processor}, which may <b>NOT</b> be null.
     */
    public Processor getProcessor() throws JBIException;
    
    /**
     * Fetches (or creates) a {@link Callback}, which may be <code>null</code>.
     */
    public Callback getCallback() throws JBIException;
}
