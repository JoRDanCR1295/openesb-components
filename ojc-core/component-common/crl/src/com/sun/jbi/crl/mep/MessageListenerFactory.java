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
 * @(#)MessageListenerFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep;

import javax.jbi.JBIException;

/**
 * Factory to create instances of {@link MessageListener}.
 * 
 * @author Kevan Simpson
 */
public interface MessageListenerFactory {
    /**
     * Returns an instance of <code>MessageListener</code>.
     * @return an instance of <code>MessageListener</code>.
     * @throws JBIException if an error occurs acquiring listener.
     */
    public MessageListener getInstance() throws JBIException;

    /**
     * Returns <code>true</code> if the {@link MessageListener} 
     * implementation provided by this factory is thread-safe.
     * @return <code>true</code> if listener is thread-safe, 
     *         else <code>false</code>.
     */
    public boolean isThreadSafe();
}
