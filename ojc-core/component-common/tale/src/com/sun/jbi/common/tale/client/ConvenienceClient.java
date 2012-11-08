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
 * @(#)ConvenienceClient.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.client;

import com.sun.jbi.common.tale.core.domain.TaleRequest;
import com.sun.jbi.common.tale.core.domain.service.TaleService;

/**
 * Base interface for ALE convenience apis around {@link TaleClient}.
 * @author Kevan Simpson
 */
public interface ConvenienceClient {
    /**
     * Returns the {@link TaleService} supporting this client.
     * @return the <code>TaleService</code> supporting this client.
     */
    public TaleService getALEService();
    
    /**
     * Returns the name of the {@link TaleRequest} template to use
     * when sending ALE messages.
     * @return the name of the <code>TaleRequest</code> template used by this client.
     */
    public String getRequestTemplateName();
    
    /**
     * Sets the name of the {@link TaleRequest} template to use
     * when sending ALE messages.
     * @param templateName the name of the <code>TaleRequest</code> template used by this client.
     */
    public void setRequestTemplateName(String templateName);
}
