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
package com.sun.jbi.engine.bpel.core.bpel.model.runtime;

import com.sun.bpel.model.PartnerLink;
import java.util.Map;

/**
 * @author Sun Inc
 * Apr 30, 2007
 */
public interface PartnerLinkScope {
    /**
     * @param pLink
     * @return
     */
    RuntimePartnerLink getRuntimePartnerLink(PartnerLink pLink);
    
    /**
     * @param pLink
     * @param runtimePLink
     */
    void setRuntimePartnerLink(PartnerLink pLink, RuntimePartnerLink runtimePLink);
    
    /**
     * 
     * @return
     */
    Map getRuntimePartnerLinks();
    
    /**
     * Helper method to create a partnerlink. Checks to see if the partnerlink belongs to this
     * scope context. If the partnerlink is not defined in this scope context delegates it 
     * to the parent context. Hence it traverses up to the parent process context for 
     * partnerlinks defined in the process instance but used in embeded or nested scopes.
     * @param partnerlink
     * @return
     */
    RuntimePartnerLink createRuntimePartnerLink(PartnerLink partnerlink); 
}
