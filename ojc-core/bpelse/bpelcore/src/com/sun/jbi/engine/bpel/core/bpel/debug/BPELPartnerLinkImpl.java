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
 * @(#)BPELPartnerLinkImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.debug;

import com.sun.bpel.model.PartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.BPELPartnerLink;

/**
 * Implementation of {@link BPELPartnerLink} which allows to query partner link
 * values in the target BPEL engine.
 * 
 * @author Sun Microsystems
 */
public class BPELPartnerLinkImpl implements BPELPartnerLink {
    
    private String name;
    
    private String serializedValue;
    
    public BPELPartnerLinkImpl(
            final PartnerLink pLink,
            final RuntimePartnerLink rLink) {
        name = pLink.getName();
        
        serializedValue = rLink.getSerializedValue();
    }

    public String getName() {
        return name;
    }
    
    public String getSerializedValue() {
        return serializedValue;
    }
}
