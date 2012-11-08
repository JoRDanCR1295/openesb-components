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
package com.sun.bpel.model.meta.impl;

import com.sun.bpel.model.BPELProcessOrScope;
import com.sun.bpel.model.impl.PartnerLinkImpl;
import com.sun.bpel.model.meta.RPartnerLink;
import com.sun.bpel.xml.common.model.XMLDocument;

/**
 * @author Sun Inc
 * Apr 30, 2007
 */
public class RPartnerLinkImpl extends PartnerLinkImpl implements RPartnerLink {
    private long mUniqueId = Long.MIN_VALUE;
    private BPELProcessOrScope mAssociatedScope;
    
    /**
     * @param d
     * @param uniqueID
     */
    public RPartnerLinkImpl(XMLDocument d, long uniqueID) {
        super(d);
        mUniqueId = uniqueID;
    }
    
    /** @see com.sun.bpel.model.meta.Common#getUniqueId()
     */
    public long getUniqueId() {
        return mUniqueId;
    }
    /** @see com.sun.bpel.model.PartnerLink#getAssociatedScope()
     */
    public BPELProcessOrScope getAssociatedScope() {
        return mAssociatedScope;
    }

    /** @see com.sun.bpel.model.meta.RPartnerLink#setAssociatedScope(com.sun.bpel.model.BPELProcessOrScope)
     */
    public void setAssociatedScope(BPELProcessOrScope scope) {
        mAssociatedScope = scope;
    }
    
}
