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

import javax.xml.namespace.QName;

import com.sun.bpel.model.PartnerLink;
import org.w3c.dom.DocumentFragment;

/**
 * @author Sun Inc
 * Apr 30, 2007
 */
public interface RuntimePartnerLink {

    /**
     * check if the PartnerLink is inserted
     *
     * @return boolean: if the PartnerLink is inserted, returns true;
     */
    boolean isInserted();

    /**
     * marks the PartnerLink as inserted
     */
    void markInserted();

    PartnerLink getStaticModel();

    Object getServiceRef();

    void setServiceRef(Object obj);

    void setSerializedValue(Object value);

    String getSerializedValue();

    /** Scope GUID that this runtime partner link belongs to
     * @return
     */
    String getScopeGuid();

    /** InternalEPR is immutable
     * @author Sun Inc
     * Nov 1, 2007
     */
    public interface InternalEPR {
        String MY_ROLE_SUFFIX = "_myRole";
        String PARTNER_ROLE_SUFFIX = "_partnerRole";

        QName getService();

        String getEndPoint();

        boolean isMyRole();

        DocumentFragment getAsReference();
    }
}
