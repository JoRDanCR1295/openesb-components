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
package com.sun.jbi.engine.bpel.core.bpel.engine;

import com.sun.bpel.model.PartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink.InternalEPR;
import javax.jbi.servicedesc.ServiceEndpoint;
import org.w3c.dom.DocumentFragment;

/**
 *
 * @author Vitaly Bychkov
 */
public interface EPReferenceComposer {

    String SERVICE_REF_NS = "http://docs.oasis-open.org/wsbpel/2.0/serviceref";
    String WS_ADDRESSING_NS = "http://schemas.xmlsoap.org/ws/2004/08/addressing";
    String INTERNAL_WS_ADDRESSING_NS = "http://java.sun.com/jbi/end-point-reference";
    String SERVICE_NAME = "ServiceName";
    String SERVICE_REF = "service-ref";
    String ENDPOINT_REFERENCE = "EndpointReference";
    String ENDPOINT_REFERENCE_TYPE = "EndpointReferenceType";
    String PORT_NAME = "PortName";
    String PORT_TYPE = "PortType";
    String ADDRESS = "Address";

    /**
     * resolves the correspondent ServiceEndpoint. In case both External and
     * Internal Endpoint parameters are available from DocumentFragment the internal one is preferable
     * @param docFrag Document Fragment. Usually ws-addressing EndpointReference or service-ref.
     * @return ServiceEndpoint - Service Endpoint
     */
    ServiceEndpoint resolveEPR(DocumentFragment docFrag);

    /**
     * resolves the correspondent ServiceEndpoint. 
     * @param docFrag Document Fragment. Usually ws-addressing EndpointReference or service-ref.
     * @param isExternal which type of endpoints is preferable
     * @return ServiceEndpoint - ServiceEndpoint
     */
    ServiceEndpoint resolveEPR(DocumentFragment docFrag, boolean isExternal);

    /**
     * resolves the correspondent internal Service Endpoint
     * @param pl - PartnerLink
     * @param internalEPR - internal endpoint reference
     * @return ServiceEndpoint - Service Endpoint
     */
    public ServiceEndpoint resolveEPR(PartnerLink pl, InternalEPR iepr);

    /**
     * returns the document fragment with external and internal endpoints data
     * @param plScope PartnerLink Scope
     * @param partnerLink PartnerLink
     * @param isMyRole the PartnerLink role
     * @return DocumentFragment with external and internal endpoints data
     */
    DocumentFragment getEndpointReference(PartnerLinkScope plScope, PartnerLink partnerLink, boolean isMyRole);

    /**
     * returns the document fragment with external endpoint data for the myRole PartnerLink endpoint
     * @param partnerLink PartnerLink
     * @return DocumentFragment with external endpoint data
     */
    DocumentFragment getExternalEndpointReference(PartnerLink partnerLink);

    /**
     * returns the document fragment with external and internal endpoints data
     * @param extEP Extended Endpoint Entity
     * @return DocumentFragment with external and internal endpoints data
     */
    DocumentFragment getEndpointReference(ExtEndpointEntity extEP);

    /**
     * returns Extended Endpoint Entity
     * @param docFrag DocumentFragment
     * @return ExtEdnpointEntity Extended Endpoint Entity
     */
    ExtEndpointEntity getEndpointEntity(DocumentFragment docFrag);

}
