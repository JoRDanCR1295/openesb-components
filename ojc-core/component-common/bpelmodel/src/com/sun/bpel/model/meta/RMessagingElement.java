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
 * @(#)RMessagingElement.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.meta;

import com.sun.bpel.model.Correlations;
import com.sun.bpel.model.PartnerLink;

import javax.wsdl.Operation;
import javax.xml.namespace.QName;


/**
 * runtime messaging element
 *
 * @author Sun Microsystems
 */
public interface RMessagingElement {
    /**
     * gets correlations
     *
     * @return Correlations correlations
     */
    Correlations getCorrelations();

    /**
     * gets wsdlmodel operation
     *
     * @return Operation wsdlmodel operation
     */
    Operation getWSDLOperation();

    /**
     * gets runtime PartnerLink
     *
     * @return PartnerLink wsdlmodel PartnerLink
     */
    PartnerLink getRPartner();

    /**
     * gets Runtime PortType
     *
     * @return PortType QName
     */
    QName getRPortType();
}
