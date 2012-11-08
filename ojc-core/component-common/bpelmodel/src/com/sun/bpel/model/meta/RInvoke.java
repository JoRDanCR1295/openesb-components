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
 * @(#)RInvoke.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.meta;

import java.util.List;

import com.sun.bpel.model.FaultHandlerScope;
import com.sun.bpel.model.Invoke;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.ServiceQuality;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public interface RInvoke extends Invoke, FaultHandlerScope {
    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    CorrelationDefnWrapper getResponseCorrelationDefnWrapper();

    /**
     * DOCUMENT ME!
     *
     * @param corrDefn DOCUMENT ME!
     */
    void setResponseCorrelationDefnWrapper(CorrelationDefnWrapper corrDefn);

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    CorrelationDefnWrapper getRequestCorrelationDefnWrapper();

    /**
     * DOCUMENT ME!
     *
     * @param corrDefn DOCUMENT ME!
     */
    void setRequestCorrelationDefnWrapper(CorrelationDefnWrapper corrDefn);
    
    /**
     * Correlation defns with pattern "request-response" defined on the
     * request message. 
     * @return
     */
    CorrelationDefnWrapper getReqRespCorrelationDefnWrapperForRequest();
    
    /**
     * Correlation defns with pattern "request-response" defined on the
     * request message. 
     * @param corrDefn
     */
    void setReqRespCorrelationDefnWrapperForRequest(CorrelationDefnWrapper corrDefn);

    /**
     * Correlation defns with pattern "request-response" defined on the
     * response message. 
     * @return
     */
    CorrelationDefnWrapper getReqRespCorrelationDefnWrapperForResponse();
    
    /**
     * Correlation defns with pattern "request-response" defined on the
     * response message. 
     * @param corrDefn
     */
    void setReqRespCorrelationDefnWrapperForResponse(CorrelationDefnWrapper corrDefn);
    
    /**
     * The ServiceQualities that are associated with this invoke activity (endpoint).
     * @param endpointInfo EndpointInfo
     * @param qosList java.util.List<ServiceQuality>
     */
    void setServiceQualityParams(EndpointInfo endpointInfo, List<ServiceQuality> qosList);

    /**
     * Returns the EndpointInfo accociated with this invoke activity.
     * @return <com.sun.jbi.common.descriptor.EndpointInfo>
     */
    EndpointInfo getServiceQualityEndpointInfo();
    
    
    /**
     * Return the particular ServiceQuality implementation.
     * @param <T> Class that extends ServiceQuality.
     * @param qosType ServiceQuality implementation that is needed.
     * @return
     */
    <T extends ServiceQuality> T getServiceQuality(Class<T> qosType);
    
}
