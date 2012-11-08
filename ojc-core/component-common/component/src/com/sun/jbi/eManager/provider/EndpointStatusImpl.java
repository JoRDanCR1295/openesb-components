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
 * @(#)EndpointStatusImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.eManager.provider;

import java.util.HashMap;
import java.util.Map;
/**
 * Status reporting and query for an individEndpointStatusImpluthor aegloff
 */
public final class EndpointStatusImpl implements EndpointStatus {

    private long sentRequests;
    private long sentReplies;
    private long sentErrors;
    private long sentDones;
    private long receivedRequests;
    private long receivedReplies;
    private long receivedErrors;
    private long receivedDones;
    private String wsdlDef;
    private Map resources = new HashMap();

    public long getSentRequests() {
        return sentRequests;
    }
    public long getSentReplies() {
        return sentReplies;
    }
    public long getSentErrors() {
        return sentErrors;
    }
    public long getSentDones() {
        return sentDones;
    }
    public long getReceivedRequests() {
        return receivedRequests;
    }
    public long getReceivedReplies() {
        return receivedReplies;
    }
    public long getReceivedErrors() {
        return receivedErrors;
    }
    public long getReceivedDones() {
        return receivedDones;
    }
    public void incrementSentRequests() {
        ++sentRequests;
    }
    public void incrementSentReplies() {
        ++sentReplies;
    }
    public void incrementSentErrors() {
        ++sentErrors;
    }
    public void incrementSentDones() {
        ++sentDones;
    }
    public void incrementReceivedRequests() {
        ++receivedRequests;
    }
    public void incrementReceivedReplies() {
        ++receivedReplies;
    }
    public void incrementReceivedErrors() {
        ++receivedErrors;
    }
    public void incrementReceivedDones() {
        ++receivedDones;
    }
    public String getWSDLDefinition() {
        return wsdlDef;
    }
    
    public void setWSDLDefinition(String val) {
        wsdlDef = val;
    }
    
    public void setWSDLImportedResources(Map val) {
        resources = val;
    }
    
    public String getWSDLImportedResource(String namespace) {
        return (String)resources.get(namespace);
    }
}
