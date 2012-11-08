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
 * @(#)EndpointStatus.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.eManager.provider;

import java.util.Map;
/**
 * Status reporting and query for an individual Endpoint
 * @author Sun Microsystems
 */
public interface EndpointStatus {
    long getSentRequests();
    long getSentReplies();
    long getSentErrors();
    long getSentDones();
    long getReceivedRequests();
    long getReceivedReplies();
    long getReceivedErrors();
    long getReceivedDones();
    String getWSDLDefinition();
    String getWSDLImportedResource(String targetNamespace);
    void incrementSentRequests();
    void incrementSentReplies();
    void incrementSentErrors();
    void incrementSentDones();
    void incrementReceivedRequests();
    void incrementReceivedReplies();
    void incrementReceivedErrors();
    void incrementReceivedDones();
    void setWSDLDefinition(String val);
    void setWSDLImportedResources(Map resourceMap);
}
