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
 * @(#)DisplayStatistics.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.generic;

import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.common.Util;

/**
 *
 * @author ylee
 */
public class DisplayStatistics {
    
    private String name;
    private String value;
    
    //
    private String endpoint = "Unknown";
    private String endpointShort;
    private String namespace;
    private String receivedRequests = "-1";
    private String receivedReplies  = "-1";
    private String receivedErrors   = "-1";
    private String receivedDones    = "-1";
    private String sentRequests     = "-1";
    private String sentReplies      = "-1";
    private String sentErrors       = "-1";
    private String sentDones        = "-1";
    
    public DisplayStatistics() {
    }
    
    /** Creates a new instance of DisplayStatistics */
    public DisplayStatistics(String name, String value) {
        this.name = name;
        this.value = value;
    }
    
    public DisplayStatistics(String name, long value) {
        this.name = name;
        this.value = value+"";
    }
    
    public DisplayStatistics(String endpoint, String receivedRequests, String receivedReplies, String receivedErrors, 
            String receivedDones, String sentRequests, String sentReplies, String sentErrors, String sentDones) {
        this.endpoint = endpoint;
        this.receivedRequests = receivedRequests;
        this.receivedReplies = receivedReplies;
        this.receivedErrors = receivedErrors;
        this.receivedDones = receivedDones;
        this.sentRequests = sentRequests;
        this.sentReplies = sentReplies;
        this.sentErrors = sentErrors;
        this.sentDones = sentDones;
        this.namespace = Util.getNamespace(endpoint,GenericConstants.COMMA_SEPARATOR);
        this.endpointShort = Util.trimRight(endpoint,GenericConstants.COMMA_SEPARATOR);  //  //$NON-NLS-1$
        this.endpointShort = Util.trimLeft(endpointShort,GenericConstants.COMMA_SEPARATOR);
        
    }
    
    
    public DisplayStatistics(String endpoint, long receivedRequests, long receivedReplies, long receivedErrors, 
            long receivedDones, long sentRequests, long sentReplies, long sentErrors, long sentDones) {
        this(endpoint,receivedRequests+"",receivedReplies+"",receivedErrors+"",receivedDones+"",sentRequests+"",sentReplies+"",
                sentErrors+"",sentDones+"");
    }
    
    
    public String getName() {
        return name;
    }
    
    public String getValue() {
        return value;
    }
    
    public String getEndpoint() {
        return endpoint;
    }

    public String getEndpointShort() {
        return endpointShort;
    }
    
    public void setEndpoint(String value) {
        this.endpoint = value;
    }
    
    public String getReceivedRequests() {
        return receivedRequests;
    }

    public void setReceivedRequests(String value) {
        this.receivedRequests = value;
    }
    
    public String getReceivedReplies() {
        return receivedReplies;
    }

    public void setReceivedReplies(String value) {
        this.receivedReplies = value;
    }    
    
       public String getReceivedErrors() {
        return receivedErrors;
    }

    public void setReceivedErrors(String value) {
        this.receivedErrors = value;
    }    
    
    public String getReceivedDones() {
        return receivedDones;
    }

    public void setReceivedDones(String value) {
        this.receivedDones = value;
    }        

    public String getSentRequests() {
        return sentRequests;
    }

    public void setSentRequests(String value) {
        this.sentRequests = value;
    }     
    

    public String getSentReplies() {
        return sentReplies;
    }

    public void setSentReplies(String value) {
        this.sentReplies = value;
    }         
    
    public String getSentErrors() {
        return sentErrors;
    }

    public void setSentErrors(String value) {
        this.sentErrors = value;
    }      

    public String getSentDones() {
        return sentDones;
    }

    public void setSentDones(String value) {
        this.sentDones = value;
    }      
        

    public void setNamespace(String namespace) {
        this.namespace = namespace;
    }
    
    public String getNamespace() {
        return namespace;
    }
    
}
