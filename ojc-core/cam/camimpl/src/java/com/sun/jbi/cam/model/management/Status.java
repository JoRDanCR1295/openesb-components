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
 * @(#)Status.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.model.management;

/**
 *
 * @author ylee
 */
public class Status {
    
    private long totalRequests;
    private long totalReplies;
    private long totalErrors;
    private long totalDones;
    private long sentRequests;
    private long sentReplies;
    private long sentErrors;
    private long sentDones;
    private long receivedRequests;
    private long receivedReplies;
    private long receivedErrors;
    private long receivedDones;
    private long totalSentRequests;
    private long totalSentReplies;
    private long totalSentErrors;
    private long totalSentDones;
    private long totalReceivedRequests;
    private long totalReceivedReplies;
    private long totalReceivedErrors;
    private long totalReceivedDones;
        
    
    
    /** Creates a new instance of Status */
    public Status() {
    }
    
    
    //  Retrieves the total number of requests handled
    public long getTotalRequests() {
        //long result = 0L;
        //result = this.invoke(objectName, "getTotalRequests", null); //$NON-NLS-1$
        //return result;\
        return totalRequests;
    }

    //  Retrieves the total number of replies sent out
    public long getTotalReplies() {
        //long result = 0L;
        //result = this.invoke(objectName, "getTotalReplies", null); //$NON-NLS-1$
        //return result;
        return totalReplies;
    }

    //  Retrieves the total number of messages with faults
    public long getTotalErrors() {
        //long result = 0L;
        //result = this.invoke(objectName, "getTotalErrors", null); //$NON-NLS-1$
        //return result;
        return totalErrors;
    }

    //  Retrieves the total number of messages successfully processed.
    public long getTotalDone() {
        //long result = 0L;
        //result = this.invoke(objectName, "getTotalDone", null); //$NON-NLS-1$
        //return result;
        return totalDones;
    }

    //  Retrieves the total number of message requests sent.
    public long getTotalSentRequests() {
        //long result = 0L;
        //result = this.invoke(objectName, "getTotalSentRequests", null); //$NON-NLS-1$
        //return result;
        return totalSentRequests;
    }

    //  Retrieves the total number of message replies sent.
    public long getTotalSentReplies() {
        //long result = 0L;
        //result = this.invoke(objectName, "getTotalSentReplies", null); //$NON-NLS-1$
        //return result;
        return totalSentReplies;
    }

    //  Retrieves the total number of errors sent.
    public long getTotalSentErrors() {
        //long result = 0L;
        //result = this.invoke(objectName, "getTotalSentErrors", null); //$NON-NLS-1$
        //return result;
        return totalSentErrors;
    }

    //  Retrieves the total number of messages sent that were successfully processed.
    public long getTotalSentDones() {
        //long result = 0L;
        //result = this.invoke(objectName, "getTotalSentDones", null); //$NON-NLS-1$
        //return result;
        return totalSentDones;
    }

    //  Retrieves the total number of requests received that were successfully processed.
    public long getTotalReceivedRequests() {
        //long result = 0L;
        //result = this.invoke(objectName, "getTotalReceivedRequests", null); //$NON-NLS-1$
        //return result;
        return totalReceivedRequests;
    }

    //  Retrieves the total number of received replies.
    public long getTotalReceivedReplies() {
        //long result = 0L;
        //result = this.invoke(objectName, "getTotalReceivedReplies", null); //$NON-NLS-1$
        //return result;
        return totalReceivedReplies;
    }

    //  Retrieves the total number of received errors.
    public long getTotalReceivedErrors() {
        //long result = 0L;
        //result = this.invoke(objectName, "getTotalReceivedErrors", null); //$NON-NLS-1$
        //return result;
        return totalReceivedErrors;
    }

    //  Retrieves the total number of receives successfully processed.
    public long getTotalReceivedDones() {
        //long result = 0L;
        //result = this.invoke(objectName, "getTotalReceivedDones", null); //$NON-NLS-1$
        //return result;
        return totalReceivedDones;
    }

    //  Retrieves the number of sent requests for the specified endpoint.
    public long getSentRequests(String endpoint) {
        //long result = 0L;
        //Object[] params = new Object[1];
        //params[0] = endpoint;
        //result = this.invokeWithParameters(objectName,
        //                     "getSentRequests", //$NON-NLS-1$
        //                     params);
        //return result;
        return sentRequests;
    }

    //  Retrieves the number of sent replies for the specified endpoint.
    public long getSentReplies(String endpoint) {
        //long result = 0L;
        //Object[] params = new Object[1];
        //params[0] = endpoint;
        //result = this.invokeWithParameters(objectName,
        //                     "getSentReplies", //$NON-NLS-1$
        //                     params);
        //return result;
        return sentReplies;
    }

    //  Retrieves the number of sent errors for the specified endpoint.
    public long getSentErrors(String endpoint) {
        //long result = 0L;
        //Object[] params = new Object[1];
        //params[0] = endpoint;
        //result = this.invokeWithParameters(objectName,
        //                     "getSentErrors", //$NON-NLS-1$
        //                     params);
        //return result;
        return sentErrors;
    }

    //  Retrieves the number of sent successfully processed for the specified endpoint.
    public long getSentDones(String endpoint) {
        //long result = 0L;
        //Object[] params = new Object[1];
        //params[0] = endpoint;
        //result = this.invokeWithParameters(objectName,
        //                    "getSentDones", //$NON-NLS-1$
        //                     params);
        //return result;
        return sentDones;
    }

    //  Retrieves the number of received requests for the specified endpoint.
    public long getReceivedRequests(String endpoint) {
        //long result = 0L;
        //Object[] params = new Object[1];
        //params[0] = endpoint;
        //result = this.invokeWithParameters(objectName,
        //                     "getReceivedRequests", //$NON-NLS-1$
        //                     params);
        //return result;
        return receivedRequests;
    }
    
    //  Retrieves the number of received replies for the specified endpoint.
    public long getReceivedReplies(String endpoint) {
        //long result = 0L;
        //Object[] params = new Object[1];
        //params[0] = endpoint;
        //result = this.invokeWithParameters(objectName,
        //                     "getReceivedReplies", //$NON-NLS-1$
        //                     params);
        //return result;
        return receivedReplies;
    }

    //  Retrieves the number of received errors for the specified endpoint.
    public long getReceivedErrors(String endpoint) {
        //long result = 0L;
        //Object[] params = new Object[1];
        //params[0] = endpoint;
        //result = this.invokeWithParameters(objectName,
        //                     "getReceivedErrors", //$NON-NLS-1$
        //                     params);
        //return result;
        return receivedErrors;
    }

    //  Retrieves the number of received successfully processed for the specified endpoint.
    public long getReceivedDones(String endpoint) {
        //long result = 0L;
        //Object[] params = new Object[1];
        //params[0] = endpoint;
        //result = this.invokeWithParameters(objectName,
        //                     "getReceivedDones", //$NON-NLS-1$
        //                     params);
        //return result;
        return receivedDones;
    }
    
    
}
