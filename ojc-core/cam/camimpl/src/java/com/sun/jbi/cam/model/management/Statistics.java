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
 * @(#)Statistics.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 *
 */
package com.sun.jbi.cam.model.management;

import com.sun.jbi.cam.common.EndpointURL;
import com.sun.jbi.cam.common.EqualsUtil;
import com.sun.jbi.cam.common.resources.Messages;
import com.sun.jbi.cam.manager.framework.generic.DisplayStatistics;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * @author ylee
 * @author Graj
 * Each endpoint currently has 8 counters
 * sentDones, receivedDones, sentErrors, receivedErrors,
 * sentRequests, receivedRequests, sentReplies, receivedReplies
 */
public class Statistics implements Serializable {

    private String endpoint;
    private long sentRequests;
    private long sentReplies;
    private long sentErrors;
    private long sentDones;

    private long receivedRequests;
    private long receivedReplies;
    private long receivedErrors;
    private long receivedDones;

    /**
     *
     */
    public Statistics() {
    }

    /**
     * @param endpointurl
     * @param sentrequests
     * @param sentreplies
     * @param senterrors
     * @param sentdones
     * @param receivedrequests
     * @param receivedreplies
     * @param receivederrors
     * @param receiveddones
     */
    public Statistics(String endpoint,
                            long sentrequests, long sentreplies, long senterrors, long sentdones,
                            long receivedrequests, long receivedreplies, long receivederrors, long receiveddones) {
        super();
        
        this.endpoint = endpoint;
        this.sentRequests = sentrequests;
        this.sentReplies = sentreplies;
        this.sentErrors = senterrors;
        this.sentDones = sentdones;
        this.receivedRequests = receivedrequests;
        this.receivedReplies = receivedreplies;
        this.receivedErrors = receivederrors;
        this.receivedDones = receiveddones;
    }

    /**
     * @param sentrequests
     * @param sentreplies
     * @param senterrors
     * @param sentdones
     * @param receivedrequests
     * @param receivedreplies
     * @param receivederrors
     * @param receiveddones
     */
    public Statistics(long sentrequests, long sentreplies, long senterrors, long sentdones,
                            long receivedrequests, long receivedreplies, long receivederrors, long receiveddones) {
        super();
        this.sentRequests = sentrequests;
        this.sentReplies = sentreplies;
        this.sentErrors = senterrors;
        this.sentDones = sentdones;
        this.receivedRequests = receivedrequests;
        this.receivedReplies = receivedreplies;
        this.receivedErrors = receivederrors;
        this.receivedDones = receiveddones;
    }

    /**
     *
     * @param endpointurl
     * @param sentrequests
     * @param sentreplies
     * @param senterrors
     * @param sentdones
     * @param receivedrequests
     * @param receivedreplies
     * @param receivederrors
     * @param receiveddones
     */
    public void setValues(String endpoint,
            long sentrequests, long sentreplies, long senterrors, long sentdones,
            long receivedrequests, long receivedreplies, long receivederrors, long receiveddones) {
        this.endpoint = endpoint;
        this.sentRequests = sentrequests;
        this.sentReplies = sentreplies;
        this.sentErrors = senterrors;
        this.sentDones = sentdones;
        this.receivedRequests = receivedrequests;
        this.receivedReplies = receivedreplies;
        this.receivedErrors = receivederrors;
        this.receivedDones = receiveddones;
    }

    /**
     * @return Returns the endpointURL.
     */
    public String getEndpoint() {
        return this.endpoint;
    }

    public void add( long sentrequests, long sentreplies, long senterrors, long sentdones,
            long receivedrequests, long receivedreplies, long receivederrors, long receiveddones) {
        this.sentRequests += sentrequests;
        this.sentReplies += sentreplies;
        this.sentErrors += senterrors;
        this.sentDones += sentdones;
        this.receivedRequests += receivedrequests;
        this.receivedReplies += receivedreplies;
        this.receivedErrors += receivederrors;
        this.receivedDones += receiveddones;
    }


    /**
     * @param endpointURL The endpointURL to set.
     */
    public void setEndpoint(String endpoint) {
        this.endpoint = endpoint;
    }


    /**
     * @return Returns the receivedDones.
     */
    public long getReceivedDones() {
        return this.receivedDones;
    }


    /**
     * @param receivedDones The receivedDones to set.
     */
    public void setReceivedDones(long receivedDones) {
        this.receivedDones = receivedDones;
    }


    /**
     * @return Returns the receivedErrors.
     */
    public long getReceivedErrors() {
        return this.receivedErrors;
    }


    /**
     * @param receivedErrors The receivedErrors to set.
     */
    public void setReceivedErrors(long receivedErrors) {
        this.receivedErrors = receivedErrors;
    }



    /**
     * @return Returns the receivedReplies.
     */
    public long getReceivedReplies() {
        return this.receivedReplies;
    }


    /**
     * @param receivedReplies The receivedReplies to set.
     */
    public void setReceivedReplies(long receivedReplies) {
        this.receivedReplies = receivedReplies;
    }



    /**
     * @return Returns the receivedRequests.
     */
    public long getReceivedRequests() {
        return this.receivedRequests;
    }



    /**
     * @param receivedRequests The receivedRequests to set.
     */
    public void setReceivedRequests(long receivedRequests) {
        this.receivedRequests = receivedRequests;
    }


    /**
     * @return Returns the sentDones.
     */
    public long getSentDones() {
        return this.sentDones;
    }


    /**
     * @param sentDones The sentDones to set.
     */
    public void setSentDones(long sentDones) {
        this.sentDones = sentDones;
    }


    /**
     * @return Returns the sentErrors.
     */
    public long getSentErrors() {
        return this.sentErrors;
    }


    /**
     * @param sentErrors The sentErrors to set.
     */
    public void setSentErrors(long sentErrors) {
        this.sentErrors = sentErrors;
    }


    /**
     * @return Returns the sentReplies.
     */
    public long getSentReplies() {
        return this.sentReplies;
    }



    /**
     * @param sentReplies The sentReplies to set.
     */
    public void setSentReplies(long sentReplies) {
        this.sentReplies = sentReplies;
    }



    /**
     * @return Returns the sentRequests.
     */
    public long getSentRequests() {
        return this.sentRequests;
    }



    /**
     * @param sentRequests The sentRequests to set.
     */
    public void setSentRequests(long sentRequests) {
        this.sentRequests = sentRequests;
    }

    
    public List<DisplayStatistics> generateDisplayStatistics() {
        List<DisplayStatistics> list = new ArrayList<DisplayStatistics>();
        DisplayStatistics displayStats;

        displayStats = new DisplayStatistics(endpoint,receivedRequests,receivedReplies,receivedErrors,receivedDones,
                sentRequests,sentReplies,sentErrors,sentDones);
        list.add(displayStats);
        
        
        return list;
    }


    public boolean equals(Object aThat) {
        // check for self-comparison
        if (this == aThat)
            return true;

        // use instanceof instead of getClass here for two reasons
        // 1. if need be, it can match any supertype, and not just one class;
        // 2. it renders an explict check for "that == null" redundant, since
        // it does the check for null already - "null instanceof [type]" always
        // returns false. (See Effective Java by Joshua Bloch.)
        if (!(aThat instanceof Statistics))
            return false;
        // Alternative to the above line :
        // if ( aThat == null || aThat.getClass() != this.getClass() ) return
        // false;

        // cast to native object is now safe
        Statistics that = (Statistics) aThat;

        // now a proper field-by-field evaluation can be made
        return EqualsUtil.areEqual(this.endpoint, that.endpoint)
                && EqualsUtil.areEqual(this.sentRequests, that.sentRequests)
                && EqualsUtil.areEqual(this.sentReplies, that.sentReplies)
                && EqualsUtil.areEqual(this.sentErrors, that.sentErrors)
                && EqualsUtil.areEqual(this.sentDones, that.sentDones)

                && EqualsUtil.areEqual(this.receivedRequests, that.receivedRequests)
                && EqualsUtil.areEqual(this.receivedReplies, that.receivedReplies)
                && EqualsUtil.areEqual(this.receivedErrors, that.receivedErrors)
                && EqualsUtil.areEqual(this.receivedDones, that.receivedDones);
    }

    public boolean equalsEndpointCompare(Object aThat) {
        // check for self-comparison
        if (this == aThat)
            return true;

        // use instanceof instead of getClass here for two reasons
        // 1. if need be, it can match any supertype, and not just one class;
        // 2. it renders an explict check for "that == null" redundant, since
        // it does the check for null already - "null instanceof [type]" always
        // returns false. (See Effective Java by Joshua Bloch.)
        if (!(aThat instanceof Statistics))
            return false;
        // Alternative to the above line :
        // if ( aThat == null || aThat.getClass() != this.getClass() ) return
        // false;

        // cast to native object is now safe
        Statistics that = (Statistics) aThat;

        // now a proper field-by-field evaluation can be made
        return EqualsUtil.areEqual(this.endpoint, that.endpoint);
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        // TODO Auto-generated method stub

    }

}
