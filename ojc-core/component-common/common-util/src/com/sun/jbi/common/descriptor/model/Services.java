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
 * @(#)Services.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.descriptor.model;

import com.sun.jbi.common.descriptor.EndpointInfo;

/**
 * Represents a list of <code>consumes</code> and <code>provides</code>
 * entries in a service unit descriptor.
 * 
 * @author Kevan Simpson
 */
public class Services {
    private EndpointInfo[] mProvides, mConsumes;

    /**
     * Constructs a <code>Services</code>.
     * @param provides An array of provisioning endpoints.
     * @param consumes An array of consuming endpoints.
     */
    public Services(EndpointInfo[] provides, EndpointInfo[] consumes) {
        mProvides = provides;
        mConsumes = consumes;
    }

    /**
     * Returns the <code>provides</code> entries in the descriptor.
     * @return the <code>provides</code> entries in the descriptor.
     */
    public EndpointInfo[] getProvides() {
        // FindBug warning fix - make copy to avoid exposing internal representation
        // Fix to preserve original semantics.  provides object may be null;
        if (mProvides == null) {
            return new EndpointInfo[0];
        }

        int len = mProvides.length;
        EndpointInfo[] dest = new EndpointInfo[len];
        System.arraycopy(mProvides, 0, dest, 0, len);
        return dest;
    }

    /**
     * Returns the <code>consumes</code> entries in the descriptor.
     * @return the <code>consumes</code> entries in the descriptor.
     */
    public EndpointInfo[] getConsumes() {
        // FindBug warning fix - make copy to avoid exposing internal representation
        // Fix to preserve original semantics.  provides object may be null;
        if (mConsumes == null) {
            return new EndpointInfo[0];
        }

        int len = mConsumes.length;
        EndpointInfo[] dest = new EndpointInfo[len];
        System.arraycopy(mConsumes, 0, dest, 0, len);
        return dest;
    }

    /**
     * Returns the <code>provides</code> and <code>consumes</code> entries in the descriptor.
     * @return the <code>provides</code> and <code>consumes</code> entries in the descriptor.
     */
    public EndpointInfo[] getEndpoints() {
        int provLen = (mProvides != null ? mProvides.length : 0);
        int consLen = (mConsumes != null ? mConsumes.length : 0);
        EndpointInfo[] svcs = new EndpointInfo[(consLen + provLen)];
        if (mProvides != null) {
            System.arraycopy(mProvides, 0, svcs, 0, provLen);
        }
        if (mConsumes != null) {
            System.arraycopy(mConsumes, 0, svcs, provLen, consLen);
        }

        return svcs;
    }
}
