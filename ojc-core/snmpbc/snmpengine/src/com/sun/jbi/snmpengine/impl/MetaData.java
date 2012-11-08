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
 * @(#)MetaData.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpengine.impl;

/**
 * Represents one single record in the cache
 * 
 * @author fkieviet
 */
public class MetaData {
    private String[] destinations;
    private int mQueryID;
    
    /**
     * Constructor
     * 
     * @param queryid identifies the query
     */
    public MetaData(int queryid) {
        mQueryID = queryid;
    }

    /**
     * @return destinations (never null)
     */
    public String[] getDestinations() {
        return destinations;
    }

    /**
     * @return true if the data is in the cache; false if it is being queried
     */
    public boolean available() {
        return destinations != null;
    }

    /**
     * @param strings set the procecessor IDs associated with the TrapID
     */
    public void setProcessorIds(String[] strings) {
        destinations = strings;
    }

    /**
     * @return queryID
     */
    public int getQueryID() {
        return mQueryID;
    }
}
