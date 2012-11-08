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
 * @(#)CorrelationSet.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine;

import java.util.HashMap;
import java.util.Iterator;


/**
 * correlation set
 *
 * @author Sun Microsystems
 */
public class CorrelationSet {
    private String mBPI;
    private int mKeySetID;
    private HashMap mKeyMap;

    /**
     * Creates a new instance of CorrelationKeys
     */
    public CorrelationSet() {
    }

    /**
     * gets BP instance
     *
     * @return String BP instance
     */
    public String getBPI() {
        return mBPI;
    }

    /**
     * sets BP instance
     *
     * @param bpi bp instance
     */
    public void setBPI(String bpi) {
        mBPI = bpi;
    }

    /**
     * gets key set ID
     *
     * @return int key set ID
     */
    public int getKeySetID() {
        return mKeySetID;
    }

    /**
     * sets key set ID
     *
     * @param keySetID key set ID
     */
    public void setKeySetID(int keySetID) {
        mKeySetID = keySetID;
    }

    /**
     * gets key map
     *
     * @return HashMap key map
     */
    public HashMap getKeyMap() {
        return mKeyMap;
    }

    /**
     * sets key map
     *
     * @param keymap key map
     */
    public void setKeyMap(HashMap keymap) {
        mKeyMap = keymap;
    }

    /**
     * matches event properties
     *
     * @param eventProperties map of event properties
     *
     * @return boolean: if event properties matches, returns true; otherwise, returns false
     */
    public boolean match(HashMap eventProperties) {
        boolean ret = true;
        Iterator i = mKeyMap.keySet().iterator();

        while (i.hasNext()) {
            Object object = i.next();
            String correlationSetPropertyName = (String) object;
            Object correlationSetPropertyValue = mKeyMap.get(correlationSetPropertyName);
            String value = (String) correlationSetPropertyValue;
            Object eventPropertyValue = eventProperties.get(correlationSetPropertyName);

            if (object == null) {
                ret = false;

                break;
            } else {
                String eventValue = (String) eventPropertyValue;

                if (!eventValue.equals(value)) {
                    ret = false;

                    break;
                }
            }
        }

        return ret;
    }

    /**
     * matches correlation set and event properties
     *
     * @param correlationSet correlation set
     * @param eventProperties event properties
     *
     * @return boolean: if event properties matches with correlationset, returns true; otherwise
     *         returns false
     */
    public static boolean match(HashMap correlationSet, HashMap eventProperties) {
        boolean ret = true;
        Iterator i = correlationSet.keySet().iterator();

        while (i.hasNext()) {
            Object object = i.next();
            String correlationSetPropertyName = (String) object;
            Object correlationSetPropertyValue = correlationSet.get(correlationSetPropertyName);
            String value = (String) correlationSetPropertyValue;
            Object eventPropertyValue = eventProperties.get(correlationSetPropertyName);

            if (object == null) {
                ret = false;

                break;
            } else {
                String eventValue = (String) eventPropertyValue;

                if (!eventValue.equals(value)) {
                    ret = false;

                    break;
                }
            }
        }

        return ret;
    }
}
