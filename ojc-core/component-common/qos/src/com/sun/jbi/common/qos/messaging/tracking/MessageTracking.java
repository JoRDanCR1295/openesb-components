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
 * @(#)MessageTracking.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.common.qos.messaging.tracking;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import javax.jbi.messaging.MessageExchange;

/**
 * Utility for message tracking systemic quality.
 * @author Kevan Simpson
 */
public class MessageTracking {

    public static final String TRACKING_NS =
            "http://www.sun.com/jbi/qos/message-tracking";
    public static final String TRACKING_CONFIG =
            "com.sun.jbi.messaging.tracking.config";
    private static final String MESSAGE_TRACKING_IDS =
            "com.sun.jbi.messaging.tracking.ids";

    /**
     * Propagates tracking ids from one exchange to another.
     * @param src The exchange from which to copy tracking ids.
     * @param dest The exchange to which tracking ids are copied.
     * @return <code>true</code> if propagation succeeded, else <code>false</code>.
     */
    public static boolean propagateTracking(MessageExchange src, MessageExchange dest) {
        if (src == null || dest == null)
            return false;

        Set<String> srcIds = getTrackingIds(src), destIds = getTrackingIds(dest);
        if (srcIds != null && destIds != null) {
            destIds.addAll(srcIds);
            return true;
        }

        return false;
    }

    /**
     * Fetches the set of tracking ids from an exchange.
     * @param msg The exchange.
     * @return the set of tracking ids, which will not be <code>null</code>.
     * @throws ClassCastException if a component has set the tracking id
     *                            property with a value other than Set&lt;String&gt;.
     */
    public static Set<String> getTrackingIds(MessageExchange msg) {
        if (msg != null) {
            Object obj = msg.getProperty(MESSAGE_TRACKING_IDS);
            if (obj != null)
                if (obj instanceof String) {
                    // in case someone put an id and not a Set of ids
                    Set<String> ids = new HashSet<String>();
                    ids.add((String) obj);
                    return ids;
                } else // better be a Set of Strings!! ;-)
                    return (Set<String>) obj;
        }

        return new HashSet<String>(); // don't return null
    }

    /**
     * Sets one or more tracking ids on a message exchange.
     * @param msg An exchange.
     * @param ids One or more tracking ids.
     */
    public static void setTrackingIds(MessageExchange msg, String... ids) {
        if (msg != null && ids != null) {
            Set<String> set = getTrackingIds(msg);
            for (String id : ids)
                set.add(id);

            msg.setProperty(MESSAGE_TRACKING_IDS, set);
        }
    }

    /**
     * Creates a {@link TrackingConfig message tracking configuration} object.
     *
     * @param enableTracking <code>true</code> if tracking is enabled, else <code>false</code>.
     * @param storePayload <code>true</code> if payload is persisted, else <code>false</code>.
     * @param externalizePayload <code>true</code> if payload is externalized, else <code>false</code>.
     * @param payloadQueries Queries to apply to payload, stored for later retrieval.
     * @param messageQueries Message properties to store for later retrieval.
     * @return a <code>TrackingConfig</code> instance.
     */
    public static TrackingConfig createTrackingConfig(boolean enableTracking,
                                                      boolean storePayload,
                                                      boolean externalizePayload,
                                                      Map<String, String> payloadQueries,
                                                      Map<String, String> messageQueries) {
        return new MTConfig(enableTracking, storePayload, externalizePayload,
                payloadQueries, messageQueries);
    }

    private static class MTConfig implements TrackingConfig {

        private final boolean mTracking, mStore, mExternalize;
        private Map<String, String> mPayloadQueries, mMessageQueries;

        public MTConfig(boolean enableTracking,
                        boolean storePayload,
                        boolean externalizePayload,
                        Map<String, String> payloadQueries,
                        Map<String, String> messageQueries) {
            mTracking = enableTracking;
            mStore = storePayload;
            mExternalize = externalizePayload;
            mPayloadQueries = (payloadQueries == null)
                    ? new TreeMap<String, String>() : payloadQueries;
            mMessageQueries = (messageQueries == null)
                    ? new TreeMap<String, String>() : messageQueries;
        }

        /** @see com.sun.jbi.common.qos.messaging.tracking.TrackingConfig#doExternalizePayload() */
        public boolean doExternalizePayload() {
            return mExternalize;
        }

        /** @see com.sun.jbi.common.qos.messaging.tracking.TrackingConfig#doStorePayload() */
        public boolean doStorePayload() {
            return mStore;
        }

        /** @see com.sun.jbi.common.qos.messaging.tracking.TrackingConfig#getMessageQueries() */
        public Map<String, String> getMessageQueries() {
            return mMessageQueries;
        }

        /** @see com.sun.jbi.common.qos.messaging.tracking.TrackingConfig#getPayloadQueries() */
        public Map<String, String> getPayloadQueries() {
            return mPayloadQueries;
        }

        /** @see com.sun.jbi.common.qos.messaging.tracking.TrackingConfig#isTrackingEnabled() */
        public boolean isTrackingEnabled() {
            return mTracking;
        }
    }
}
