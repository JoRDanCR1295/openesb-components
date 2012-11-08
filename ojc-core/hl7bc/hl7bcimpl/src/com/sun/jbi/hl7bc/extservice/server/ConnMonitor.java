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
 * @(#)ConnMonitor.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.server;

import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Connection Monitor MBean, allow to view the active client connections with the server
 * 
 * @author aegloff
 */
public class ConnMonitor implements ConnMonitorMBean {

    private static final Logger mLogger = Logger.getLogger(ConnMonitor.class.getName());

    private String mServerID;

    private ConcurrentHashMap<String, String> mConcurrentMap;

    private ConcurrentHashMap<String, String> mLastMessageReceivedMap;

    private ConcurrentHashMap<String, String> mLastACKMessageSentMap;

    public ConnMonitor(String serverID) {
        mServerID = serverID;
    }

    /**
     * MBean Getter for the attribute "ExternalSystemConnStatus"
     * 
     * @return a map containing the client connection address, and its status
     */
    public ConcurrentHashMap<String, String> getExternalSystemConnStatus() {
        if (mLogger.isLoggable(Level.FINE) && mConcurrentMap != null) {
            mLogger.log(Level.FINE, "The Active Client Connections for server: " + mServerID + " are: "
                    + mConcurrentMap.toString());
        }
        return mConcurrentMap;
    }

    /**
     * MBean Setter for the attribute "ExternalSystemConnStatus"
     * 
     * @param cMap - a map containing the client connection address, and its status
     */
    public void setExternalSystemConnStatus(ConcurrentHashMap<String, String> cMap) {
        mConcurrentMap = cMap;
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "The Active Client Connections for server: " + mServerID + " are: "
                    + mConcurrentMap.toString());
        }
    }

    /**
     * MBean Getter for the attribute Last MessageReceivedTimeStamp"
     * 
     * @return a map containing the client connection address, and its last message received
     */
    public ConcurrentHashMap<String, String> getMessageReceivedTimeStamp() {
        if (mLogger.isLoggable(Level.FINE) && mLastMessageReceivedMap != null) {
            mLogger.log(Level.FINE, "Last message received from Active Client Connections for server: " + mServerID + " are: "
                    + mLastMessageReceivedMap.toString());
        }
        return mLastMessageReceivedMap;
    }

    /**
     * MBean Setter for the attribute "MessageReceivedTimeStamp"
     * 
     * @param cMap - a map containing the client connection address, and its last message received
     */
    public void setMessageReceivedTimeStamp(ConcurrentHashMap<String, String> cMap) {
        mLastMessageReceivedMap = cMap;
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Last message received from Active Client Connections for server: " + mServerID + " are: "
                    + mLastMessageReceivedMap.toString());
        }
    }

    /**
     * MBean Getter for the attribute Last MessageReceivedTimeStamp"
     * 
     * @return a map containing the client connection address, and its last message received
     */
    public ConcurrentHashMap<String, String> getACKMessageSentTimeStamp() {
        if (mLogger.isLoggable(Level.FINE) && mLastACKMessageSentMap != null) {
            mLogger.log(Level.FINE, "Last ACK message sent to Active Client Connections for server: " + mServerID + " are: "
                    + mLastACKMessageSentMap.toString());
        }
        return mLastACKMessageSentMap;
    }

    /**
     * MBean Setter for the attribute "MessageReceivedTimeStamp"
     * 
     * @param cMap - a map containing the client connection address, and its last message received
     */
    public void setACKMessageSentTimeStamp(ConcurrentHashMap<String, String> cMap) {
        mLastACKMessageSentMap = cMap;
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Last ACK message sent to Active Client Connections for server: " + mServerID + " are: "
                    + mLastACKMessageSentMap.toString());
        }
    }

}
