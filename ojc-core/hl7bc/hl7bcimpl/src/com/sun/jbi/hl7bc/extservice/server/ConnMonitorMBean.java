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
 * @(#) ConnMonitorMBean.java
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.hl7bc.extservice.server;

import java.util.concurrent.ConcurrentHashMap;

/**
 * MBean Interface
 * 
 * @author S. Nageswara Rao
 */
public interface ConnMonitorMBean {

    /**
     * MBean Getter for the attribute "ExternalSystemConnStatus"
     * 
     * @return a map containing the client connection address, and its status
     */
    public ConcurrentHashMap<String, String> getExternalSystemConnStatus();

    /**
     * MBean Setter for the attribute "ExternalSystemConnStatus"
     * 
     * @param cMap - a map containing the client connection address, and its status
     */
    public void setExternalSystemConnStatus(ConcurrentHashMap<String, String> cMap);

    /**
     * MBean Getter for the attribute Last MessageReceivedTimeStamp"
     * 
     * @param cMap - a map containing the client connection address, and its last message received
     */
    public ConcurrentHashMap<String, String> getMessageReceivedTimeStamp();

    /**
     * MBean setter for the attribute Last MessageReceivedTimeStamp"
     * 
     * @param cMap - a map containing the client connection address, and its last message received
     */
    public void setMessageReceivedTimeStamp(ConcurrentHashMap<String, String> cMap);

    /**
     * MBean Getter for the attribute Last MessageSentTimeStamp"
     * 
     * @param cMap - a map containing the client connection address, and its last ACK message sent
     */
    public ConcurrentHashMap<String, String> getACKMessageSentTimeStamp();

    /**
     * MBean setter for the attribute Last MessageSentTimeStamp"
     * 
     * @param cMap - a map containing the client connection address, and its last ACk message sent
     */
    public void setACKMessageSentTimeStamp(ConcurrentHashMap<String, String> cMap);

}
