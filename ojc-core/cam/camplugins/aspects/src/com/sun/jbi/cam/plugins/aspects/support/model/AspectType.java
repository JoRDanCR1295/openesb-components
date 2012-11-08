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
 * @(#)AspectType.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.plugins.aspects.support.model;

/**
 * @author graj
 *
 */
public enum AspectType {
    Logging("logging"), 
    Cache("cache"), 
    AutoReconnect("autoReconnect"),
    Queueing("queueing"), 
    MessageTracking("messageTracking"), 
    Tee("tee"),
    ContentBasedRouting("contentBasedRouting"), 
    Throttling("throttling"); 

    String aspectType;

    /** @param protocolString */
    private AspectType(String aspectTypeString) {
        this.aspectType = aspectTypeString;
    }

    /** @return the protocol */
    public String getAspectType() {
        return aspectType;
    }
    
    /**
     * 
     * @param aString
     * @return
     */
    public static AspectType convert(String aString) {
        AspectType type = null;
        if("logging".equals(aString)) {
            type = Logging;
        }
        if("cache".equals(aString)) {
            type = Cache;
        }
        if("autoReconnect".equals(aString)) {
            type = AutoReconnect;
        }
        if("queueing".equals(aString)) {
            type = Queueing;
        }
        if("messageTracking".equals(aString)) {
            type = MessageTracking;
        }
        if("tee".equals(aString)) {
            type = Tee;
        }
        if("contentBasedRouting".equals(aString)) {
            type = ContentBasedRouting;
        }
        if("throttling".equals(aString)) {
            type = Throttling;
        }
        return type;
    }
    
    

    /** @return the protocol */
    public String getDescription() {
        switch (this) {
        case Logging:
            return "logging";
        case Cache:
            return "cache";
        case AutoReconnect:
            return "autoReconnect";
        case Queueing:
            return "queueing";
        case MessageTracking:
            return "messageTracking";
        case Tee:
            return "tee";
        case ContentBasedRouting:
            return "contentBasedRouting";
        case Throttling:
            return "throttling";
        default:
            return "Unknown";
        }
    }

}
