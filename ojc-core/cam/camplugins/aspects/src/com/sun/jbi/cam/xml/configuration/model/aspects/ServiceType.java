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
 * @(#)ServiceType.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.xml.configuration.model.aspects;

/**
 * @author graj
 *
 */
public enum ServiceType {
    REQUEST_REPLY_SERVICE("requestReplyService"), FILTER_ONE_WAY("filterOneWay"), FILTER_REQUEST_REPLY("filterRequestReply");

    String service;

    /** @param protocolString */
    private ServiceType(String serviceString) {
        this.service = serviceString;
    }

    /** @return the direction */
    public String getService() {
        return service;
    }

    /** @return the direction */
    public String getDescription() {
        switch (this) {
        case REQUEST_REPLY_SERVICE:
            return "Simple A invoking B in a request/reply scenario";
        case FILTER_ONE_WAY:
            return "A invoking C filtered through B in a one-way scenario";
        case FILTER_REQUEST_REPLY:
            return "A invoking C filtered through B in a request/reply scenario";
        default:
            return "Unknown";
        }
    }
}
