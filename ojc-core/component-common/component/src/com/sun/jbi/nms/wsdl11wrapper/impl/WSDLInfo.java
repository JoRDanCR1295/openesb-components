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
 * @(#)WSDLInfo.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.nms.wsdl11wrapper.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;

import javax.wsdl.Message;
import javax.wsdl.Part;
import javax.xml.namespace.QName;

/**
 * Holds (and may cache) WSDL information for use by the Wrapper Builders and Parsers
 * @author Sun Microsystems
 */
public class WSDLInfo {

    static Map cachedInfos = new WeakHashMap();

//    int msgPartCount;
    QName messageType;
    List orderedMessageParts;
    List partsOrder = new ArrayList();

    /** Disallow instantiating this directly, use the getInstance factory method */
    private WSDLInfo() {
    }

    /**
     * Factory method to get a WSDLInfo instance for a given message definition
     * @param wsdlMessageDefinition the message definition to provide information about
     * @return an instance with information regarding the wsdlMessageDefinitin passed in
     */
    public static WSDLInfo getInstance(Message wsdlMessageDefinition) {
        WSDLInfo info = (WSDLInfo)cachedInfos.get(wsdlMessageDefinition);
        if (info == null) {
            info = prepareInfo(wsdlMessageDefinition);
        }
        return info;
    }

    /**
     * @return the qualified name of the message
     */
    public QName getMessageType() {
        return messageType;
    }

    /**
     * @return all the parts of the mssage, in the order they are defined in the message
     */
    public List getOrderedMessageParts() {
        return orderedMessageParts;
    }

    /**
     * @return the name of all the message parts, in the order defined in the message
     * Note that this is NOT the optional order defined in the binding
     */
    public List getPartsOrder() {
        return partsOrder;
    }

    /**
     * Internal helper to extract the information regarding a message
     */
    static WSDLInfo prepareInfo(Message wsdlMessageDefinition) {
        WSDLInfo info = new WSDLInfo();
        List msgParts = wsdlMessageDefinition.getOrderedParts(null);
        info.orderedMessageParts = msgParts;

        for (int partCount = 0; partCount < msgParts.size(); partCount++) {
            Part currentPart = (Part) msgParts.get(partCount);
            info.partsOrder.add(currentPart.getName());
        }
//        info.msgPartCount = info.partsOrder.size();
        info.messageType = wsdlMessageDefinition.getQName();

        cachedInfos.put(wsdlMessageDefinition, info);
        return info;
    }

}
