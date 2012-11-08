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
 * @(#)Event.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine;

import java.util.HashMap;

import javax.xml.namespace.QName;

import com.sun.bpel.model.PartnerLink;


/**
 * Event class
 *
 * @author Sun Microsystems
 */
public class Event {
    /** DONE status */
    public static final int DONE = 0;

    /** Reply */
    public static final int REPLY_FAULT = 1;

    /** Request */
    public static final int REQUEST = 2;

    /** ERROR */
    public static final int ERROR = 3;

    /** ERROR on IN-OUT request*/
    public static final int RESPONSE_ERROR = 4;

    private String mMessageExchangeKey;
    private PartnerLink mPartnerLink;
    private QName mPort;
    private String mOperation;
    private int mType;
    private HashMap mProperties;
    private Object mContents;

    /**
     * Creates a new instance of Event
     */
    public Event() {
    }

    /**
     * gets type
     *
     * @return int event type
     */
    public int getType() {
        return mType;
    }

    /**
     * sets event type
     *
     * @param type event type
     */
    public void setType(int type) {
        mType = type;
    }

    /**
     * gets operation
     *
     * @return String operation
     */
    public String getOperation() {
        return mOperation;
    }

    /**
     * sets operation
     *
     * @param operation operation
     */
    public void setOperation(String operation) {
        mOperation = operation;
    }

    /**
     * gets partner link
     *
     * @return PartnerLink partner link
     */
    public PartnerLink getPartnerLink() {
        return mPartnerLink;
    }

    /**
     * sets partner link
     *
     * @param partnerLink partner link
     */
    public void setPartnerLink(PartnerLink partnerLink) {
        mPartnerLink = partnerLink;
    }

    /**
     * gets port
     *
     * @return QName port qname
     */
    public QName getPort() {
        return mPort;
    }

    /**
     * sets port
     *
     * @param port port
     */
    public void setPort(QName port) {
        mPort = port;
    }

    /**
     * adds property
     *
     * @param name property name
     * @param value property value
     */
    public void addProperty(String name, String value) {
        mProperties.put(name, value);
    }

    /**
     * gets property
     *
     * @param name property name
     *
     * @return String property value
     */
    public String getProperty(String name) {
        Object object = mProperties.get(name);
        String ret = (String) object;

        return ret;
    }

    /**
     * gets properties
     *
     * @return HashMap properties
     */
    public HashMap getProperties() {
        return mProperties;
    }

    /**
     * sets properties
     *
     * @param properties map of properties
     */
    public void setProperties(HashMap properties) {
        mProperties = properties;
    }

    /**
     * gets contents
     *
     * @return Object contents
     */
    public Object getContents() {
        return mContents;
    }

    /**
     * sets contents
     *
     * @param object contents
     */
    public void setContents(Object object) {
        mContents = object;
    }

    /**
     * gets message exchange key
     *
     * @return String message exchange key
     */
    public String getMessageExchangeKey() {
        return mMessageExchangeKey;
    }

    /**
     * sets message exchange key
     *
     * @param messageExchangeKey message exchange key
     */
    public void setMessageExchangeKey(String messageExchangeKey) {
        mMessageExchangeKey = messageExchangeKey;
    }
}
