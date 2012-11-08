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
 * @(#)EndpointURL.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 *
 */
package com.sun.jbi.cam.common;

import java.io.Serializable;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

/**
 * @author ylee
 * @author Graj
 *
 */
public class EndpointURL implements Serializable {

    private String portName;

    private String namespaceURI;

    private String localPart;

    private String prefix = "";

    private String endpointIdentifier;

    private QName serviceQName;

    private Logger logger = Logger.getLogger(EndpointURL.class.getName());

    /**
     *
     */
    public EndpointURL() {
    }

    /**
     *
     * @param urlString
     */
    public EndpointURL(String urlString) {
        this.reconstructEndpointURL(urlString);
    }

    /**
     * @param name
     * @param namespaceuri
     * @param part
     */
    public EndpointURL(String namespaceuri, String part, String port,
            String identifier) {
        this.namespaceURI = namespaceuri;
        this.localPart = part;
        this.serviceQName = new QName(namespaceuri, part);
        this.portName = port;
        this.endpointIdentifier = identifier;
    }

    /**
     * @param name
     * @param namespaceuri
     * @param part
     * @param prefix
     */
    public EndpointURL(String namespaceuri, String prefix, String part,
            String port, String identifier) {
        this.namespaceURI = namespaceuri;
        this.prefix = prefix;
        this.localPart = part;
        this.portName = port;
        if (prefix == null) {
            prefix = "";
        }
        this.endpointIdentifier = identifier;
        this.serviceQName = new QName(namespaceuri, part, prefix);
    }

    /**
     * @param serviceName
     * @param port
     */
    public EndpointURL(QName serviceName, String port, String identifier) {
        this.serviceQName = serviceName;
        this.portName = port;
        this.namespaceURI = serviceName.getNamespaceURI();
        this.localPart = serviceName.getLocalPart();
        this.prefix = serviceName.getPrefix();
        this.endpointIdentifier = identifier;
    }

    /**
     *
     * @return endpointURL as a String
     */
    public String getEndpointURL() {
        String endpointURLString = null;
        endpointURLString = this.namespaceURI + "," + this.localPart + ","
                + this.portName + "," + this.endpointIdentifier;
        return endpointURLString;
    }

    /**
     *
     * @param someURL
     * @return
     */
    public EndpointURL getConnectingEndpointURL() {
        EndpointURL connectingURL = new EndpointURL(this.getEndpointURL());
        if ("Provider".equals(connectingURL.getEndpointIdentifier()) == true) {
            connectingURL.setEndpointIdentifier("Consumer");
        } else {
            connectingURL.setEndpointIdentifier("Provider");
        }
        return connectingURL;
    }

    /**
     *
     * @param endpointURLString
     * @return
     */
    public EndpointURL reconstructEndpointURL(String endpointURLString) {
        if (endpointURLString == null) {
            return null;
        }
        EndpointURL endpointURL = null;

        String[] entries = endpointURLString.split(",", 0);
        if (entries != null) {
            if (entries.length == 4) {
                this.namespaceURI = entries[0];
                this.localPart = entries[1];
                this.serviceQName = new QName(namespaceURI, localPart);
                this.portName = entries[2];
                this.endpointIdentifier = entries[3];
            }
        }
        return this;
    }

    /**
     * @return Returns the localPart.
     */
    public String getLocalPart() {
        return this.localPart;
    }

    /**
     * @param localPart
     *            The localPart to set.
     */
    public void setLocalPart(String localPart) {
        this.localPart = localPart;
    }

    /**
     * @return Returns the namespaceURI.
     */
    public String getNamespaceURI() {
        return this.namespaceURI;
    }

    /**
     * @param namespaceURI
     *            The namespaceURI to set.
     */
    public void setNamespaceURI(String namespaceURI) {
        this.namespaceURI = namespaceURI;
    }

    /**
     * @return Returns the portName.
     */
    public String getPortName() {
        return this.portName;
    }

    /**
     * @param portName
     *            The portName to set.
     */
    public void setPortName(String portName) {
        this.portName = portName;
    }

    /**
     * @return Returns the prefix.
     */
    public String getPrefix() {
        return this.prefix;
    }

    /**
     * @param prefix
     *            The prefix to set.
     */
    public void setPrefix(String prefix) {
        this.prefix = prefix;
    }

    /**
     * @return Returns the serviceQName.
     */
    public QName getServiceQName() {
        return this.serviceQName;
    }

    /**
     * @param serviceQName
     *            The serviceQName to set.
     */
    public void setServiceQName(QName serviceQName) {
        this.serviceQName = serviceQName;
    }

    /**
     * @return Returns the endpointIdentifier.
     */
    public String getEndpointIdentifier() {
        return this.endpointIdentifier;
    }

    /**
     * @param endpointIdentifier
     *            The endpointIdentifier to set.
     */
    public void setEndpointIdentifier(String endpointIdentifier) {
        this.endpointIdentifier = endpointIdentifier;
    }

    public void dump() {
        logger.info("/////////////////////////////////////////////////");
        logger.info("// Namespace URI: " + this.namespaceURI);
        logger.info("// Local Part: " + this.localPart);
        logger.info("// Prefix: " + this.prefix);
        logger.info("// Endpoint Port Name: " + this.portName);
        logger.info("// Endpoint Identifier: " + this.endpointIdentifier);
        logger.info("// Endpoint URL String: " + this.getEndpointURL());
        logger.info("/////////////////////////////////////////////////");
    }

    public boolean equalsWithoutIdentifier(Object aThat) {
        // check for self-comparison
        if (this == aThat)
            return true;

        // use instanceof instead of getClass here for two reasons
        // 1. if need be, it can match any supertype, and not just one class;
        // 2. it renders an explict check for "that == null" redundant, since
        // it does the check for null already - "null instanceof [type]" always
        // returns false. (See Effective Java by Joshua Bloch.)
        // if (!(aThat instanceof EndpointURL))
        // return false;
        // Alternative to the above line :
        if (aThat == null || aThat.getClass() != this.getClass())
            return false;

        // cast to native object is now safe
        EndpointURL that = (EndpointURL) aThat;

        // now a proper field-by-field evaluation can be made
        return EqualsUtil.areEqual(this.serviceQName, that.serviceQName)
                && EqualsUtil.areEqual(this.portName, that.portName)
                && EqualsUtil.areEqual(this.namespaceURI, that.namespaceURI)
                && EqualsUtil.areEqual(this.localPart, that.localPart)
                && EqualsUtil.areEqual(this.prefix, that.prefix);
    }

//    public boolean equals(Object aThat) {
//        // check for self-comparison
//        if (this == aThat)
//            return true;
//
//        // use instanceof instead of getClass here for two reasons
//        // 1. if need be, it can match any supertype, and not just one class;
//        // 2. it renders an explict check for "that == null" redundant, since
//        // it does the check for null already - "null instanceof [type]" always
//        // returns false. (See Effective Java by Joshua Bloch.)
//        if (!(aThat instanceof EndpointURL))
//            return false;
//        // Alternative to the above line :
//        // if ( aThat == null || aThat.getClass() != this.getClass() ) return
//        // false;
//
//        // cast to native object is now safe
//        EndpointURL that = (EndpointURL) aThat;
//
//        // now a proper field-by-field evaluation can be made
//        return EqualsUtil.areEqual(this.portName, that.portName)
//                && EqualsUtil.areEqual(this.namespaceURI, that.namespaceURI)
//                && EqualsUtil.areEqual(this.localPart, that.localPart)
//                && EqualsUtil.areEqual(this.prefix, that.prefix)
//                && EqualsUtil.areEqual(this.endpointIdentifier,
//                        that.endpointIdentifier)
//                && EqualsUtil.areEqual(this.serviceQName, that.serviceQName);
//    }
//
//    public int hashCode() {
//        int result = HashCodeUtil.SEED;
//        result = HashCodeUtil.hash(result, this.portName);
//        result = HashCodeUtil.hash(result, this.namespaceURI);
//        result = HashCodeUtil.hash(result, this.localPart);
//        result = HashCodeUtil.hash(result, this.prefix);
//        result = HashCodeUtil.hash(result, this.endpointIdentifier);
//        return result;
//    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        // TODO Auto-generated method stub

    }

}
