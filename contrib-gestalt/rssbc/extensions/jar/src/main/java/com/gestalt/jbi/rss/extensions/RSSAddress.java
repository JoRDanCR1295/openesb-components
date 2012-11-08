/**
 *   rss-binding-component-extensions - Extensions for the RSS Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com
 *
 *   This library is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU Lesser General Public
 *   License version 2.1 as published by the Free Software Foundation.
 *
 *   This library is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *   Lesser General Public License for more details.
 *
 *   You should have received a copy of the GNU Lesser General Public
 *   License along with this library; if not, write to the Free Software
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
 */
package com.gestalt.jbi.rss.extensions;

import java.io.Serializable;

import javax.wsdl.extensions.ExtensibilityElement;

import javax.xml.namespace.QName;


/**
 * Extension element for the address portion of the WSDL
 * Author: cgallemore
 * Date: May 8, 2007
 */
public class RSSAddress implements ExtensibilityElement, Serializable {
    private static final long serialVersionUID = 1L;
    public static final String ELEM_ADDRESS = "address";
    public static final QName QNAME_ADDRESS = new QName(RSSConstants.RSS_NS_URI,
            ELEM_ADDRESS);
    private Boolean required;
    private String feedTitle;
    private String feedDescription;
    private String feedType;
    private String location;
    private String username;
    private String password;
    private String correlationId;

    /**
     * Constructor
     */
    public RSSAddress() {
    }

    /**
     * Sets the element type - Currently doesn't set anything.
     * @param qName
     */
    public void setElementType(QName qName) {
    }

    /**
     * Gets the Element type for this extension
     * @return QName
     */
    public QName getElementType() {
        return QNAME_ADDRESS;
    }

    /**
     * Sets the required attribute.
     * @param required
     */
    public void setRequired(Boolean required) {
        this.required = required;
    }

    /**
     * Get whether required (for wsdl:required)
     * @return Boolean
     */
    public Boolean getRequired() {
        return required;
    }

    /**
     * gets the feed title attribute.
     * @return String
     */
    public String getFeedTitle() {
        return feedTitle;
    }

    /**
     * sets the feed title attribute.
     * @param feedTitle
     */
    public void setFeedTitle(String feedTitle) {
        this.feedTitle = feedTitle;
    }

    /**
     * gets the feed descritpion attibute.
     * @return String
     */
    public String getFeedDescription() {
        return feedDescription;
    }

    /**
     * sets the feed description attribute.
     * @param feedDescription
     */
    public void setFeedDescription(String feedDescription) {
        this.feedDescription = feedDescription;
    }

    /**
     * gets the feed type attribute.
     * @return String
     */
    public String getFeedType() {
        return feedType;
    }

    /**
     * sets the feed type attribute.
     * @param feedType
     */
    public void setFeedType(String feedType) {
        this.feedType = feedType;
    }

    /**
     * gets the location, which is the feed URL, attribute.
     * @return String
     */
    public String getLocation() {
        return location;
    }

    /**
     * sets the location, which is the feed URL, attribute.
     * @param location
     */
    public void setLocation(String location) {
        this.location = location;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public String getCorrelationId() {
        return correlationId;
    }

    public void setCorrelationId(String correlationId) {
        this.correlationId = correlationId;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder(super.toString());
        builder.append("\nRSS Element Type ");
        builder.append(RSSAddress.QNAME_ADDRESS);
        builder.append(":");
        builder.append("\nRequired=");
        builder.append(required);

        return builder.toString();
    }

    /**
     * Attributes for the address extension.
     */
    public enum Attributes {feedTitle,
        feedDescription,
        feedType,
        location,
        username,
        password,
        correlationId;
    }
}
