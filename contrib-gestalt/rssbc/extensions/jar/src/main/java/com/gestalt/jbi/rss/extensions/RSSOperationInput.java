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

import com.ibm.wsdl.Constants;

import java.io.Serializable;

import javax.wsdl.extensions.ExtensibilityElement;

import javax.xml.namespace.QName;


/**
 * Extension element for the input portion of the WSDL
 * Author: cgallemore
 * Date: May 8, 2007
 */
public class RSSOperationInput implements ExtensibilityElement, Serializable {
    private static final long serialVersionUID = 1L;
    public static final QName QNAME_OPERATION_INPUT = new QName(RSSConstants.RSS_NS_URI,
            Constants.ELEM_INPUT);
    private static final int DEFAULT_POLLING_INTERVAL = 10;
    private Boolean required = null;
    private String entryTitle = null;
    private String entryLink = null;
    private String entryDescription = null;
    private int pollingInterval = DEFAULT_POLLING_INTERVAL;
    private FilterTypes filterByType = FilterTypes.publishDate;
    private String filterByValue = "now";
    private String longitude = "";
    private String latitude = "";
    private String feedList = "";
    private String destinationUrl = "";
    private Integer entriesPerMessage = null;

    /**
     * Get the extensibility element type
     * @return the extensibility element's type
     */
    public QName getElementType() {
        return RSSOperationInput.QNAME_OPERATION_INPUT;
    }

    /**
     * Set the extensibility element type
     * @param elementType the type
     */
    public void setElementType(final QName elementType) {
    }

    /**
     * Get whether required (for wsdl:required)
     */
    public Boolean getRequired() {
        return required;
    }

    /**
     * Set whether required (for wsdl:required)
     */
    public void setRequired(final Boolean required) {
        this.required = required;
    }

    /**
     * Gets the polling interval
     * @return String
     */
    public int getPollingInterval() {
        return pollingInterval;
    }

    /**
     * Sets the polling interval
     * @param pollingInterval
     */
    public void setPollingInterval(int pollingInterval) {
        this.pollingInterval = pollingInterval;
    }

    /**
     * Gets the max RSS entries to send per JBI message
     * @return Integer
     */
    public Integer getEntriesPerMessage() {
        return entriesPerMessage;
    }

    /**
     * Sets the max RSS entries to send per JBI message
     * @param entriesPerMessage
     */
    public void setEntriesPerMessage(int entriesPerMessage) {
        this.entriesPerMessage = entriesPerMessage;
    }

    /**
     * Gets the Filter Type
     * @return FilterTypes
     */
    public FilterTypes getFilterByType() {
        return filterByType;
    }

    /**
     * Sets the Filter Type
     * @param filterByType
     */
    public void setFilterByType(FilterTypes filterByType) {
        this.filterByType = filterByType;
    }

    /**
     * Gets the Filter value
     * @return String
     */
    public String getFilterByValue() {
        return filterByValue;
    }

    /**
     * Sets the filter Value
     * @param filterByValue
     */
    public void setFilterByValue(String filterByValue) {
        this.filterByValue = filterByValue;
    }

    /**
     * Gets the Entry Title
     * @return String
     */
    public String getEntryTitle() {
        return entryTitle;
    }

    /**
     * Sets the Entry Title
     * @param entryTitle
     */
    public void setEntryTitle(String entryTitle) {
        this.entryTitle = entryTitle;
    }

    /**
     * Gets the Entry Link
     * @return String
     */
    public String getEntryLink() {
        return entryLink;
    }

    /**
     * Sets the Entry Link
     * @param entryLink
     */
    public void setEntryLink(String entryLink) {
        this.entryLink = entryLink;
    }

    /**
     * Gets the Entry Description
     * @return String
     */
    public String getEntryDescription() {
        return entryDescription;
    }

    /**
     * Sets the Entry Description
     * @param entryDescription
     */
    public void setEntryDescription(String entryDescription) {
        this.entryDescription = entryDescription;
    }

    /**
     * Gets the longitude
     * @return String
     */
    public String getLongitude() {
        return longitude;
    }

    /**
     * Sets the longitude
     * @param longitude
     */
    public void setLongitude(String longitude) {
        this.longitude = longitude;
    }

    /**
     * Gets the latitude
     * @return String
     */
    public String getLatitude() {
        return latitude;
    }

    /**
     * Set the latitude
     * @param latitude
     */
    public void setLatitude(String latitude) {
        this.latitude = latitude;
    }

    /**
     * Get the FeedList
     * @return String
     */
    public String getFeedList() {
        return feedList;
    }

    /**
     * Set the FeedList
     * @param feedList
     */
    public void setFeedList(String feedList) {
        this.feedList = feedList;
    }

    /**
     * Get the destinationUrl
     * @return String
     */
    public String getDestinationUrl() {
        return destinationUrl;
    }

    /**
     * Set the destinationUrl
     * @param destinationUrl This is the destination URL that the specific entry will be
     *        published to.
     */
    public void setDestinationUrl(String destinationUrl) {
        this.destinationUrl = destinationUrl;
    }

    @Override
    public String toString() {
        final StringBuilder builder = new StringBuilder(super.toString());
        builder.append("\nRSS operation Input (");
        builder.append(RSSOperationInput.QNAME_OPERATION_INPUT);
        builder.append("):");
        builder.append("\nRequired=");
        builder.append(required);

        return builder.toString();
    }
    public enum Attributes {entryTitle,
        entryLink,
        entryDescription,
        pollingInterval,
        filterByValue,
        filterByType,
        longitude,
        latitude,
        feedList,
        destinationUrl,
        entriesPerMessage;
    }
    public enum FilterTypes {none,
        publishDate;
    }
}
