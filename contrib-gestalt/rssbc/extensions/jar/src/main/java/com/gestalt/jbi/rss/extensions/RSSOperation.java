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
 * Extension element for the operation portion of the WSDL
 * Author: cgallemore
 * Date: May 8, 2007
 */
public class RSSOperation implements ExtensibilityElement, Serializable {
    private static final long serialVersionUID = 1L;
    public static final QName QNAME_OPERATION = new QName(RSSConstants.RSS_NS_URI,
            Constants.ELEM_OPERATION);
    private Boolean required = null;
    private OperationName operationName;
    private RSSOperationInput rssOperationInput;

    /**
     * Get the extensibility element type
     * @return the extensibility element's type
     */
    public QName getElementType() {
        return RSSOperation.QNAME_OPERATION;
    }

    /**
     * Set the extensibility element type
     * @param elementType the type
     */
    public void setElementType(final QName elementType) {
    }

    /**
     * Get whether required (for wsdl:required)
     * @return Boolean
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
     * Get the OperationName.
     * @return OperationName
     */
    public OperationName getOperationName() {
        return operationName;
    }

    /**
     * Sets the operation name
     * @param operationName
     */
    public void setOperationName(OperationName operationName) {
        this.operationName = operationName;
    }

    /**
     * sets the input
     * @param rssOperationInput
     */
    public void setRSSOperationInput(RSSOperationInput rssOperationInput) {
        this.rssOperationInput = rssOperationInput;
    }

    /**
     * Gets the input
     * @return RSSOperationInput
     */
    public RSSOperationInput getRSSOperationInput() {
        return rssOperationInput;
    }

    @Override
    public String toString() {
        final StringBuilder builder = new StringBuilder(super.toString());
        builder.append("\nRSS operation (");
        builder.append(RSSOperation.QNAME_OPERATION);
        builder.append("):");
        builder.append("\nRequired=");
        builder.append(required);

        return builder.toString();
    }

    /**
     * Attributes that are defined in the WSDL
     */
    public enum Attributes {name;
    }

    /**
     * The valid operation names that can be set in the WSDL
     */
    public enum OperationName {publish,
        subscribe;
    }
}
