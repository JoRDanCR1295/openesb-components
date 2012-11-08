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
 * Extension element for the output portion of the WSDL
 * Author: cgallemore
 * Date: May 8, 2007
 */
public class RSSOperationOutput implements ExtensibilityElement, Serializable {
    private static final long serialVersionUID = 1L;
    public static final QName QNAME_OPERATION_OUTPUT = new QName(RSSConstants.RSS_NS_URI,
            Constants.ELEM_OUTPUT);
    private Boolean required;

    /**
     * Get the extensibility element type
     * @return the extensibility element's type
     */
    public QName getElementType() {
        return RSSOperationOutput.QNAME_OPERATION_OUTPUT;
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

    @Override
    public String toString() {
        final StringBuilder builder = new StringBuilder(super.toString());
        builder.append("\nRSS operation (");
        builder.append(RSSOperationOutput.QNAME_OPERATION_OUTPUT);
        builder.append("):");
        builder.append("\nRequired=");
        builder.append(required);

        return builder.toString();
    }
}
