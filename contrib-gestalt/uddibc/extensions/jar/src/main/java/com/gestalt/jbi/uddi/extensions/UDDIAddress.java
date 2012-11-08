/**
 *   uddi-binding-component-extensions - Extensions for the UDDI Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com/
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
package com.gestalt.jbi.uddi.extensions;

import java.io.Serializable;

import javax.wsdl.extensions.ExtensibilityElement;

import javax.xml.namespace.QName;


/**
 *
 * @author rriven
 */
public class UDDIAddress implements ExtensibilityElement, Serializable {
    private static final long serialVersionUID = 1L;
    public static final String ELEM_ADDRESS = "address";
    public static final QName QNAME_ADDRESS = new QName(UDDIConstants.UDDI_NS_URI,
            ELEM_ADDRESS);
    private Boolean required;
    private String publishUri;
    private String inquiryUri;

    /** Creates a new instance of UDDIProxy */
    public UDDIAddress() {
    }

    public void setElementType(QName qName) {
    }

    public QName getElementType() {
        return QNAME_ADDRESS;
    }

    public void setRequired(Boolean required) {
        this.required = required;
    }

    public Boolean getRequired() {
        return required;
    }

    public void setPublishUri(String publishUri) {
        this.publishUri = publishUri;
    }

    public String getPublishUri() {
        return publishUri;
    }

    public void setInquiryUri(String inquiryUri) {
        this.inquiryUri = inquiryUri;
    }

    public String getInquiryUri() {
        return inquiryUri;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder(super.toString());
        builder.append("\nUDDI Element Type ");
        builder.append(UDDIAddress.QNAME_ADDRESS);
        builder.append(":");
        builder.append("\nRequired=");
        builder.append(required);

        return builder.toString();
    }
    public enum Attributes {publishUri,
        inquiryUri;
    }
}
