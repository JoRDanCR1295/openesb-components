/**
 *   xmpp-binding-component-extensions - Extensions for the XMPP Binding Component
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
package com.gestalt.jbi.xmpp.extensions;

import com.ibm.wsdl.Constants;

import java.io.Serializable;

import javax.wsdl.extensions.ExtensibilityElement;

import javax.xml.namespace.QName;


/**
 * @author aegloff
 */
public class XMPPOperationOutput implements ExtensibilityElement, Serializable {
    private static final long serialVersionUID = 1L;
    public static final QName QNAME_OPERATION_OUTPUT = new QName(XMPPConstants.XMPP_NS_URI,
            Constants.ELEM_OUTPUT);
    private Boolean required = null;
    private String jabberId;
    private String packetId;
    private String message;

    /**
     * Get the extensibility element type
     *
     * @return the extensibility element's type
     */
    public QName getElementType() {
        return XMPPOperationOutput.QNAME_OPERATION_OUTPUT;
    }

    /**
     * Set the extensibility element type
     *
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

    public String getPacketId() {
        return packetId;
    }

    public void setPacketId(String packetId) {
        this.packetId = packetId;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public String getJabberId() {
        return jabberId;
    }

    public void setJabberId(String jabberId) {
        this.jabberId = jabberId;
    }

    @Override
    public String toString() {
        final StringBuilder builder = new StringBuilder(super.toString());
        builder.append("\nXMPP operation (");
        builder.append(XMPPOperationOutput.QNAME_OPERATION_OUTPUT);
        builder.append("):");
        builder.append("\nRequired=");
        builder.append(required);

        return builder.toString();
    }
    public static enum Attributes {jabberId,
        packetId,
        message;
    }
}
