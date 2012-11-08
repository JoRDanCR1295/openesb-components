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


public class XMPPOperation implements ExtensibilityElement, Serializable {
    private static final long serialVersionUID = 1L;
    public static final QName QNAME_OPERATION = new QName(XMPPConstants.XMPP_NS_URI,
            Constants.ELEM_OPERATION);
    private Boolean required = null;
    private OperationName operationName;
    private XMPPOperationInput xmppOperationInput;
    private XMPPOperationOutput xmppOperationOutput;

    /**
     * Get the extensibility element type
     *
     * @return the extensibility element's type
     */
    public QName getElementType() {
        return XMPPOperation.QNAME_OPERATION;
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
     * Get the OperationName.
     * @return OperationName
     */
    public OperationName getOperationName() {
        return operationName;
    }

    /**
     * Sets the operation name
     * @param operationName - The Operation Name
     */
    public void setOperationName(OperationName operationName) {
        this.operationName = operationName;
    }

    /**
     * Set whether required (for wsdl:required)
     */
    public void setRequired(final Boolean required) {
        this.required = required;
    }

    /**
     * Sets the XMPPOperationInput
     * @param xmppOperationInput
     */
    public void setXMPPOperationInput(XMPPOperationInput xmppOperationInput) {
        this.xmppOperationInput = xmppOperationInput;
    }

    public XMPPOperationInput getXMPPOperationInput() {
        return xmppOperationInput;
    }

    public XMPPOperationOutput getXMPPOperationOutput() {
        return xmppOperationOutput;
    }

    public void setXMPPOperationOutput(XMPPOperationOutput xmppOperationOutput) {
        this.xmppOperationOutput = xmppOperationOutput;
    }

    @Override
    public String toString() {
        final StringBuilder builder = new StringBuilder(super.toString());
        builder.append("\nXMPP operation (");
        builder.append(XMPPOperation.QNAME_OPERATION);
        builder.append("):");
        builder.append("\nRequired=");
        builder.append(required);

        return builder.toString();
    }
    public enum Attributes {name;
    }

    /**
     * Opertion defined for the XMPP BC
     * joinGroup - join a group chat
     * leaveGroup - leave a group chat
     * sendMessage - sends an instant message.
     */
    public enum OperationName {joinGroup,
        leaveGroup,
        sendMessage,
        receiveMessage,
        kick,
        invite,
        createGroup,
        destroyGroup,
        getOccupantsPresence;
    }
}
