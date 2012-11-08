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
 *
 * @author rriven
 */
public class XMPPBinding implements ExtensibilityElement, Serializable {
    private static final long serialVersionUID = 1L;
    public static final QName QNAME_BINDING = new QName(XMPPConstants.XMPP_NS_URI,
            Constants.ELEM_BINDING);
    private Boolean required;
    private Boolean tlsEnabled;
    private Boolean saslEnabled;

    /** Creates a new instance of XMPPBinding */
    public XMPPBinding() {
    }

    public void setElementType(QName qName) {
    }

    public QName getElementType() {
        return QNAME_BINDING;
    }

    public void setRequired(final Boolean required) {
        this.required = required;
    }

    public Boolean getRequired() {
        return required;
    }

    public Boolean getSaslEnabled() {
        return saslEnabled;
    }

    public void setSaslEnabled(Boolean saslEnabled) {
        this.saslEnabled = saslEnabled;
    }

    public Boolean getTlsEnabled() {
        return tlsEnabled;
    }

    public void setTlsEnabled(Boolean tlsEnabled) {
        this.tlsEnabled = tlsEnabled;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder(super.toString());
        builder.append("\nXMPPBinding ");
        builder.append(QNAME_BINDING);
        builder.append(":");
        builder.append("\nRequired=");
        builder.append(required);

        return builder.toString();
    }
    public static enum Attributes {tlsEnabled,
        saslEnabled;
    }
}
