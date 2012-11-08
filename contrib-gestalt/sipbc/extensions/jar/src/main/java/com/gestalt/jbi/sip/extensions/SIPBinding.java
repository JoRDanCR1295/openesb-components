/**
 *   sip-binding-component-extensions - Extensions for the SIP Binding Component
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
package com.gestalt.jbi.sip.extensions;

import com.ibm.wsdl.Constants;

import java.io.Serializable;

import javax.wsdl.extensions.ExtensibilityElement;

import javax.xml.namespace.QName;


/**
 * @author : csturtz
 */
public class SIPBinding implements ExtensibilityElement, Serializable {
    public static final QName QNAME_BINDING = new QName(SIPConstants.SIP_NS_URI,
            Constants.ELEM_BINDING);
    public Boolean required;

    public SIPBinding() {
    }

    public QName getElementType() {
        return QNAME_BINDING;
    }

    public void setElementType(QName qName) {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public Boolean getRequired() {
        return this.required;
    }

    public void setRequired(Boolean aBoolean) {
        this.required = aBoolean;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder(super.toString());
        builder.append("\nSIP Binding ");
        builder.append(QNAME_BINDING);
        builder.append(":");
        builder.append("\nRequired=");
        builder.append(required);

        return builder.toString();
    }
}
