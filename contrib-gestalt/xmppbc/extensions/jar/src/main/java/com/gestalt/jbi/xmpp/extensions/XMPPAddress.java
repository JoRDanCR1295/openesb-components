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

import java.io.Serializable;

import javax.wsdl.extensions.ExtensibilityElement;

import javax.xml.namespace.QName;


/**
 * @author rriven
 */
public class XMPPAddress implements ExtensibilityElement, Serializable {
    private static final long serialVersionUID = 1L;
    public static final String ELEM_ADDRESS = "address";
    public static final QName QNAME_ADDRESS = new QName(XMPPConstants.XMPP_NS_URI,
            ELEM_ADDRESS);
    private Boolean required;
    private String domain;
    private String username;
    private String password;
    private String resource;
    private Integer port;
    private String group;

    /**
     * Creates a new instance of XMPPProxy
     */
    public XMPPAddress() {
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

    public String getDomain() {
        return domain;
    }

    public void setDomain(String domain) {
        this.domain = domain;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public Integer getPort() {
        return port;
    }

    public void setPort(Integer port) {
        this.port = port;
    }

    public String getResource() {
        return resource;
    }

    public void setResource(String resource) {
        this.resource = resource;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public void setGroup(String group) {
        this.group = group;
    }

    public String getGroup() {
        return group;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder(super.toString());
        builder.append("\nXMPP Element Type ");
        builder.append(XMPPAddress.QNAME_ADDRESS);
        builder.append(":");
        builder.append("\nRequired=");
        builder.append(required);

        return builder.toString();
    }
    public enum Attributes {domain,
        username,
        password,
        resource,
        port,
        group;
    }
}
