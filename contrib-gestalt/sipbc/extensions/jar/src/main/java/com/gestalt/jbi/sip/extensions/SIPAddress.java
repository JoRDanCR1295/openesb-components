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

import java.io.Serializable;

import javax.wsdl.extensions.ExtensibilityElement;

import javax.xml.namespace.QName;


/**
 * @author : csturtz
 */
public class SIPAddress implements ExtensibilityElement, Serializable {
    public static final String ELEMENT_ADDRESS = "address";
    public static final QName QNAME_ADDRESS = new QName(SIPConstants.SIP_NS_URI,
            ELEMENT_ADDRESS);
    private Boolean required;
    private String proxydomain;
    private Integer proxyport;
    private Integer proxytimeout;
    private String username;
    private String password;

    public QName getElementType() {
        return QNAME_ADDRESS;
    }

    public void setElementType(QName qName) {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public Boolean getRequired() {
        return required;
    }

    public void setRequired(Boolean aBoolean) {
        this.required = aBoolean;
    }

    public String getProxydomain() {
        return proxydomain;
    }

    public void setProxydomain(String proxydomain) {
        this.proxydomain = proxydomain;
    }

    public Integer getProxytimeout() {
        return proxytimeout;
    }

    public void setProxytimeout(Integer proxytimeout) {
        this.proxytimeout = proxytimeout;
    }

    public Integer getProxyport() {
        return proxyport;
    }

    public void setProxyport(Integer proxyport) {
        this.proxyport = proxyport;
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

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder(super.toString());
        builder.append("\nSIP Address ");
        builder.append(SIPAddress.QNAME_ADDRESS);
        builder.append(":");
        builder.append("\nRequired=");
        builder.append(required);

        return builder.toString();
    }
    public enum Attributes {proxyDomain,
        proxyPort,
        proxyTimeout,
        username,
        password;
    }
}
