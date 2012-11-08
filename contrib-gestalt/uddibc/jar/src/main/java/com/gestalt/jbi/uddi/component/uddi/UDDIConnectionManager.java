/**
 *   uddi-binding-component - UDDI Binding Component
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
package com.gestalt.jbi.uddi.component.uddi;

import com.gestalt.jbi.component.manager.Endpoint;

import org.uddi4j.client.UDDIProxy;

import java.net.MalformedURLException;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;
import java.util.logging.Logger;


public class UDDIConnectionManager {
    protected static final Logger log = Logger.getLogger(UDDIConnectionManager.class.getName());
    private ConcurrentHashMap<String, UDDIProxyWrapper> inquiryProxies = new ConcurrentHashMap<String, UDDIProxyWrapper>();

    public UDDIProxy createInquiryProxy(String inquiryURI, Endpoint endpoint)
        throws MalformedURLException {
        log.log(Level.INFO, "Creating inquiry uddi proxy for - " + inquiryURI);

        UDDIProxyWrapper uddiProxyWrapper = new UDDIProxyWrapper(null);
        UDDIProxyWrapper tmp = inquiryProxies.putIfAbsent(inquiryURI,
                uddiProxyWrapper);

        if (tmp == null) {
            uddiProxyWrapper.setUDDIProxy(createInquiryUDDIProxy(inquiryURI));
            uddiProxyWrapper.registerEndpoint(endpoint);
            log.log(Level.INFO, "UDDI Proxy created");

            return uddiProxyWrapper.getUDDIProxy();
        }

        tmp.registerEndpoint(endpoint);
        log.log(Level.INFO, "Registered with existing UDDI Proxy");

        return tmp.getUDDIProxy();
    }

    private UDDIProxy createInquiryUDDIProxy(String uri)
        throws MalformedURLException {
        Properties p = new Properties();
        p.setProperty("org.uddi4j.TransportClassName",
            "org.uddi4j.transport.ApacheSOAPTransport");
        p.setProperty("org.uddi4j.inquiryURL", uri);

        return new UDDIProxy(p);
    }

    public void destroyProxy(String inquiryURI, Endpoint endpoint) {
        UDDIProxyWrapper uddiProxyWrapper = inquiryProxies.get(inquiryURI);

        if (uddiProxyWrapper != null) {
            uddiProxyWrapper.unregisterEndpoint(endpoint);

            if (!uddiProxyWrapper.hasRegisteredEndpoints()) {
                inquiryProxies.remove(inquiryURI);
                log.log(Level.INFO,
                    "UDDI Proxy no longer has endpoints and has been removed - " +
                    inquiryURI);
            }
        }
    }

    /**
     * Getter Method. Should only be used for testing, therefore it is a
     * protected method.
     *
     * @return ConcurrentHashMap<String, UDDIProxyWrapper>
     */
    protected ConcurrentHashMap<String, UDDIProxyWrapper> getInquiryProxies() {
        return inquiryProxies;
    }

    /**
     * Setter Method. Should only be used for testing, therefore it is a
     * protected method.
     *
     * @param inquiryProxies
     */
    protected void setInquiryProxies(
        ConcurrentHashMap<String, UDDIProxyWrapper> inquiryProxies) {
        this.inquiryProxies = inquiryProxies;
    }

    private class UDDIProxyWrapper {
        private UDDIProxy uddiProxy;
        private List<Endpoint> endpoints = new ArrayList<Endpoint>();

        public UDDIProxyWrapper(UDDIProxy uddiProxy) {
            setUDDIProxy(uddiProxy);
        }

        public void setUDDIProxy(UDDIProxy uddiProxy) {
            this.uddiProxy = uddiProxy;
        }

        public UDDIProxy getUDDIProxy() {
            return uddiProxy;
        }

        public void registerEndpoint(Endpoint endpoint) {
            endpoints.add(endpoint);
        }

        public void unregisterEndpoint(Endpoint endpoint) {
            endpoints.remove(endpoint);
        }

        public boolean hasRegisteredEndpoints() {
            return endpoints.size() > 0;
        }
    }
}
