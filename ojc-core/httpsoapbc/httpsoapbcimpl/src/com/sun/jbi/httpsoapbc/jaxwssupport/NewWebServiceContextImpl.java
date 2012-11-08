/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)NewWebServiceContextImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.jaxwssupport;

import java.security.Principal;

import javax.xml.ws.EndpointReference;
import javax.xml.ws.handler.MessageContext;
import javax.xml.ws.WebServiceContext;
import javax.xml.ws.wsaddressing.W3CEndpointReference;

import org.w3c.dom.Element;

/**
 * Provides the webservice context 
 * based on the JAX-WS implementation of com.sun.enterprise.webservice.NewWebServiceContextImpl
 */
public class NewWebServiceContextImpl implements WebServiceContext {
    
    public static ThreadLocal msgContext = new ThreadLocal();
    
    public static ThreadLocal principal = new ThreadLocal();

    private WebServiceContext jaxwsContextDelegate;

    public void setContextDelegate(WebServiceContext wsc) {
        this.jaxwsContextDelegate = wsc;
    }
    
    public MessageContext getMessageContext() {
        return this.jaxwsContextDelegate.getMessageContext();
    }

    public void setMessageContext(MessageContext ctxt) {
        msgContext.set(ctxt);
    }
    
    public Principal getUserPrincipal() {
        return this.jaxwsContextDelegate.getUserPrincipal();
    }

    public boolean isUserInRole(String role) {
        return this.jaxwsContextDelegate.isUserInRole(role);
    }
    
    public EndpointReference getEndpointReference(Element...referenceParameters) {
        return getEndpointReference(W3CEndpointReference.class, referenceParameters);
    }

    public <T extends EndpointReference> T getEndpointReference(Class<T> clazz, Element...referenceParameters) {
// TODO: is this needed?        
//        Packet packet = getRequestPacket();
//        String address = packet.webServiceContextDelegate.getEPRAddress(packet, endpoint);
//        return (T) ((WSEndpointImpl)endpoint).getEndpointReference(clazz,address);
        return null;
    }
    
}
