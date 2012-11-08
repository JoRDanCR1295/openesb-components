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
 * @(#)WsdlRegistry.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.descriptor;

import java.io.File;
import java.net.URI;

import javax.jbi.management.DeploymentException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;

import org.w3c.dom.Document;
import org.xml.sax.EntityResolver;

import com.sun.jbi.common.util.EntryRegistry;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.common.xml.XmlUtil;


/**
 * Utility to assist component implementation of 
 * {@link javax.jbi.Component#getServiceDescription(javax.jbi.servicedesc.ServiceEndpoint)}.
 * 
 * @author Kevan Simpson
 */
public class WsdlRegistry {
    private EntryRegistry<EndpointInfo, Document> mWsdlDocs;
    private EntryRegistry<EndpointInfo, EntityResolver> mResolvers;
    
    public WsdlRegistry() {
        mWsdlDocs = new EntryRegistry<EndpointInfo, Document>();
        mResolvers = new EntryRegistry<EndpointInfo, EntityResolver>();
    }
    
    public Document lookupServiceDescription(EndpointInfo info) {
        Document doc = null;
        if (info != null) {
            
        }

        return doc;
    }

    public Document lookupServiceDescription(ServiceEndpoint endpt) {
        return (endpt != null) 
                ? lookupServiceDescription(EndpointInfo.valueOf(endpt, true))
                : null;
    }
    
    public void registerWsdl(EndpointInfo info, Definition wsdl, 
                             EntityResolver resolver) throws DeploymentException {
        if (info != null && wsdl != null) {
            if (Util.isEmpty(wsdl.getDocumentBaseURI())) {
                // TODO log? log + throw error?
                return;
            }
            
            try {
                Document doc = XmlUtil.readXml(
                        new File(new URI(wsdl.getDocumentBaseURI())));
                if (doc != null) {
                    getWsdlDocs().register(info, doc);
                    if (resolver != null) {
                        getResolvers().register(info, resolver);
                    }
                }
            }
            catch (Exception ex) {
                throw new DeploymentException(ex.getMessage(), ex);
            }
        }
    }

    public void unregisterWsdls(EndpointInfo... info) throws DeploymentException {
        getWsdlDocs().removeAll(info);
    }

    protected EntryRegistry<EndpointInfo, EntityResolver> getResolvers() {
        return mResolvers;
    }

    protected EntryRegistry<EndpointInfo, Document> getWsdlDocs() {
        return mWsdlDocs;
    }
}
