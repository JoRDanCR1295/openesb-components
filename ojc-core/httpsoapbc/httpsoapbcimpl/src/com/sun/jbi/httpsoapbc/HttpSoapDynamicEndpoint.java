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
 * @(#)HttpSoapBindingComponent.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import org.w3c.dom.DocumentFragment;

public class HttpSoapDynamicEndpoint implements ServiceEndpoint {
    private DocumentFragment mEpr;
    private QName mServiceName;
    private String mEndpointName;
    private QName mInterfaceName;
    private Endpoint mEndpointInfo;
    private String mDynamicUrl;
    
    public HttpSoapDynamicEndpoint (QName serviceName, String endpointName, QName interfaceName, DocumentFragment epr) {
        mEpr = epr;
        mServiceName = serviceName;
        mEndpointName = endpointName;
        mInterfaceName = interfaceName;
    }
    
    public QName getServiceName() {
        return mServiceName;
    }
    
    public String getEndpointName() {    	
        return mEndpointName;
    }
    
    public DocumentFragment getAsReference(QName operation) {
        return mEpr;
    }
    
    public QName[] getInterfaces() {
        return mInterfaceName != null? new QName[] { mInterfaceName } : null;   
    }
    
    public void setDynamicUrl(String url) {
        mDynamicUrl = url;
    }
    
    public String getDynamicUrl() {
        return mDynamicUrl;
    }
    
    public void setEndpointInfo(Endpoint endpoint) {
        mEndpointInfo = endpoint;
    }
    
    public Endpoint getEndpointInfo() {
        return mEndpointInfo;
    }
}