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
 * @(#)EndpointInfo.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.descriptor;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import com.sun.jbi.common.util.Util;

/**
 * Describes a JBI internal endpoint, more specifically, a &quot;provides&quot;
 * or &quot;consumes&quot; element in a JBI descriptor file.
 * 
 * @author Kevan Simpson
 */
public class EndpointInfo {
    /**
     * Enum for endpoint link-type.
     * <p>
     * Enum instances are defined in lowercase purposely to match actual values.
     * 
     * @author Kevan Simpson
     */
    public enum LinkType { standard, hard, soft };
    
    /**
     * Provides an <code>EndpointInfo</code> based on the specified parameters.
     * 
     * @param service A qualified service name.
     * @param endpoint An endpoint name.
     * @param iface A qualified interface name or <code>null</code> (if a connection).
     * @param provides  Flag to indicate whether endpoint is provider or consumer.
     * @param link The {@link LinkType} of the endpoint.
     * @return an <code>EndpointInfo</code>.
     */
    public static EndpointInfo valueOf(QName service, String endpoint, QName iface, boolean provides, LinkType link) {
        if (service == null) {
            throw new IllegalArgumentException("Cannot create EndpointInfo with \"null\" service name!");
        }
        else if (Util.isEmpty(endpoint)) {
            throw new IllegalArgumentException("Cannot create EndpointInfo with \"null\" or \"\" endpoint name!");
        }
        
        return new EndpointInfo(provides, endpoint, iface, service, link);
    }
    
    /**
     * Provides an <code>EndpointInfo</code> based on the specified {@link ServiceEndpoint}.
     * 
     * @param srvcEndpoint An active endpoint reference.
     * @param provides Flag to indicate whether endpoint is provider or consumer.
     * @return an <code>EndpointInfo</code>.
     * @throws IllegalArgumentException if the specified endpoint is <code>null</code>.
     */
    public static EndpointInfo valueOf(ServiceEndpoint srvcEndpoint, boolean provides) {
        if (srvcEndpoint == null) {
            throw new IllegalArgumentException("Cannot create EndpointInfo with \"null\" service endpoint!");
        }
        
        return valueOf(srvcEndpoint.getServiceName(), srvcEndpoint.getEndpointName(), null, provides, null);
    }
    
    private String mEndptName = null;
    private QName mInterfaceName = null, mServiceName = null;
    private boolean mProvides = true;
    private LinkType mLinkType = null;
    
    public EndpointInfo(boolean provides, String endptNm, QName interfaceNm,
                        QName serviceNm, LinkType linkType) {
        mProvides = provides;
        mEndptName = endptNm;
        mInterfaceName = interfaceNm;
        mServiceName = serviceNm;
        mLinkType = linkType;
    }
    
    /**
     * Returns the endpoint name.
     * @return the endpoint name.
     */
    public String getEndpointName() { return mEndptName; }

    /**
     * Returns the interface name.
     * @return the interface name.
     */
    public QName getInterfaceName() { return mInterfaceName; }
    
    /**
     * Returns the service name.
     * @return the service name.
     */
    public QName getServiceName() { return mServiceName; }

    /** 
     * Tests endpoint to determine whether or not it is provisioning.
     * @return <code>true</code> if this endpoint is a &quot;provides&quot;, 
     *         <code>false</code> otherwise.
     */    
    public boolean isProvides() { return mProvides; }

    /**
     * Returns a <code>link-type</code> if a value was declared or <code>null</code>
     * if this endpoint entry is a &quot;provides&quot; or no value was declared.
     * 
     * @return a <code>link-type</code> value or <code>null</code>.
     */
    public LinkType getLinkType() { return mLinkType; }

	/** @see java.lang.Object#equals(java.lang.Object) */
    public boolean equals(Object obj) {
        if (obj instanceof EndpointInfo) {
            EndpointInfo info = (EndpointInfo) obj;
            if (this.getInterfaceName() == null || info.getInterfaceName() == null) {
            	// service-assembly connections will not include interface-name
	            return (this.isProvides() == info.isProvides() &&
	                    Util.equals(this.getEndpointName(), info.getEndpointName()) &&
	                    Util.equals(this.getServiceName(), info.getServiceName()));
            }
            else {
	            return (this.isProvides() == info.isProvides() &&
	                    Util.equals(this.getEndpointName(), info.getEndpointName()) &&
	                    Util.equals(this.getInterfaceName(), info.getInterfaceName()) &&
	                    Util.equals(this.getServiceName(), info.getServiceName()));
            }
        }
        return false;
    }

    /** @see java.lang.Object#hashCode() */
    public int hashCode() {
        return ((Util.hashCode(getEndpointName()) * 7) +
                /*
                 * Interface name is optional (i.e. missing from service assembly 
                 * connections); therefore, sporadic failures in map.get(info) occur
                 * if interface name is included in hashCode calculation
                 */
//                (Util.hashCode(getInterfaceName()) * 13) +
                (Util.hashCode(getServiceName()) * 19)) *
                (isProvides() ? 3 : 5);
    }

    /** @see java.lang.Object#toString() */
    public String toString() {
        StringBuffer buff = new StringBuffer(); 
        buff.append("EndpointInfo[name=").append(getEndpointName())
            .append(",service=").append(getServiceName())
            .append((isProvides() ? ",provides]" : ",consumes]"));
        return buff.toString();
    }
    
    /**
     * This method should not be used, it is unnecessary; please instead use
     * {@link #valueOf(ServiceEndpoint, boolean)} passing in {@link MessageExchange#getEndpoint()}.
     * 
     * @param mex A message exchange.
     * @param provides <code>true</code> if provisioning, else <code>false</code>.
     * @return An endpoint descriptor.
     * @deprecated Please use {@link #valueOf(ServiceEndpoint, boolean)} passing
     *             in {@link MessageExchange#getEndpoint()}.
     */
    public static EndpointInfo createEndpointInfo(MessageExchange mex, boolean provides) {
        return (mex == null) ? null : valueOf(mex.getEndpoint(), provides);
    }
}
