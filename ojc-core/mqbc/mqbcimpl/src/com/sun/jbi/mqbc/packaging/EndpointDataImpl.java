/*
 * EndpointDataImpl.java
 * 
 * Created on Jul 10, 2007, 3:01:29 PM
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.mqbc.packaging;

import javax.xml.namespace.QName;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.descriptor.QosServices;
import com.sun.jbi.mqbc.Endpoint;

/**
 *
 * @author rchen
 * @author Noel.Ang@sun.com
 */
public class EndpointDataImpl implements EndpointData  {
    private final QName mInterface;
    private final QName mService;
    private final String mEndpointName;
    private final int mDirection;
    private final String mAppConf;
    private final EndpointInfo mEndpointInfo;

    EndpointDataImpl(EndpointInfo endpoint, QosServices services) {
        assert endpoint != null;
        assert services != null;
        
        mInterface = endpoint.getInterfaceName();
        mService = endpoint.getServiceName();
        mEndpointName = endpoint.getEndpointName();
        mAppConf = services.getApplicationConfiguration(endpoint);
        mDirection = endpoint.isProvides()
                        ? Endpoint.EndpointType.OUTBOUND
                        : Endpoint.EndpointType.INBOUND;
        mEndpointInfo = endpoint;
    }

    public String getInterface() {
        return mInterface.toString();
    }
         
        public String getService() {
            return mService.toString();
        }
        
        public String getEndpoint() {
            return mEndpointName;
        }
        
        public int getDirection() {
            return mDirection;
        }

    /**
     * The name of the application configuration object associated with this
     * endpoint, if any.
     *
     * @return The name of the associated application configuration object, if
     *         any, or else a blank string.
     */
    public String getApplicationConfigurationObjectName() {
        return (mAppConf != null ? mAppConf : "");
    }
    
    /**
     * Returns the EndpointInfo representation.
     */
    public EndpointInfo toEndpointInfo() {
        return mEndpointInfo;
    }
}

