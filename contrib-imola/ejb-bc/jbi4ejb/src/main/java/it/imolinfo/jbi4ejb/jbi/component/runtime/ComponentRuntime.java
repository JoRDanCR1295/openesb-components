/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * Copyright 2004-2006 Sun Microsystems, Inc. All Rights Reserved.
 */

/*
 * ComponentRuntime.java
 */

package it.imolinfo.jbi4ejb.jbi.component.runtime;

import javax.jbi.component.Component;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.servicedesc.ServiceEndpoint;

/**
 * This is the Base implementation of the Component apis. Each component will
 * extend this class to provide the component specific implemenation of the api
 * by overriding the default implemenation or providing the implemenation of the
 * create methods for ComponentLifecycle and ServiceUnitManager.
 *
 * @see javax.jbi.Component
 *
 * @author Sun Microsystems, Inc.
 */
public abstract class ComponentRuntime implements Component {
    /** Component LifeCycle implemenation */
    private ComponentLifeCycle mLifeCycle;
    /** ServiceUnitManager implemenation */
    private ServiceUnitManager mSUManager;
    /**
     * constructor
     */
    protected  ComponentRuntime() {
        
        // create default logger
        RuntimeContext.getInstance().setLogger(this.getClass().getPackage().getName(), null);
        // create component lifecycle implementation
        this.mLifeCycle = createComponentLifeCycle();
        
        if ( this.mLifeCycle == null ) {
            // can not have a null component lifecycle. so, create default one.
            this.mLifeCycle = new DefaultComponentLifeCycle(this);
            
            RuntimeHelper.logDebug(
                "ComponentLifeCycle is not implemented by extended runtime." +
                " Default ComponentLifeCycle created");
        }
        // create service unit manager if supported.
        this.mSUManager = createServiceUnitManager();
        
        if ( this.mSUManager == null ) {
            RuntimeHelper.logDebug("ServiceUnit Deployment is not supported in the Component");
        }
    }
    
    ///////////////////////////////////////////////////////////////////////////
    // Component interface implemenation
    ///////////////////////////////////////////////////////////////////////////
    
    /**
     * Get the life cycle control interface for this component.
     *
     * @return the life cycle control interface for this component
     * @see javax.jbi.Component#getLifeCycle()
     */
    public final ComponentLifeCycle getLifeCycle() {
        return this.mLifeCycle;
    }
    
    /**
     * Get the Service Unit manager for this component.
     *
     * @return the <code>ServiceUnitManager</code> for this component, or
     *         <code>null</code> if there is none.
     * @see javax.jbi.Component#getServiceUnitManager()
     */
    public final ServiceUnitManager getServiceUnitManager() {
        return this.mSUManager;
    }
    
    /**
     * Retrieves a DOM representation containing metadata which describes the
     * service provided by this component, through the given endpoint.
     *
     * @param endpoint the service endpoint.
     * @return the description for the specified service endpoint.
     * @see javax.jbi.Component#getServiceDescription(javax.jbi.servicedesc.ServiceEndpoint)
     */
    public org.w3c.dom.Document getServiceDescription(ServiceEndpoint ref) {
        return null;
    }
    
    /**
     * This method is called by JBI to check if this component, in the role of
     * provider of the service indicated by the given exchange, can actually
     * perform the operation desired.
     *
     * @param endpoint the endpoint to be used by the consumer; must be
     *        non-null.
     * @param exchange the proposed message exchange to be performed; must be
     *        non-null.
     * @return <code>true</code> if this provider component can perform the
     *         given exchange with the described consumer.
     */
    public boolean isExchangeWithConsumerOkay(
        javax.jbi.servicedesc.ServiceEndpoint endpoint,
        javax.jbi.messaging.MessageExchange exchange) {
        return true;
    }
    
    /**
     * This method is called by JBI to check if this component, in the role of
     * consumer of the service indicated by the given exchange, can actually
     * interact with the provider properly. The provider is described by the
     * given endpoint and the service description supplied by that endpoint.
     *
     * @param endpoint the endpoint to be used by the provider; must be
     *        non-null.
     * @param exchange the proposed message exchange to be performed; must be
     *        non-null.
     * @return <code>true</code> if this consumer component can interact with
     *         the described provider to perform the given exchange.
     */
    public boolean isExchangeWithProviderOkay(
        javax.jbi.servicedesc.ServiceEndpoint endpoint,
        javax.jbi.messaging.MessageExchange exchange) {
        return true;
    }
    /**
     * Resolve the given endpoint reference.
     *
     * @param epr the endpoint reference, in some XML dialect understood by
     *        the appropriate component (usually a binding); must be non-null.
     * @return the service endpoint for the EPR; <code>null</code> if the
     *         EPR cannot be resolved by this component.
     * @see javax.jbi.Component#resolveEndpointReference(org.w3c.dom.DocumentFragment)
     */
    public javax.jbi.servicedesc.ServiceEndpoint resolveEndpointReference(
        org.w3c.dom.DocumentFragment epr) {
        return null;
    }
    
    ///////////////////////////////////////////////////////////////////////////
    // Helper Methods
    ///////////////////////////////////////////////////////////////////////////
    /**
     * return the ComponentLifeCycle implementaion. if returned null, the
     * DefaultComponentLifeCycle will be used as the component lifecycle
     */
    protected abstract ComponentLifeCycle createComponentLifeCycle();
    
    /**
     * if this component supports service unit deployment, then return the
     * service unit manager, else return null
     */
    protected abstract ServiceUnitManager createServiceUnitManager();
    
}
