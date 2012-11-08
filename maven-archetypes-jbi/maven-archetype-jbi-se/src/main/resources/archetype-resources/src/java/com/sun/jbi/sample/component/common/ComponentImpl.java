/*
 * ComponentImpl.java
 */
package com.sun.jbi.sample.component.common;

import javax.jbi.component.Component;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;

/**
 * This class implements javax.jbi.component.Component interface which is
 * resposible for creating the Component Lifecycle implementation and the
 * ServiceUnitManager implementation for the jbi component.
 *
 * This class makes sure that non null component lifecycle implementation is
 * returns  by this class or any classes extended from this class which is
 * required as a component contract. It also provides default implementation of
 * all methods of the Component interface which the extended classes can
 * override to add more functionality.
 *
 * @see javax.jbi.component.Component
 * @author chikkala
 */
public class ComponentImpl implements Component {
    /** Component LifeCycle implementation */
    private ComponentLifeCycle mLifeCycle;
    /** ServiceUnitManager implementation  */
    private ServiceUnitManager mSUManager;
    
    /** Creates a new instance of ComponentImpl */
    public ComponentImpl() {
        this.mLifeCycle = null;
        this.mSUManager = null;
    }
    /**
     * returns the ComponentContext. Any component runtime class extended from
     * this class can override this method to return their own reference to the
     * ComponentContext.
     * @return ComponentContext component context.
     */
    public ComponentContext getComponentContext() {
        ComponentContext ctx = null;
        if ( this.mLifeCycle != null && this.mLifeCycle instanceof ComponentLifeCycleImpl ) {
            ctx = ((ComponentLifeCycleImpl) this.mLifeCycle).getComponentContext();
        }
        return ctx;
    }
    /**
     * return the ComponentLifeCycle implementaion. if returned null, the
     * ComponentLifeCycleImpl will be used as the component lifecycle
     * Extended classes can override this method and do their own ComponentLifecyle
     * specific creation.
     */
    protected ComponentLifeCycle createComponentLifeCycle() {
        return new ComponentLifeCycleImpl();
    }
    /**
     * Get the life cycle control interface for this component.
     *
     * @return the life cycle control interface for this component
     * @see javax.jbi.Component#getLifeCycle()
     */
    public final ComponentLifeCycle getLifeCycle() {
        if ( this.mLifeCycle == null ) {
            this.mLifeCycle = createComponentLifeCycle();
            if ( this.mLifeCycle == null ) {
                // use the default ComponentLifeCycleImpl if a component
                // specific implementation return null in createComponentLifeCycle.
                this.mLifeCycle = new ComponentLifeCycleImpl();
            }
        }
        return this.mLifeCycle;
    }
    /**
     * if this component supports service unit deployment, then return the
     * service unit manager, else return null.
     * Extended classes can override this method and do their own ServiceUnitManager
     * specific creation.
     */
    protected ServiceUnitManager createServiceUnitManager() {
        return new ServiceUnitManagerImpl(this);
    }
    /**
     * Get the Service Unit manager for this component.
     *
     * @return the <code>ServiceUnitManager</code> for this component, or
     *         <code>null</code> if there is none.
     * @see javax.jbi.Component#getServiceUnitManager()
     */
    public final ServiceUnitManager getServiceUnitManager() {
        if ( this.mSUManager == null ) {
            this.mSUManager = createServiceUnitManager();
        }
        return this.mSUManager;
    }
    /**
     * Retrieves a DOM representation containing metadata which describes the
     * service provided by this component, through the given endpoint.
     *
     * Default implemeantion does not support service description.
     *
     * @see javax.jbi.Component#getServiceDescription(javax.jbi.servicedesc.ServiceEndpoint)
     */
    public Document getServiceDescription(ServiceEndpoint serviceEndpoint) {
        return null;
    }
    /**
     * This method is called by JBI to check if this component, in the role of
     * provider of the service indicated by the given exchange, can actually
     * perform the operation desired.
     *
     * Default implementation has no policy and allows all exchanges with consumer.
     *
     * @see javax.jbi.Component#isExchangeWithProviderOkay(
     * javax.jbi.servicedesc.ServiceEndpoint, javax.jbi.messaging.MessageExchange)
     */
    public boolean isExchangeWithConsumerOkay(
        ServiceEndpoint serviceEndpoint, MessageExchange messageExchange) {
        return true;
    }
    /**
     * This method is called by JBI to check if this component, in the role of
     * consumer of the service indicated by the given exchange, can actually
     * interact with the provider properly. The provider is described by the
     * given endpoint and the service description supplied by that endpoint.
     *
     * Default implementation has no policy and allows all exchanges with provider.
     *
     * @see javax.jbi.Component#isExchangeWithProviderOkay(
     * javax.jbi.servicedesc.ServiceEndpoint, javax.jbi.messaging.MessageExchange)
     */
    public boolean isExchangeWithProviderOkay(
        ServiceEndpoint serviceEndpoint, MessageExchange messageExchange) {
        return true;
    }
    /**
     * Resolve the given endpoint reference.
     *
     * Default implmenation does not have any XML dialet. So can not resolve the
     * endpoint from the document fragment.
     *
     * @see javax.jbi.Component#resolveEndpointReference(org.w3c.dom.DocumentFragment)
     */
    public ServiceEndpoint resolveEndpointReference(DocumentFragment documentFragment) {
        return null;
    }
}
