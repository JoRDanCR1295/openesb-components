/*
 * ComponentRuntime.java
 */
package net.openesb.component.BindingComponent-archetype.common;

import java.util.logging.Logger;
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
 * responsible for creating the Component Lifecycle implementation and the
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
public class ComponentRuntime implements Component {
    /** Component LifeCycle implementation */
    private ComponentLifeCycle mLifeCycle;
    /** ServiceUnitManager implementation  */
    private ServiceUnitManager mSUManager;
    /** default logger*/
    private Logger mDefLogger;
    
    /** Creates a new instance of ComponentImpl */
    public ComponentRuntime() {
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
        if ( this.mLifeCycle != null && this.mLifeCycle instanceof BasicComponentLifeCycle ) {
            ctx = ((BasicComponentLifeCycle) this.mLifeCycle).getComponentContext();
        }
        return ctx;
    }
    /**
     * Returns logger initialized from the component context or a default logger.
     * @return Logger
     */
    public Logger getLogger() {
        Logger logger = null;
        if ( this.mLifeCycle != null && this.mLifeCycle instanceof BasicComponentLifeCycle ) {
            logger = ((BasicComponentLifeCycle) this.mLifeCycle).getLogger();
        }
        // init default logger if required
        if ( logger == null && this.mDefLogger == null) {
            this.mDefLogger = Logger.getLogger(this.getClass().getName(), null);
        }
        return (logger != null) ? logger : this.mDefLogger;
    }
    /**
     * return the ComponentLifeCycle implementation. if returned null, the
     * ComponentLifeCycleImpl will be used as the component lifecycle
     * Extended classes can override this method and do their own ComponentLifecyle
     * specific creation.
     */
    protected ComponentLifeCycle createComponentLifeCycle() {
        return new BasicComponentLifeCycle(this);
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
                this.getLogger().fine("Creating basic component lifecycle implemenation");
                // use the default ComponentLifeCycle Impl if a component
                // specific implementation return null in createComponentLifeCycle.
                this.mLifeCycle = new BasicComponentLifeCycle(this);
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
        return null;
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
     * Default implementation does not support service description.
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
     * Default implementation does not have any XML dialect. So can not resolve the
     * endpoint from the document fragment.
     *
     * @see javax.jbi.Component#resolveEndpointReference(org.w3c.dom.DocumentFragment)
     */
    public ServiceEndpoint resolveEndpointReference(DocumentFragment documentFragment) {
        return null;
    }
}
