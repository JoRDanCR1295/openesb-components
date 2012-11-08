/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: RuntimeContext.java,v 1.2 2008/01/27 20:59:42 tiago_cury Exp $
 */

package com.sun.jbi.sample.component.common;

import java.util.logging.Logger;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;

public final class RuntimeContext {
    /** RuntimeContext singleton instance */
    private static RuntimeContext sRuntimeContext;
    /** default logger*/
    private Logger mDefLogger;
    /** Logger object. */
    private Logger mLogger;
    /** Holds value of property ComponentContext.  */
    private ComponentContext mComponentContext;
    /** Holds value of property DeliveryChannel.  */
    private DeliveryChannel mDeliveryChannel;
    /** MessageExchange processing support **/
    private MessageExchangeSupport mMESupport;
    
    /** outside code can not instantiate RuntimeContext */
    private RuntimeContext() {
    }
    /**
     * @return RuntimeContext instance.
     */
    public static RuntimeContext getInstance() {
        if ( sRuntimeContext == null ) {
            synchronized (RuntimeContext.class) {
                if ( sRuntimeContext == null ) {
                    sRuntimeContext = new RuntimeContext();
                }
            }
        }
        return sRuntimeContext;
    }
    /**
     * Getter for to obtaining ComponentContext from any where in the component runtime.
     * @return ComponentContext.
     */
    public ComponentContext getComponentContext() {
        return this.mComponentContext;
    }
    /**
     * Sets the Component context reference in Runtime context for global access
     * any class in the component runtime.
     * com.sun.jbi.sample.component.common.AbstractComponentLifeCycle#initContext
     * method implementation should call this method to set the global context.
     * see com.sun.jbi.sample.component.common.DefaultComponentLifeCycle#initContext
     * for details.
     * @param componentContext New value of property ComponentContext.
     * @see BasicComponentLifeCycle#initGlobalContext
     */
    public void setComponentContext(ComponentContext componentContext) {
        if ( this.mComponentContext != null ) {
            (new IllegalStateException("Component Context already " +
                "initialized in RuntimeContext")).printStackTrace();
        }
        this.mComponentContext = componentContext;
    }
    /**
     * Getter for obtaining opened delivery channel from any where in the component runtime.
     * @return  DeliveryChannel.
     */
    public DeliveryChannel getDeliveryChannel() {
        return this.mDeliveryChannel;
    }
    /**
     * Sets the opened delivery channel reference in Runtime context for global access
     * any class in the component runtime.
     * com.sun.jbi.sample.component.common.AbstractComponentLifeCycle#initContext
     * method implementation should call this method to set the global context.
     * see com.sun.jbi.sample.component.common.DefaultComponentLifeCycle#initContext
     * for details.
     *
     * @param componentContext New value of property ComponentContext.
     */
    public void setDeliveryChannel(DeliveryChannel deliveryChannel) {
        if ( deliveryChannel != null && this.mDeliveryChannel != null ) {
            (new IllegalStateException("Delivery Channel already " +
                "initialized in RuntimeContext")).printStackTrace();
        }
        this.mDeliveryChannel = deliveryChannel;
    }
    /**
     * Sets the logger.
     *
     * @param name name for the Logger.
     * @param resourceBundle resource bundle for the logger. can be null.
     */
    public void setLogger(String name, String resourceBundle) {
        
        if (this.mComponentContext != null) {
            // get the logger from component context if the component context is not null
            try {
                this.mLogger = this.mComponentContext.getLogger(name, resourceBundle);
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        } else {
            this.mDefLogger = Logger.getLogger(name, resourceBundle);
        }
    }
    /**
     * Returns the logger.
     *
     * @return Logger
     */
    public Logger  getLogger() {
        
        // try init logger
        if (this.mLogger == null && this.mComponentContext != null ) {
            try {
                this.mLogger =
                    this.mComponentContext.getLogger(this.getClass().getName(), null);
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }
        // init default logger if required
        if ( this.mLogger == null && this.mDefLogger == null) {
            this.mDefLogger = Logger.getLogger(this.getClass().getName(), null);
        }
        return (this.mLogger != null) ? this.mLogger : this.mDefLogger;
    }
    /**
     * Returns the Component Name if the ComponentContext is set. else null
     * @return component name
     */
    public String getComponentName() {
        String componentName = null;
        if (this.mComponentContext != null) {
            componentName = this.mComponentContext.getComponentName();
        }
        return componentName;
    }
    /**
     * Global MessageExchangeSupport reference. Various classes in the common
     * component runtime would use this method to obtain the MessageExchangeSupport
     * for processing message exchange objects received from delivery channel.
     */
    public MessageExchangeSupport getMessageExchangeSupport() {
        if ( this.mMESupport == null ) {
            this.mMESupport = new MessageExchangeSupport();
        }
        return this.mMESupport;
    }
}
