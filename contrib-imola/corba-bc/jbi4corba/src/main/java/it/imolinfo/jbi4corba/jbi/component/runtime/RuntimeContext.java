 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
/*
 * RuntimeContext.java
 *
 */

package it.imolinfo.jbi4corba.jbi.component.runtime;

import java.util.MissingResourceException;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;

/**
 * This class is used to store and retrieve the information that
 * should be available anywhere in the component runtime. Each instance variable
 * of this class will be initialized at various points of the component runtime using
 * setter methods on this class. For example, the ComponentContext and related resources
 * from this class are not available to the component runtime until the jbi framework calls
 * the init method of the ComponentLifeCycle of this component.
 *
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class RuntimeContext {
    
    private static RuntimeContext sRuntimeContext;
    
    /** Creates a new instance of RuntimeContext */
    private RuntimeContext() {
    }
    
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
     * Holds value of property ComponentContext.
     */
    private ComponentContext mComponentContext;
    
    /**
     * Getter for property mComponentContext.
     *
     * @return Value of property mComponentContext.
     */
    public ComponentContext getComponentContext() {
        return this.mComponentContext;
    }
    
    /**
     * Setter for property ComponentContext.
     *
     * @param componentContext New value of property ComponentContext.
     */
    public void setComponentContext(ComponentContext componentContext) {
        this.mComponentContext = componentContext;        
        this.mDeliveryChannel = null;
    }
    
    /**
     * Holds value of property DeliveryChannel.
     */
    private DeliveryChannel mDeliveryChannel;
    
    /**
     * Getter for property DeliveryChannel.
     *
     * @return Value of property DeliveryChannel.
     */
    public DeliveryChannel getDeliveryChannel() {
        return this.mDeliveryChannel;
    }
    
    /**
     * opens the delivery channel to accept the message exchange objects or 
     * send the message exchange objects
     */
    public void openDeliveryChannel() {
        try {
            // open the delivery channel from the component context
            ComponentContext compCtx = getComponentContext();
            this.mDeliveryChannel = compCtx.getDeliveryChannel();
        } catch (MessagingException ex) {
            RuntimeHelper.logDebug(ex);
        } catch ( Exception ex) {
            RuntimeHelper.logDebug(ex);
        }
    }
    /**
     * closes the delivery channel as part of the component shutdown process.
     */
    public void closeDeliveryChannel() {
        try {
            // closes delivery channel
            if ( this.mDeliveryChannel != null ) {
                this.mDeliveryChannel.close();
            }
        } catch (MessagingException ex) {
            RuntimeHelper.logDebug(ex);
        } finally {
            // clear channel in the runtime context
            this.mDeliveryChannel = null;
        }
    }
        
    /** default logger*/
    private Logger mDefLogger;
    /**
     * returns the default Logger for the component runtime.
     */
    private Logger getDefaultLogger() {        
        if ( mDefLogger == null ) {
           this.mDefLogger = Logger.getLogger(RuntimeContext.class.getName(), null); 
        }
        return this.mDefLogger;
    }
    
    /**
     * Logger object.
     */
    private Logger mLogger;
    
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
            } catch (MissingResourceException ex) {
                ex.printStackTrace();
            } catch (JBIException ex) {
                ex.printStackTrace();
            }
        } else {
            this.mLogger = Logger.getLogger(name, resourceBundle);
        }
    }
    
    /**
     * Returns the logger.
     *
     * @return Logger
     */
    public Logger  getLogger() {
        
        if (this.mLogger == null) {
            // if nobody set the logger, then return the default 
            // logger 
            return getDefaultLogger();
        }
        return this.mLogger;
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
     * Holds value of property MessageExchangeHandlerFactory.
     */
    private MessageExchangeHandlerFactory mMEHandlerFactory;
    
    /**
     * Getter for property MessageExchangeHandlerFactory.
     *
     * @return Value of property MessageExchangeHandlerFactory.
     */
    public MessageExchangeHandlerFactory getMessageExchangeHandlerFactory() {
        return this.mMEHandlerFactory;
    }
    
    /**
     * Setter for property MessageExchangeHandlerFactory.
     *
     * @param componentContext New value of property MessageExchangeHandlerFactory.
     */
    public void setMessageExchangeHandlerFactory(MessageExchangeHandlerFactory factory) {
        this.mMEHandlerFactory = factory;
    }
    /**
     * helper method to create a new message exchange handler using the message exchange
     * factory set on the runtime context.
     */
    public MessageExchangeHandler newMessageExchangeHandler(MessageExchange msgExchange) {
        MessageExchangeHandlerFactory factory = getMessageExchangeHandlerFactory();
        if ( factory == null ) {
            return null;
        }
        return factory.newHandler(msgExchange);
    }
}
