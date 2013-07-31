/*
 * MessageExchangeSupport.java
 *
 */

package net.openesb.component.BindingComponent-archetype.common;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchange.Role;
import javax.xml.namespace.QName;

/**
 * This class supports the registering and unregistering MessageExchangeListeners and the
 * MessageExchangeHandlers that can be used to process the received message exchange from a 
 * delivery channel. A global reference to this class will be created in RuntimeContext object to
 * provide access to this class from any where in the component runtime.
 *
 * A ProviderEndpoint or ConsumerEndpoint interested in a MessageExchange received from a delivery 
 * channel for a particular service endpoint will register the MessageExchangeListener with this 
 * class. The DefaultMessageExchangeReceiver when received a message exchange object from the delivery
 * channel uses MessageExchangeSupport.processMessageExchange method to notify the registered listener.
 *
 * The MessageExchangeListener implementation ( ProviderEndpoint or ConsumerEndpoint ) while processing
 * received message ( messageExchangeReceived()) will create and register the message exchange handlers
 * to process the message exchange.
 *
 * @see RuntimeContext#getMessageExchangeSupport
 * @see RuntimeHelper#getMessageExchangeSupport
 * @see DefaultMessageExchangeReceiver#receiveAndProcessMessageExchange
 * @see com.sun.jbi.sample.component.common.deployment.ProviderEndpoint
 * @author chikkala
 */
public class MessageExchangeSupport {
    /** listener map to notify a message exchange received event */
    private Map<String, MessageExchangeListener> mListeners;
    /** handlers that can process the message exchange */
    private Map<String, MessageExchangeHandler> mHandlers;
    /** Creates a new instance of MessageExchangeSupport */
    public MessageExchangeSupport() {
        this.mListeners = Collections.synchronizedMap(new HashMap<String, MessageExchangeListener>());
        this.mHandlers = Collections.synchronizedMap(new HashMap<String, MessageExchangeHandler>());
    }
    /**
     * generates key that will be used to store the MessageExchangeListener objects.
     * @param serviceName QName of the service
     * @param endpointName endpoint name of the service.
     */
    public String createListenerKey(Role role, QName interfaceName, QName serviceName, String endpointName) {
        
        StringBuffer strBuff = new StringBuffer();
        strBuff.append(interfaceName).append("+");
        strBuff.append(serviceName).append("+");
        strBuff.append(endpointName).append("+");
        String roleType = null;
        if ( Role.CONSUMER.equals(role) ) {
            roleType = "CONSUMER";
        } else if ( Role.PROVIDER.equals(role) ) {
            roleType = "PROVIDER";
        }
        strBuff.append(roleType);
        return strBuff.toString();        
    }
    /**
     * adds message exchange listener who are interested in receiving the notification when the 
     * message exchange is received from delivery channel. The listener can be registered with the
     * following combination of the parameters: (role,interfaceName,serviceName, endpointName),
     * (role,interfaceName,null, null), (role, null,serviceName, endpointName), (role, null,serviceName, null)
     *
     * @param role role of the message exchange listener PROVIDER or CONSUMER - can not be null.
     * @param interfaceName QName of the interface ( protType ) - can be null if the serviceName is not null
     * @param serviceName QName of the service - can be null if interfaceName is not null.
     * @param endpointName endpoint name of the service. - can be null if the serviceName is not null.
     * @param listener MessageExchangeListener object
     */
    public  synchronized void addMessageExchangeListener(Role role, QName interfaceName, QName serviceName, String endpointName, MessageExchangeListener listener) {
        String key = createListenerKey(role, interfaceName, serviceName, endpointName);
        this.mListeners.put(key, listener);
    }
    /**
     * removes any message exchange listener registered for the service endpoint specified by the serviceName and 
     * endpoint name.
     * @param serviceName QName of the service
     * @param endpointName endpoint name of the service.
     */    
    public  synchronized void removeMessageExchangeListener(Role role, QName interfaceName, QName serviceName, String endpointName) {
        String key = createListenerKey(role, interfaceName, serviceName, endpointName);
        MessageExchangeListener listener = this.mListeners.remove(key);
        if ( listener == null ) {
            RuntimeHelper.getLogger().fine("No message exchange listener removed with key " + key);
        }
    }
    /**
     * finds the MessageExchangeListner registers for the endpoint for which the message exchange is
     * received and call the method on the listener
     */
    public boolean fireMessageExchangeReceived(ExchangeStatus status, MessageExchange me) {
        boolean notified = false;
        Role role = me.getRole();
        QName interfaceName = me.getInterfaceName();
        QName serviceName = me.getEndpoint().getServiceName();
        String endpointName = me.getEndpoint().getEndpointName();
        // lookup with complete service description ( role + interface + service + endpoint )
        String key = createListenerKey(role, interfaceName, serviceName, endpointName);
        RuntimeHelper.getLogger().fine("looking up for mx listener with key " + key);
        MessageExchangeListener listener = this.mListeners.get(key);
        
        if ( listener == null ) { // lookup with role+interface name             
            key = createListenerKey(role, interfaceName, null, null);
            RuntimeHelper.getLogger().fine("looking up for mx listener with key " + key);
            listener = this.mListeners.get(key);
        }                
        if ( listener == null ) { // lookup with role + serviceName + endpointName 
            key = createListenerKey(role, null, serviceName, endpointName);
            RuntimeHelper.getLogger().fine("looking up for mx listener with key " + key);
            listener = this.mListeners.get(key);
        }        
        if ( listener == null ) { // lookup with with role + serviceName 
            key = createListenerKey(role, null, serviceName, null);
            RuntimeHelper.getLogger().fine("looking up for mx listener with key " + key);
            listener = this.mListeners.get(key);
        }        
        if ( listener == null ) {
            RuntimeHelper.getLogger().fine("Could not find listener for message exchange" + me);
            notified = false;
        } else {
            listener.messageExchangeReceived(status,me);
            notified = true;
        }
        return notified;
    }
    /**
     * registers the MessageExchangeHandler implementation against the message exchange id.
     * @param me MessageExchange for which the handler need to be assigned
     * @param handler MessageExchangeHandler implementation that will be registers.
     */
    public synchronized void addMessageExchangeHandler(MessageExchange me, MessageExchangeHandler handler) {
        this.mHandlers.put(me.getExchangeId(), handler);
    }
    /**
     * removes the MessageExchangeHandler for the particular message exchange object
     */
    public synchronized void removeMessageExchangeHandler(MessageExchange me) {
        MessageExchangeHandler handler = this.mHandlers.remove(me.getExchangeId());
        if ( handler == null ) {
            RuntimeHelper.getLogger().fine("No MessageExchangeHandler found for removing " + me.getExchangeId());
        }
    }
    /**
     * looks up the registered message exchange handler for the message exchange object
     * @param me MessageExchangeHandler.
     */
    public  synchronized MessageExchangeHandler findMessageExchangeHandler(MessageExchange me) {
        return this.mHandlers.get(me.getExchangeId());
    }
    /**
     * looks up the registered message exchange handler for the message exchange object with Id
     * @param exchangeId  message exchange object's id
     */    
    public  synchronized MessageExchangeHandler findMessageExchangeHandler(String exchangeId) {
        return this.mHandlers.get(exchangeId);
    }
    
}
