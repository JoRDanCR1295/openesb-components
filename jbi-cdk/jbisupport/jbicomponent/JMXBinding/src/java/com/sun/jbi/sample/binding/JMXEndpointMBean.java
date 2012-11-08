/*
 * JMXEndpointMBean.java
 */
package com.sun.jbi.sample.binding;

/**
 * The sample BC implemented as a jmx binding component exposes this
 * interface as a external connectivity (endpoint address) to the external
 * service consumers that can exchange messages over jmx protocol to invoke services
 * provided inside jbi environment.
 *
 * A jmx client can send a message by invoking this method using jmx management
 * interface. When this mbean method is executed in the BC as a result of mbean
 * call from the jmx client, the BC acts as a proxy service consumer inside the
 * jbi environment and starts a message exchange with the service provider.
 *
 * If the operation being executed is a InOut operation, the BC should return
 * the Out or Fault message from the InOut message exchange as a return value to
 * complete invocation of the service by the external consumer.
 *
 * @author  chikkala
 */
public interface JMXEndpointMBean {
    /**
     * This method will be called by a jmx client as a service consumer to send 
     * message to the echo service provider to invoke the service for a InOut 
     * message exchange.
     * @param operation operation name on a service
     * @param inputDoc input xml document for the InOut operation
     * @return output xml document in a StringBuffer received from InOut operation of the
     * service invoked. or null if the operation is InOnly.
     * @throws java.lang.Exception if any error occurs in invoking the operation on the service.
     */
    StringBuffer sendMessage(String operation, StringBuffer inputDoc) throws Exception;    
}
