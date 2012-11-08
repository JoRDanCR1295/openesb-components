/*
 * EchoServiceDescriptor.java
 */
package com.sun.jbi.sample.component.common;

import javax.jbi.messaging.MessageExchange.Role;
import javax.xml.namespace.QName;

/**
 * This class provides the details of the wsdl model for the echo service that
 * is provided by the sample service engine and consumed by the sample binding
 * component. This is an exmaple that shows the required information from the
 * wsdl of a service that is provided or consumed by the component.
 *
 * @author chikkala
 */
public class EchoServiceDescriptor {
    /** service description info **/
    public final static String SERVICE_NS = "http://www.sun.com/jbi/examples/sample-service/echo";
    public final static String INTERFACE_NAME = "echo";
    public final static String OPERATION_NAME = "echo";
    public final static String OPERATION_INPUT_NAME = "echo";
    public final static String OPERATION_OUTPUT_NAME = "echo";
    public final static String SERVICE_NAME = "echoService";
    /** specific to each concrete wsdl */
    private String mEndpointName;
    /** Consumer or Provider role*/
    private Role mRole;
    /** Constructor */
    public EchoServiceDescriptor(Role role, String endpointName) {
        this.mRole = role;
        this.mEndpointName = endpointName;
    }
    /** generates the jbi internal endpoint name based on a component name*/
    public static String generateJBIInternalEndpointName(String componentName) {
        return componentName + "_JBIPort";
    }
    /** @return interface QName */
    public QName getInterfaceQName() {
        QName interfaceQName =
            new QName(EchoServiceDescriptor.SERVICE_NS, EchoServiceDescriptor.INTERFACE_NAME);
        return interfaceQName;
    }
    /** @return operation QName */
    public QName getOperationQName() {
        QName opQName =
            new QName(EchoServiceDescriptor.SERVICE_NS, EchoServiceDescriptor.OPERATION_NAME);
        return opQName;
    }
    /** @return service QName */
    public QName getServiceQName() {
        QName serviceQName =
            new QName(EchoServiceDescriptor.SERVICE_NS, EchoServiceDescriptor.SERVICE_NAME);
        return serviceQName;
    }
    /** @return endpiont name */
    public String getEndpointName() {
        return this.mEndpointName;
    }
    /** @return provider or consumer role */
    public Role getRole() {
        return this.mRole;
    }
    /** print to string */
    public String toString() {
        return "Service: " + this.getServiceQName() + " \n" +
            "Endpoint: " + this.getEndpointName() + " \n" +
            "Interface: " + this.getInterfaceQName() ;
    }
}
