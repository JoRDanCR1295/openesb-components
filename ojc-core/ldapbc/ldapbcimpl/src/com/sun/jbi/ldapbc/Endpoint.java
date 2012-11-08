/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ldapbc;

import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.ldapbc.extensions.LDAPAddress;
import com.sun.jbi.ldapbc.extensions.LDAPBinding;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;
import org.w3c.dom.Document;

/**
 *
 * @author tianlize
 */
public interface Endpoint {

    String getServiceUnitID();

    void setServiceUnitID(String id);

    ////////
    //
    //  Binding and Port Information Methods
    //
    ////////
    QName getServiceName();

    void setServiceName(QName serviceName);

    String getEndpointName();

    void setEndpointName(String endpointName);

    Definition getDefinition();

    void setDefinition(Definition definition);

    ////////
    //
    //  State Information Methods
    //
    ////////
    int getState();

    void setState(int state);

    EndpointStatus getEndpointStatus();

    void setEndpointStatus(EndpointStatus status);

    ////////
    //
    //  Outbound (provider) or inbound (consumer) - with respect to SE
    //
    ////////
    int getEndpointType();

    void setEndpointType(int type);

    ////////
    //
    //  JBI-representation of Endpoint
    //
    ////////
    ServiceEndpoint getServiceEndpoint();

    void setServiceEndpoint(ServiceEndpoint serviceEndpoint);

    Document getServiceDescription();

    void setServiceDescription(Document serviceDescription);

    ////////
    //
    //  File Binding specific Endpoint.
    //
    ////////
    LDAPAddress getLDAPAddress();

    void setLDAPAddress(LDAPAddress address);

    LDAPBinding getLDAPBinding();

    void setLDAPBinding(LDAPBinding binding);

    String getUniqueName();
}
