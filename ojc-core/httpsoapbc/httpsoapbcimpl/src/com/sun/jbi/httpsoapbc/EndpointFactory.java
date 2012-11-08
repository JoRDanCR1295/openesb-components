/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)EndpointFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import com.sun.jbi.httpsoapbc.security.api.EndpointSecurityConfig;
import com.sun.jbi.httpsoapbc.security.impl.CredentialValidatorManager;
import com.sun.jbi.internationalization.Messages;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.soap.SOAPAddress;
import javax.wsdl.extensions.soap.SOAPBinding;
import javax.wsdl.extensions.soap12.SOAP12Address;
import javax.wsdl.extensions.soap12.SOAP12Binding;
import javax.wsdl.extensions.http.HTTPAddress;
import javax.wsdl.extensions.http.HTTPBinding;
import javax.wsdl.Port;
import javax.wsdl.Service;
import javax.xml.namespace.QName;

/**
 * EndpointFactory allows the creation of different types of Endpoint objects.
 * An particular type of Endpoint is created based on whether the
 * ExtensibilityElement exists for that Endpoint in the binding section and
 * port section of the WSDL.
 *
 */
public class EndpointFactory {
    
    private static final Messages mMessages =
        Messages.getMessages(EndpointFactory.class);

    /**
     * Creates an Endpoint based on the service name and port name of
     * the Endpoint.  The proper type of Endpoint is generated based
     * on examining the def object.
     *
     * @param        def the WSDL Definition
     * @param        serviceName the name of the service for this endpoint
     * @param        endpointName the name of the endpoint.  This corresponds
     * to the name of the port
     * @param        isProvider designates whether the endpoint is a provider
     * of the service or a consumer.
     * @return       an Endpoint
     * @exception    Exception if any errror occurs.
     */
    public static Endpoint createEndpoint(Definition def,
                                          QName serviceName,
                                          String endpointName,
                                          QName interfaceName,
                                          boolean isProvider,
                                          HttpClientConnectionProperties clientConnProps,
                                          boolean propagateSoapHeader,
                                          String wsdlPath,
                                          EndpointSecurityConfig securityConfig,
                                          String serviceUnitID,
                                          String serviceUnitRootPath,
                                          CredentialValidatorManager cvm)
        throws Exception {
        Endpoint bcEndpoint = null;
        Port matchedPort = getPort(def, serviceName.toString(), endpointName);
        if (matchedPort != null) {
            Binding matchedBinding = matchedPort.getBinding();
            // If it has a soap binding, we need a soap address.
            if (hasSOAPBinding(matchedBinding)) {
                bcEndpoint =  new HttpSoapEndpoint(def,
                                                   serviceName,
                                                   endpointName,
                                                   interfaceName,
                                                   isProvider,
                                                   clientConnProps,
                                                   propagateSoapHeader,
                                                   wsdlPath,
                                                   securityConfig,
                                                   serviceUnitID,
                                                   serviceUnitRootPath,
                                                   cvm);
            }
            if (hasSOAP12Binding(matchedBinding)) {
                bcEndpoint =  new HttpSoap12Endpoint(def,
                                                   serviceName,
                                                   endpointName,
                                                   interfaceName,
                                                   isProvider,
                                                   clientConnProps,
                                                   propagateSoapHeader,
                                                   wsdlPath,
                                                   securityConfig,
                                                   serviceUnitID,
                                                   serviceUnitRootPath,
                                                   cvm);
            }
            
            if (hasHTTPBinding(matchedBinding)) {
                bcEndpoint = new HttpEndpoint(def,
                                              serviceName,
                                              endpointName,
                                              interfaceName,
                                              isProvider,
                                              clientConnProps,
                                              propagateSoapHeader,
                                              wsdlPath,
                                              securityConfig,
                                              serviceUnitID,
                                              serviceUnitRootPath,
                                              cvm);
            }                    
        }
        return bcEndpoint;
    }


    private static boolean hasSOAPBinding(Binding binding) 
        throws Exception {
        //soap:binding
        boolean hasSoapBinding = false;
        if (binding != null) {
            List extElems = binding.getExtensibilityElements();
            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext()) {
                ExtensibilityElement ee =
                    (ExtensibilityElement) extIter.next();
                if (SOAPBinding.class.isInstance(ee)) {
                    if (hasSoapBinding) {
                        // We already found at least one SOAP Binding.
                        // That's bad.  There should be at most one SOAP
                        // binding.
                        throw new Exception(mMessages.getString("HttpSoapBindingDeployer.Multiple_Soap_binding_elements"));
                    }
                    hasSoapBinding = true;
                }

            }
        }
        return hasSoapBinding;
    }
    
    private static boolean hasSOAP12Binding(Binding binding) 
    throws Exception {
    //soap:binding
    boolean hasSoap12Binding = false;
    if (binding != null) {
        List extElems = binding.getExtensibilityElements();
        Iterator extIter = extElems == null ? null : extElems.iterator();
        while (extIter != null && extIter.hasNext()) {
            ExtensibilityElement ee =
                (ExtensibilityElement) extIter.next();
            if (SOAP12Binding.class.isInstance(ee)) {
                if (hasSoap12Binding) {
                    // We already found at least one SOAP Binding.
                    // That's bad.  There should be at most one SOAP
                    // binding.
                    throw new Exception(mMessages.getString("HttpSoapBindingDeployer.Multiple_Soap_binding_elements"));
                }
                hasSoap12Binding = true;
            }

        }
    }
    return hasSoap12Binding;
}        



    private static boolean hasHTTPBinding(Binding binding) throws Exception {
        //soap:binding
        boolean hasHttpBinding = false;
        if (binding != null) {
            List extElems = binding.getExtensibilityElements();
            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext()) {
                ExtensibilityElement ee =
                    (ExtensibilityElement) extIter.next();
                if (HTTPBinding.class.isInstance(ee)) {
                    if (hasHttpBinding) {
                        // We already found at least one SOAP Binding.
                        // That's bad.  There should be at most one SOAP
                        // binding.
                        throw new Exception(mMessages.getString("HttpSoapBindingDeployer.Multiple_Http_binding_elements"));
                    }
                    hasHttpBinding = true;
                }

            }
        }
        return hasHttpBinding;
    }   
        
    private static Port getPort(Definition def, String serviceName,
                                String endpointName) {
        Port port = null;
        
        // DO NOT use the getService() method.  It checks all imported WSDLs
        // which is bad for us in this case.
        //    Service svc = def.getService(QName.valueOf(serviceName));
        
        Map services = def.getServices();
        Service svc = (Service)services.get(QName.valueOf(serviceName));

        if (svc == null) {
            return null;
        }
        port = svc.getPort(QName.valueOf(endpointName).getLocalPart());

        if (port != null) {
            // Check that the soap:address doesn't have "REPLACE_WITH_ACTUAL_URL"
            Iterator elements = port.getExtensibilityElements().iterator();
            while (elements.hasNext()) {
                ExtensibilityElement element =
                    (ExtensibilityElement)elements.next();
                if (element instanceof SOAPAddress) {
                    SOAPAddress addr = (SOAPAddress)element;
                    String location = addr.getLocationURI();

                    // Only return the port if we have at least one SOAP Address
                    // that has a real URL
                    if (!location.equals("REPLACE_WITH_ACTUAL_URL"))
                        return port;
                }else if (element instanceof SOAP12Address) {
                    SOAP12Address addr = (SOAP12Address)element;
                    String location = addr.getLocationURI();

                    // Only return the port if we have at least one SOAP Address
                    // that has a real URL
                    if (!location.equals("REPLACE_WITH_ACTUAL_URL"))
                        return port;
                }else if (element instanceof HTTPAddress) {
                    return port;
                }
            }
            
        }
        return null;
    }

}
