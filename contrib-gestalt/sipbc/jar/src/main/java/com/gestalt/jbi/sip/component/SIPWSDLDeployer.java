/**
 *   sip-binding-component - SIP Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com/
 *
 *   This library is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU Lesser General Public
 *   License version 2.1 as published by the Free Software Foundation.
 *
 *   This library is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *   Lesser General Public License for more details.
 *
 *   You should have received a copy of the GNU Lesser General Public
 *   License along with this library; if not, write to the Free Software
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
 */
package com.gestalt.jbi.sip.component;

import com.gestalt.jbi.component.AbstractComponent;
import com.gestalt.jbi.component.manager.ServiceUnit;
import com.gestalt.jbi.component.manager.deployment.AbstractWSDLDeployer;
import com.gestalt.jbi.sip.extensions.SIPAddress;
import com.gestalt.jbi.sip.extensions.SIPBinding;
import com.gestalt.jbi.sip.extensions.SIPExtensionRegistry;
import com.gestalt.jbi.sip.extensions.SIPOperation;
import com.gestalt.jbi.sip.extensions.SIPOperationInput;
import com.gestalt.jbi.sip.extensions.SIPOperationOutput;
import com.sun.jbi.internationalization.Messages;

import java.io.File;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.jbi.messaging.MessageExchange;

import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;

import javax.xml.namespace.QName;


public class SIPWSDLDeployer extends AbstractWSDLDeployer {
    private static final Logger log = Messages.getLogger(SIPWSDLDeployer.class);

    public SIPWSDLDeployer(AbstractComponent abstractComponent) {
        super(abstractComponent);
    }

    protected void addEndpoint(ServiceUnit serviceUnit, File wsdl,
        QName serviceName, String endpointName, QName interfaceName,
        MessageExchange.Role role) throws Exception {

        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Attempting to add an endpoint " + role);
        }

        Definition def = getWsdlDefinition(wsdl);

        SIPBinding binding = getConcreteBinding(def, serviceName.toString(),
                endpointName, SIPBinding.class);

        if (binding == null) {
            return;
        }

        SIPAddress address = getConcreteAddress(def, serviceName.toString(),
                endpointName, SIPAddress.class);

        if (address == null) {
            return;
        }

        Map<QName, SIPOperation> operations = getSIPOperations(def,
                serviceName.toString(), endpointName);

        if ((operations == null) || (operations.size() == 0)) {
            return;
        }

        SIPEndpoint endpoint = createEndpoint(serviceName, interfaceName,
                endpointName, serviceUnit, wsdl, def, role);
        endpoint.setSIPBinding(binding);
        endpoint.setSIPAddress(address);
        endpoint.setSIPOperations(operations);
        setEndpointInputsOutputs(endpoint, operations.values());
        serviceUnit.addEndpoint(endpoint);
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Added an endpoint " + role);
        }
    }

    public ExtensionRegistry createExtensionRegistry() {
        return new SIPExtensionRegistry();
    }

    protected SIPEndpoint createEndpoint(QName serviceName,
        QName interfaceName, String endpointName, ServiceUnit serviceUnit,
        File wsdl, Definition def, MessageExchange.Role role)
        throws Exception {
        return new SIPEndpoint(serviceName, interfaceName, endpointName, role,
            getWsdlDocument(wsdl), def, serviceUnit);
    }

    @SuppressWarnings("unchecked")
    protected Map<QName, SIPOperation> getSIPOperations(Definition definition,
        String serviceName, String endpointName) {
        Map<QName, SIPOperation> sipOperations = new HashMap<QName, SIPOperation>();
        Binding binding = getBinding(definition, serviceName, endpointName);

        if (binding != null) {
            List<BindingOperation> bindingOperations = (List<BindingOperation>) binding.getBindingOperations();

            for (BindingOperation bindingOperation : bindingOperations) {
                List<ExtensibilityElement> extensibilityElements = (List<ExtensibilityElement>) bindingOperation.getExtensibilityElements();

                for (ExtensibilityElement extensibilityElement : extensibilityElements) {
                    if (extensibilityElement instanceof SIPOperation) {
                        SIPOperation sipOperation = (SIPOperation) extensibilityElement;
                        BindingInput bindingInput = bindingOperation.getBindingInput();

                        if (bindingInput != null) {
                            List<ExtensibilityElement> bindingInputExtensibilityElements =
                                (List<ExtensibilityElement>) bindingInput.getExtensibilityElements();

                            for (ExtensibilityElement bindingInputExtensibilityElement : bindingInputExtensibilityElements) {
                                if (bindingInputExtensibilityElement instanceof SIPOperationInput) {
                                    SIPOperationInput sipOperationInput = (SIPOperationInput) bindingInputExtensibilityElement;
                                    sipOperation.setSIPOperationInput(sipOperationInput);
                                }
                            }
                        }

                        BindingOutput bindingOutput = bindingOperation.getBindingOutput();

                        if (bindingOutput != null) {
                            List<ExtensibilityElement> bindingOutputExtensibilityElements =
                                (List<ExtensibilityElement>) bindingOutput.getExtensibilityElements();

                            for (ExtensibilityElement bindingOutputExtensibilityElement : bindingOutputExtensibilityElements) {
                                if (bindingOutputExtensibilityElement instanceof SIPOperationOutput) {
                                    SIPOperationOutput sipOperationOutput = (SIPOperationOutput) bindingOutputExtensibilityElement;
                                    sipOperation.setSipOperationOutput(sipOperationOutput);
                                }
                            }
                        }

                        sipOperations.put(QName.valueOf(
                                bindingOperation.getName()), sipOperation);
                    }
                }
            }
        }

        return sipOperations;
    }

    protected void setEndpointInputsOutputs(SIPEndpoint endpoint,
        Collection<SIPOperation> operations) {
        for (SIPOperation op : operations) {
            endpoint.setSIPOperationInput(op, op.getSIPOperationInput());
            endpoint.setSIPOperationOutput(op, op.getSipOperationOutput());
        }
    }
}
