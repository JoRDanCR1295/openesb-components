/**
 *   uddi-binding-component - UDDI Binding Component
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
package com.gestalt.jbi.uddi.component;

import com.gestalt.jbi.component.AbstractComponent;
import com.gestalt.jbi.component.manager.ServiceUnit;
import com.gestalt.jbi.component.manager.deployment.AbstractWSDLDeployer;
import com.gestalt.jbi.uddi.extensions.UDDIAddress;
import com.gestalt.jbi.uddi.extensions.UDDIBinding;
import com.gestalt.jbi.uddi.extensions.UDDIExtensionRegistry;
import com.gestalt.jbi.uddi.extensions.UDDIOperation;
import com.gestalt.jbi.uddi.extensions.UDDIOperationInput;

import java.io.File;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.jbi.messaging.MessageExchange;

import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.Definition;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;

import javax.xml.namespace.QName;


public class UDDIWSDLDeployer extends AbstractWSDLDeployer {
    public UDDIWSDLDeployer(AbstractComponent component) {
        super(component);
    }

    protected void addEndpoint(ServiceUnit serviceUnit, File wsdl,
        QName serviceName, String endpointName, QName interfaceName,
        MessageExchange.Role role) throws Exception {
        log.fine("Attempting to add an endpoint " + role);

        Definition def = getWsdlDefinition(wsdl);

        UDDIBinding binding = getConcreteBinding(def, serviceName.toString(),
                endpointName, UDDIBinding.class);

        if (binding == null) {
            return;
        }

        UDDIAddress address = getConcreteAddress(def, serviceName.toString(),
                endpointName, UDDIAddress.class);

        if (address == null) {
            return;
        }

        Map<QName, UDDIOperation> operations = getUDDIOperations(def,
                serviceName.toString(), endpointName);

        if ((operations == null) || (operations.size() == 0)) {
            return;
        }

        UDDIEndpoint endpoint = createEndpoint(serviceName, interfaceName,
                endpointName, serviceUnit, wsdl, def, role);
        endpoint.setUDDIBinding(binding);
        endpoint.setUDDIAddress(address);
        endpoint.setUDDIOperations(operations);
        setEndpointInputs(endpoint, operations.values());
        serviceUnit.addEndpoint(endpoint);
        log.fine("Added an endpoint " + role);
    }

    public ExtensionRegistry createExtensionRegistry() {
        return new UDDIExtensionRegistry();
    }

    protected UDDIEndpoint createEndpoint(QName serviceName,
        QName interfaceName, String endpointName, ServiceUnit serviceUnit,
        File wsdl, Definition def, MessageExchange.Role role)
        throws Exception {
        return new UDDIEndpoint(serviceName, interfaceName, endpointName, role,
            getWsdlDocument(wsdl), def, serviceUnit);
    }

    @SuppressWarnings("unchecked")
    protected Map<QName, UDDIOperation> getUDDIOperations(
        Definition definition, String serviceName, String endpointName) {
        Map<QName, UDDIOperation> uddiOperations = new HashMap<QName, UDDIOperation>();
        Binding binding = getBinding(definition, serviceName, endpointName);

        if (binding != null) {
            List<BindingOperation> bindingOperations = (List<BindingOperation>) binding.getBindingOperations();

            for (BindingOperation bindingOperation : bindingOperations) {
                List<ExtensibilityElement> extensibilityElements = bindingOperation.getExtensibilityElements();

                for (ExtensibilityElement extensibilityElement : extensibilityElements) {
                    if (extensibilityElement instanceof UDDIOperation) {
                        UDDIOperation uddiOperation = (UDDIOperation) extensibilityElement;
                        BindingInput bindingInput = bindingOperation.getBindingInput();

                        if (bindingInput != null) {
                            List<ExtensibilityElement> bindingInputExtensibilityElements =
                                bindingInput.getExtensibilityElements();

                            for (ExtensibilityElement bindingInputExtensibilityElement : bindingInputExtensibilityElements) {
                                if (bindingInputExtensibilityElement instanceof UDDIOperationInput) {
                                    UDDIOperationInput uddiOperationInput = (UDDIOperationInput) bindingInputExtensibilityElement;
                                    uddiOperation.setUDDIOperationInput(uddiOperationInput);
                                }
                            }
                        }

                        uddiOperations.put(QName.valueOf(
                                bindingOperation.getName()), uddiOperation);
                    }
                }
            }
        }

        return uddiOperations;
    }

    protected void setEndpointInputs(UDDIEndpoint endpoint,
        Collection<UDDIOperation> operations) {
        for (UDDIOperation op : operations) {
            endpoint.setUDDIOperationInput(op, op.getUDDIOperationInput());
        }
    }
}
