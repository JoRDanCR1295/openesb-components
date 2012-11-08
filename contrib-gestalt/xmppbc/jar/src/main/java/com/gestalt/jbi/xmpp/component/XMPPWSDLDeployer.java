/**
 *   xmpp-binding-component - XMPP Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com
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
package com.gestalt.jbi.xmpp.component;

import com.gestalt.jbi.component.AbstractComponent;
import com.gestalt.jbi.component.manager.ServiceUnit;
import com.gestalt.jbi.component.manager.deployment.AbstractWSDLDeployer;
import com.gestalt.jbi.xmpp.extensions.XMPPAddress;
import com.gestalt.jbi.xmpp.extensions.XMPPBinding;
import com.gestalt.jbi.xmpp.extensions.XMPPExtensionRegistry;
import com.gestalt.jbi.xmpp.extensions.XMPPOperation;
import com.gestalt.jbi.xmpp.extensions.XMPPOperationInput;
import com.gestalt.jbi.xmpp.extensions.XMPPOperationOutput;

import java.io.File;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.jbi.messaging.MessageExchange;

import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;

import javax.xml.namespace.QName;


public class XMPPWSDLDeployer extends AbstractWSDLDeployer {
    public XMPPWSDLDeployer(AbstractComponent abstractComponent) {
        super(abstractComponent);
    }

    protected void addEndpoint(ServiceUnit serviceUnit, File wsdl,
        QName serviceName, String endpointName, QName interfaceName,
        MessageExchange.Role role) throws Exception {
        log.fine("Attempting to add an endpoint " + role);
        log.fine("adding endpoint using XMPPWSDLDeployer...");
        Definition def = getWsdlDefinition(wsdl);

        XMPPBinding binding = getConcreteBinding(def, serviceName.toString(),
                endpointName, XMPPBinding.class);

        if (binding == null) {
            return;
        }

        XMPPAddress address = getConcreteAddress(def, serviceName.toString(),
                endpointName, XMPPAddress.class);

        if (address == null) {
            return;
        }

        Map<QName, XMPPOperation> operations = getXMPPOperations(def,
                serviceName.toString(), endpointName);

        if ((operations == null) || (operations.size() == 0)) {
            return;
        }

        XMPPEndpoint endpoint = createEndpoint(serviceName, interfaceName,
                endpointName, serviceUnit, wsdl, def, role);
        endpoint.setXMPPBinding(binding);
        endpoint.setXMPPAddress(address);
        endpoint.setXMPPOperations(operations);
        setEndpointInputsOutputs(endpoint, operations.values());
        serviceUnit.addEndpoint(endpoint);
        log.fine("Added an endpoint " + role);
    }

    public ExtensionRegistry createExtensionRegistry() {
        return new XMPPExtensionRegistry();
    }

    protected XMPPEndpoint createEndpoint(QName serviceName,
        QName interfaceName, String endpointName, ServiceUnit serviceUnit,
        File wsdl, Definition def, MessageExchange.Role role)
        throws Exception {
        return new XMPPEndpoint(serviceName, interfaceName, endpointName, role,
            getWsdlDocument(wsdl), def, serviceUnit, false);
    }

    @SuppressWarnings("unchecked")
    protected Map<QName, XMPPOperation> getXMPPOperations(
        Definition definition, String serviceName, String endpointName) {
        Map<QName, XMPPOperation> xmppOperations = new HashMap<QName, XMPPOperation>();
        Binding binding = getBinding(definition, serviceName, endpointName);

        if (binding != null) {
            List<BindingOperation> bindingOperations = (List<BindingOperation>) binding.getBindingOperations();

            for (BindingOperation bindingOperation : bindingOperations) {
                List<ExtensibilityElement> extensibilityElements = (List<ExtensibilityElement>) bindingOperation.getExtensibilityElements();

                for (ExtensibilityElement extensibilityElement : extensibilityElements) {
                    if (extensibilityElement instanceof XMPPOperation) {
                        XMPPOperation xmppOperation = (XMPPOperation) extensibilityElement;
                        BindingInput bindingInput = bindingOperation.getBindingInput();

                        if (bindingInput != null) {
                            List<ExtensibilityElement> bindingInputExtensibilityElements =
                                bindingInput.getExtensibilityElements();

                            for (ExtensibilityElement bindingInputExtensibilityElement : bindingInputExtensibilityElements) {
                                if (bindingInputExtensibilityElement instanceof XMPPOperationInput) {
                                    XMPPOperationInput xmppOperationInput = (XMPPOperationInput) bindingInputExtensibilityElement;
                                    xmppOperation.setXMPPOperationInput(xmppOperationInput);
                                }
                            }
                        }

                        BindingOutput bindingOutput = bindingOperation.getBindingOutput();

                        if (bindingOutput != null) {
                            List<ExtensibilityElement> bindingOutputExtensibilityElements =
                                bindingOutput.getExtensibilityElements();

                            for (ExtensibilityElement bindingOutputExtensibilityElement : bindingOutputExtensibilityElements) {
                                if (bindingOutputExtensibilityElement instanceof XMPPOperationOutput) {
                                    XMPPOperationOutput xmppOperationOutput = (XMPPOperationOutput) bindingOutputExtensibilityElement;
                                    xmppOperation.setXMPPOperationOutput(xmppOperationOutput);
                                }
                            }
                        }

                        xmppOperations.put(QName.valueOf(
                                bindingOperation.getName()), xmppOperation);
                    }
                }
            }
        }

        return xmppOperations;
    }

    protected void setEndpointInputsOutputs(XMPPEndpoint endpoint,
        Collection<XMPPOperation> operations) {
        for (XMPPOperation op : operations) {
            endpoint.setXMPPOperationInput(op, op.getXMPPOperationInput());
            endpoint.setXMPPOperationOutput(op, op.getXMPPOperationOutput());
        }
    }
}
