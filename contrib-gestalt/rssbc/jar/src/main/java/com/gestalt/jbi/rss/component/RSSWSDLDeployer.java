/**
 *   rss-binding-component - RSS Binding Component
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
package com.gestalt.jbi.rss.component;

import com.gestalt.jbi.component.AbstractComponent;
import com.gestalt.jbi.component.manager.ServiceUnit;
import com.gestalt.jbi.component.manager.deployment.AbstractWSDLDeployer;
import com.gestalt.jbi.rss.extensions.*;

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


public class RSSWSDLDeployer extends AbstractWSDLDeployer {
    public RSSWSDLDeployer(AbstractComponent abstractComponent) {
        super(abstractComponent);
    }

    protected void addEndpoint(ServiceUnit serviceUnit, File wsdl,
        QName serviceName, String endpointName, QName interfaceName,
        MessageExchange.Role role) throws Exception {
        log.fine("Attempting to add an endpoint " + role);

        Definition def = getWsdlDefinition(wsdl);

        RSSBinding binding = getConcreteBinding(def, serviceName.toString(),
                endpointName, RSSBinding.class);

        if (binding == null) {
            return;
        }

        RSSAddress address = getConcreteAddress(def, serviceName.toString(),
                endpointName, RSSAddress.class);

        if (address == null) {
            return;
        }

        Map<QName, RSSOperation> operations = getRSSOperations(def,
                serviceName.toString(), endpointName);

        if ((operations == null) || (operations.size() == 0)) {
            return;
        }

        RSSEndpoint endpoint = createEndpoint(serviceName, interfaceName,
                endpointName, serviceUnit, wsdl, def, role);
        endpoint.setRSSAddress(address);
        endpoint.setRSSOperations(operations);
        setEndpointInputs(endpoint, operations.values());
        serviceUnit.addEndpoint(endpoint);
        log.fine("Added an endpoint " + role);
    }

    public ExtensionRegistry createExtensionRegistry() {
        return new RSSExtensionRegistry();
    }

    protected RSSEndpoint createEndpoint(QName serviceName,
        QName interfaceName, String endpointName, ServiceUnit serviceUnit,
        File wsdl, Definition def, MessageExchange.Role role)
        throws Exception {
        return new RSSEndpoint(serviceName, interfaceName, endpointName, role,
            getWsdlDocument(wsdl), def, serviceUnit);
    }

    /**
     * Gets the RSS Operations and sets the inputs as well
     */
    @SuppressWarnings("unchecked")
    protected Map<QName, RSSOperation> getRSSOperations(Definition definition,
        String serviceName, String endpointName) {
        Map<QName, RSSOperation> rssOperations = new HashMap<QName, RSSOperation>();
        Binding binding = getBinding(definition, serviceName, endpointName);

        if (binding != null) {
            List<BindingOperation> bindingOperations = (List<BindingOperation>) binding.getBindingOperations();

            for (BindingOperation bindingOperation : bindingOperations) {
                List<ExtensibilityElement> extensibilityElements = (List<ExtensibilityElement>) bindingOperation.getExtensibilityElements();

                for (ExtensibilityElement extensibilityElement : extensibilityElements) {
                    if (extensibilityElement instanceof RSSOperation) {
                        RSSOperation rssOperation = (RSSOperation) extensibilityElement;
                        BindingInput bindingInput = bindingOperation.getBindingInput();

                        if (bindingInput != null) {
                            List<ExtensibilityElement> bindingInputExtensibilityElements =
                                bindingInput.getExtensibilityElements();

                            for (ExtensibilityElement bindingInputExtensibilityElement : bindingInputExtensibilityElements) {
                                if (bindingInputExtensibilityElement instanceof RSSOperationInput) {
                                    RSSOperationInput rssOperationInput = (RSSOperationInput) bindingInputExtensibilityElement;
                                    rssOperation.setRSSOperationInput(rssOperationInput);
                                }
                            }
                        }

                        rssOperations.put(QName.valueOf(
                                bindingOperation.getName()), rssOperation);
                    }
                }
            }
        }

        return rssOperations;
    }

    protected void setEndpointInputs(RSSEndpoint endpoint,
        Collection<RSSOperation> operations) {
        for (RSSOperation op : operations) {
            endpoint.setRSSOperationInput(op, op.getRSSOperationInput());
        }
    }
}
