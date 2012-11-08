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

import com.gestalt.jbi.component.handler.AbstractMessageExchangeHandler;
import com.gestalt.jbi.component.manager.PooledProviderEndpoint;
import com.gestalt.jbi.component.manager.ServiceUnit;
import com.gestalt.jbi.uddi.component.uddi.UDDIUtils;
import com.gestalt.jbi.uddi.extensions.UDDIAddress;
import com.gestalt.jbi.uddi.extensions.UDDIBinding;
import com.gestalt.jbi.uddi.extensions.UDDIOperation;
import com.gestalt.jbi.uddi.extensions.UDDIOperationInput;
import com.gestalt.jbi.uddi.extensions.UDDIOperationOutput;

import org.uddi4j.client.UDDIProxy;

import org.w3c.dom.Document;

import java.util.HashMap;
import java.util.Map;

import javax.jbi.messaging.MessageExchange.Role;

import javax.wsdl.Definition;

import javax.xml.namespace.QName;


public class UDDIEndpoint extends PooledProviderEndpoint {
    private static UDDIUtils uddiUtils = new UDDIUtils();
    private UDDIProxy inquiryProxy;
    private UDDIBinding uddiBinding;
    private UDDIAddress uddiAddress;
    private Map<QName, UDDIOperation> uddiOperations;
    private Map<UDDIOperation, UDDIOperationInput> uddiOperationInputs = new HashMap<UDDIOperation, UDDIOperationInput>();
    private Map<UDDIOperation, UDDIOperationOutput> uddiOperationOutputs = new HashMap<UDDIOperation, UDDIOperationOutput>();

    public UDDIEndpoint(QName serviceName, QName interfaceName,
        String endpointName, Role role, Document serviceDescription,
        Definition def, ServiceUnit serviceUnit) {
        super(serviceName, interfaceName, endpointName, role,
            serviceDescription, def, serviceUnit);
    }

    public void setUDDIBinding(UDDIBinding uddiBinding) {
        this.uddiBinding = uddiBinding;
    }

    public UDDIBinding getUDDIBinding() {
        return uddiBinding;
    }

    public UDDIProxy getInquiryProxy() {
        return inquiryProxy;
    }

    public void setUDDIAddress(UDDIAddress uddiAddress) {
        this.uddiAddress = uddiAddress;
    }

    public UDDIAddress getUDDIAddress() {
        return uddiAddress;
    }

    public void setUDDIOperations(Map<QName, UDDIOperation> uddiOperations) {
        this.uddiOperations = uddiOperations;
    }

    public Map<QName, UDDIOperation> getUDDIOperations() {
        return uddiOperations;
    }

    public void setUDDIOperationOutput(UDDIOperation uddiOperation,
        UDDIOperationOutput uddiOperationOutput) {
        uddiOperationOutputs.put(uddiOperation, uddiOperationOutput);
    }

    public UDDIOperationOutput getUDDIOperationOutput(
        UDDIOperation uddiOperation) {
        return uddiOperationOutputs.get(uddiOperation);
    }

    public void setUDDIOperationInput(UDDIOperation uddiOperation,
        UDDIOperationInput uddiOperationInput) {
        uddiOperationInputs.put(uddiOperation, uddiOperationInput);
    }

    public UDDIOperationInput getUDDIOperationInput(UDDIOperation uddiOperation) {
        return uddiOperationInputs.get(uddiOperation);
    }

    public Map<UDDIOperation, UDDIOperationInput> getUddiOperationInputs() {
        return uddiOperationInputs;
    }

    public void setUDDIOperationInputs(
        Map<UDDIOperation, UDDIOperationInput> uddiOperationInputs) {
        this.uddiOperationInputs = uddiOperationInputs;
    }

    public Map<UDDIOperation, UDDIOperationOutput> getUddiOperationOutputs() {
        return uddiOperationOutputs;
    }

    public void setUddiOperationOutputs(
        Map<UDDIOperation, UDDIOperationOutput> uddiOperationOutputs) {
        this.uddiOperationOutputs = uddiOperationOutputs;
    }

    public Map<QName, UDDIOperation> getOperations() {
        return uddiOperations;
    }

    @Override
    public void activate() throws Exception {
        super.activate();
        inquiryProxy = ((UDDIComponentLifeCycle) serviceUnit.getComponent()
                        .getLifeCycle()).getUDDIConnectionManager()
                        .createInquiryProxy(uddiAddress.getInquiryUri(), this);
    }

    @Override
    public void deactivate() throws Exception {
        ((UDDIComponentLifeCycle) serviceUnit.getComponent().getLifeCycle()).getUDDIConnectionManager()
         .destroyProxy(uddiAddress.getInquiryUri(), this);
        super.deactivate();
    }

    @Override
    public AbstractMessageExchangeHandler createConsumerExchangeHandler() {
        return null;
    }

    @Override
    public AbstractMessageExchangeHandler createProviderExchangeHandler() {
        UDDIProviderHandler uddiProviderHandler = new UDDIProviderHandler(this);
        uddiProviderHandler.setUddiUtils(uddiUtils);

        return uddiProviderHandler;
    }
}
