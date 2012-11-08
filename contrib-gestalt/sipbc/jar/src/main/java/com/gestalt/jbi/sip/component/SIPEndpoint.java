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

import com.gestalt.jbi.component.handler.AbstractMessageExchangeHandler;
import com.gestalt.jbi.component.manager.PooledEndpoint;
import com.gestalt.jbi.component.manager.ServiceUnit;
import com.gestalt.jbi.sip.SIPConnection;
import com.gestalt.jbi.sip.SIPConnectionManager;
import com.gestalt.jbi.sip.SIPUser;
import com.gestalt.jbi.sip.extensions.SIPAddress;
import com.gestalt.jbi.sip.extensions.SIPBinding;
import com.gestalt.jbi.sip.extensions.SIPOperation;
import com.gestalt.jbi.sip.extensions.SIPOperationInput;
import com.gestalt.jbi.sip.extensions.SIPOperationOutput;
import com.sun.jbi.internationalization.Messages;

import org.w3c.dom.Document;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.MessageExchange;

import javax.wsdl.Definition;

import javax.xml.namespace.QName;


/**
 * @author : csturtz
 */
public class SIPEndpoint extends PooledEndpoint {
    private static final Logger log = Messages.getLogger(SIPEndpoint.class);
    private static Messages messages = Messages.getMessages(SIPEndpoint.class);

    SIPConnection sipConnection;
    SIPUser sipUser;

    private SIPBinding sipBinding;
    private SIPAddress sipAddress;
    private Map<QName, SIPOperation> sipOperations = new HashMap<QName, SIPOperation>();
    private Map<SIPOperation, SIPOperationInput> sipOperationInputs = new HashMap<SIPOperation, SIPOperationInput>();
    private Map<SIPOperation, SIPOperationOutput> sipOperationOutputs = new HashMap<SIPOperation, SIPOperationOutput>();

    public SIPEndpoint(QName qName, QName qName1, String s,
        MessageExchange.Role role, Document document, Definition definition,
        ServiceUnit serviceUnit) {
        super(qName, qName1, s, role, document, definition, serviceUnit);
    }

    public SIPConnection getSipConnection() {
        return this.sipConnection;
    }

    public void activate() throws Exception {
        log.log(Level.INFO,messages.getString("SIPBC-R00422.activatingASIPBCEndpoint"));

        super.activate();

        sipUser = SIPUser.getSIPUser(sipAddress.getUsername(),
                sipAddress.getPassword(), sipAddress.getProxydomain(),
                sipAddress.getProxyport(), sipAddress.getProxytimeout());

        SIPConnectionManager sipConnectionManager = SIPConnectionManager.getInstance();
        sipConnection = sipConnectionManager.createConnection(sipUser);

        if (sipConnection == null) {
            String msg = messages.getString("SIPBC-E00423.SIPBCEndpointNotSuccessfullyActivatedPleaseUndeploy");
            log.log(Level.SEVERE, msg);
            throw new Exception(msg);
        } else if (!sipConnection.isRegistered()) {
            String msg = messages.getString("SIPBC-E00424.SIPBCWasUnableToRegisterDefinedUserPleaseUndeploy");
            log.log(Level.SEVERE, msg);
            throw new Exception(msg);
        }

        // IMPORTANT - This must be called after the sipConnection is
        // established!
        getProcessor();
    }

    /**
     * For some reason when throwing an exception up the stack to
     * AbstractServiceUnitManager.stop(), netbeans continues to fail undeploy.
     * Therefore catch all exceptions and log them here.
     *
     * @throws Exception
     */
    public void deactivate() throws Exception {
        log.log(Level.INFO,messages.getString("SIPBC-R00425.dectivatingASIPBCEndpoint"));
        try {
            boolean removed = SIPConnectionManager.getInstance()
                                .removeConnection(sipUser,
                                sipConnection.getSipStack());

            sipConnection = null;
            sipUser = null;
            processor = null;

            if (!removed) {
                log.log(Level.WARNING,
                    messages.getString("SIPBC-W00426.SIPBCWasUnableToUnregisterWhenDeactivatingTheEndpoint"));
            }

            super.deactivate();
        } catch (Exception e) {
            log.log(Level.WARNING,
                    messages.getString("SIPBC-W00427.anExceptionWasThrownWhileDeactivatingTheEndpoint"),e);
        }
    }

    public Map<QName, SIPOperation> getOperations() {
        return sipOperations;
    }

    public AbstractMessageExchangeHandler createConsumerExchangeHandler() {
        return new SIPConsumerHandler(this);
    }

    public AbstractMessageExchangeHandler createProviderExchangeHandler() {
        return new SIPProviderHandler(this);
    }

    public SIPBinding getSIPBinding() {
        return sipBinding;
    }

    public void setSIPBinding(SIPBinding sipBinding) {
        this.sipBinding = sipBinding;
    }

    public SIPAddress getSIPAddress() {
        return sipAddress;
    }

    public void setSIPAddress(SIPAddress sipAddress) {
        this.sipAddress = sipAddress;
    }

    public Map<QName, SIPOperation> getSIPOperations() {
        return sipOperations;
    }

    public void setSIPOperations(Map<QName, SIPOperation> sipOperations) {
        this.sipOperations = sipOperations;
    }

    public Map<SIPOperation, SIPOperationInput> getSIPOperationInputs() {
        return sipOperationInputs;
    }

    public void setSIPOperationInputs(
        Map<SIPOperation, SIPOperationInput> sipOperationInputs) {
        this.sipOperationInputs = sipOperationInputs;
    }

    public void setSIPOperationInput(SIPOperation sipOperation,
        SIPOperationInput sipOperationInput) {
        this.sipOperationInputs.put(sipOperation, sipOperationInput);
    }

    public SIPOperationInput getSIPOperationInput(SIPOperation sipOperation) {
        return this.sipOperationInputs.get(sipOperation);
    }

    public Map<SIPOperation, SIPOperationOutput> getSIPOperationOutputs() {
        return sipOperationOutputs;
    }

    public void setSIPOperationOutputs(
        Map<SIPOperation, SIPOperationOutput> sipOperationOutputs) {
        this.sipOperationOutputs = sipOperationOutputs;
    }

    public void setSIPOperationOutput(SIPOperation sipOperation,
        SIPOperationOutput sipOperationOutput) {
        this.sipOperationOutputs.put(sipOperation, sipOperationOutput);
    }

    public SIPOperationOutput getSIPOperationOutput(SIPOperation sipOperation) {
        return this.sipOperationOutputs.get(sipOperation);
    }
}
