/**
 *   sip-binding-component-extensions - Extensions for the SIP Binding Component
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
package com.gestalt.jbi.sip.extensions;

import com.ibm.wsdl.Constants;
import com.ibm.wsdl.util.xml.DOMUtils;

import org.w3c.dom.Element;

import java.io.PrintWriter;
import java.io.Serializable;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.wsdl.*;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;

import javax.xml.namespace.QName;

import com.sun.jbi.internationalization.Messages;


/**
 * @author : csturtz
 */
public class SIPExtSerializer implements ExtensionSerializer,
    ExtensionDeserializer, Serializable {
    private static final Logger log = Messages.getLogger(SIPExtSerializer.class);
    private Messages messages = Messages.getMessages(SIPExtSerializer.class);

    public SIPExtSerializer() {
    }

    /**
     * Registers the serializers / deserializers
     */
    public void registerSerializer(final ExtensionRegistry registry) {
        registry.registerSerializer(Binding.class, SIPBinding.QNAME_BINDING,
            this);
        registry.registerDeserializer(Binding.class, SIPBinding.QNAME_BINDING,
            this);
        registry.mapExtensionTypes(Binding.class, SIPBinding.QNAME_BINDING,
            SIPBinding.class);

        registry.registerSerializer(Port.class, SIPAddress.QNAME_ADDRESS, this);
        registry.registerDeserializer(Port.class, SIPAddress.QNAME_ADDRESS, this);
        registry.mapExtensionTypes(Port.class, SIPAddress.QNAME_ADDRESS,
            SIPAddress.class);

        registry.registerSerializer(BindingOperation.class,
            SIPOperation.QNAME_OPERATION, this);
        registry.registerDeserializer(BindingOperation.class,
            SIPOperation.QNAME_OPERATION, this);
        registry.mapExtensionTypes(BindingOperation.class,
            SIPOperation.QNAME_OPERATION, SIPOperation.class);

        registry.registerSerializer(BindingInput.class,
            SIPOperationInput.QNAME_OPERATION_INPUT, this);
        registry.registerDeserializer(BindingInput.class,
            SIPOperationInput.QNAME_OPERATION_INPUT, this);
        registry.mapExtensionTypes(BindingInput.class,
            SIPOperationInput.QNAME_OPERATION_INPUT, SIPOperationInput.class);

        registry.registerSerializer(BindingOutput.class,
            SIPOperationOutput.QNAME_OPERATION_OUTPUT, this);
        registry.registerDeserializer(BindingOutput.class,
            SIPOperationOutput.QNAME_OPERATION_OUTPUT, this);
        registry.mapExtensionTypes(BindingOutput.class,
            SIPOperationOutput.QNAME_OPERATION_OUTPUT, SIPOperationOutput.class);
    }

    public void marshall(Class aClass, QName qName,
        ExtensibilityElement extensibilityElement, PrintWriter printWriter,
        Definition definition, ExtensionRegistry extensionRegistry)
        throws WSDLException {
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE, "Marshalling QNAME: " + qName);
        }
        if (extensibilityElement == null) {
            return;
        } else if (extensibilityElement instanceof SIPBinding) {
            marshallSIPBinding((SIPBinding) extensibilityElement, definition,
                printWriter);
        } else if (extensibilityElement instanceof SIPAddress) {
            marshallSIPAddress((SIPAddress) extensibilityElement, definition,
                printWriter);
        }
    }

    private void marshallSIPBinding(final SIPBinding sipBinding,
        Definition definition, PrintWriter printWriter)
        throws WSDLException {
        printWriter.print("      <sip:binding");

        final Boolean required = sipBinding.getRequired();

        if (required != null) {
            DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED,
                required.toString(), definition, printWriter);
        }

        printWriter.println("/>");
    }

    private void marshallSIPAddress(final SIPAddress sipAddress,
        Definition definition, PrintWriter printWriter)
        throws WSDLException {
        printWriter.print("      <sip:address");

        final Boolean required = sipAddress.getRequired();

        if (required != null) {
            DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED,
                required.toString(), definition, printWriter);
        }

        try {
            final String proxydomain = sipAddress.getProxydomain();

            if (proxydomain != null) {
                DOMUtils.printAttribute(SIPAddress.Attributes.proxyDomain.toString(),
                    proxydomain, printWriter);
            }

            final Integer proxyport = sipAddress.getProxyport();

            if (proxyport != null) {
                DOMUtils.printAttribute(SIPAddress.Attributes.proxyPort.toString(),
                    String.valueOf(proxyport), printWriter);
            }

            final Integer proxytimeout = sipAddress.getProxytimeout();

            if (proxytimeout != null) {
                DOMUtils.printAttribute(SIPAddress.Attributes.proxyTimeout.toString(),
                    String.valueOf(proxytimeout), printWriter);
            }

            final String username = sipAddress.getUsername();

            if (username != null) {
                DOMUtils.printAttribute(SIPAddress.Attributes.username.toString(),
                    username, printWriter);
            }

            final String password = sipAddress.getPassword();

            if (password != null) {
                DOMUtils.printAttribute(SIPAddress.Attributes.password.toString(),
                    password, printWriter);
            }
        } catch (final Exception e) {
            throw new WSDLException(WSDLException.PARSER_ERROR,
                "Failed to marshal location attribute", e);
        }

        printWriter.println("/>");
    }

    public ExtensibilityElement unmarshall(Class aClass, QName qName,
        Element element, Definition definition,
        ExtensionRegistry extensionRegistry) throws WSDLException {
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Unmarshalling QNAME: " + qName);
        }
        if (SIPBinding.QNAME_BINDING.equals(qName)) {
            return new SIPBinding();
        } else if (SIPAddress.QNAME_ADDRESS.equals(qName)) {
            return unmarshallSIPAddress(element);
        } else if (SIPOperation.QNAME_OPERATION.equals(qName)) {
            return unmarshallSIPOperation(element);
        } else if (SIPOperationOutput.QNAME_OPERATION_OUTPUT.equals(qName)) {
            return unmarshallSIPOperationOutput(element);
        } else if (SIPOperationInput.QNAME_OPERATION_INPUT.equals(qName)) {
            return unmarshallSIPOperationInput(element);
        }

        return null;
    }

    private SIPAddress unmarshallSIPAddress(final Element element)
        throws WSDLException {
        final SIPAddress sipAddress = new SIPAddress();

        final String proxydomain = DOMUtils.getAttribute(element,
                SIPAddress.Attributes.proxyDomain.toString());

        if (proxydomain != null) {
            sipAddress.setProxydomain(proxydomain);
        }

        final String proxyport = DOMUtils.getAttribute(element,
                SIPAddress.Attributes.proxyPort.toString());

        if (proxyport != null) {
            try {
                sipAddress.setProxyport(Integer.valueOf(proxyport));
            } catch (NumberFormatException nfe) {
                throw new WSDLException(WSDLException.PARSER_ERROR,
                    "Failed to unmarshal " + SIPAddress.Attributes.proxyPort);
            }
        }

        final String proxytimeout = DOMUtils.getAttribute(element,
                SIPAddress.Attributes.proxyTimeout.toString());

        if (proxytimeout != null) {
            try {
                sipAddress.setProxytimeout(Integer.valueOf(proxytimeout));
            } catch (NumberFormatException nfe) {
                log.log(Level.WARNING,
                    messages.getString("SIPBC-W00100.exceptionParsingAttribute",
                    SIPAddress.Attributes.proxyTimeout.toString()));
                sipAddress.setProxytimeout(5000);
            }
        }

        final String username = DOMUtils.getAttribute(element,
                SIPAddress.Attributes.username.toString());

        if (username != null) {
            sipAddress.setUsername(username);
        }

        final String password = DOMUtils.getAttribute(element,
                SIPAddress.Attributes.password.toString());

        if (password != null) {
            sipAddress.setPassword(password);
        }

        return sipAddress;
    }

    private ExtensibilityElement unmarshallSIPOperation(final Element element) {
        final SIPOperation op = new SIPOperation();

        final String operationName = DOMUtils.getAttribute(element,
                SIPOperation.Attributes.name.toString());

        if (operationName != null) {
            op.setOperationName(SIPOperation.OperationName.valueOf(
                    operationName));
        }

        return op;
    }

    private SIPOperationInput unmarshallSIPOperationInput(final Element element) {
        final SIPOperationInput sipOperationInput = new SIPOperationInput();

        final String requestMethod = DOMUtils.getAttribute(element,
                SIPOperationInput.Attributes.requestMethod.toString());

        if (requestMethod != null) {
            sipOperationInput.setRequestMethod(requestMethod);
        }

        final String responseMethod = DOMUtils.getAttribute(element,
                SIPOperationInput.Attributes.responseMethod.toString());

        if (responseMethod != null) {
            sipOperationInput.setResponseMethod(responseMethod);
        }

        final String responseStatus = DOMUtils.getAttribute(element,
                SIPOperationInput.Attributes.responseStatus.toString());

        if (responseStatus != null) {
            sipOperationInput.setResponseStatus(responseStatus);
        }

        final String remoteUri = DOMUtils.getAttribute(element,
                SIPOperationInput.Attributes.remoteUri.toString());

        if (remoteUri != null) {
            sipOperationInput.setRemoteUri(remoteUri);
        }

        final String content = DOMUtils.getAttribute(element,
                SIPOperationInput.Attributes.content.toString());

        if (content != null) {
            sipOperationInput.setContent(content);
        }

        return sipOperationInput;
    }

    private SIPOperationOutput unmarshallSIPOperationOutput(
        final Element element) {
        return new SIPOperationOutput();
    }
}
