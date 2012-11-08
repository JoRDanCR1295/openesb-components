/**
 *   xmpp-binding-component-extensions - Extensions for the XMPP Binding Component
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
package com.gestalt.jbi.xmpp.extensions;

import com.ibm.wsdl.Constants;
import com.ibm.wsdl.util.xml.DOMUtils;

import org.w3c.dom.Element;

import java.io.PrintWriter;
import java.io.Serializable;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;

import javax.xml.namespace.QName;


/**
 * @author rriven
 */
public class XMPPExtSerializer implements ExtensionSerializer,
    ExtensionDeserializer, Serializable {
    private static final long serialVersionUID = 1L;
    private static final Logger log = Logger.getLogger(XMPPExtSerializer.class.getName());

    /**
     * Registers the serializers / deserializers
     */
    public void registerSerializer(final ExtensionRegistry registry) {
        registry.registerSerializer(Binding.class, XMPPBinding.QNAME_BINDING,
            this);
        registry.registerDeserializer(Binding.class, XMPPBinding.QNAME_BINDING,
            this);
        registry.mapExtensionTypes(Binding.class, XMPPBinding.QNAME_BINDING,
            XMPPBinding.class);

        registry.registerSerializer(Port.class, XMPPAddress.QNAME_ADDRESS, this);
        registry.registerDeserializer(Port.class, XMPPAddress.QNAME_ADDRESS,
            this);
        registry.mapExtensionTypes(Port.class, XMPPAddress.QNAME_ADDRESS,
            XMPPAddress.class);

        registry.registerSerializer(BindingOperation.class,
            XMPPOperation.QNAME_OPERATION, this);
        registry.registerDeserializer(BindingOperation.class,
            XMPPOperation.QNAME_OPERATION, this);
        registry.mapExtensionTypes(BindingOperation.class,
            XMPPOperation.QNAME_OPERATION, XMPPOperation.class);

        registry.registerSerializer(BindingInput.class,
            XMPPOperationInput.QNAME_OPERATION_INPUT, this);
        registry.registerDeserializer(BindingInput.class,
            XMPPOperationInput.QNAME_OPERATION_INPUT, this);
        registry.mapExtensionTypes(BindingInput.class,
            XMPPOperationInput.QNAME_OPERATION_INPUT, XMPPOperationInput.class);

        registry.registerSerializer(BindingOutput.class,
            XMPPOperationOutput.QNAME_OPERATION_OUTPUT, this);
        registry.registerDeserializer(BindingOutput.class,
            XMPPOperationOutput.QNAME_OPERATION_OUTPUT, this);
        registry.mapExtensionTypes(BindingOutput.class,
            XMPPOperationOutput.QNAME_OPERATION_OUTPUT,
            XMPPOperationOutput.class);
    }

    public void marshall(Class aClass, QName qName,
        ExtensibilityElement extensibilityElement, PrintWriter printWriter,
        Definition definition, ExtensionRegistry extensionRegistry)
        throws WSDLException {
        if (extensibilityElement == null) {
            return;
        }

        if (extensibilityElement instanceof XMPPBinding) {
            XMPPBinding xmppBinding = (XMPPBinding) extensibilityElement;
            printWriter.print("      <xmpp:binding");

            final Boolean required = xmppBinding.getRequired();

            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED,
                    required.toString(), definition, printWriter);
            }

            final Boolean tslEnabled = xmppBinding.getTlsEnabled();

            if (tslEnabled != null) {
                DOMUtils.printAttribute(XMPPBinding.Attributes.tlsEnabled.toString(),
                    tslEnabled.toString(), printWriter);
            }

            final Boolean saslEnabled = xmppBinding.getSaslEnabled();

            if (saslEnabled != null) {
                DOMUtils.printAttribute(XMPPBinding.Attributes.saslEnabled.toString(),
                    saslEnabled.toString(), printWriter);
            }

            printWriter.println("/>");
        } else if (extensibilityElement instanceof XMPPAddress) {
            marshallXMPPAddress((XMPPAddress) extensibilityElement, definition,
                printWriter);
        }
    }

    private void marshallXMPPAddress(final XMPPAddress xmppAddress,
        Definition definition, PrintWriter printWriter)
        throws WSDLException {
        printWriter.print("      <xmpp:address");

        final Boolean required = xmppAddress.getRequired();

        if (required != null) {
            DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED,
                required.toString(), definition, printWriter);
        }

        try {
            final String domain = xmppAddress.getDomain();

            if (domain != null) {
                DOMUtils.printAttribute(XMPPAddress.Attributes.domain.toString(),
                    domain, printWriter);
            }

            final String username = xmppAddress.getUsername();

            if (username != null) {
                DOMUtils.printAttribute(XMPPAddress.Attributes.username.toString(),
                    username, printWriter);
            }

            final String password = xmppAddress.getPassword();

            if (domain != null) {
                DOMUtils.printAttribute(XMPPAddress.Attributes.password.toString(),
                    password, printWriter);
            }

            final String resource = xmppAddress.getResource();

            if (username != null) {
                DOMUtils.printAttribute(XMPPAddress.Attributes.resource.toString(),
                    resource, printWriter);
            }

            final Integer port = xmppAddress.getPort();

            if (domain != null) {
                DOMUtils.printAttribute(XMPPAddress.Attributes.port.toString(),
                    port.toString(), printWriter);
            }

            final String group = xmppAddress.getGroup();

            if (group != null) {
                DOMUtils.printAttribute(XMPPAddress.Attributes.group.toString(),
                    group, printWriter);
            }
        } catch (final Exception ex) {
            throw new WSDLException(WSDLException.PARSER_ERROR,
                "Failed to marshal location attribute", ex);
        }

        printWriter.println("/>");
    }

    public ExtensibilityElement unmarshall(Class aClass, QName qName,
        Element element, Definition definition,
        ExtensionRegistry extensionRegistry) throws WSDLException {
        log.log(Level.INFO, "Unmarshalling QNAME - " + qName);

        if (XMPPBinding.QNAME_BINDING.equals(qName)) {
            return unmarshallXMPPBinding(element);
        } else if (XMPPAddress.QNAME_ADDRESS.equals(qName)) {
            return unmarshallXMPPAddress(element);
        } else if (XMPPOperation.QNAME_OPERATION.equals(qName)) {
            return unmarshallXMPPOperation(element);
        } else if (XMPPOperationOutput.QNAME_OPERATION_OUTPUT.equals(qName)) {
            return unmarshallXMPPOperationOutput(element);
        } else if (XMPPOperationInput.QNAME_OPERATION_INPUT.equals(qName)) {
            return umarshallXMPPOperationInput(element);
        }

        return null;
    }

    private XMPPBinding unmarshallXMPPBinding(final Element el) {
        final XMPPBinding binding = new XMPPBinding();

        final Boolean tlsEnabled = Boolean.valueOf(DOMUtils.getAttribute(el,
                    XMPPBinding.Attributes.tlsEnabled.toString()));

        if (tlsEnabled != null) {
            binding.setTlsEnabled(tlsEnabled);
        }

        final Boolean saslEnabled = Boolean.valueOf(DOMUtils.getAttribute(el,
                    XMPPBinding.Attributes.saslEnabled.toString()));

        if (tlsEnabled != null) {
            binding.setSaslEnabled(saslEnabled);
        }

        return binding;
    }

    private XMPPAddress unmarshallXMPPAddress(final Element el) {
        // get the proxy elemnet
        final XMPPAddress address = new XMPPAddress();

        final String domain = DOMUtils.getAttribute(el,
                XMPPAddress.Attributes.domain.toString());

        if (domain != null) {
            address.setDomain(domain);
        }

        final String username = DOMUtils.getAttribute(el,
                XMPPAddress.Attributes.username.toString());

        if (username != null) {
            address.setUsername(username);
        }

        final String password = DOMUtils.getAttribute(el,
                XMPPAddress.Attributes.password.toString());

        if (password != null) {
            address.setPassword(password);
        }

        final String resource = DOMUtils.getAttribute(el,
                XMPPAddress.Attributes.resource.toString());

        if (resource != null) {
            address.setResource(resource);
        }

        final Integer port = Integer.valueOf(DOMUtils.getAttribute(el,
                    XMPPAddress.Attributes.port.toString()));

        if (port != null) {
            address.setPort(port);
        }

        final String group = DOMUtils.getAttribute(el,
                XMPPAddress.Attributes.group.toString());

        if (group != null) {
            address.setGroup(group);
        }

        return address;
    }

    private XMPPOperation unmarshallXMPPOperation(Element element) {
        final XMPPOperation op = new XMPPOperation();
        String operationName = DOMUtils.getAttribute(element,
                XMPPOperation.Attributes.name.toString());
        op.setOperationName(XMPPOperation.OperationName.valueOf(operationName));

        return op;
    }

    private XMPPOperationInput umarshallXMPPOperationInput(final Element el) {
        final XMPPOperationInput input = new XMPPOperationInput();
        input.setJabberId(DOMUtils.getAttribute(el,
                XMPPOperationInput.Attributes.jabberId.toString()));
        input.setStaticJabberId(DOMUtils.getAttribute(el,
                XMPPOperationInput.Attributes.staticJabberId.toString()));
        input.setPacketId(DOMUtils.getAttribute(el,
                XMPPOperationInput.Attributes.packetId.toString()));
        input.setMessage(DOMUtils.getAttribute(el,
                XMPPOperationInput.Attributes.message.toString()));
        input.setGroupChat(DOMUtils.getAttribute(el,
                XMPPOperationInput.Attributes.groupChat.toString()));
        input.setGroupList(DOMUtils.getAttribute(el,
                XMPPOperationInput.Attributes.groupList.toString()));
        input.setUserList(DOMUtils.getAttribute(el,
                XMPPOperationInput.Attributes.userList.toString()));
        input.setRoomName(DOMUtils.getAttribute(el,
                XMPPOperationInput.Attributes.roomName.toString()));
        input.setPacketType(DOMUtils.getAttribute(el,
                XMPPOperationInput.Attributes.packetType.toString()));

        return input;
    }

    private XMPPOperationOutput unmarshallXMPPOperationOutput(final Element el) {
        final XMPPOperationOutput output = new XMPPOperationOutput();
        output.setJabberId(DOMUtils.getAttribute(el,
                XMPPOperationOutput.Attributes.jabberId.toString()));
        output.setPacketId(DOMUtils.getAttribute(el,
                XMPPOperationOutput.Attributes.packetId.toString()));
        output.setMessage(DOMUtils.getAttribute(el,
                XMPPOperationOutput.Attributes.message.toString()));

        return output;
    }
}
