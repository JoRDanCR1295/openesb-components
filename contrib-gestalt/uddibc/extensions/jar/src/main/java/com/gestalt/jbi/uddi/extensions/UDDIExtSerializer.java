/**
 *   uddi-binding-component-extensions - Extensions for the UDDI Binding Component
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
package com.gestalt.jbi.uddi.extensions;

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
 *
 * @author rriven
 */
public class UDDIExtSerializer implements ExtensionSerializer,
    ExtensionDeserializer, Serializable {
    private static final long serialVersionUID = 1L;
    private static final Logger log = Logger.getLogger(UDDIExtSerializer.class.getName());

    /**
     * Registers the serializers / deserializers
     */
    public void registerSerializer(final ExtensionRegistry registry) {
        registry.registerSerializer(Binding.class, UDDIBinding.QNAME_BINDING,
            this);
        registry.registerDeserializer(Binding.class, UDDIBinding.QNAME_BINDING,
            this);
        registry.mapExtensionTypes(Binding.class, UDDIBinding.QNAME_BINDING,
            UDDIBinding.class);

        registry.registerSerializer(Port.class, UDDIAddress.QNAME_ADDRESS, this);
        registry.registerDeserializer(Port.class, UDDIAddress.QNAME_ADDRESS,
            this);
        registry.mapExtensionTypes(Port.class, UDDIAddress.QNAME_ADDRESS,
            UDDIAddress.class);

        registry.registerSerializer(BindingOperation.class,
            UDDIOperation.QNAME_OPERATION, this);
        registry.registerDeserializer(BindingOperation.class,
            UDDIOperation.QNAME_OPERATION, this);
        registry.mapExtensionTypes(BindingOperation.class,
            UDDIOperation.QNAME_OPERATION, UDDIOperation.class);

        registry.registerSerializer(BindingInput.class,
            UDDIOperationInput.QNAME_OPERATION_INPUT, this);
        registry.registerDeserializer(BindingInput.class,
            UDDIOperationInput.QNAME_OPERATION_INPUT, this);
        registry.mapExtensionTypes(BindingInput.class,
            UDDIOperationInput.QNAME_OPERATION_INPUT, UDDIOperationInput.class);

        registry.registerSerializer(BindingOutput.class,
            UDDIOperationOutput.QNAME_OPERATION_OUTPUT, this);
        registry.registerDeserializer(BindingOutput.class,
            UDDIOperationOutput.QNAME_OPERATION_OUTPUT, this);
        registry.mapExtensionTypes(BindingOutput.class,
            UDDIOperationOutput.QNAME_OPERATION_OUTPUT,
            UDDIOperationOutput.class);
    }

    public void marshall(Class aClass, QName qName,
        ExtensibilityElement extensibilityElement, PrintWriter printWriter,
        Definition definition, ExtensionRegistry extensionRegistry)
        throws WSDLException {
        if (extensibilityElement == null) {
            return;
        }

        if (extensibilityElement instanceof UDDIBinding) {
            UDDIBinding uddiBinding = (UDDIBinding) extensibilityElement;
            printWriter.print("      <uddi:binding");

            final Boolean required = uddiBinding.getRequired();

            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED,
                    required.toString(), definition, printWriter);
            }

            printWriter.println("/>");
        } else if (extensibilityElement instanceof UDDIAddress) {
            marshallUDDIAddress((UDDIAddress) extensibilityElement, definition,
                printWriter);
        }
    }

    private void marshallUDDIAddress(final UDDIAddress uddiAddress,
        Definition definition, PrintWriter printWriter)
        throws WSDLException {
        printWriter.print("      <uddi:address");

        final Boolean required = uddiAddress.getRequired();

        if (required != null) {
            DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED,
                required.toString(), definition, printWriter);
        }

        try {
            final String inquiryURI = uddiAddress.getInquiryUri();

            if (inquiryURI != null) {
                DOMUtils.printAttribute(UDDIAddress.Attributes.inquiryUri.toString(),
                    inquiryURI, printWriter);
            }

            final String publishURI = uddiAddress.getPublishUri();

            if (publishURI != null) {
                DOMUtils.printAttribute(UDDIAddress.Attributes.publishUri.toString(),
                    publishURI, printWriter);
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

        if (UDDIBinding.QNAME_BINDING.equals(qName)) {
            return new UDDIBinding();
        } else if (UDDIAddress.QNAME_ADDRESS.equals(qName)) {
            return unmarshallUDDIAddress(element);
        } else if (UDDIOperation.QNAME_OPERATION.equals(qName)) {
            return umarshallUDDIOperation(element);
        } else if (UDDIOperationOutput.QNAME_OPERATION_OUTPUT.equals(qName)) {
            return new UDDIOperationOutput();
        } else if (UDDIOperationInput.QNAME_OPERATION_INPUT.equals(qName)) {
            return umarshallUDDIOperationInput(element);
        }

        return null;
    }

    private UDDIAddress unmarshallUDDIAddress(final Element el) {
        // get the proxy elemnet
        final UDDIAddress address = new UDDIAddress();

        final String publishURI = DOMUtils.getAttribute(el,
                UDDIAddress.Attributes.publishUri.toString());

        if (publishURI != null) {
            address.setPublishUri(publishURI);
        }

        final String inquiryURI = DOMUtils.getAttribute(el,
                UDDIAddress.Attributes.inquiryUri.toString());

        if (inquiryURI != null) {
            address.setInquiryUri(inquiryURI);
        }

        return address;
    }

    private ExtensibilityElement umarshallUDDIOperation(final Element el) {
        final UDDIOperation op = new UDDIOperation();
        String operationName = DOMUtils.getAttribute(el,
                UDDIOperation.Attributes.name.toString());
        op.setOperationName(UDDIOperation.OperationName.valueOf(operationName));

        return op;
    }

    private UDDIOperationInput umarshallUDDIOperationInput(final Element el) {
        final UDDIOperationInput input = new UDDIOperationInput();
        input.setBusinessName(DOMUtils.getAttribute(el,
                UDDIOperationInput.Attributes.businessName.toString()));
        input.setServiceName(DOMUtils.getAttribute(el,
                UDDIOperationInput.Attributes.serviceName.toString()));

        return input;
    }
}
