/**
 *   rss-binding-component-extensions - Extensions for the RSS Binding Component
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
package com.gestalt.jbi.rss.extensions;

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
 * Handles the marshalling and the unmarshalling of the WSDL Author: cgallemore
 * Date: May 8, 2007
 */
public class RSSExtSerializer implements ExtensionSerializer,
    ExtensionDeserializer, Serializable {
    private static final long serialVersionUID = 1L;
    private static final Logger log = Logger.getLogger(RSSExtSerializer.class.getName());

    /**
     * Registers an ExtensionRegistry.
     *
     * @param registry
     */
    public void registerSerializer(final ExtensionRegistry registry) {
        registry.registerSerializer(Binding.class, RSSBinding.QNAME_BINDING,
            this);
        registry.registerDeserializer(Binding.class, RSSBinding.QNAME_BINDING,
            this);
        registry.mapExtensionTypes(Binding.class, RSSBinding.QNAME_BINDING,
            RSSBinding.class);

        registry.registerSerializer(Port.class, RSSAddress.QNAME_ADDRESS, this);
        registry.registerDeserializer(Port.class, RSSAddress.QNAME_ADDRESS, this);
        registry.mapExtensionTypes(Port.class, RSSAddress.QNAME_ADDRESS,
            RSSAddress.class);

        registry.registerSerializer(BindingOperation.class,
            RSSOperation.QNAME_OPERATION, this);
        registry.registerDeserializer(BindingOperation.class,
            RSSOperation.QNAME_OPERATION, this);
        registry.mapExtensionTypes(BindingOperation.class,
            RSSOperation.QNAME_OPERATION, RSSOperation.class);

        registry.registerSerializer(BindingInput.class,
            RSSOperationInput.QNAME_OPERATION_INPUT, this);
        registry.registerDeserializer(BindingInput.class,
            RSSOperationInput.QNAME_OPERATION_INPUT, this);
        registry.mapExtensionTypes(BindingInput.class,
            RSSOperationInput.QNAME_OPERATION_INPUT, RSSOperationInput.class);

        registry.registerSerializer(BindingOutput.class,
            RSSOperationOutput.QNAME_OPERATION_OUTPUT, this);
        registry.registerDeserializer(BindingOutput.class,
            RSSOperationOutput.QNAME_OPERATION_OUTPUT, this);
        registry.mapExtensionTypes(BindingOutput.class,
            RSSOperationOutput.QNAME_OPERATION_OUTPUT, RSSOperationOutput.class);
    }

    /**
     * Handles the marshalling of the WSDL and gets all the beans associated
     * with the binding and address.
     *
     * @param aClass
     * @param qName
     * @param extensibilityElement
     * @param printWriter
     * @param definition
     * @param extensionRegistry
     * @throws WSDLException
     */
    public void marshall(Class aClass, QName qName,
        ExtensibilityElement extensibilityElement, PrintWriter printWriter,
        Definition definition, ExtensionRegistry extensionRegistry)
        throws WSDLException {
        if (extensibilityElement == null) {
            return;
        }

        if (extensibilityElement instanceof RSSBinding) {
            RSSBinding rssBinding = (RSSBinding) extensibilityElement;
            printWriter.print("      <rss:binding");

            final Boolean required = rssBinding.getRequired();

            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED,
                    required.toString(), definition, printWriter);
            }

            printWriter.println("/>");
        } else if (extensibilityElement instanceof RSSAddress) {
            marshallRSSAddress((RSSAddress) extensibilityElement, definition,
                printWriter);
        }
    }

    /**
     * Marshalls the address extension and gets all the beans.
     *
     * @param rssAddress
     * @param definition
     * @param printWriter
     * @throws WSDLException
     */
    private void marshallRSSAddress(final RSSAddress rssAddress,
        Definition definition, PrintWriter printWriter)
        throws WSDLException {
        printWriter.print("      <rss:address");

        final Boolean required = rssAddress.getRequired();

        if (required != null) {
            DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED,
                required.toString(), definition, printWriter);
        }

        try {
            final String feedTitle = rssAddress.getFeedTitle();

            if (feedTitle != null) {
                DOMUtils.printAttribute(RSSAddress.Attributes.feedTitle.toString(),
                    feedTitle, printWriter);
            }

            final String feedDescription = rssAddress.getFeedDescription();

            if (feedDescription != null) {
                DOMUtils.printAttribute(RSSAddress.Attributes.feedDescription.toString(),
                    feedDescription, printWriter);
            }

            final String feedType = rssAddress.getFeedType();

            if (feedType != null) {
                DOMUtils.printAttribute(RSSAddress.Attributes.feedType.toString(),
                    feedType, printWriter);
            }

            final String location = rssAddress.getLocation();

            if (location != null) {
                DOMUtils.printAttribute(RSSAddress.Attributes.location.toString(),
                    location, printWriter);
            }

            final String username = rssAddress.getUsername();

            if (username != null) {
                DOMUtils.printAttribute(RSSAddress.Attributes.username.toString(),
                    username, printWriter);
            }

            final String password = rssAddress.getPassword();

            if (password != null) {
                DOMUtils.printAttribute(RSSAddress.Attributes.password.toString(),
                    password, printWriter);
            }

            final String correlationId = rssAddress.getCorrelationId();

            if (correlationId != null) {
                DOMUtils.printAttribute(RSSAddress.Attributes.correlationId.toString(),
                    correlationId, printWriter);
            }
        } catch (final Exception ex) {
            throw new WSDLException(WSDLException.PARSER_ERROR,
                "Failed to marshal location attribute", ex);
        }

        printWriter.println("/>");
    }

    /**
     * Handles the unmarshalling of the WSDL
     *
     * @param aClass
     * @param qName
     * @param element
     * @param definition
     * @param extensionRegistry
     * @return ExtensibilityElement
     * @throws WSDLException
     */
    public ExtensibilityElement unmarshall(Class aClass, QName qName,
        Element element, Definition definition,
        ExtensionRegistry extensionRegistry) throws WSDLException {
        log.log(Level.INFO, "Unmarshalling QNAME - " + qName);

        if (RSSBinding.QNAME_BINDING.equals(qName)) {
            return unmarshallRSSBinding(element);
        } else if (RSSAddress.QNAME_ADDRESS.equals(qName)) {
            return unmarshallRSSAddress(element);
        } else if (RSSOperation.QNAME_OPERATION.equals(qName)) {
            return unmarshallRSSOperation(element);
        } else if (RSSOperationOutput.QNAME_OPERATION_OUTPUT.equals(qName)) {
            return unmarshallRSSOperationOutput(element);
        } else if (RSSOperationInput.QNAME_OPERATION_INPUT.equals(qName)) {
            return umarshallRSSOperationInput(element);
        }

        return null;
    }

    /**
     * Unmarshalls the RSSOperation, and sets the OperationName with the value
     * in the WSDL.
     *
     * @param el
     * @return ExtensibilityElement
     */
    private ExtensibilityElement unmarshallRSSOperation(Element el) {
        final RSSOperation op = new RSSOperation();
        String operationName = DOMUtils.getAttribute(el,
                RSSOperation.Attributes.name.toString());
        op.setOperationName(RSSOperation.OperationName.valueOf(operationName));

        return op;
    }

    /**
     * Unmarshalls the binding.
     *
     * @param el
     * @return RSSBinding
     */
    private RSSBinding unmarshallRSSBinding(final Element el) {
        final RSSBinding binding = new RSSBinding();

        return binding;
    }

    /**
     * Unmarshalls the address, and sets all beans provided with the RSSAddress
     * extension.
     *
     * @param el
     * @return RSSAddress
     */
    private RSSAddress unmarshallRSSAddress(final Element el) {
        log.fine("Unmarshalling RSSAddress......");

        final RSSAddress address = new RSSAddress();
        String location = DOMUtils.getAttribute(el,
                RSSAddress.Attributes.location.toString());
        log.fine("Location is: " + location);
        address.setLocation(location);

        String feedDescription = DOMUtils.getAttribute(el,
                RSSAddress.Attributes.feedDescription.toString());
        address.setFeedDescription(feedDescription);

        String feedTitle = DOMUtils.getAttribute(el,
                RSSAddress.Attributes.feedTitle.toString());
        address.setFeedTitle(feedTitle);

        String feedType = DOMUtils.getAttribute(el,
                RSSAddress.Attributes.feedType.toString());
        address.setFeedType(feedType);

        String username = DOMUtils.getAttribute(el,
                RSSAddress.Attributes.username.toString());
        address.setUsername(username);

        String password = DOMUtils.getAttribute(el,
                RSSAddress.Attributes.password.toString());
        address.setPassword(password);

        String correlationId = DOMUtils.getAttribute(el,
                RSSAddress.Attributes.correlationId.toString());
        address.setCorrelationId(correlationId);

        return address;
    }

    /**
     * Unmarshalls the input, and sets all beans provided with the
     * RSSOperationInput extension.
     *
     * @param el
     * @return RSSOperationInput
     */
    private RSSOperationInput umarshallRSSOperationInput(final Element el) {
        final RSSOperationInput input = new RSSOperationInput();
        input.setEntryTitle(DOMUtils.getAttribute(el,
                RSSOperationInput.Attributes.entryTitle.toString()));
        input.setEntryLink(DOMUtils.getAttribute(el,
                RSSOperationInput.Attributes.entryLink.toString()));
        input.setEntryDescription(DOMUtils.getAttribute(el,
                RSSOperationInput.Attributes.entryDescription.toString()));
        input.setLongitude(DOMUtils.getAttribute(el,
                RSSOperationInput.Attributes.longitude.toString()));
        input.setLatitude(DOMUtils.getAttribute(el,
                RSSOperationInput.Attributes.latitude.toString()));
        input.setFeedList(DOMUtils.getAttribute(el,
                RSSOperationInput.Attributes.feedList.toString()));
        input.setDestinationUrl(DOMUtils.getAttribute(el,
                RSSOperationInput.Attributes.destinationUrl.toString()));


        String pollingInterval = DOMUtils.getAttribute(el,
                RSSOperationInput.Attributes.pollingInterval.toString());

        if ((pollingInterval != null) && !pollingInterval.equals("")) {
            input.setPollingInterval(Integer.parseInt(pollingInterval));
        }

        String entriesPerMessage = DOMUtils.getAttribute(el,
                RSSOperationInput.Attributes.entriesPerMessage.toString());

        if ((entriesPerMessage != null) && !entriesPerMessage.equals("")) {
            input.setEntriesPerMessage(Integer.valueOf(entriesPerMessage));
        }
        
        String filterType = DOMUtils.getAttribute(el,
                RSSOperationInput.Attributes.filterByType.toString());

        if ((filterType != null) && !filterType.equals("")) {
            input.setFilterByType(RSSOperationInput.FilterTypes.valueOf(
                    filterType));
        }

        String filterValue = DOMUtils.getAttribute(el,
                RSSOperationInput.Attributes.filterByValue.toString());

        if ((filterValue != null) && !filterValue.equals("")) {
            input.setFilterByValue(filterValue);
        }

        return input;
    }

    /**
     * Unmarshalls the output, and sets all beans provided with the
     * RSSOperationOutput extension.
     *
     * @param el
     * @return RSSOperationOutput
     */
    private RSSOperationOutput unmarshallRSSOperationOutput(final Element el) {
        final RSSOperationOutput output = new RSSOperationOutput();

        return output;
    }
}
