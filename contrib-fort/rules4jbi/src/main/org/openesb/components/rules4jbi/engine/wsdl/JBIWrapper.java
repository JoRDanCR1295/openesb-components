/*
 * @(#)JBIWrapper.java        $Revision: 1.3 $ $Date: 2008/11/15 01:22:24 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.engine.wsdl;

import javax.xml.transform.dom.DOMSource;

import nu.xom.Attribute;
import nu.xom.Element;

import org.openesb.components.rules4jbi.shared.util.XOMUtils;
import org.openesb.components.rules4jbi.shared.wsdl.WSDLConstants;

/**
 * This class is used to construct the wrapper message that wraps messages
 * described using wsdl 1.1, as defined in JBI spec, section [5.5.1.1.4].
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.3 $ $Date: 2008/11/15 01:22:24 $
 * 
 * @since 0.1
 */
public class JBIWrapper {
    
    public enum Type {
        
        /** Wrapper for input requests. */
        INPUT,
    
        /** Wrapper for responses. */
        OUTPUT
    }
    
    private static final String WRAPPER_NAMESPACE = "http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper";
    
    private static final String WRAPPER_NAMESPACE_PREFIX = "jbi";
    
    private static final String WSDL_MESSAGE_NAMESPACE_PREFIX = "msgns";
    
    private static final String TYPES_NAMESPACE_PREFIX = "types";
    
    private static final String INPUT_DATA_PREFIXED_NAME =
            TYPES_NAMESPACE_PREFIX + ":" + WSDLConstants.INPUT_ELEMENT_NAME;
    
    private static final String OUTPUT_DATA_PREFIXED_NAME = 
            TYPES_NAMESPACE_PREFIX + ":" + WSDLConstants.OUTPUT_ELEMENT_NAME;
    
    private static final String WSDL_INPUT_MESSAGE_PREFIXED_NAME = 
            WSDL_MESSAGE_NAMESPACE_PREFIX + ":" + WSDLConstants.INPUT_MESSAGE_LOCAL_NAME;

    private static final String WSDL_OUTPUT_MESSAGE_PREFIXED_NAME =
            WSDL_MESSAGE_NAMESPACE_PREFIX + ":" + WSDLConstants.OUTPUT_MESSAGE_LOCAL_NAME;

    private static final String MESSAGE_ELEMENT_NAME =  WRAPPER_NAMESPACE_PREFIX + ":message";
    
    private static final String VERSION_ATTRIBUTE_NAME = "version";
    
    private static final String VERSION_ATTRIBUTE_VALUE = "1.0";
    
    private static final String TYPE_ATTRIBUTE_NAME = "type";
    
    private static final String NAME_ATTRIBUTE_NAME = "name";
    
    private static final String PART_ELEMENT_NAME = WRAPPER_NAMESPACE_PREFIX + ":part";
    
    private Type type;
    
    private boolean rootElementConstructed;
    
    private boolean businessObjectAdded = false;
    
    private Element wrapper = null;
    
    private Element data = null;

    private String wsdlTargetNamespace;

    public JBIWrapper(Type type, String wsdlTargetNamespace) {
        this.type = type;
        this.wsdlTargetNamespace = wsdlTargetNamespace;
        
        rootElementConstructed = false;
        
        wrapper = createMessageElement();
        
        // TODO: we don't need this if we construct the wrapper in constructor
        rootElementConstructed = true;
    }
    
    public Element getWrapper() {
        return wrapper;
    }
    
    public DOMSource toDOMSource() {
        return new DOMSource(XOMUtils.elementToDocument(wrapper));
    }
    
    public void addBusinessObject(Element businessObject) {
        Element copy = (Element) businessObject.copy();
        
        data.appendChild(copy);
    }
    
    Element createMessageElement() {
        Element message = new Element(MESSAGE_ELEMENT_NAME, WRAPPER_NAMESPACE);

        Attribute version = new Attribute(VERSION_ATTRIBUTE_NAME, VERSION_ATTRIBUTE_VALUE);
        message.addAttribute(version);

        Attribute wrapperType = new Attribute(TYPE_ATTRIBUTE_NAME,
                type == Type.INPUT
                        ? WSDL_INPUT_MESSAGE_PREFIXED_NAME 
                        : WSDL_OUTPUT_MESSAGE_PREFIXED_NAME);
        
        message.addAttribute(wrapperType);

        message.addNamespaceDeclaration(WSDL_MESSAGE_NAMESPACE_PREFIX, wsdlTargetNamespace);

        Element part = new Element(PART_ELEMENT_NAME, WRAPPER_NAMESPACE);
        
        message.appendChild(part);
        
        data = new Element(type == Type.INPUT ? INPUT_DATA_PREFIXED_NAME : OUTPUT_DATA_PREFIXED_NAME,
                WSDLConstants.TYPES_NAMESPACE_URI);
        
        part.appendChild(data);
        
        return message;
    }
    
    public static void main(String[] args) {
        String customerXML = "<ns1:customer xmlns:ns1='http://www.example.org/data'>"
                + " <ns1:name>Joe User</ns1:name><ns1:creditLimit>500</ns1:creditLimit>"
                + "</ns1:customer>";
        
        Element customer = XOMUtils.toElement(customerXML);
        
        JBIWrapper inputDataWrapper = new JBIWrapper(Type.INPUT,
                "http://www.abc.com/rules/definitions");
        
        JBIWrapper outputDataWrapper = new JBIWrapper(Type.OUTPUT,
                "http://www.abc.com/rules/definitions");

        inputDataWrapper.addBusinessObject(customer);
        outputDataWrapper.addBusinessObject(customer);
        outputDataWrapper.addBusinessObject(customer);
        
        XOMUtils.prettyPrint(inputDataWrapper.getWrapper(), 50);
        
        System.out.println();
        
        XOMUtils.prettyPrint(outputDataWrapper.getWrapper(), 50);
    }
}
