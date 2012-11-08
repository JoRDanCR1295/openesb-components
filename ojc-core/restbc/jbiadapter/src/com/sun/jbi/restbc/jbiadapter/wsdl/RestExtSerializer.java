package com.sun.jbi.restbc.jbiadapter.wsdl;

import java.io.PrintWriter;
import java.io.Serializable;

import javax.wsdl.Binding;
import javax.wsdl.BindingOperation;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.xml.namespace.QName;

import org.w3c.dom.Element;

/**
 * RestExtSerializer.java
 *
 * @author Edward Chou
 */
@SuppressWarnings("serial")
public class RestExtSerializer implements ExtensionSerializer, ExtensionDeserializer, Serializable {

    public RestExtSerializer() {
    }

    /**
     * Registers the serializers / deserializers
     * @param registry 
     */
    public void registerSerializer(ExtensionRegistry registry) {
        // Register and map Rest Binding
        registry.registerSerializer(Binding.class, RestConstants.QNAME_BINDING, this);
        registry.registerDeserializer(Binding.class, RestConstants.QNAME_BINDING, this);
        registry.mapExtensionTypes(Binding.class, RestConstants.QNAME_BINDING, RestBinding.class);

        // Register and map Rest Operation
        registry.registerSerializer(BindingOperation.class, RestConstants.QNAME_OPERATION, this);
        registry.registerDeserializer(BindingOperation.class, RestConstants.QNAME_OPERATION, this);
        registry.mapExtensionTypes(BindingOperation.class, RestConstants.QNAME_OPERATION, RestOperation.class);

        // Register and map Rest Address
        registry.registerSerializer(Port.class, RestConstants.QNAME_ADDRESS, this);
        registry.registerDeserializer(Port.class, RestConstants.QNAME_ADDRESS, this);
        registry.mapExtensionTypes(Port.class, RestConstants.QNAME_ADDRESS, RestAddress.class);
    }


    public void marshall(Class parentType, QName elementType, ExtensibilityElement extension,
            PrintWriter pw, javax.wsdl.Definition def, ExtensionRegistry extReg) throws WSDLException {
        // no op
    }


    public javax.wsdl.extensions.ExtensibilityElement unmarshall(Class parentType, QName elementType,
            Element el, Definition def, ExtensionRegistry extReg) throws javax.wsdl.WSDLException {

        ExtensibilityElement returnValue = null;

        if (RestConstants.QNAME_BINDING.equals(elementType)) {
            RestBinding restBinding = new RestBinding();
            returnValue = restBinding;
        } else if (RestConstants.QNAME_OPERATION.equals(elementType)) {
            RestOperation restOperation = new RestOperation(el);
            returnValue = restOperation;
        } else if (RestConstants.QNAME_ADDRESS.equals(elementType)) {
            RestAddress restAddress = new RestAddress();
            returnValue = restAddress;
        }
        
        return returnValue;
    }
    
    private boolean nonEmptyString(String strToTest) {
        boolean nonEmpty = false;
        if (strToTest != null && strToTest.length() > 0) {
            nonEmpty = true;
        }
        return nonEmpty;
    }

}
