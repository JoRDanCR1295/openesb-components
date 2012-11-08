/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)SNMPExtSerializer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpbc.extensions;

import com.ibm.wsdl.util.xml.DOMUtils;
import com.sun.jbi.internationalization.Messages;
import org.w3c.dom.Element;
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
import java.io.PrintWriter;
import java.io.Serializable;


public class SNMPExtSerializer implements ExtensionSerializer, ExtensionDeserializer, Serializable {

    private static final Messages mMessages = Messages.getMessages(SNMPExtSerializer.class);

    /** Creates a new instance of SNMPExtSerializer */
    public SNMPExtSerializer() {
    }

    /**
     * Registers the serializers / deserializers
     * @param registry 
     */
    public void registerSerializer(ExtensionRegistry registry) {
        // Register and map SNMP Binding
        registry.registerSerializer(Binding.class, SNMPConstants.QNAME_BINDING, this);
        registry.registerDeserializer(Binding.class, SNMPConstants.QNAME_BINDING, this);
        registry.mapExtensionTypes(Binding.class, SNMPConstants.QNAME_BINDING, SNMPBinding.class);

        // Register and map SNMP Operation
        registry.registerSerializer(BindingOperation.class, SNMPConstants.QNAME_OPERATION, this);
        registry.registerDeserializer(BindingOperation.class, SNMPConstants.QNAME_OPERATION, this);
        registry.mapExtensionTypes(BindingOperation.class, SNMPConstants.QNAME_OPERATION, SNMPOperation.class);

        // Register and map SNMP Input
        registry.registerSerializer(BindingInput.class, SNMPConstants.QNAME_MESSAGE, this);
        registry.registerDeserializer(BindingInput.class, SNMPConstants.QNAME_MESSAGE, this);
        registry.mapExtensionTypes(BindingInput.class, SNMPConstants.QNAME_MESSAGE, SNMPMessage.class);

        // Register and map SNMP Output
        registry.registerSerializer(BindingOutput.class, SNMPConstants.QNAME_MESSAGE, this);
        registry.registerDeserializer(BindingOutput.class, SNMPConstants.QNAME_MESSAGE, this);
        registry.mapExtensionTypes(BindingOutput.class, SNMPConstants.QNAME_MESSAGE, SNMPMessage.class);

        // Register and map SNMP Address
        registry.registerSerializer(Port.class, SNMPConstants.QNAME_ADDRESS, this);
        registry.registerDeserializer(Port.class, SNMPConstants.QNAME_ADDRESS, this);
        registry.mapExtensionTypes(Port.class, SNMPConstants.QNAME_ADDRESS, SNMPAddress.class);
    }


    public void marshall(Class parentType, QName elementType, ExtensibilityElement extension,
            PrintWriter pw, javax.wsdl.Definition def, ExtensionRegistry extReg) throws WSDLException {
        // no op
    }


    public javax.wsdl.extensions.ExtensibilityElement unmarshall(Class parentType, QName elementType,
            Element el, Definition def, ExtensionRegistry extReg) throws javax.wsdl.WSDLException {

        ExtensibilityElement returnValue = null;

        if (SNMPConstants.QNAME_BINDING.equals(elementType)) {
            SNMPBinding snmpBinding = new SNMPBinding();
            returnValue = snmpBinding;
        } else if (SNMPConstants.QNAME_OPERATION.equals(elementType)) {
            SNMPOperation snmpOperation = new SNMPOperation();
            
            //
            // unmarshal attributes
            //
            String type = DOMUtils.getAttribute(el, SNMPOperation.ATTR_TYPE);
            if (nonEmptyString(type)) {
                snmpOperation.setType(SNMPOperation.SnmpOpType.parse(type));
            }
            String mofId = DOMUtils.getAttribute(el, SNMPOperation.ATTR_MOF_ID);
            if (nonEmptyString(mofId)) {
                snmpOperation.setMofId(mofId);
            }
            String adaptationId = DOMUtils.getAttribute(el, SNMPOperation.ATTR_ADAPTATION_ID);
            if (nonEmptyString(adaptationId)) {
                snmpOperation.setAdaptationId(adaptationId);
            }
            String mofIdRef = DOMUtils.getAttribute(el, SNMPOperation.ATTR_MOF_ID_REF);
            if (nonEmptyString(mofIdRef)) {
                snmpOperation.setMofIdRef(mofIdRef);
            }
            
            returnValue = snmpOperation;
        } else if (SNMPConstants.QNAME_ADDRESS.equals(elementType)) {
            SNMPAddress snmpAddress = new SNMPAddress();
            
            String port = DOMUtils.getAttribute(el, SNMPAddress.ATTR_PORT);
            if (nonEmptyString(port)) {
                snmpAddress.setPort(Integer.valueOf(port));
            }
            
            returnValue = snmpAddress;
        } else if (SNMPConstants.QNAME_MESSAGE.equals(elementType)) {                        
            SNMPMessage message = new SNMPMessage();
            
            // unmarshal SNMP attributes
            String trapPart = DOMUtils.getAttribute(el, SNMPMessage.ATTR_TRAPPART);
            if (nonEmptyString(trapPart)) {
                message.setTrapPart(trapPart);
            }
            
            returnValue = message;
        } 
        
        return returnValue;
    }
    
    private boolean nonEmptyString (String strToTest) {
        boolean nonEmpty = false;
        if (strToTest != null && strToTest.length() > 0) {
            nonEmpty = true;
        }
        return nonEmpty;
    }

}
