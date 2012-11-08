/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4cics.jbi.wsdl;

import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;


import org.w3c.dom.Element;

import com.ibm.wsdl.util.xml.DOMUtils;

/**
 * Deserializer for the Jbi4Corba WSDL Extension (addess element), according with JWSDL specs.
 * See JSR 110.
 * 
 * @author marcopiraccini
 */
public class Jbi4CicsAddressDeserializer implements ExtensionDeserializer {

	/**
	 * void constructor.
	 */
	  public Jbi4CicsAddressDeserializer(){
		  super();
	  }

    /*
     * (non-Javadoc)
     * 
     * @see javax.wsdl.extensions.ExtensionDeserializer#unmarshall(java.lang.Class,
     *      javax.xml.namespace.QName, org.w3c.dom.Element,
     *      javax.wsdl.Definition, javax.wsdl.extensions.ExtensionRegistry)
     */
    public ExtensibilityElement unmarshall(Class parentType, QName elementType,
            Element el, Definition def, ExtensionRegistry extReg)
    throws WSDLException {

        Jbi4CicsAddress jbi4CicsAddress = (Jbi4CicsAddress) extReg
            .createExtension(parentType, elementType);                       

        jbi4CicsAddress.setUsername(DOMUtils.getAttribute(el,
                Jbi4CicsExtension.USERNAME_ATTRIBUTE));
        
        jbi4CicsAddress.setPassword(DOMUtils.getAttribute(el,
                Jbi4CicsExtension.PASSWORD_ATTRIBUTE));        
               
        jbi4CicsAddress.setConnectionType(DOMUtils.getAttribute(el,
                Jbi4CicsExtension.CONNECTION_TYPE_ATTRIBUTE));        
        
        jbi4CicsAddress.setJNDIConnectionName(DOMUtils.getAttribute(el,
                Jbi4CicsExtension.JNDI_CONNECTION_NAME_ATTRIBUTE));      
        
        jbi4CicsAddress.setProgramName(DOMUtils.getAttribute(el,
                Jbi4CicsExtension.PROGRAM_NAME_ATTRIBUTE));
        
        jbi4CicsAddress.setTransactionName(DOMUtils.getAttribute(el,
                Jbi4CicsExtension.TRANSACTION_NAME_ATTRIBUTE));
        
        jbi4CicsAddress.setTpn(Boolean.valueOf(DOMUtils.getAttribute(el,
                Jbi4CicsExtension.TPN_ATTRIBUTE)));                        
        
        return jbi4CicsAddress;
    }

}
