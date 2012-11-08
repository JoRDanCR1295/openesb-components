/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 ******************************************************************************/


package it.imolinfo.jbi4cics.jbi.wsdl;


import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.xml.namespace.QName;
import com.ibm.wsdl.util.xml.DOMUtils;
import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;
import it.imolinfo.jbi4cics.jbi.Messages;


/**
 * Deserializer for the Jbi4Cics WSDL Extension (addess element), according 
 * with JWSDL specs.
 * See JSR 110.
 * 
 * @author amedeocannone, marcopiraccini
 */

public class Jbi4CicsAddressSerializer implements ExtensionSerializer {

    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(Jbi4CicsAddressSerializer.class);

    /**
     * The responsible to translate localized messages.
     */
    private static final Messages MESSAGES
            = Messages.getMessages(Jbi4CicsAddressSerializer.class);
    
    /**
	 * void constructor.
	 */
	  public Jbi4CicsAddressSerializer(){
		  super();
	  }
    
    /*
     * (non-Javadoc)
     * 
     * @see javax.wsdl.extensions.ExtensionDeserializer#unmarshall(
     *      java.lang.Class,javax.xml.namespace.QName, org.w3c.dom.Element,
     *      javax.wsdl.Definition, javax.wsdl.extensions.ExtensionRegistry)
     */

    public void marshall(Class parentType, QName elementType,
            ExtensibilityElement extension, java.io.PrintWriter pw, 
            Definition def, ExtensionRegistry extReg)           
    throws WSDLException {

         // Gets the QN prefix
        String prefix = null;

        try {
            prefix = DOMUtils.getPrefix(Jbi4CicsExtension.NS_URI_JBI4CICS, 
                def);
        } catch (WSDLException ex) {
            LOG.warn("CIC001300_Jbi4cics_namespace_not_found", Jbi4CicsExtension.NS_URI_JBI4CICS);
        }

        // If prefix is null, adds it
        if (prefix == null) {
            prefix = Jbi4CicsExtension.DEFAULT_PREFIX;

            // Adds the namespace
            def.addNamespace(Jbi4CicsExtension.DEFAULT_PREFIX, 
                    Jbi4CicsExtension.NS_URI_JBI4CICS);
        }     

        prefix += ":";         
        LOG.debug("prefix found: " + prefix);
        
        if (extension instanceof Jbi4CicsAddress) {
            Jbi4CicsAddress jbi4CicsAddress = (Jbi4CicsAddress) extension;            
            pw.print("<" + prefix + Jbi4CicsExtension.ADDRESS_ELEMENT);            
      
            // username            
            if (jbi4CicsAddress.getUsername() != null) {
                DOMUtils.printAttribute(
                        Jbi4CicsExtension.USERNAME_ATTRIBUTE, 
                        jbi4CicsAddress.getUsername(), pw);
            } else {
            LOG.error("CIC001301_Invalid_username");      
            throw new WSDLException(WSDLException.INVALID_WSDL, MESSAGES.getString("CIC001301_Invalid_username"));
            }                                    

            // password            
            if (jbi4CicsAddress.getPassword() != null) {
                DOMUtils.printAttribute(
                        Jbi4CicsExtension.PASSWORD_ATTRIBUTE, 
                        jbi4CicsAddress.getPassword(), pw);
            } else {
            LOG.error("CIC001302_Invalid_password");      
            throw new WSDLException(WSDLException.INVALID_WSDL, MESSAGES.getString("CIC001302_Invalid_password"));
            }                                    

            // connectionType            
            if ((jbi4CicsAddress.getConnectionType().equals("CICS")) || 
                    (jbi4CicsAddress.getConnectionType().equals("DUMMY"))) {
                DOMUtils.printAttribute(
                        Jbi4CicsExtension.CONNECTION_TYPE_ATTRIBUTE, 
                        jbi4CicsAddress.getConnectionType(), pw);
            } else {
            LOG.error("CIC001303_Invalid_connection_type");      
            throw new WSDLException(WSDLException.INVALID_WSDL, MESSAGES.getString("CIC001303_Invalid_connection_type"));
            }                                    

            // JNDIConnectionName            
            if (jbi4CicsAddress.getJNDIConnectionName() != null) {
                DOMUtils.printAttribute(
                        Jbi4CicsExtension.JNDI_CONNECTION_NAME_ATTRIBUTE, 
                        jbi4CicsAddress.getJNDIConnectionName(), pw);
            } else {
            LOG.error("CIC001304_Invalid_jndi_connection_name");      
            throw new WSDLException(WSDLException.INVALID_WSDL, MESSAGES.getString("CIC001304_Invalid_jndi_connection_name"));
            }                                    

            // programName            
            if (jbi4CicsAddress.getProgramName() != null) {
                DOMUtils.printAttribute(
                        Jbi4CicsExtension.PROGRAM_NAME_ATTRIBUTE, 
                        jbi4CicsAddress.getProgramName(), pw);
            } else {
            LOG.error("CIC001305_Invalid_program_name");      
            throw new WSDLException(WSDLException.INVALID_WSDL, MESSAGES.getString("CIC001305_Invalid_program_name"));
            }                                    
            // transactionName            
            if (jbi4CicsAddress.getTransactionName() != null) {
                DOMUtils.printAttribute(
                        Jbi4CicsExtension.TRANSACTION_NAME_ATTRIBUTE, 
                        jbi4CicsAddress.getTransactionName(), pw);
            } else {
            LOG.error("CIC001306_Invalid_transaction_name");      
            throw new WSDLException(WSDLException.INVALID_WSDL, MESSAGES.getString("CIC001306_Invalid_transaction_name"));
            }                                    

            // tpn            
            if ((jbi4CicsAddress.getTpn().toString().equals("true")) || 
                    (jbi4CicsAddress.getTpn().toString().equals("false"))) {
                DOMUtils.printAttribute(
                        Jbi4CicsExtension.TPN_ATTRIBUTE, 
                        jbi4CicsAddress.getTpn().toString(), pw);
            } else {
            LOG.error("CIC001307_Invalid_tpn");      
            throw new WSDLException(WSDLException.INVALID_WSDL, MESSAGES.getString("CIC001307_Invalid_tpn"));
            }                                    

            pw.print(">\n");       
            pw.print("</" + prefix + Jbi4CicsExtension.ADDRESS_ELEMENT+">\n");         
        } else {
            LOG.warn("CIC001308_Invalid_extension_element", Jbi4CicsExtension.ADDRESS_ELEMENT);            
        }       
    }

}
