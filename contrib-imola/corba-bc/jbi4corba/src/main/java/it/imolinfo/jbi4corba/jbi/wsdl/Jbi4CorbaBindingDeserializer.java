 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.wsdl;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;

import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;

import org.w3c.dom.Element;

import com.ibm.wsdl.util.xml.DOMUtils;
import com.ibm.wsdl.util.xml.QNameUtils;

/**
 * Deserializer for the Jbi4Corba WSDL Extension (Binding element),
 * according with JWSDL specs.
 * See JSR 110.
 *
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class Jbi4CorbaBindingDeserializer implements ExtensionDeserializer {

    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(Jbi4CorbaBindingDeserializer.class);

    /**
     * Default constructor.
     */
    public Jbi4CorbaBindingDeserializer(){
    }
       
    /**
     * Unmarshall the CorbaBinding.
     * @param parentType      The parent type
     * @param elementType     The element type
     * @param el              The element
     * @param def             The definition
     * @param extReg          The extension registry
     * @return                The return
     * @throws WSDLException  The WSDL exception
     */
    @SuppressWarnings("unchecked")
	public ExtensibilityElement unmarshall(
      Class parentType,
      QName elementType,
      Element el,
      Definition def,
      ExtensionRegistry extReg) throws WSDLException {

      Jbi4CorbaBinding jbi4CorbaBinding = (Jbi4CorbaBinding)
        extReg.createExtension(parentType, elementType);
            
        // Reads the Corba IDL
        Element tempEl = DOMUtils.getFirstChildElement(el);
        while (tempEl != null) {

            // IDL Element
            if (QNameUtils.matches(Jbi4CorbaExtension.Q_ELEM_JBI4CORBA_IDLENTRY,
                    tempEl)) {
                // loads the IDL
                String idlContent = tempEl.getTextContent();
                tempEl.getAttributes();
                String location=tempEl.getAttribute(Jbi4CorbaExtension.RELATIVE_PATH);
                if ("".equals(location)){
                    location=Jbi4CorbaExtension.DEFAULT_RELATIVE_PATH;
                }
                String filename=tempEl.getAttribute(Jbi4CorbaExtension.FILENAME);
                if ("".equals(filename)){
                    filename=Jbi4CorbaExtension.DEFAULT_FILENAME;
                }
                String rootString=tempEl.getAttribute(Jbi4CorbaExtension.ROOT);
                if ("".equals(rootString)){
                    rootString="true";
                }
                boolean root=Boolean.parseBoolean(rootString);
                Jbi4CorbaIDLEntry jbi4CorbaIDLEntry=new Jbi4CorbaIDLEntry();
                jbi4CorbaIDLEntry.setIDL(idlContent);
                jbi4CorbaIDLEntry.setIdlFilename(filename);
                jbi4CorbaIDLEntry.setRelativePath(location);
                jbi4CorbaIDLEntry.setRoot(root);
                jbi4CorbaBinding.getJbi4CorbaDLEntryList().add(jbi4CorbaIDLEntry);
                LOG.debug("Loaded idl entry: " + jbi4CorbaIDLEntry);
            }

            tempEl = DOMUtils.getNextSiblingElement(tempEl);

        }
        LOG.debug("Loaded binding: " + jbi4CorbaBinding);
        return jbi4CorbaBinding;
    }
}
