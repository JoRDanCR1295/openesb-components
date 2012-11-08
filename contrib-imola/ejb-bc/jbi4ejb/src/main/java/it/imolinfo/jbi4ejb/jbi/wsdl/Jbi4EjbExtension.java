/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.jbi.wsdl;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;

import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;

/**
 * JWSDL Extension class. See JSR 110.
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public final class Jbi4EjbExtension {

    /** The Constant NS_URI_JBI4EJB. */
    public static final String NS_URI_JBI4EJB = "uri://schemas.imola.it/jbi/wsdl-extensions/ejb/";

    /** The Constant DEFAULT_PREFIX. */
    public static final String DEFAULT_PREFIX = "imolaejb";    

    /** The Constant BINDING_ELEMENT. */
    public static final String BINDING_ELEMENT = "binding";

    /** The Constant ADDRESS_ELEMENT. */
    public static final String ADDRESS_ELEMENT = "address";
    
    /** The Constant TYPES_ELEMENT. */
    public static final String TYPES_ELEMENT = "types";
    
    /** The Constant SERIAL_VERSION_UID_ELEMENT. */
    public static final String SERIAL_VERSION_UID_ELEMENT = "serialVersionUID";
    
    /** The Constant LOCALIZATION_TYPE_ATTRIBUTE. */
    public static final String LOCALIZATION_TYPE_ATTRIBUTE = "localizationType";    

    /** The Constant ORB_ELEMENT. */
    public static final String ORB_ELEMENT = "orb";
    
    /** The Constant JNDI_ELEMENT. */
    public static final String JNDI_ELEMENT = "jndi";

    /** The Constant PROPERTY_ELEMENT. */
    public static final String PROPERTY_ELEMENT = "property";

    /** The Constant NAME_ATTRIBUTE. */
    public static final String NAME_ATTRIBUTE = "name";

    /** The Constant VALUE_ATTRIBUTE. */
    public static final String VALUE_ATTRIBUTE = "value";
    
    /** The Constant CLASSNAME_ATTRIBUTE. */
    public static final String CLASSNAME_ATTRIBUTE = "className";

    /** The Constant UID_ATTRIBUTE. */
    public static final String UID_ATTRIBUTE = "UID";       
    

    /** The Constant Q_ELEM_JBI4EJB_BINDING. */
    public static final QName Q_ELEM_JBI4EJB_BINDING = new QName(NS_URI_JBI4EJB,
            BINDING_ELEMENT);

    /** The Constant Q_ELEM_JBI4EJB_ADDRESS. */
    public static final QName Q_ELEM_JBI4EJB_ADDRESS = new QName(NS_URI_JBI4EJB,
            ADDRESS_ELEMENT);
    
    /** The Constant Q_ELEM_JBI4EJB_TYPES. */
    public static final QName Q_ELEM_JBI4EJB_TYPES = new QName(NS_URI_JBI4EJB,
            TYPES_ELEMENT);    

    /** The Constant Q_ELEM_JBI4EJB_ORB. */
    public static final QName Q_ELEM_JBI4EJB_ORB = new QName(NS_URI_JBI4EJB,
            ORB_ELEMENT);
    
    /** The Constant Q_ELEM_JBI4EJB_JNDI. */
    public static final QName Q_ELEM_JBI4EJB_JNDI = new QName(NS_URI_JBI4EJB,
            JNDI_ELEMENT);
    
    /** The Constant Q_ELEM_JBI4EJB_SERIAL_VERSION_UID. */
    public static final QName Q_ELEM_JBI4EJB_SERIAL_VERSION_UID = new QName(NS_URI_JBI4EJB,
            SERIAL_VERSION_UID_ELEMENT);    

        
    /** The logger for this class and its instances. */
    private static final Logger LOG
            = LoggerFactory.getLogger(Jbi4EjbExtension.class);
    
    
    /**
     * Instantiates a new jbi4 ejb extension.
     */
    private Jbi4EjbExtension() {}

    /**
     * Register the Jbi4Corba WSDL extension.
     * 
     * @param registry the extension registry
     */
    public static void register(ExtensionRegistry registry) {

        LOG.debug("Start ExtensionRegistry registration");

        // Address
        registry.mapExtensionTypes(
                javax.wsdl.Port.class,
                Q_ELEM_JBI4EJB_ADDRESS, Jbi4EjbAddress.class);

        registry.registerDeserializer(
                javax.wsdl.Port.class,
                Q_ELEM_JBI4EJB_ADDRESS, new Jbi4EjbAddressDeserializer());

        registry.registerSerializer(
               javax.wsdl.Port.class,
               Q_ELEM_JBI4EJB_ADDRESS, new Jbi4EjbAddressSerializer());

        // Binding
        registry.mapExtensionTypes(javax.wsdl.Binding.class,
                Q_ELEM_JBI4EJB_BINDING, Jbi4EjbBinding.class);

        registry.registerDeserializer(javax.wsdl.Binding.class,
                Q_ELEM_JBI4EJB_BINDING, new Jbi4EjbBindingDeserializer());

        registry.registerSerializer(javax.wsdl.Binding.class,
                Q_ELEM_JBI4EJB_BINDING, new Jbi4EjbBindingSerializer());

        // Types (At the definition level!!!!)
        registry.mapExtensionTypes(javax.wsdl.Definition.class,
                Q_ELEM_JBI4EJB_TYPES, Jbi4EjbTypes.class);
        
        registry.registerDeserializer(javax.wsdl.Definition.class,
                Q_ELEM_JBI4EJB_TYPES, new Jbi4EjbTypesDeserializer());       
        
        registry.registerSerializer(javax.wsdl.Definition.class,
                Q_ELEM_JBI4EJB_TYPES, new Jbi4EjbTypesSerializer());
    }
}
