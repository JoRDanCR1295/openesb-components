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

import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;

/**
 * JWSDL Extension class. See JSR 110.
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class Jbi4CorbaExtension {

    /**
     * The namespace of the jbi4corba extensibility element.
     */
    public static final String NS_URI_JBI4CORBA = "uri://schemas.imola.it/jbi/wsdl-extensions/corba/";

    /**
     * The conventional prefix for the XML element of the extensibility element.
     */
    public static final String DEFAULT_PREFIX = "imolacorba";

    /**
     * An element name.
     */
    public static final String BINDING_ELEMENT = "binding";

    /**
     * An element name.
     */
    public static final String ADDRESS_ELEMENT = "address";

    /**
     * The localization types are: NameService, corbaloc, corbaname, IOR.
     */
    public static final String LOCALIZATION_TYPE_ATTRIBUTE = "localizationType";

    /**
     * An element name.
     */
    public static final String ORB_ELEMENT = "orb";

    /**
     * An element name.
     */
    public static final String PROPERTY_ELEMENT = "property";

    /**
     * An attribute name.
     */
    public static final String NAME_ATTRIBUTE = "name";

    /**
     * An attribute name.
     */
    public static final String VALUE_ATTRIBUTE = "value";

    /**
     * An element name.
     */
    //public static final String IDL_ELEMENT = "idl";
    public static final String IDL_ENTRY = "idl";
    public static final String FILENAME = "filename";
    public static final String RELATIVE_PATH = "relativepath";
    public static final String ROOT = "root";
    public static final String DEFAULT_FILENAME = "root.idl";
    public static final String DEFAULT_RELATIVE_PATH = ".";
    public static final boolean DEFAULT_ROOT = true;

    public static final QName Q_ELEM_JBI4CORBA_BINDING = new QName(
            NS_URI_JBI4CORBA, BINDING_ELEMENT);

    public static final QName Q_ELEM_JBI4CORBA_ADDRESS = new QName(
            NS_URI_JBI4CORBA, ADDRESS_ELEMENT);

    public static final QName Q_ELEM_JBI4CORBA_IDLENTRY = new QName(
            NS_URI_JBI4CORBA, IDL_ENTRY);

    public static final QName Q_ELEM_JBI4CORBA_ORB = new QName(
            NS_URI_JBI4CORBA, ORB_ELEMENT);

    /**
     * The logger for this class and its instances.
     */
    private static final transient Logger LOG = LoggerFactory
            .getLogger(Jbi4CorbaExtension.class);

    /**
     * Default constructor.
     * 
     */
    public Jbi4CorbaExtension() {

    }

    /**
     * Register the Jbi4Corba WSDL extension.
     * 
     * @param registry
     *            The registry
     */
    public static void register(ExtensionRegistry registry) {

        LOG.debug("Start ExtensionRegistry registration");

        // Address
        registry.mapExtensionTypes(javax.wsdl.Port.class,
                Q_ELEM_JBI4CORBA_ADDRESS, Jbi4CorbaAddress.class);

        registry.registerDeserializer(javax.wsdl.Port.class,
                Q_ELEM_JBI4CORBA_ADDRESS, new Jbi4CorbaAddressDeserializer());

        registry.registerSerializer(javax.wsdl.Port.class,
                Q_ELEM_JBI4CORBA_ADDRESS, new Jbi4CorbaAddressSerializer());

        // Binding
        registry.mapExtensionTypes(javax.wsdl.Binding.class,
                Q_ELEM_JBI4CORBA_BINDING, Jbi4CorbaBinding.class);

        registry.registerDeserializer(javax.wsdl.Binding.class,
                Q_ELEM_JBI4CORBA_BINDING, new Jbi4CorbaBindingDeserializer());

        registry.registerSerializer(javax.wsdl.Binding.class,
                Q_ELEM_JBI4CORBA_BINDING, new Jbi4CorbaBindingSerializer());

    }

    public static void overrideAddressDeserializer(ExtensionRegistry registry,
            Jbi4CorbaAddressDeserializer addressDeserializer) {
        registry.registerDeserializer(javax.wsdl.Port.class,
                Q_ELEM_JBI4CORBA_ADDRESS, addressDeserializer);

    }

}
