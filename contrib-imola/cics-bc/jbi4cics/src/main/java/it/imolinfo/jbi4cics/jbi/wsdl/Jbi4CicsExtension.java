/*
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 */


package it.imolinfo.jbi4cics.jbi.wsdl;

import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;
import javax.wsdl.Binding;
import javax.wsdl.Port;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;

/**
 * JWSDL Extension class. See JSR 110.
 *
 * @author amedeocannone
 * @author marcopiraccini
 * @author <a href="mailto:mcimatti@imolinfo.it">Marco Cimatti</a>
 */
public final class Jbi4CicsExtension {

    /**
     * The namespace used for Jbi4Cics WSDL elements.
     */
    public static final String NS_URI_JBI4CICS
            = "uri://schemas.imola.it/jbi/wsdl-extensions/cics/";

    public static final String DEFAULT_PREFIX = "imolacics";
    public static final String BINDING_ELEMENT = "binding";
    public static final String ADDRESS_ELEMENT = "address";
    public static final String COPY_COBOL_ELEMENT = "copyCobol";
    public static final String OUTPUT_COPY_COBOL_ELEMENT = "outputCopyCobol";
    public static final String SERVICE_PACKAGE_NAME_ATTRIBUTE = "servicePackageName";
    public static final String CODE_PAGE_NAME_ATTRIBUTE = "codePage";
    public static final String SAME_COPY_COBOL_ATTRIBUTE = "sameCopyCobol";
    public static final String USERNAME_ATTRIBUTE = "username";
    public static final String PASSWORD_ATTRIBUTE = "password";
    public static final String CONNECTION_TYPE_ATTRIBUTE = "connectionType";
    public static final String JNDI_CONNECTION_NAME_ATTRIBUTE = "JNDIConnectionName";
    public static final String PROGRAM_NAME_ATTRIBUTE = "programName";
    public static final String TRANSACTION_NAME_ATTRIBUTE = "transactionName";
    public static final String TPN_ATTRIBUTE = "tpn";

    /**
     * The qualified name of Jbi4Cics extension binding element.
     */
    public static final QName Q_ELEM_JBI4CICS_BINDING
            = new QName(NS_URI_JBI4CICS, BINDING_ELEMENT);

    /**
     * The qualified name of Jbi4Cics extension address element.
     */
    public static final QName Q_ELEM_JBI4CICS_ADDRESS
            = new QName(NS_URI_JBI4CICS, ADDRESS_ELEMENT);

    /**
     * The qualified name of the copy Cobol used for input and, optionally, also
     * for output.
     */
    public static final QName Q_ELEM_JBI4CICS_COPY_COBOL
            = new QName(NS_URI_JBI4CICS, COPY_COBOL_ELEMENT);

    /**
     * The qualified name of the output copy Cobol.
     */
    public static final QName Q_ELEM_JBI4CICS_OUTPUT_COPY_COBOL
            = new QName(NS_URI_JBI4CICS, OUTPUT_COPY_COBOL_ELEMENT);

    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(Jbi4CicsExtension.class);

    /**
     * Constructor. It's declared <code>private</code> to prevent instance
     * creation for this class.
     */
    private Jbi4CicsExtension() {
    }

    /**
     * Registers the Jbi4Cics WSDL extensions into the specified registry.
     *
     * @param  registry  the extensions registry to register the Jbi4Cics WSDL
     *                   extensions.
     */
    public static void register(ExtensionRegistry registry) {
        LOG.debug("Start ExtensionRegistry registration");

        // Address
        registry.mapExtensionTypes(
                Port.class, Q_ELEM_JBI4CICS_ADDRESS, Jbi4CicsAddress.class);
        registry.registerDeserializer(Port.class, Q_ELEM_JBI4CICS_ADDRESS,
                                      new Jbi4CicsAddressDeserializer());
        registry.registerSerializer(Port.class, Q_ELEM_JBI4CICS_ADDRESS,
                                    new Jbi4CicsAddressSerializer());

        // Binding
        registry.mapExtensionTypes(Binding.class,
                Q_ELEM_JBI4CICS_BINDING, Jbi4CicsBinding.class);
        registry.registerDeserializer(Binding.class,
                Q_ELEM_JBI4CICS_BINDING, new Jbi4CicsBindingDeserializer());
        registry.registerSerializer(Binding.class,
                Q_ELEM_JBI4CICS_BINDING, new Jbi4CicsBindingSerializer());
    }
}
