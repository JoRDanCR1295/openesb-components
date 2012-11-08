/*
 * Jbi4CorbaPartnerLinkExtension.java
 * 
 * Created on 12-set-2007, 17.59.07
 * 
 * This is an Utility class that contains the constants for the 
 * partner link WSDL4J library.
 */

package it.imolinfo.jbi4corba.utils.plugin.wsdl;

import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;

/**
 *
 * @author mcastaldi
 */
public class Jbi4CorbaPartnerLinkExtension {

    /**
     * The name of the partner link element
    */
    public static final String PRTLNK_ELEMENT = "partnerLinkType";

    /**
     * The name's attribute of the partner link element
    */
    public static final String PRTLNK_NAME_ATTRIBUTE = "name";
    
    /**
     * The role element
    */
    public static final String ROLE_ELEMENT = "role";

    /**
     * The name's attribute of the role element
    */
    public static final String ROLE_ATTRIBUTE_NAME = "name";

    /**
     * The port's attribute of the role element
    */
    public static final String ROLE_ATTRIBUTE_PORT = "portType";

    /**
     * The NameSpace of the partner link
    */
    public static final String NS_PTNLNK
        = "http://docs.oasis-open.org/wsbpel/2.0/plnktype";
    
    /**
     * The namespace prefix for the partner link
    */
    public static final String NS_PREFIX
        = "plnk";
    
    /**
     * The QName of the partner link
    */
    public static final QName Q_ELEMENT_PTNLNK = 
        new QName(PRTLNK_ELEMENT);

    /**
     * The suffix of the role name attribute
    */
    public static final String ROLE_NAME_SUFFIX = 
        "PortTypeRole";

    /**
    * Utility method that registers the extensibility elements for WSDL4J.
    *
    * @param registry The ExtensionRegistry where the partner link extension is registered.
    */
    public static void register(ExtensionRegistry registry) 
    {
        // PartnerLink
        registry.mapExtensionTypes(
                javax.wsdl.Definition.class,
                Q_ELEMENT_PTNLNK, Jbi4CorbaPartnerLink.class);

        registry.registerDeserializer(
                javax.wsdl.Definition.class,
                Q_ELEMENT_PTNLNK, new Jbi4CorbaPartnerLinkDeserializer());

        registry.registerSerializer(
               javax.wsdl.Definition.class,
               Q_ELEMENT_PTNLNK, new Jbi4CorbaPartnerLinkSerializer());
  }
    

}
