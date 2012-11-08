/* *************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/
package com.sun.jbi.dcombc.util;

import java.util.HashMap;
import javax.xml.namespace.QName;

/**
 * This is the utility class to handle WSDL data.
 * 
 * @author Chandrakanth Belde
 */
public class WSDLUtilities {
    
    private static HashMap builtInTypes = new HashMap();

    private static final String XSD_NAMESPACE = "http://www.w3.org/2001/XMLSchema";

    // This is not a complete list, but good for now
    // Will append to this list later
    static {
        builtInTypes.put("string", "");
        builtInTypes.put("nonNegativeInteger", "");
        builtInTypes.put("byte", "");
        builtInTypes.put("short", "");
        builtInTypes.put("int", "");
        builtInTypes.put("long", "");
        builtInTypes.put("negativeInteger", "");
        builtInTypes.put("dateTime", "");
        builtInTypes.put("nonPositiveInteger", "");
        builtInTypes.put("duration", "");
        builtInTypes.put("ENTITIES", "");
        builtInTypes.put("double", "");
        builtInTypes.put("ENTITY", "");
        builtInTypes.put("float", "");
        // builtInTypes.put("base64Binary", "");
    }

    public static boolean isBuiltInType(QName typename) {
        return (XSD_NAMESPACE.equals(typename.getNamespaceURI()) && builtInTypes.containsKey(typename.getLocalPart()));
    }

    public static boolean isBuiltInType(String namespaceURI) {
        return (XSD_NAMESPACE.equals(namespaceURI));
    }

}