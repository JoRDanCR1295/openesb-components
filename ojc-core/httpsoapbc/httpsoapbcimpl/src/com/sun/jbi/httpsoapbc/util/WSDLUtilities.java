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
 * @(#)WSDLUtilities.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.util;

import java.util.HashMap;
import javax.xml.namespace.QName;

/**
 * @author Sherry Weng
 * @version   
 */
public class WSDLUtilities {
    private static HashMap builtInTypes = new HashMap();
    private static final String XSD_NAMESPACE = "http://www.w3.org/2001/XMLSchema";
    
    // This is not a complete list, but good for now
    // Will append to this list later
    static {
    	builtInTypes.put("string", "");
        builtInTypes.put("nonNegativeInteger","");
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
        builtInTypes.put("base64Binary", "");
    }
    
    // This is not a very sophiscated way to determine built-in types.
    // Will also need to resolve derived types etc.
    public static boolean isBuiltInType(QName typename) {
        return (XSD_NAMESPACE.equals(typename.getNamespaceURI()) &&
                builtInTypes.containsKey(typename.getLocalPart()));
    }
    
    // This is not a very sophiscated way to determine built-in types.
    // Will also need to resolve derived types etc.
    public static boolean isBuiltInType(String namespaceURI) {
        return (XSD_NAMESPACE.equals(namespaceURI));
    }
   
}
