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
package com.sun.jbi.filebc.util;

import java.util.List;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.xml.namespace.QName;

/*
 *
 * @author Sherry Weng
 * @author Jim Fu
 * 
 */
public class WSDLUtilities {
    private static final String XSD_2001_NAMESPACE = "http://www.w3.org/2001/XMLSchema";
    private static final String XSD_1999_NAMESPACE = "http://www.w3.org/1999/XMLSchema";
    private static final Pattern ENV_VAR_REGEX_PATT = Pattern.compile("\\$\\{([a-zA-Z0-9\\.\\-\\_^\\{\\}]+)\\}");

    private WSDLUtilities() {}
    
    public static boolean isBuiltInType(QName typename) {
        return XSD_2001_NAMESPACE.equals(typename.getNamespaceURI()) ||
                XSD_1999_NAMESPACE.equals(typename.getNamespaceURI());
    }

    public static boolean isBuiltInType(String namespaceURI) {
        return (XSD_2001_NAMESPACE.equals(namespaceURI) ||
                XSD_1999_NAMESPACE.equals(namespaceURI));
    }

    public static boolean isXsdAnyType(QName typename) {
        return (XSD_2001_NAMESPACE.equals(typename.getNamespaceURI()) ||
                XSD_1999_NAMESPACE.equals(typename.getNamespaceURI())) &&
                "anyType".equals(typename.getLocalPart());
    }
    /**
     * helper, extract all app var references in a given string (attribute value)
     * @param attrVal
     * @return - an array of app var names referenced in the given string (attr val)
     */
    public static final Object[] getAppVariableNames(String attrVal) {
        Matcher m = ENV_VAR_REGEX_PATT.matcher(attrVal);
        List refs = null;
        while (m.find()) {
            if (refs == null) {
                refs = new Vector();
            }
            refs.add(m.group(1));
        }
        return refs != null ? refs.toArray() : null;
    }

    /**
     * check if a string contains app var reference ${my_var_001}
     * @param attrVal - the string value to be examed
     * @return true - the string contains at least one occurrence of app var reference
     */
    public static final boolean hasMigrationAppVarRef(String attrVal) {
        return ENV_VAR_REGEX_PATT.matcher(attrVal).find();
    }
}
