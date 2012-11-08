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
 * @(#)Util.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema.impl;

/**
 * This class contains some utility methods that facilitate coding.
 * 
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
public class Util {

    /**
     * Tests if two namespaces are same.
     * 
     * @param ns1 the first namespace
     * @param ns2 the second namespace
     * @return <code>true</code> if the namespaces are same, otherwise
     *         <code>false</code>
     */
    public static boolean sameNamespace(String ns1, String ns2) {
        if (ns1 != null && ns1.length() == 0) {
            ns1 = null;
        }
        if (ns2 != null && ns2.length() == 0) {
            ns2 = null;
        }
        return !(ns1 != null && !ns1.equals(ns2)
                || ns2 != null && !ns2.equals(ns1));
    }
}
