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
 * @(#)SwareDOMImplementationFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware;

import org.w3c.dom.DOMImplementation;

import com.sun.xml.transform.sware.impl.SwareDOMImplementationImpl;

/**
 * A factory class that creates SwareDOMImplementation instances.
 * 
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
public class SwareDOMImplementationFactory {

    /**
     * Creates a SwareDOMImplementation instance.
     * 
     * @param domImpl an instance of DOMImplementation
     * @return an instance of SwareDOMImplementation
     */
    public static SwareDOMImplementation newSwareDOMImplementation(
            DOMImplementation domImpl) {
        return new SwareDOMImplementationImpl(domImpl);
    }
}
