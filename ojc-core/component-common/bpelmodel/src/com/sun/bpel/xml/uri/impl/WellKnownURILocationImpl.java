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
 * @(#)WellKnownURILocationImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.xml.uri.impl;

import javax.xml.XMLConstants;

import com.sun.bpel.xml.xsd.XSDURILocationImpl;

/**
 * Implements a well-known URI Location.
 *
 * @author Sun Microsystems
 * @version 
 */
public class WellKnownURILocationImpl extends XSDURILocationImpl {
    
    /** Creates a new instance of WellKnownURILocationImpl.
     * @param   urn     URN for well-known namespace.
     */
    public WellKnownURILocationImpl(String urn) {
        super(urn,
            "<xs:schema targetNamespace=\"" + urn + "\" xmlns:xs=\""
            + XMLConstants.W3C_XML_SCHEMA_NS_URI + "\">"
            + "<xs:documentation>This is a well-known document.</xs:documentation>"
            + "</xs:schema>");
    }    
}
