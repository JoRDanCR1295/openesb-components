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
 * @(#)WSDLDocument.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.xml.wsdl;

import javax.wsdl.Definition;

/**
 * An instance of the interface represents a container for a WSDL definition.
 * Introducing the interface just to be in line with the old WSDL model
 * methodology.
 * 
 * @author Jun Xu
 * @version $Revision: 1.2 $
 */
public interface WSDLDocument {

    /** Namespace for WSDL */
    public static final String WSDL_NAMESPACE =
        "http://schemas.xmlsoap.org/wsdl/";
    
    /** Namespace for WSDL Service Link Type */
    public static final String WSDL_SLNK_NAMESPACE =
        "http://docs.oasis-open.org/wsbpel/2.0/plnktype";
    
    /** Namespace for WSDL Service Reference */
    public static final String WSDL_SREF_NAMESPACE =
        "http://schemas.xmlsoap.org/ws/2002/07/service-reference/";
    
    /**
     * Gets the URI from which the WSDL definition is loaded.
     * 
     * @return The URI
     */
    public String getURI();
    
    /**
     * Gets the WSDL definition contained by the document.
     * 
     * @return The WSDL definition.
     */
    public Definition getDefinition();
}
