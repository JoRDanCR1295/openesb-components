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
 * @(#)WSDLElementEx.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl4j.ext;

import javax.wsdl.Definition;
import javax.wsdl.WSDLElement;

/**
 * Extends WSDLElement to be able to get the WSDL definition it belongs to.
 * 
 * @author Jun Xu
 * @version $Revision: 1.2 $
 */
public interface WSDLElementEx extends WSDLElement {
    
    /**
     * Gets the WSDL definition this element belongs to.
     * 
     * @return The WSDL definition.
     */
    public Definition getContainingDefinition();
    
    /**
     * Sets the WSDL definition this element belongs to.
     * 
     * @param definition the WSDL definition this element belongs to
     */
    public void setContainingDefinition(Definition definition);
}
