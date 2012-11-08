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
 * @(#)WSDLDocumentImm.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.immutable;

/**
 *  
 * Created on Jul 8, 2004
 * Immutable WSDLDocument.
 *
 * @author Sun Microsystems
 * @version 
 * @since   5.1.0
 */
public interface WSDLDocumentImm {
        
    /**
     * Gets the root &lt;definitions&gt; element of the WSDL document.
     * @return the root element
     */
    WSDLDefinitionsImm getDocumentDefinitions();
         
    /**
     * Serializes accurately the WSDL document into a string that can be parsed
     * back to recreate the WSDL document model.
     *
     * @param formatArgs, pass info as to how to format the string.
     * @return  A serialized form of the WSDL document.
     */
    String serialize(Object[] formatArgs);

}
