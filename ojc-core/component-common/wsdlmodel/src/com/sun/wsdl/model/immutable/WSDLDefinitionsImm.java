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
 * @(#)WSDLDefinitionsImm.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.immutable;

import java.util.List;

/**
 * Describes the WSDL &lt;definitions&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 * @since   5.1.0
 */
public interface WSDLDefinitionsImm extends WSDLElementImm {

	/** Gets the target namespace URI.
	 * @return  Target namespace URI.
	 */
	String getTargetNamespace();
    
    /**
     * Getter for property name.
     * @return Value of property name.
     */
    String getName();
    
    /**
     * Gets the list of all imports
     * @return a read-only list of Imports.
     */
    List getImports();
   
    /**
     * Gets the list of all imported WSDL documents.
     * @return a read-only list of WSDLDocuments
     */
    List getImportedDocuments();
    
    /**
     * Gets the types element.
     * @return  Immutable types element.
     */
    TypesImm getTypes();
    
    /**
     * Gets the list of all messages.
     * @return a read-only list of WSDLMessages.
     */
    List getMessages();
    
    /**
     * Gets the list of all portTypes
     * @return a read-only list of PortTypes.
     */
    List getPortTypes();
    
    /**
     * Gets the list of all bindings
     * @return a read-only list of bindings.
     */
    List getBindings();
    
    /**
     * Gets the list of all services
     * @return a read-only list of Services.
     */
    List getServices();
    
    /**
     * Gets the list of all serviceLinkTypes
     * @return a read-only list of Services.
     */
    List getServiceLinkTypes();
    
    /** Gets a non-modifiable collection of all bpws:property elements.
     * @return Read-only list of property elements.
     */
    List getBPWSProperties();
    
    /** Gets a non-modifiable collection of all bpws:propertyAlias elements.
     * @return Read-only list of propertyAlias elements.
     */
    List getBPWSPropertyAliases();
    
}
