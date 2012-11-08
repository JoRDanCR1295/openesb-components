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
 * @(#)JBIWSDL11WrapperWtr.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.nms.wsdl11wrapper;

import javax.wsdl.Message;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * @author Sun Microsystems
 * Nov 3, 2005
 */
public interface JBIWSDL11WrapperWtr {
    /**
     * Set the optional message "name" into wrapped document.
     * This is the logical name defined in the operation binding, not the type name.
     */
    void setMessageName();
    
    /**
     * Add a part in the right position (wrapped in a JBI part wrapper) to the JBI message wrapper element
     * @param partName the name of the message part
     * @param partNode the part node (payload)
     * The part node does not have to be associated with the normalDoc yet, it will be imported
     * @throws WrapperProcessingException if the part could not be added
     */
    void addPart(String partName, Element partNode) throws WrapperProcessingException;
   
    /**
     * Initialize the builder to start a build sequence.
     *
     * Also re-sets a result document if it already exists.
     *
     * @pram docToPopulate Provide an empty document to popluate, or null if the
     * builder should create a new document itself
     * @param wsdlMessageDefinition sets the WSDL message definition of the message to normalize
     * @param operationBindingMessageName The name defined in the WSDL operation binding for the message to normalize to.
     *
     * @throws WrapperProcessingException if the builder could not be initialized
     */
    void initialize(Document docToPopulate, Message wsdlMessageDefinition, String operationBindingMessageName) throws WrapperProcessingException;

}
