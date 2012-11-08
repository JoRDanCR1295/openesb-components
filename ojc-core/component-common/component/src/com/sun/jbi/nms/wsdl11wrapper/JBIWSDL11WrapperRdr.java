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
 * @(#)JBIWSDL11WrapperRdr.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.nms.wsdl11wrapper;

import javax.xml.namespace.QName;

import org.w3c.dom.Element;

/**
 * @author Sun Microsystems
 * Nov 3, 2005
 */
public interface JBIWSDL11WrapperRdr {
    
    /**
     * Returns the part, including the <jbi:part> part wrapper element around
     * the part 'payload'
     *
     * (a jbi:part element may legally contain multiple Elements, or text)
     * @param partName the name of the part
     * @return the wrapped normalized message part
     * @throws WrapperProcessingException if the part could not be returned
     */
    Element getWrappedPart(String partName) throws WrapperProcessingException;
    /**
     * @return the names of the parts in the normalized message
     */
    String[] getPartNames();

    /**
     * Get the message type of the message wrapper in the wrapped document.
     * @return the message type as QName
     */
    QName getMessageType() throws WrapperProcessingException;

    /**
     * Get the optional message "name" defined in the wrapped document.
     * This is the logical name defined in the operation binding, not the type name.
     * @return the message name
     */
    String getMessageName() throws WrapperProcessingException;
}
