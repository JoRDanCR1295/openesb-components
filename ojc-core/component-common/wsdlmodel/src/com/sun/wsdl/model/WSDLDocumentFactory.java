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
 * @(#)WSDLDocumentFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model;

import com.sun.wsdl.model.common.model.EInsightModelException;

/**
 *
 * Factory to create WSDL documents.
 *
 * @author Sun Microsystems
 * @version 
 */
public abstract class WSDLDocumentFactory {
   
    /** Holds factory singleton. */
    private static WSDLDocumentFactory factory = null;
        
    /**
     * Gets the WSDL document factory singleton.
     * @return  a WSDL document factory.
     * @throws  EInsightModelException  When implementing factory class not found.
     */
    public static synchronized WSDLDocumentFactory getInstance() throws EInsightModelException {
        if (null == factory) {
            String wsdlFac = System.getProperty("com.sun.wsdl.model.WSDLDocumentFactory",
                                                "com.sun.wsdl.model.impl.WSDLDocumentFactoryImpl");
            try {
                factory = (WSDLDocumentFactory) Class.forName(wsdlFac).newInstance();
            } catch (Exception e) {
                throw new EInsightModelException(
                    "WSDLDocumentFactory.getInstance(): Cannot find class: " + wsdlFac, e);
            }
        }
        return factory;
    }    
    
    /**
     *
     * Creates a new WSDL document.
     * @return  New WSDL XML document.
     */
    public abstract WSDLDocument newWSDLDocument();
}
