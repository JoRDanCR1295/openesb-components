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
 * @(#)XMLSchemaFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.xsd;

import java.util.logging.Logger;

import com.sun.wsdl.model.common.model.EInsightModelException;
import com.sun.jbi.internationalization.Messages;


/**
 * @author Sun Microsystems
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public abstract class XMLSchemaFactory {
	/** Holds factory singleton. */
    private static XMLSchemaFactory factory = null;
    
    private static final Messages MESSAGES = 
            Messages.getMessages(XMLSchemaFactory.class);
    private static final Logger LOGGER = 
            Messages.getLogger(XMLSchemaFactory.class);
        
    /**
     *
     * Gets the WSDL document factory singleton.
     * @return  a WSDL document factory.
     * @throws  EInsightModelException  When implementing factory class not found.
     */
    public static synchronized XMLSchemaFactory getInstance() throws EInsightModelException {
        if (null == factory) {
            String artiFac = System.getProperty("com.sun.wsdl.model.xsd.XMLSchemaFactory",
                                                "com.sun.wsdl.model.xsd.impl.XMLSchemaFactoryImpl");
            									 
            									 
            try {
                factory = (XMLSchemaFactory) Class.forName(artiFac).newInstance();
            } catch (Exception e) {
                throw new EInsightModelException(
                    MESSAGES.getString(
                        "XMLSchemaFactory.ARTIFACTUTILFACT_GETINSTANCE_CANNOT_FIND_CLASS_x",
                        new Object[]{artiFac}), e);
            }
        }
        return factory;
    }    
    
    /**

     * Creates a new XMLSchema document.
     * @return  New XMLSchema XML document.
     */
    public abstract XMLSchema newXMLSchema();
}
