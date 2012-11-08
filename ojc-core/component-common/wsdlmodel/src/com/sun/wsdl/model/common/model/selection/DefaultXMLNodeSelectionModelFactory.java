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
 * @(#)DefaultXMLNodeSelectionModelFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.common.model.selection;

import java.util.logging.Logger;
import com.sun.jbi.internationalization.Messages;
import com.sun.wsdl.model.common.model.EInsightModelException;
import com.sun.wsdl.model.common.model.XMLDocument;

/**
 *
 * @author Sun Microsystems
 */
public abstract class DefaultXMLNodeSelectionModelFactory {
    
    private static final Messages MESSAGES = 
            Messages.getMessages(DefaultXMLNodeSelectionModelFactory.class);
    private static final Logger LOGGER = 
            Messages.getLogger(DefaultXMLNodeSelectionModelFactory.class);
    
    private static DefaultXMLNodeSelectionModelFactory factory = null;
    
    
     /**
     * Gets the DefaultXMLNodeSelectionModelFactory factory singleton.
     * @return  a DefaultXMLNodeSelectionModelFactory factory.
     * @throws  EInsightModelException  When implementing factory class not found.
     */
    public static synchronized DefaultXMLNodeSelectionModelFactory getInstance() throws EInsightModelException {
        if (null == factory) {
            String fac = System.getProperty("com.sun.wsdl.model.common.model.selection.DefaultXMLNodeSelectionModelFactory",
                                                "com.sun.wsdl.model.common.model.selection.impl.DefaultXMLNodeSelectionModelFactoryImpl");
            try {
                factory = (DefaultXMLNodeSelectionModelFactory) Class.forName(fac).newInstance();
            } catch (Exception e) {
                throw new EInsightModelException(
                    MESSAGES.getString(
                        "DefaultXMLNodeSelectionModelFactory.GET_INSTANCE_CANNOT_FIND_CLASS", fac), e);
            }
        }
        return factory;
    }
    
    
    public abstract XMLNodeSelectionModel creatXMLNodeSelectionModel(XMLDocument owner);
    
}
