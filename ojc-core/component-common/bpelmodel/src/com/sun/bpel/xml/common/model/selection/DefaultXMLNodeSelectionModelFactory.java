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

package com.sun.bpel.xml.common.model.selection;

import com.sun.bpel.model.common.EInsightModelException;
import com.sun.bpel.model.util.I18n;
import com.sun.bpel.xml.common.model.XMLDocument;

/**
 *
 * @author Sun Microsystems
 */
public abstract class DefaultXMLNodeSelectionModelFactory {

    private static DefaultXMLNodeSelectionModelFactory factory = null;
    
    
     /**
     * Gets the DefaultXMLNodeSelectionModelFactory factory singleton.
     * @return  a DefaultXMLNodeSelectionModelFactory factory.
     * @throws  EInsightModelException  When implementing factory class not found.
     */
    public static synchronized DefaultXMLNodeSelectionModelFactory getInstance() throws EInsightModelException {
        if (null == factory) {
            String fac = System.getProperty("com.sun.bpel.xml.common.model.selection.DefaultXMLNodeSelectionModelFactory",
                                                "com.sun.bpel.xml.common.model.selection.impl.DefaultXMLNodeSelectionModelFactoryImpl");
            try {
                factory = (DefaultXMLNodeSelectionModelFactory) Class.forName(fac).newInstance();
            } catch (Exception e) {
                throw new EInsightModelException(
                		I18n.loc("BPMOD-7000: DefaultXMLNodeSelectionModelFactory.getInstance(): Cannot find class:", 
                				fac), e);
            }
        }
        return factory;
    }
    
    
    public abstract XMLNodeSelectionModel creatXMLNodeSelectionModel(XMLDocument owner);
    
}
