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
 * @(#)FastWSDLDefinitionsFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model;

import java.io.InputStream;
import java.io.Reader;

import com.sun.wsdl.model.common.model.EInsightModelException;

/**
 * @author Sun Microsystems
 *
 * A factory which parses wsdl fast. 
 * Just parse some attributes from wsdl and ignore rests.
 */
public abstract class FastWSDLDefinitionsFactory {
	
	private static FastWSDLDefinitionsFactory factory;
	
	public FastWSDLDefinitionsFactory() {
		
	}
        
    /**
     * Gets the Fast WSDL Definitions factory singleton.
     * @return  a Fast WSDL Definitions factory.
     * @throws  EInsightModelException  When implementing factory class not found.
     */
    public static synchronized FastWSDLDefinitionsFactory getInstance() throws EInsightModelException {
        if (null == factory) {
            String wsdlFac = System.getProperty("com.sun.wsdl.model.FastWSDLDefinitionsFactory",
                                                "com.sun.wsdl.model.impl.FastWSDLDefinitionsFactoryImpl");
            try {
                factory = (FastWSDLDefinitionsFactory) Class.forName(wsdlFac).newInstance();
            } catch (Exception e) {
                throw new EInsightModelException(
                    "FastWSDLDefinitionsFactory.getInstance(): Cannot find class: " + wsdlFac, e);
            }
        }
        return factory;
    }    
    
    public abstract FastWSDLDefinitions newFastWSDLDefinitions(Reader in, boolean parseImports);
    
    public abstract FastWSDLDefinitions newFastWSDLDefinitions(InputStream in, boolean parseImports);
    
	public abstract FastWSDLDefinitions newFastWSDLDefinitions(String defFileUrl);
	
	public abstract FastWSDLDefinitions newFastWSDLDefinitions(String defFileUrl, 
													  			boolean parseImports);
		
}
