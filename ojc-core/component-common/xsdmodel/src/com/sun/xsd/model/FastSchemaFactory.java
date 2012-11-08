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
 * @(#)FastSchemaFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xsd.model;

import java.io.InputStream;


/**
 * @author Sun Microsystems
 *
 * A factory which parses wsdl fast. 
 * Just parse some attributes from wsdl and ignore rests.
 */
public abstract class FastSchemaFactory {
	
	private static FastSchemaFactory factory;
	
	public FastSchemaFactory() {
		
	}
        
    /**
     * Gets the Fast WSDL Definitions factory singleton.
     * @return  a Fast WSDL Definitions factory.
     * @throws  EInsightModelException  When implementing factory class not found.
     */
    public static synchronized FastSchemaFactory getInstance() throws Exception {
        if (null == factory) {
            String fac = System.getProperty("com.sun.xsd.model.FastSchemaFactory",
                                                "com.sun.xsd.model.impl.FastSchemaFactoryImpl");
            try {
                factory = (FastSchemaFactory) Class.forName(fac).newInstance();
            } catch (Exception e) {
                throw new Exception (
                    "FastSchemaFactory.getInstance(): Cannot find class: " + fac, e);
            }
        }
        return factory;
    }    
    
    public abstract FastSchema newFastSchema(InputStream in, boolean parseImports);
    
	public abstract FastSchema newFastSchema(String defFileUrl);
	
	public abstract FastSchema newFastSchema(String defFileUrl, 
													  			boolean parseImports);
		
}
