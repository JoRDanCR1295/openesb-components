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
 * @(#)FileURIResolverFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.xml.uri;

import java.util.HashMap;
import java.util.Iterator;

import com.sun.bpel.model.common.EInsightModelException;

/**
 * Describes a factory that produces URI Resolvers utilizing the file based systems such as local drives
 * and HTTP.
 *
 * @author Sun Microsystems
 * @version 
 */
public abstract class FileURIResolverFactory implements BaseURIResolverFactory {
    
    /** Creates a new instance of FileURIResolverFactory */
    public FileURIResolverFactory() {
    }
    
    /** Holds map of factory singletons according to repository handle */
    private static HashMap factoryMap = new HashMap();
    

    static public FileURIResolverFactory getInstance() {
    	FileURIResolverFactory factory = null;
	    String uriResFac = System.getProperty("com.sun.bpel.xml.uri.FileURIResolverFactory",
	                                          "com.sun.bpel.xml.uri.impl.FileURIResolverFactoryImpl");
	    try {
	            factory = (FileURIResolverFactory) Class.forName(uriResFac).newInstance();

	    } catch (Exception e) {
	        throw new EInsightModelException(
	            "FileURIResolverFactory.getInstance(): Cannot find class: " + uriResFac, e);
	    }
	
	return factory;
	}


}
