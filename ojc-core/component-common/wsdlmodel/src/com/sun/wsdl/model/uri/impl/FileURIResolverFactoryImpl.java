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
 * @(#)FileURIResolverFactoryImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.uri.impl;

import com.sun.wsdl.model.uri.BaseURIResolver;
import com.sun.wsdl.model.uri.CatalogResolver;
import com.sun.wsdl.model.uri.FileURIResolverFactory;


/**
 * Implements a factory that produces URI Resolvers utilizing the file based systems such as local drives
 * and HTTP.
 * @author Sun Microsystems
 * @version 
 */
public class FileURIResolverFactoryImpl extends FileURIResolverFactory {
    
    /** Holds the File URI Resolver. */
    private BaseURIResolver uriResolver = null;
    
    /** Holds the EInsightManager creator */

    
    /** Creates a new instance of FileURIResolverFactoryImpl */
    public FileURIResolverFactoryImpl() {
    }
    
    /** @see com.sun.wsdl.model.common.repository.EInsightManagerCreatable#initializeEInsightManager
     */
    
    /** @see BaseURIResolverFactory#getURIResolver
     */
    public BaseURIResolver getURIResolver(CatalogResolver catalogResolver, String documentBase) {
        if (null == uriResolver) {
            uriResolver = new FileURIResolverImpl(catalogResolver, documentBase);
        }
        return uriResolver;
    }
}
