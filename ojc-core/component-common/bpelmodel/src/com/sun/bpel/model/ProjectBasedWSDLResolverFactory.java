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
 * @(#)ProjectBasedWSDLResolverFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import java.util.List;

import com.sun.bpel.model.common.EInsightModelException;

/**
 * @author Sun Microsystems
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public abstract class ProjectBasedWSDLResolverFactory {
	
	    /** Holds factory singleton. */
	    private static ProjectBasedWSDLResolverFactory factory = null;
	        
	    /**
	     * Gets the WsdlLoaderFactory factory singleton.
	     * @return  a WsdlLoaderFactory factory.
	     * @throws  EInsightModelException  When implementing factory cannot be found.
	     */
	    public static synchronized ProjectBasedWSDLResolverFactory getInstance() throws EInsightModelException {
	        if (null == factory) {
	            String bpelFac = System.getProperty("com.sun.bpel.model.ProjectBasedWSDLResolverFactory",
	                                                "com.sun.bpel.model.impl.ProjectBasedWSDLResolverFactoryImpl");
	            try {
	                factory = (ProjectBasedWSDLResolverFactory) Class.forName(bpelFac).newInstance();
	            } catch (Exception e) {
	                throw new EInsightModelException(
	                    "WsdlLoaderFactory.getInstance(): Cannot find class: " + bpelFac, e);
	            }
	        }
	        return factory;
	    }
	    
	    /**
	     *
	     * Creates a new WsdlLoader.
	     * @return  a new WsdlLoader.
	     */
	    public abstract ProjectBasedWSDLResolver newWSDLResolver(String baseURI, BPELParseContext parseContext);
	    
}
