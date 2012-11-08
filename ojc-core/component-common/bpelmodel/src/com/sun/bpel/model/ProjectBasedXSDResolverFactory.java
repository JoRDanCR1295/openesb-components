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
 * @(#)ProjectBasedXSDResolverFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import com.sun.bpel.model.common.EInsightModelException;

/**
 * 
 * @author Sun Microsystems
 *
 */
public abstract class ProjectBasedXSDResolverFactory {
	
	/** Holds factory singleton. */
    private static ProjectBasedXSDResolverFactory factory = null;
        
    /**
     * Gets the ProjectBasedXSDResolverFactory factory singleton.
     * @return  a ProjectBasedXSDResolverFactory factory.
     * @throws  EInsightModelException  When implementing factory cannot be found.
     */
    public static synchronized ProjectBasedXSDResolverFactory getInstance() throws EInsightModelException {
        if (null == factory) {
            String bpelFac = System.getProperty("com.sun.bpel.model.ProjectBasedXSDResolverFactory",
                                                "com.sun.bpel.model.impl.ProjectBasedXSDResolverFactoryImpl");
            try {
                factory = (ProjectBasedXSDResolverFactory) Class.forName(bpelFac).newInstance();
            } catch (Exception e) {
                throw new EInsightModelException(
                    "ProjectBasedXSDResolverFactory.getInstance(): Cannot find class: " + bpelFac, e);
            }
        }
        return factory;
    }
    
    /**
     *
     * Creates a new ProjectBasedXSDResolver.
     * @return  a new ProjectBasedXSDResolver.
     */
    public abstract ProjectBasedXSDResolver newXSDResolver(String baseURI, BPELParseContext parseContext);

}
