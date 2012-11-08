/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */
 /*
  * $Id: BPELXPathContext.java,v 1.3 2008/10/01 06:39:56 mpottlapelli Exp $
  *
  * Copyright 2006 Sun Microsystems, Inc. All Rights Reserved.  */

package com.sun.jbi.engine.bpel.core.bpel.util;

import org.apache.commons.jxpath.ri.NamespaceResolver;

import com.sun.jbi.common.classloader.CustomClassLoaderUtil;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine.TransformEngine;

/**
 * Utility interface to expose BPEL's base URI in XPath context.
 * 
 * @author Kevan Simpson
 */
public interface BPELXPathContext {
    public void setNamespaceResolver(NamespaceResolver resolver);
    
    public void setBaseURI(String uri);
    public String getBaseURI();
    
    public TransformEngine getTransformEngine();
    public CustomClassLoaderUtil getClassLoaderContext();
}
