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
 * @(#)ProxyClass.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.pojose.core.anno.processor;

import java.util.List;

/**
 * An interface to represent Java class for POJO SE related functions.
 * 
 * There are 2 implementations. One is actual/live Java Class ClassLoader  
 * based another using Netbeans ClassFile APIs.
 * 
 * @author gpatil
 */
public interface ProxyClass {
    public ProxyPOJOAnnotation getAnnotationPOJO();    
    public ProxyProviderAnnotation getAnnotationProvider();    
    public List<ProxyMethod> getMethods();
    public String getName();
}
