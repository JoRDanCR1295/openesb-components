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
 * @(#)WrapperClassLoader.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.pojose.core.anno.processor.impl;

import java.util.logging.Level;
import java.util.logging.Logger;
import org.glassfish.openesb.pojose.core.anno.processor.*;

/**
 * Implementation using Java or live classloader.
 * 
 * @author gpatil
 */
public class WrapperClassLoader implements ProxyClassLoader{
    private ClassLoader cl;
    private static Logger logger = Logger.getLogger(POJOAnnotationProcessor.class.getName());

    public WrapperClassLoader(ClassLoader cl){
        this.cl = cl;
    }
    
    public ProxyClass loadClass(String fqClsName) throws ClassNotFoundException{
        Class cls = null;
        
        ClassLoader thdCtxCl = Thread.currentThread().getContextClassLoader();
        try {
            Thread.currentThread().setContextClassLoader(this.cl);
            cls = this.cl.loadClass(fqClsName);            
        } catch (Exception ex) {
            logger.log(Level.SEVERE, null, ex);
        } finally {
            if (thdCtxCl != null){
                Thread.currentThread().setContextClassLoader(thdCtxCl);
            }
        }                

        if (cls != null){
            return ProxyFactory.getClass(cls);            
        }else{
            return null;
        }
    }
}
