/*
 * @(#)SelfFirstClassLoader.java        $Revision: 1.1 $ $Date: 2008/12/17 23:24:43 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.shared.classloader;

import java.net.URL;
import java.net.URLClassLoader;

/**
 * A classloader that first tries to load classes and resources
 * from its own search path; delegates to parent classloader only
 * if unsuccessful.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/12/17 23:24:43 $
 * 
 * @since 0.4
 */
final class SelfFirstClassLoader extends URLClassLoader {

    private final ClassLoader parent;

    public SelfFirstClassLoader(ClassLoader parent, URL... urls) {
        this(urls, parent);
    }
    
    public SelfFirstClassLoader(URL[] urls, ClassLoader parent) {
        super(urls, parent);
        
        if (parent == null) {
            throw new NullPointerException("Parent classloader cannot be null for this classloader");
        }
        
        this.parent = parent;
    }

    @Override
    protected synchronized Class<?> loadClass(final String name, final boolean resolve)
            throws ClassNotFoundException
    {
        if (name == null) {
            throw new ClassNotFoundException("Attempt to load a class with null name");
        }
        
        Class<?> clazz = findLoadedClass(name);
        
        if (clazz == null) {
            if (name.startsWith("java.")) {
                clazz = parent.loadClass(name);
                
            } else {
                try {
                    clazz = findClass(name);

                } catch (ClassNotFoundException e) {
                    clazz = parent.loadClass(name);
                }
            }
        }
        
        if (resolve) {
            resolveClass(clazz);
        }
        
        return clazz;
    }
    
    @Override
    public URL getResource(final String name) {
        final URL result = findResource(name);
        
        return result != null ? result : parent.getResource(name);
    }
}
