 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator;

import java.net.URL;
import java.net.URLClassLoader;
import java.net.URLStreamHandlerFactory;

/**
 * URLClassLoader that look first in the child URL.
 * @author Marco Piraccini
 */
public class ChildFirstClassLoader extends URLClassLoader {


    public ChildFirstClassLoader() {
        this(null, null, null);
    }


    public ChildFirstClassLoader(final ClassLoader parent) {
        this(new URL[0], parent, null);
    }

    public ChildFirstClassLoader(final URL[] urls) {
        this(urls, null, null);
    }

    public ChildFirstClassLoader(final URL[] urls, final ClassLoader parent) {
        this(urls, parent, null);
    }
    public ChildFirstClassLoader(final URL[] urls, final ClassLoader parent, final URLStreamHandlerFactory factory) {
        super(urls, parent, factory);
    }
    public static final URLClassLoader newInstance(final URL[] urls) {
        return new ChildFirstClassLoader(urls);
    }      
    public static final URLClassLoader newInstance(final URL[] urls, final ClassLoader parent) {
        return new ChildFirstClassLoader(urls, parent);
    } 

    public final Class loadClass(String name, boolean resolve) throws ClassNotFoundException {
        // First check if it's already loaded
        Class clazz = findLoadedClass(name);
        
        if (clazz == null) {
            try {
                // Child first!!!!
                clazz = findClass(name);
            } catch (ClassNotFoundException cnfe) {
                ClassLoader parent = getParent();
                
                if (parent != null) {
                    // Ask to parent
                    clazz = parent.loadClass(name);
                } else {
                    // Propagate exception
                    throw cnfe;
                }
            }
        }

        if (resolve) {
            resolveClass(clazz);
        }

        return clazz;
    }         

}

