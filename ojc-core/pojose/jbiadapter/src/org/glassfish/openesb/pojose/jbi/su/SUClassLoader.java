/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.glassfish.openesb.pojose.jbi.su;

import java.net.URL;
import java.net.URLClassLoader;
import java.net.URLStreamHandlerFactory;

/**
 * TODO
 * @author gpatil
 */
public class SUClassLoader extends URLClassLoader{

    public SUClassLoader(URL[] urls, ClassLoader parent, URLStreamHandlerFactory factory) {
        super(urls, parent, factory);
    }

    public SUClassLoader(URL[] urls) {
        super(urls);
    }

    public SUClassLoader(URL[] urls, ClassLoader parent) {
        super(urls, parent);
    }

}
