/*
 * @(#)JAXBClassLoader.java        $Revision: 1.1 $ $Date: 2008/12/17 23:24:43 $
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

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.HashMap;
import java.util.Map;

import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;

/**
 * A classloader that annotates the loaded classes with JAXB annotations
 * in case they don't have them.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/12/17 23:24:43 $
 * 
 * @see org.openesb.components.rules4jbi.shared.classloader.JAXBClassAdapter
 * @since 0.4
 */
final class JAXBClassLoader extends URLClassLoader {
    
    private final Map<String, Class<?>> loadedClasses;
    
    public JAXBClassLoader(URL[] urls, ClassLoader parent) {
        super(urls, parent);
        
        loadedClasses = new HashMap<String, Class<?>>();
    }

    @Override
    protected synchronized Class<?> findClass(final String name) throws ClassNotFoundException {
        if (name == null) {
            throw new ClassNotFoundException("Attempt to load a class with null name");
        }
        
        if (name.startsWith("java.") || name.startsWith("javax.")) {
            throw new ClassNotFoundException(
                    "Loading classes from reserved packages is not allowed for this classloader: " + name);
        }

        final Class<?> loadedClass = loadedClasses.get(name);
        
        if (loadedClass != null) {
            return loadedClass;
        }
        
        InputStream inputStream = null;
        final byte[] classBytes;
        
        try {
            inputStream = getResourceAsStream(name.replace('.', '/') + ".class");
            
            /* Throws IOException also when inputStream is null */
            ClassReader classReader = new ClassReader(inputStream);
            
            /*
             * Because we don't modify methods of the transformed class,
             * we can set ClassWriter to have a reference to the ClassReader,
             * and this will allow the ClassReader to detect that methods are not
             * modified and it will just copy the byte array representation
             * of methods directly to the ClassWriter, w/o generating proper events
             * for them. The result in better performance of the class transformation.
             */
            ClassWriter classWriter = new ClassWriter(classReader, 0);
            
            
            classReader.accept(new JAXBClassAdapter(classWriter), 0);
            
//            System.out.println("--BEFORE--");
//            classReader.accept(new TraceClassVisitor(new PrintWriter(System.out)), 0);
//            
//            System.out.println("--AFTER--");
//            classReader.accept(
//                    new JAXBClassAdapter(new TraceClassVisitor(classWriter, new PrintWriter(System.out))), 0);
            
            classBytes = classWriter.toByteArray();
            
        } catch (IOException e) {
            throw new ClassNotFoundException("Could not read the input stream", e);
            
        } finally {
            if (inputStream != null) {
                
                try {
                    inputStream.close();
                    
                } catch (IOException e) {
                    //ignore
                }
            }
        }
        
        final Class<?> clazz = defineClass(name, classBytes, 0, classBytes.length);
        
        if (clazz == null) {
            throw new ClassNotFoundException("Failed to define class " + name);
        }
        
        loadedClasses.put(name, clazz);
        
        return clazz;
    }
}
