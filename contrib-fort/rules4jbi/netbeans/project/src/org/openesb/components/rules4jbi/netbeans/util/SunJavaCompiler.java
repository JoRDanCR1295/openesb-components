/*
 * @(#)SunJavaCompiler.java        $Revision: 1.1 $ $Date: 2009/01/25 21:00:54 $
 * 
 * Copyright (c) 2008, 2009 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.netbeans.util;

import java.io.File;
import java.io.PrintWriter;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.logging.Logger;

/**
 * Implements Java source files compilation using the proprietary
 * <code>com.sun.tools.javac.Main</code> class. Since JDK5 however,
 * this class is documented and officially part of Sun's JDK.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2009/01/25 21:00:54 $
 * 
 * @see org.openesb.components.rules4jbi.netbeans.util.JavaCompiler
 * @since 0.4
 */
final class SunJavaCompiler extends JavaCompiler {
    
    private static final Logger logger = Logger.getLogger(SunJavaCompiler.class.getName());
    
    private static final String JAVA_COMPILER_CLASS_NAME = "com.sun.tools.javac.Main";
    
    private static final String DEFAULT_TOOLS_JAR_LOCATION = "lib"  + File.separator + "tools.jar";
    
    private static final Class<?> compiler;

    static {
        Class<?> clazz = null;

        try {
            File file = new File(System.getProperty("java.home"));

            if (file.getName().equalsIgnoreCase("jre")) {
                file = file.getParentFile();
            }

            file = new File(file, DEFAULT_TOOLS_JAR_LOCATION);

            ClassLoader classLoader = URLClassLoader.newInstance(new URL[] {file.toURI().toURL()});

            clazz = Class.forName(JAVA_COMPILER_CLASS_NAME, true, classLoader);

        } catch (Throwable t) {
            // ignore
        }
        
        compiler = clazz;
    }
    
    boolean compile(final PrintWriter out, File sourceDirectory, File destinationDirectory,
            File... javaSourceFiles) throws JavaCompilerNotFoundException
    {
        if (compiler == null) {
            throw new JavaCompilerNotFoundException("Could not obtain the Java programming language compiler");
        }
        
        assert javaSourceFiles.length > 0;
        
        final String[] options = new String[] {
            "-deprecation",
            "-source", "1.6",
            "-sourcepath", sourceDirectory.getAbsolutePath(),
            "-classpath", destinationDirectory.getAbsolutePath(),
            "-d", destinationDirectory.getAbsolutePath()
        };

        final String[] optionsAndSources = new String[options.length + javaSourceFiles.length];
        
        for (int i = 0; i < options.length; i++) {
            optionsAndSources[i] = options[i];
        }

        for (int i = options.length; i < options.length + javaSourceFiles.length; i++) {
            optionsAndSources[i] = javaSourceFiles[i - options.length].getAbsolutePath();
        }
        
        int exitCode = -1;
        
        try {
            Method compileMethod = compiler.getDeclaredMethod("compile", String[].class, PrintWriter.class);

            Object result = compileMethod.invoke(null, optionsAndSources, out);

            exitCode = (Integer) result;

        } catch (Exception e) {
            throw new JavaCompilerNotFoundException(e);
        }
        
        logger.fine("Compilation finished with exit code " + exitCode);
        return exitCode == 0;
    }
}
