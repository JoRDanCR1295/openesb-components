/*
 * @(#)JavaCompiler.java        $Revision: 1.3 $ $Date: 2009/01/25 21:00:54 $
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
import java.util.Arrays;
import java.util.logging.Logger;

/**
 * Utility class used to compile Java source files.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.3 $ $Date: 2009/01/25 21:00:54 $
 * 
 * @since 0.1
 */
public abstract class JavaCompiler {
    
    private static final Logger logger = Logger.getLogger(JavaCompiler.class.getName());
    
    private static final JavaCompiler COMPILER = new SunJavaCompiler();
    
    public static JavaCompiler getCompiler() {
        return COMPILER;
    }
    
    /**
     * Compiles a single java source file.
     * 
     * @param out specifies where to redirect error and warning messages
     * @param sourceDirectory <code>javac -sourcepath</code> option
     * @param destinationDirectory <code>javac -d</code> and <code>-classpath</code> option
     * @param javaSourceFile java source file to compile
     * @return true iff the compilation was successful
     */
    public boolean compileSingle(PrintWriter out, File sourceDirectory, File destinationDirectory,
            File javaSourceFile) throws JavaCompilerNotFoundException
    {
        logger.fine("Compiling file: " + javaSourceFile.getAbsolutePath());
        
        return compile(out, sourceDirectory, destinationDirectory, javaSourceFile);
    }
    
    /**
     * Compiles all java source files underneath the specified source directory.
     * 
     * @param out specifies where to redirect error and warning messages
     * @param sourceDirectory directory to search for java source files, will be also specified as
     * <code>-sourcepath</code> option of the <code>javac</code> compiler
     * @param destinationDirectory <code>javac -d</code> and <code>-classpath</code> option
     * @return true if <em>all</em> java source files underneath the specified source directory compiled
     * successfully; false othewise
     */
    public boolean compileAll(PrintWriter out, File sourceDirectory, File destinationDirectory,
            File[] javaSourceFiles) throws JavaCompilerNotFoundException
    {
        logger.fine("Compiling files: " + Arrays.toString(javaSourceFiles));
        
        if (javaSourceFiles.length == 0) {

            /* Nothing to compile; should not fail the build (questionable decision) */
            return true;
        }
        
        return compile(out, sourceDirectory, destinationDirectory, javaSourceFiles);
    }
    
    abstract boolean compile(PrintWriter out, File sourceDirectory, File destinationDirectory,
            File... javaSourceFiles) throws JavaCompilerNotFoundException;
}
