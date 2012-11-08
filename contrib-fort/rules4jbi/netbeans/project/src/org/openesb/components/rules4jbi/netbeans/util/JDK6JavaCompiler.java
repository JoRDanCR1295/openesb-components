/*
 * @(#)JDK6JavaCompiler.java        $Revision: 1.1 $ $Date: 2009/01/25 21:00:54 $
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
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Logger;

import javax.tools.Diagnostic;
import javax.tools.DiagnosticListener;
import javax.tools.JavaCompiler.CompilationTask;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.ToolProvider;

/**
 * Implements Java source files compilation using the Java compiler
 * available through the Java core APIs since JDK6.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2009/01/25 21:00:54 $
 * 
 * @see org.openesb.components.rules4jbi.netbeans.util.JavaCompiler
 * @since 0.1
 */
final class JDK6JavaCompiler extends JavaCompiler {
    
    private static final Logger logger = Logger.getLogger(JDK6JavaCompiler.class.getName());
    
    boolean compile(final PrintWriter out, File sourceDirectory, File destinationDirectory,
            File... javaSourceFiles) throws JavaCompilerNotFoundException
    {
        javax.tools.JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();

        if (compiler == null) {
            throw new JavaCompilerNotFoundException("Could not obtain the Java programming language compiler");
        }
        
        assert javaSourceFiles.length > 0;
        
        DiagnosticListener<JavaFileObject> diagnosticListener = new DiagnosticListener<JavaFileObject>() {

            public void report(Diagnostic<? extends JavaFileObject> diagnostic) {
                logger.finer("Problem during compilation: " + diagnostic.getMessage(null));
                
                out.print(diagnostic.getKind().toString() + ": ");

                /*
                 * If we don't split the message and print it line by line,
                 * PrintWriter implementation retrieved from InputOutput.getErr()
                 * will print only the last line in red; probably a bug in InputOutput
                 */
                String[] messages = diagnostic.getMessage(null).split("\\n");

                for (String message : messages) {
                    out.println(message);
                }

                JavaFileObject source = diagnostic.getSource();
                
                if (source != null) {
                    long lineNumber = diagnostic.getLineNumber();
                    
                    if (lineNumber != Diagnostic.NOPOS) {
                        out.printf("Source: %s:%d%n", source.getName(), lineNumber);
                        
                    } else {
                        out.printf("Source: %s%n", source.getName());
                    }
                }
            }
        };

        StandardJavaFileManager javaFileManager =
                compiler.getStandardFileManager(diagnosticListener, null, null);

        Iterable<? extends JavaFileObject> compilationUnits =
                javaFileManager.getJavaFileObjects(javaSourceFiles);
        
        List<String> options = Arrays.asList(
                "-deprecation",
                "-source", "1.6",
                "-sourcepath", sourceDirectory.getAbsolutePath(),
                "-classpath", destinationDirectory.getAbsolutePath(),
                "-d", destinationDirectory.getAbsolutePath());

        CompilationTask task =
                compiler.getTask(out, javaFileManager, diagnosticListener, options, null, compilationUnits);

        Boolean success = task.call();

        try {
            javaFileManager.close();

        } catch (IOException e) {
            logger.fine("Failed to close java file manager: " + e.getMessage());
        }
        
        logger.fine("Compilation " + (success ? "successful" : "failed"));
        return success;
    }
}
