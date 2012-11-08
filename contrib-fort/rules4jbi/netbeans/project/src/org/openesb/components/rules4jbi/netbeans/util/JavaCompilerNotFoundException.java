/*
 * @(#)JavaCompilerNotFoundException.java        $Revision: 1.2 $ $Date: 2009/01/25 21:00:54 $
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

/**
 * This exception gets thrown by the <code>JavaCompiler</code> utility class,
 * when it cannot obtain the Java programming language compiler provided with
 * the current Java platform; e.g. when
 * <code>javax.tools.ToolProvider.getSystemJavaCompiler()</code> returns
 * <code>null</code> or when the class <code>com.sun.tools.javac.Main</code>
 * could not be loaded.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2009/01/25 21:00:54 $
 * 
 * @see org.openesb.components.rules4jbi.netbeans.util.JavaCompiler
 * @see javax.tools.ToolProvider#getSystemJavaCompiler()
 * @since 0.4
 */
public class JavaCompilerNotFoundException extends Exception {

    private static final long serialVersionUID = -2449366228226639442L;

    public JavaCompilerNotFoundException(String message) {
        super(message);
    }

    public JavaCompilerNotFoundException(String message, Exception cause) {
        super(message, cause);
    }

    public JavaCompilerNotFoundException(Exception cause) {
        super(cause);
    }
}
