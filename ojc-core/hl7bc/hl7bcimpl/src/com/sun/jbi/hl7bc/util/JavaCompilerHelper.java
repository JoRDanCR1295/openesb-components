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
 * @(#)JavaCompilerHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.hl7bc.util;

import java.io.OutputStream;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.logging.Logger;
import java.util.logging.Level;
import com.sun.jbi.hl7bc.I18n;


public class JavaCompilerHelper {

	private static final Class[] compileMethodSignature;

	private static final Logger log = Logger.getLogger(JavaCompilerHelper.class.getName());

	static {
		compileMethodSignature = new Class[2];
		compileMethodSignature[0] = (new String[0]).getClass();
		compileMethodSignature[1] = PrintWriter.class;
	}

	public static boolean compile(String[] args, OutputStream out) {
		ClassLoader cl = Thread.currentThread().getContextClassLoader();
		Class comSunToolsJavacMainClass = null;
		try {
			comSunToolsJavacMainClass = cl
					.loadClass("com.sun.tools.javac.Main");
			try {
				Method compileMethod = comSunToolsJavacMainClass.getMethod(
						"compile", compileMethodSignature);
				try {
					Object result = compileMethod.invoke(null, new Object[] {
							args, new PrintWriter(out) });
					if (!(result instanceof Integer)) {
                        log.log(Level.SEVERE, I18n.msg("E0316: Reason is Compilation failed for generated JAXB source files {0}.", out));
						return false;
					}
                    log.log(Level.INFO, I18n.msg("I0172: Successfully compiled all generated JAXB sources files {0}.", out));
					return ((Integer) result).intValue() == 0;
				} catch (IllegalAccessException e3) {
					return false;
				} catch (IllegalArgumentException e3) {
					return false;
				} catch (InvocationTargetException e3) {
					return false;
				}
			} catch (NoSuchMethodException e2) {
				log.log(
						Level.SEVERE,
						I18n.msg("E0317: There is no such method {0} available.",
							"compile"));
				return false;
			}
		} catch (ClassNotFoundException e) {
			log.log(
					Level.SEVERE,
					I18n.msg("E0318: {0} is not available in the classpath.",
						"com.sun.tools.javac.Main" ));

			return false;
		} catch (SecurityException e) {
			return false;
		}
	}

}
