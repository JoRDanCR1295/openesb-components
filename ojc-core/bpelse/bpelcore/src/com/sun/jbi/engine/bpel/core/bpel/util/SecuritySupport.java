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
 * @(#)$Id: SecuritySupport.java,v 1.2 2007/08/10 21:21:57 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;

class SecuritySupport {

	ClassLoader getContextClassLoader() {
		return (ClassLoader) AccessController
				.doPrivileged(new PrivilegedAction() {
					public Object run() {
						ClassLoader cl = null;
						try {
							cl = Thread.currentThread().getContextClassLoader();
						} catch (SecurityException ex) {
						}
						return cl;
					}
				});
	}

	String getSystemProperty(final String propName) {
		return (String) AccessController.doPrivileged(new PrivilegedAction() {
			public Object run() {
				return System.getProperty(propName);
			}
		});
	}

	FileInputStream getFileInputStream(final File file)
			throws FileNotFoundException {
		try {
			return (FileInputStream) AccessController
					.doPrivileged(new PrivilegedExceptionAction() {
						public Object run() throws FileNotFoundException {
							return new FileInputStream(file);
						}
					});
		} catch (PrivilegedActionException e) {
			throw (FileNotFoundException) e.getException();
		}
	}

	InputStream getResourceAsStream(final ClassLoader cl, final String name) {
		return (InputStream) AccessController
				.doPrivileged(new PrivilegedAction() {
					public Object run() {
						InputStream ris;
						if (cl == null) {
							ris = ClassLoader.getSystemResourceAsStream(name);
						} else {
							ris = cl.getResourceAsStream(name);
						}
						return ris;
					}
				});
	}

	boolean doesFileExist(final File f) {
		return ((Boolean) AccessController.doPrivileged(new PrivilegedAction() {
			public Object run() {
				return new Boolean(f.exists());
			}
		})).booleanValue();
	}
}
