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
 * @(#)PojoClassScanner.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.pojose.core.anno.processor;

import org.glassfish.openesb.pojose.core.anno.processor.visitor.Visitor;
import java.io.File;
import java.io.FileFilter;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.glassfish.openesb.pojose.core.util.I18n;

/**
 *
 * @author gpatil
 */
public class PojoClassScanner {

    private FileFilter ff;
    private ProxyClassLoader cl;

    private static Logger logger = Logger.getLogger(POJOAnnotationProcessor.class.getName());

    public PojoClassScanner(ProxyClassLoader cl) {
        this.cl = cl;
        this.ff = new FileFilter() {
            public boolean accept(File pathname) {
                if (pathname.isDirectory() || pathname.getName().endsWith(".class")) { //NOI18N
                    return true;
                }
                return false;
            }
        };
    }

    /**
     * Scans the directory recursivly for class files and scan for POJO SE annotation
     *
     * @param root directory
     * @param cl ClassLoader
     * @return List of POJOClassMetadata
     * @throws java.lang.ClassNotFoundException
     */
    public void scan(File root, Visitor v) {
        scan(root, "", v); //NOI18N
    }

    /**
     * Scans the directory recursivly for class files and scan for POJO SE annotation
     * @param dir directory to scan recursivly.
     * @param cl ClassLoader
     * @param pkg package name empty string or "." ending String.
     * @throws java.lang.ClassNotFoundException
     */
    private void scan(File dir, String pkg, Visitor v) {
        String cn = null;

        if (dir != null) {
            File[] files = dir.listFiles(ff);
            if (files != null) {
                for (File file : files) {
                    if (file.isFile()) {
                        cn = pkg + file.getName().substring(0, file.getName().lastIndexOf(".class")); //NOI18N
                        ProxyClass cls;
                        try {
                            cls = cl.loadClass(cn);
                            v.visitClass(cls);
                        } catch (ClassNotFoundException ex) {
                            String msg = I18n.loc("POJOSE-7005: Error loading class {0}", cn);
                            logger.log(Level.SEVERE, msg, ex);
                        }
                    } else if (file.isDirectory()) {
                        scan(file, pkg + file.getName() + ".", v); //NOI18N
                    }
                }
            }
        }
    }
}
