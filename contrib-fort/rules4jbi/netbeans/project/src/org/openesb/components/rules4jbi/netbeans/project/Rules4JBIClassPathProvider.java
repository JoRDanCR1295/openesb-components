/*
 * @(#)Rules4JBIClassPathProvider.java        $Revision: 1.2 $ $Date: 2009/01/25 21:00:54 $
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

package org.openesb.components.rules4jbi.netbeans.project;

import org.openesb.components.rules4jbi.netbeans.project.directory.DirectoryManager;
import java.util.logging.Logger;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.java.platform.JavaPlatformManager;
import org.netbeans.spi.java.classpath.ClassPathProvider;
import org.netbeans.spi.java.classpath.support.ClassPathSupport;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2009/01/25 21:00:54 $
 * 
 * @see org.netbeans.spi.java.classpath.ClassPathProvider
 * @since 0.1
 */
public final class Rules4JBIClassPathProvider implements ClassPathProvider {
    
    private static final Logger logger = Logger.getLogger(Rules4JBIClassPathProvider.class.getName());

    private final Rules4JBIProject project;
    
    private final ClassPath bootClassPath;
    
    private final ClassPath sourceClassPath;

    public Rules4JBIClassPathProvider(Rules4JBIProject project) {
        this.project = project;

        bootClassPath = JavaPlatformManager.getDefault().getDefaultPlatform().getBootstrapLibraries();
        
        sourceClassPath = ClassPathSupport.createClassPath(project.getDirectoryManager().getSourceDirectory());
    }

    public ClassPath findClassPath(FileObject file, String type) {
        logger.fine("Retrieving " + type + " classpath of " + FileUtil.getFileDisplayName(file));

        final DirectoryManager directoryManager = project.getDirectoryManager();

        final FileObject sourceDirectory = directoryManager.getSourceDirectory();
        if (!sourceDirectory.equals(file) && !FileUtil.isParentOf(sourceDirectory, file)) {
            return null;
        }

        if (ClassPath.BOOT.equals(type)) {
            return bootClassPath;
        }
        
        if (ClassPath.SOURCE.equals(type)) {
            return sourceClassPath;
        }
        
        if (ClassPath.COMPILE.equals(type)) {
            if (directoryManager.classesDirectoryExists()) {
                logger.finer("Compile classpath found");

                return ClassPathSupport.createClassPath(directoryManager.getOrCreateClassesDirectory());

            } else {
                return null;
            }
        }
        
        return null;
    }
    
    ClassPath getBootClassPath() {
        return bootClassPath;
    }

    ClassPath getSourceClassPath() {
        return sourceClassPath;
    }
}
