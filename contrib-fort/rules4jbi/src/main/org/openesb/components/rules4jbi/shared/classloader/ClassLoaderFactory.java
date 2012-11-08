/*
 * @(#)ClassLoaderFactory.java        $Revision: 1.1 $ $Date: 2008/12/17 23:24:43 $
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

import java.io.File;
import java.net.MalformedURLException;

import org.openesb.components.rules4jbi.shared.util.FilesystemUtils;

/**
 * Factory for constructing classloaders used by service units.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/12/17 23:24:43 $
 * 
 * @since 0.4
 */
public final class ClassLoaderFactory {
    
    private ClassLoaderFactory() {}
    
    public static ClassLoader createServiceUnitClassLoader(final File classesDir, final File libDir)
            throws BusinessObjectsNotFoundException
    {
        return createServiceUnitClassLoader(
                classesDir, libDir, null, Thread.currentThread().getContextClassLoader());
    }
    
    public static ClassLoader createServiceUnitClassLoader(final File classesDir, final File libDir,
            final File engineDir, final ClassLoader parent) throws BusinessObjectsNotFoundException
    {
        if (parent == null) {
            throw new NullPointerException("Parent classloader must not be null");
        }
        
        boolean classesDirUsed = isNonEmptyDirectory(classesDir);

        boolean librariesDirUsed = isNonEmptyDirectory(libDir);
        
        boolean engineDirUsed = isNonEmptyDirectory(engineDir);
        
        File[] libJars = null;
        File[] engineJars = null;
        
        if (librariesDirUsed) {
            libJars = FilesystemUtils.findAllJarFiles(libDir);
            
            librariesDirUsed = libJars.length > 0;
        }

        if (engineDirUsed) {
            engineJars = FilesystemUtils.findAllJarFiles(engineDir);
            
            engineDirUsed = engineJars.length > 0;
        }
        
        if (!classesDirUsed && !librariesDirUsed) {
            
            throw new BusinessObjectsNotFoundException(
                    "At least one of classesDir or libDir must exist and be non-empty");
        }
        
        ClassLoader result = parent;
        
        if (classesDirUsed) {
            if (engineDirUsed) {
                result = new SelfFirstClassLoader(parent,
                        FilesystemUtils.toURLs(prepend(classesDir, engineJars)));
                
            } else {
                try {
                    result = new SelfFirstClassLoader(parent, classesDir.toURI().toURL());
                    
                } catch (MalformedURLException e) {
                    
                    throw new AssertionError(
                            "We have verified previously that the classesDir exists and is valid");
                }
            }
            
        } else if (engineDirUsed) {
            result = new SelfFirstClassLoader(parent, FilesystemUtils.toURLs(engineJars));
        }
        
        if (librariesDirUsed) {
            result = new JAXBClassLoader(FilesystemUtils.toURLs(libJars), result);
        }
        
        return result;
    }
    
    static boolean isNonEmptyDirectory(final File directory) {
        return directory != null && directory.exists() && directory.isDirectory()
                && (directory.list().length > 0);
    }
    
    static File[] prepend(final File file, final File[] fileArray) {
        assert file != null;
        assert fileArray != null;
        
        final File[] result = new File[fileArray.length + 1];
        
        result[0] = file;
        
        System.arraycopy(fileArray, 0, result, 1, fileArray.length);
        
        return result;
    }
}
