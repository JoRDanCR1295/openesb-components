/*
 * @(#)JarArchiver.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
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

package org.openesb.components.rules4jbi.netbeans.util;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;
import java.util.logging.Logger;

/**
 * Utility class for creating Jar archives.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
 * 
 * @since 0.1
 */
public final class JarArchiver {
    
    private static final Logger logger = Logger.getLogger(JarArchiver.class.getName());
    
    private static final int BUFFER_SIZE = 2048;

    private JarArchiver() {}

    /**
     * Creates the jar archive of the source directory with the given <code>archiveName</code>
     * inside the given destination directory. The source directory root will not be included
     * in the archive. Any empty subdirectories won't be included in the archive as well. In this
     * context, empty directory is also a directory that contains only subdirectories (no files).
     * 
     * @param sourceDirectory source directory, whose content will be included in the archive.
     * Must not be <code>null</code> and must exist on the disk.
     * @param destinationDirectory destination directory where to place the created archive.
     * Must not be <code>null</code> and must exist on the disk.
     * @param archiveName the name of the archive file. Must not be <code>null</code> and must end with '.jar'.
     * @throws java.io.IOException if an i/o error occurs.
     */
    public static void createJarArchive(File sourceDirectory, File destinationDirectory, String archiveName)
            throws IOException
    {
        if (sourceDirectory == null) {
            throw new NullPointerException("Source directory must not be null");
        }

        if (destinationDirectory == null) {
            throw new NullPointerException("Destination directory must not be null");
        }

        if (archiveName == null) {
            throw new NullPointerException("Archive name must not be null");
        }
        
        if (!sourceDirectory.exists()) {
            throw new IllegalArgumentException("Source directory must exist on the disk");
        }
        
        if (!destinationDirectory.exists()) {
            throw new IllegalArgumentException("Destination directory must exist on the disk");
        }
        
        if (!sourceDirectory.isDirectory()) {
            throw new IllegalArgumentException("Parameter source directory must be a directory");
        }

        if (!destinationDirectory.isDirectory()) {
            throw new IllegalArgumentException("Parameter destination directory must be a directory");
        }
        
        if (!archiveName.endsWith(".jar")) {
            throw new IllegalArgumentException("Parameter archiveName must end with '.jar'");
        }
        
        File destinationFile = new File(destinationDirectory, archiveName);
        
        logger.fine("Packaging " + sourceDirectory.getAbsolutePath()
                + " into " + destinationFile.getAbsolutePath());
        
        JarOutputStream out = null;
        try {
            out = new JarOutputStream(
                    new BufferedOutputStream(
                            new FileOutputStream(destinationFile)));

            File[] directoryContent = sourceDirectory.listFiles();

            for (File file : directoryContent) {
                if (file.isDirectory()) {
                    addDirectory("", file, out);
                    
                } else {
                    addFile("", file, out);
                }
            }
            
        } finally {
            if (out != null) {
                try {
                    out.close();
                    
                } catch (IOException e) {
                    logger.fine("Failed to close the output stream: " + e.getMessage());
                }
            }
        }
        
        logger.fine("Packaging completed successfully");
    }

    private static void addDirectory(String context, File directory, JarOutputStream out) throws IOException {
        assert directory.isDirectory();
        
        final String currentContext = context + directory.getName() + "/";
        
        File[] directoryContent = directory.listFiles();
        
        for (File file : directoryContent) {
            if (file.isDirectory()) {
                addDirectory(currentContext, file, out);
                
            } else {
                addFile(currentContext, file, out);
            }
        }
    }
    
    private static void addFile(String context, File file, JarOutputStream out) throws IOException {
        assert file.isFile();
        
        final String entryName = context + file.getName();
        
        logger.finer("Adding file: " + entryName);

        JarEntry entry = new JarEntry(entryName);
        out.putNextEntry(entry);
        
        byte[] data = new byte[BUFFER_SIZE];
        
        BufferedInputStream input = null;
        try {
            input = new BufferedInputStream(new FileInputStream(file), BUFFER_SIZE);

            int count = -1;
            while ((count = input.read(data, 0, BUFFER_SIZE)) != -1) {
                out.write(data, 0, count);
            }
            
            out.closeEntry();
            
        } finally {
            if (input != null) {
                try {
                    input.close();
                    
                } catch (IOException e) {
                    logger.fine("Failed to close the input stream: " + e.getMessage());
                }
            }
        }
    }
}
