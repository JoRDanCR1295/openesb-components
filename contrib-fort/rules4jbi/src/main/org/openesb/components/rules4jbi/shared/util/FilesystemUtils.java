/*
 * @(#)FilesystemUtils.java        $Revision: 1.3 $ $Date: 2009/01/14 02:53:13 $
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

package org.openesb.components.rules4jbi.shared.util;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.channels.FileChannel;

/**
 * This class provides various filesystem utilities.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.3 $ $Date: 2009/01/14 02:53:13 $
 * 
 * @since 0.1
 */
public final class FilesystemUtils {

    /* We do not want to instantiate this class */
    private FilesystemUtils() {}
    
    /**
     * Converts an array of <code>File</code> objects to an array of their corresponding
     * <code>URL</code> objects.
     * 
     * @param files array of <code>File</code>s to convert; must not be null.
     * @return an array of corresponding <code>URL</code>s.
     */
    public static URL[] toURLs(File[] files) {
        if (files == null) {
            throw new NullPointerException("Parameter files must not be null");
        }
        
        URL[] result = new URL[files.length];
        
        for (int i = 0; i < files.length; i++) {
            try {
                result[i] = files[i].toURI().toURL();
                
            } catch (MalformedURLException e) {
                result[i] = null;
            }
        }
        
        return result;
    }
    
    /**
     * Finds all <code>*.jar</code> files in the given directory. There is no guarantee
     * that the returned <code>*.jar</code> files will be in any particular order.
     * 
     * @param directory directory to search for <code>*.jar</code> files; must not be null.
     * @return an array of all <code>*.jar</code> files in the given directory. Returns an empty array
     * if no <code>*.jar</code> files were found. Never returns null.
     */
    public static File[] findAllJarFiles(File directory) {
        if (directory == null) {
            throw new NullPointerException("Parameter directory must not be null");
        }
        
        if (!directory.isDirectory()) {
            throw new IllegalArgumentException("Parameter directory must be a directory");
        }
        
        final File[] result = directory.listFiles(new FileFilter() {

            public boolean accept(File pathname) {
                return pathname.isDirectory() ? false : pathname.getAbsolutePath().endsWith(".jar");
            }
        });
        
        return result;
    }
    
    /**
     * Copies all files from source directory to destination directory,
     * completely replicating the source directory structure.
     * If the destination directory does not exist, it will be created.
     * 
     * @param source source directory.
     * @param destination destination directory.
     * @throws java.io.IOException if an error occurs.
     */
    public static void copyDirectory(File source, File destination) throws IOException {
        if (source.isDirectory()) {
            if (!destination.exists()) {
                destination.mkdir();
            }

            String[] content = source.list();
            for (int i = 0; i < content.length; i++) {
                copyDirectory(new File(source, content[i]), new File(destination, content[i]));
            }
            
        } else {
            copyFile(source, destination);
        }
    }
    
    /**
     * Copies the source file to the destination file. If the destination file does not
     * exist, it will be created. Othewise, it will be overwritten.
     * 
     * @param source source file.
     * @param destination destination file.
     * @throws java.io.IOException if an error occurs.
     */
    public static void copyFile(File source, File destination) throws IOException {
        if (!destination.exists()) {
            destination.createNewFile();
        }

        FileChannel sourceChannel = null;
        FileChannel destinationChannel = null;
        try {
            sourceChannel = new FileInputStream(source).getChannel();
            destinationChannel = new FileOutputStream(destination).getChannel();
            
            destinationChannel.transferFrom(sourceChannel, 0, sourceChannel.size());
            
        } finally {
            if (sourceChannel != null) {
                sourceChannel.close();
            }
            
            if (destinationChannel != null) {
                destinationChannel.close();
            }
        }
    }
    
//    public static void main(String[] args) throws IOException {
//        copyDirectory(new File("/tmp/classes"), new File("/tmp/test"));
//        
//        copyFile(new File("/tmp/a.txt"), new File("/tmp/b.txt"));
//    }
}
