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
 * @(#)JarUtil.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.util;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;
import java.util.jar.JarInputStream;


/**
 * an utility class to create a jar file
 * @author Raghunadh
 *
 */
public class JarUtil {

	private static final int BUFFER_SIZE = 0x10000; //64k

	private static byte[] buffer = new byte[BUFFER_SIZE];
    
    private static String mAcceptAckJarName;
    
    private static String mAppAcceptAckJarName;

	/*
     * jar directory into target archive file, one level jaring
     */
    public static void compress(File sourceDir, File targetArchive) throws Exception {
        BufferedOutputStream bos = null;
        JarOutputStream jos = null;
        
        try {
            bos = new BufferedOutputStream(new FileOutputStream(targetArchive), BUFFER_SIZE);
            jos = new JarOutputStream(bos);
            
            recurse(sourceDir, jos, ""); // NOI18N
            
        } finally {
            safeclose(jos);
        }
    }
    
    /*
     * helper method to recurse
     */
    private static void recurse(File source, JarOutputStream jos, String entryName) throws Exception {
        File[] children = source.listFiles();
        for (int i = 0; i < children.length; i++) {
            File curFile = children[i];
            String curName = entryName.equals("") ? // NOI18N
                    curFile.getName() : entryName + "/" + curFile.getName(); // NOI18N
            if (curFile.isDirectory()) {
                recurse(curFile, jos, curName);
            } else {
                jos.putNextEntry(new JarEntry(curName));
                BufferedInputStream bis = null;
                try {
                    bis = new BufferedInputStream(new FileInputStream(curFile), BUFFER_SIZE);
                    for (int numBytes = bis.read(buffer); numBytes > 0;
                        numBytes = bis.read(buffer)) {
                        jos.write(buffer, 0, numBytes);
                    }
                    jos.closeEntry();
                } finally {
                    safeclose(bis);
                }
            }
        }
    }
    
    public static void safeclose(Closeable c) {
        try {
            if (c != null) {
                c.close();
            }
        } catch (Exception e) {
            // do nothing
        }
    }
    
    /*
     * jar entry name by the given name
     */
    public static String getJarEntryName(String jarName, String entryName) throws Exception {
        JarInputStream jis = new JarInputStream(new FileInputStream(jarName));
        JarEntry entry = jis.getNextJarEntry();
        String name = null;
        while (entry != null) {
            name = entry.getName();
            if (name.endsWith(entryName + ".class")) {
                name = name.substring(0, name.length() - 6);
                name = name.replace('/', '.');
                System.out.print("> " + name);
                break;
            }
            entry = jis.getNextJarEntry();
        }
        return name;
    }
    
    public static void setAcceptACKJarName(String jarName){
        mAcceptAckJarName = jarName;
    }
    
    public static void setAppAcceptACKJarName(String jarName){
        mAppAcceptAckJarName = jarName;
    }
    
    public String getAcceptACKJarName(){
         return  mAppAcceptAckJarName;
    }
    
    public String getAppAcceptACKJarName() {
        return mAppAcceptAckJarName;
    }
    
    /*
     * Url ClassLoader for Jar
     * 
     */
    public static URLClassLoader getURLClassLoader(URL jarURL) {
        return new URLClassLoader(new URL[]{jarURL});
   }

}
