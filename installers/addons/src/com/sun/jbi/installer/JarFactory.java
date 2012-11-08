/*
 * BEGIN_HEADER - DO NOT EDIT
 *
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)JarFactory.java
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.installer;


import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.IOException;

import java.util.Enumeration;
import java.util.jar.JarEntry;
import java.util.jar.JarException;
import java.util.jar.JarFile;
import java.io.File;

import java.util.zip.ZipFile;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;




public class JarFactory
{
    
    /* Buffer to read data 
    *
    */
    private byte[] mBuffer;
    
    /* Unzip Directory 
    *
    */
    private String mDir;
    
    /**
      * This object provides utility methods to manipulate DOM tree.
      * @param dir Directory to unjar
      */
    public JarFactory (String dir)
    {
        
        this.mDir = dir;
        this.mBuffer = new byte[8092];
        
    }
    
    /**
     * Unjars a file.
     *
     * @param jarFile File to be unjared
     * @throws IOException if error trying to read entry.
     * @throws java.util.jar.JarException if error trying to read jar file.
     */
    public void unJar (File jarFile) throws IOException, JarException, ZipException
    {
        
        // process all entries in that JAR file
        JarFile jar = new JarFile (jarFile);
        unJar(jar);
        jar.close();
    }
    
    /**
     * Unjars a file.
     *
     * @param jar JarFile to be unjared
     * @throws IOException if error trying to read entry.
     * @throws java.util.jar.JarException if error trying to read jar file.
     */
    public void unJar (java.util.zip.ZipFile jar) throws IOException, ZipException
    {
        
        // process all entries in that JAR file
        Enumeration all = jar.entries ();
        while (all.hasMoreElements ())
        {
            getEntry (jar, ((ZipEntry) (all.nextElement ())));
        }
    }
    
    /**
     * Gets one file <code>entry</code> from <code>jarFile</code>.
     *
     * @param jarFile the JAR file reference to retrieve <code>entry</code> from.
     * @param entry the file from the JAR to extract.
     * @return the ZipEntry name
     * @throws IOException if error trying to read entry.
     */
    public String getEntry (ZipFile jarFile, ZipEntry entry) throws IOException
    {
        
        String entryName = entry.getName ();
        // if a directory, mkdir it (remember to create 
        // intervening subdirectories if needed!)
        if (entryName.endsWith ("/"))
        {
            new File (mDir, entryName).mkdirs ();
            return entryName;
        }
        
        File f = new File (mDir, entryName);
        
        if (!f.getParentFile ().exists ())
        {
            f.getParentFile ().mkdirs ();
        }
        
        // Must be a file; create output stream to the file
        FileOutputStream fostream = new FileOutputStream (f);
        InputStream istream = jarFile.getInputStream (entry);
        
        // extract files
        int n = 0;
        while ((n = istream.read (mBuffer)) > 0)
        {
            fostream.write (mBuffer, 0, n);
        }
        
        try
        {
            istream.close ();
            fostream.close ();
        }
        catch (IOException e)
        {
            // do nothing
        }
        return entryName;
    }
}
