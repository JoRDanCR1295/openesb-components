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
 * @(#)ZipUtil.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 *
 * @author nnahata
 */
public class ZipUtil {
    
    /** Creates a new instance of ZipUtil */
    public ZipUtil() {
    }
    
    /**
     * Zip the srcFolder into the zipFileName. All the folder subtree of the src folder is added to the zipFileName
     * archive. 
     * 
     * 
     * @param srcFolder String, the path of the srcFolder
     * @param zipFileName String, the path of the destination zipFile. This file will be created or erased.
     * @return String value of the path to the created zos File
     */
    static public String zipFolder(String srcFolder, String destFolderName, String zipFileName) {
        ZipOutputStream zos = null;
        FileOutputStream fos = null;
        
        File outFolder = new File(destFolderName);
        if ( !outFolder.exists() ){
            outFolder.mkdirs();
        }
        
        String destZipFile = destFolderName + File.separator + zipFileName;
        try {
            fos = new FileOutputStream(destZipFile);
            zos = new ZipOutputStream(fos);
            
            addFolderToZip("", srcFolder, zos);
            zos.flush();
        }
        catch (Exception ex){
            ex.printStackTrace();
        } finally{
            try {
                if (zos != null){
                    zos.close();
                }
            } catch (Exception ex) {}
            try {
                if (fos != null){
                    fos.close();
                }
            } catch (Exception ex) {}
        }
        
        return destZipFile;
    }
    
    /**
    * add the srcFolder to the zip stream. 
    * 
    * @param path String, the relative path with the root archive.
    * @param srcFile String, the absolute path of the file to add
    * @param zip ZipOutputStram, the stream to use to write the given file.
    */
    static private void addFolderToZip(String path, String srcFolder, ZipOutputStream zip) {
        File folder = new File(srcFolder);
        String fileList[] = folder.list();
        try {
                int i = 0;
                while (i < fileList.length ) {
                    String relPath = path + "/" + folder.getName();
                    String srcFile = srcFolder + "/" +fileList[i];
                    addToZip(relPath, srcFile, zip);
                    i++;
                }
        }
        catch (Exception ex) {
        }
    }    

    /**
    * Write the content of srcFile in a new ZipEntry, named path+srcFile, of the zip stream. The result
    * is that the srcFile will be in the path folder in the generated archive.
    * 
    * @param path String, the relative path with the root archive.
    * @param srcFile String, the absolute path of the file to add
    * @param zip ZipOutputStram, the stream to use to write the given file.
    */
    static private void addToZip(String path, String srcFile, ZipOutputStream zip) {
        File folder = new File(srcFile);
        if (folder.isDirectory()) {
            addFolderToZip(path, srcFile, zip);
        }
        else{
            byte[] buf = new byte[1024];
            int len;
            try {
                FileInputStream in = new FileInputStream(srcFile);
                zip.putNextEntry(new ZipEntry(path +"/"+ folder.getName()));
                while ((len = in.read(buf)) > 0) {
                    zip.write(buf, 0, len);
                }
            }
            catch (Exception ex){
                ex.printStackTrace();
            }
        }
    }
    
}
