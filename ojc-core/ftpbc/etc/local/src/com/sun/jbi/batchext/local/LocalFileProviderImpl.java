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
 * @(#)LocalFileProviderImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/******************************************************************************
 * Copyright © 2006 Sun Microsystems, Inc., 4150 Network Circle, Santa Clara,
 * California 95054, U.S.A. All rights reserved. Sun Microsystems, Inc. has
 * intellectual property rights relating to technology embodied in the product
 * that is described in this document. In particular, and without limitation,
 * these intellectual property rights may include one or more of the U.S. patents
 * listed at http://www.sun.com/patents and one or more additional patents or
 * pending patent applications in the U.S. and in other countries. THIS PRODUCT
 * CONTAINS CONFIDENTIAL INFORMATION AND TRADE SECRETS OF SUN MICROSYSTEMS, INC.
 * USE, DISCLOSURE OR REPRODUCTION IS PROHIBITED WITHOUT THE PRIOR EXPRESS WRITTEN
 * PERMISSION OF SUN MICROSYSTEMS, INC.U.S. Government Rights - Commercial
 * software.  Government users are subject to the Sun Microsystems, Inc. standard
 * license agreement and applicable provisions of the FAR and its supplements.
 * Use is subject to license terms.  This distribution may include materials
 * developed by third parties. Sun, Sun Microsystems, the Sun logo, Java
 * Composite Application Platform Suite,  SeeBeyond, eGate, eInsight, eVision, eTL,
 * eXchange, eView, eIndex, eBAM and  eWay are trademarks or registered trademarks of
 * Sun Microsystems, Inc. in the U.S. and other countries. All SPARC trademarks are
 * used under license and are trademarks or registered trademarks of SPARC
 * International, Inc. in the U.S. and other countries. Products bearing SPARC
 * trademarks are based upon architecture developed by Sun Microsystems, Inc.
 * UNIX is a registered trademark in the U.S. and other countries, exclusively
 * licensed through X/Open Company, Ltd. This product is covered and controlled by
 * U.S. Export Control laws and may be subject to the export or import laws in
 * other countries.  Nuclear, missile, chemical biological weapons or nuclear
 * maritime end uses or end users, whether direct or indirect, are strictly
 * prohibited.  Export or reexport to countries subject to U.S. embargo or to
 * entities identified on U.S. export exclusion lists, including, but not limited
 * to, the denied persons and specially designated nationals lists is strictly
 * prohibited.
 **/
package com.sun.jbi.batchext.local;

import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.util.Arrays;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;

import com.sun.jbi.batchext.streaming.FileInputStream;
import com.sun.jbi.batchext.streaming.StreamUtil;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * It is used to expose a group of local file functions.
 * @author Harry Liu
 * @version cvs revision:    Last Modified: 
 */
// Note: I just clone the monk file functions here because
//       1. I don't know why those functions are requested. Actually, users
//          can operate files easily just using standard java.io package. ??!!
//       2. In implementation of BatchLocal, I don't use any method in
//          this class, so I don't know what methods are really needed.
//          Let's assume users just want the same functions as monk. !!
public class LocalFileProviderImpl implements LocalFileProvider {
    //private static final Messages mMessages =
    //        Messages.getMessages(LocalFileProviderImpl.class);
    private static final Logger mLogger =
            Messages.getLogger(LocalFileProviderImpl.class);
    
    static protected final String SP_MAX_INITIAL_APPEND_SIZE = "BatchLocal.maxInitialAppendSize";
    static protected final long SP_MAX_INITIAL_APPEND_SIZE_DEFAULT = Long.MAX_VALUE;
    
    protected BatchLocal etd = null;
    protected long maxInitialAppendSize = Long.getLong(SP_MAX_INITIAL_APPEND_SIZE,
            SP_MAX_INITIAL_APPEND_SIZE_DEFAULT).longValue();
    
    LocalFileProviderImpl() {
    }
    
    public void initialize(BatchLocal etd) {
        this.etd = etd;
    }
    
    public boolean reset() {
        return true;
    }
    
    /**
     * Append the contents of the source file to the destination file.
     * If the destination file does not exist, it is created.
     * @param      srcFileName  The full name of source file.
     * @param     destFileName  The full name of destination file.
     * @return     True if successfully completed, false if not.
     */
    public boolean append(String srcFileName, String destFileName) {
        String msg = null;
        
        boolean ret = false;
        try {
            FileInputStream fis = new FileInputStream(srcFileName);
            FileOutputStream fos = new FileOutputStream(destFileName, true);
            StreamUtil.copyStream(fis, fos, 32768);
            fos.getFD().sync();
            fos.flush();
            fis.close();
            fos.close();
            ret = true;
        } catch (Exception e) {
            msg = "LocalFileProviderImpl.append(): Got exception [" + e.toString() + "].";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            // Don't throw exception, just return false
        }
        
        if (ret) {
            if (mLogger.isLoggable(Level.FINE)) {
                msg = "LocalFileProviderImpl.append(): File [" + srcFileName + "] is appended to file ["
                        + destFileName + "].";
                mLogger.log(Level.FINE, msg);
            }
        } else {
            msg = "LocalFileProviderImpl.append(): Failed to append file [" + srcFileName + "] to file ["
                    + destFileName + "].";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
        }
        
        return ret;
    }
    
    /**
     * Copy the contents of the source file to the destination file.
     * If the destination file does not exist, it is created.
     * @param      srcFileName  The full name of source file.
     * @param     destFileName  The full name of destination file.
     */
    public void copy(String srcFileName,String destFileName) throws LocalFileException {
        String msg = null;
        
        boolean ret = false;
        FileInputStream fis = null;
        FileOutputStream fos = null;
        try {
            fis = new FileInputStream(srcFileName);
            fos = new FileOutputStream(destFileName, false);
            StreamUtil.copyStream(fis, fos, 32768);
            fos.getFD().sync();
            fos.flush();
            fos.close();
            fos = null;
            fis.close();
            fis = null;
            ret = true;
        } catch (Exception ex) {
            msg = "LocalFileProviderImpl.copy(): Failed to copy file ["
                    + srcFileName + "] to file [" + destFileName + "].";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
                mLogger.log(Level.SEVERE, ex.toString());
            }
            throw new LocalFileException(msg, ex);
        } finally {
            try {
                if (fis != null) {
                    fis.close();
                }
                if (fos != null) {
                    fos.close();
                }
            } catch (Exception ex) {
                // do nothing
            }
        }
        
        if (ret) {
            if (mLogger.isLoggable(Level.FINE)) {
                msg = "LocalFileProviderImpl.copy(): File [" + srcFileName
                        + "] is copied to file [" + destFileName + "].";
                mLogger.log(Level.FINE, msg);
            }
        } else {
            msg = "LocalFileProviderImpl.copy(): Failed to copy file ["
                    + srcFileName + "] to file [" + destFileName + "].";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new LocalFileException(msg);
        }
    }
    
    /**
     * Delete the file.
     * @param      fileName  The full file name.
     */
    public void delete(String fileName) throws LocalFileException {
        String msg = null;
        
        boolean ret = false;
        try {
            File file = new File(fileName);
            ret = file.delete();
        } catch (Exception ex) {
            msg = "LocalFileProviderImpl.delete(): Failed to delete file ["
                    + fileName + "].";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
                mLogger.log(Level.SEVERE, msg);
                mLogger.log(Level.SEVERE, ex.toString());
            }
            throw new LocalFileException(msg, ex);
        }
        
        if (ret) {
            if (mLogger.isLoggable(Level.FINE)) {
                msg = "LocalFileProviderImpl.delete(): File [" + fileName
                        + "] is deleted.";
                mLogger.log(Level.FINE, msg);
            }
        } else {
            msg = "LocalFileProviderImpl.delete(): Failed to delete file ["
                    + fileName + "].";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new LocalFileException(msg);
        }
    }
    
    /**
     * Create the named directory, returning true if and only if
     * the directory was created; false otherwise. See another method
     * mkdirs(String dirName).
     * @param      dirName  A valid directory name to be created.
     */
    public void mkdir(String dirName) throws LocalFileException {
        String msg = null;
        
        boolean ret = false;
        File dir = null;
        try {
            dir = new File(dirName);
            ret = dir.mkdir();
        } catch (Exception ex) {
            msg = "LocalFileProviderImpl.mkdir(): Failed to create directory ["
                    + dirName + "].";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
                mLogger.log(Level.SEVERE, ex.toString());
            }
            throw new LocalFileException(msg, ex);
        }
        
        if (ret) {
            if (mLogger.isLoggable(Level.FINE)) {
                msg = "LocalFileProviderImpl.mkdir(): Directory ["
                        + dirName + "] is created.";
                mLogger.log(Level.FINE, msg);
            }
        } else if ((dir != null) && dir.exists()) {
            if (mLogger.isLoggable(Level.FINE)) {
                msg = "LocalFileProviderImpl.mkdir(): Directory ["
                        + dirName + "] already exists.";
                mLogger.log(Level.FINE, msg);
            }
        } else {
            msg = "LocalFileProviderImpl.mkdir(): Failed to create directory ["
                    + dirName + "].";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new LocalFileException(msg);
        }
    }
    
    /**
     * Create the named directory, including any necessary but
     * nonexistent parent directories. Note that if this operation
     * fails it may have succeeded in creating some of the necessary
     * parent directories. See another method mkdir(String dirName).
     * @param      dirName  A valid directory name to be created.
     */
    public void mkdirs(String dirName) throws LocalFileException {
        String msg = null;
        
        boolean ret = false;
        File dir = null;
        try {
            dir = new File(dirName);
            ret = dir.mkdirs();
        } catch (Exception ex) {
            msg = "LocalFileProviderImpl.mkdirs(): Failed to create directory ["
                    + dirName + "].";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
                mLogger.log(Level.SEVERE, ex.toString());
            }
            throw new LocalFileException(msg, ex);
        }
        
        if (ret) {
            if (mLogger.isLoggable(Level.FINE)) {
                msg = "LocalFileProviderImpl.mkdirs(): Directory ["
                        + dirName + "] is created.";
                mLogger.log(Level.FINE, msg);
            }
        } else if ((dir != null) && dir.exists()) {
            if (mLogger.isLoggable(Level.FINE)) {
                msg = "LocalFileProviderImpl.mkdirs(): Directory ["
                        + dirName + "] already exists.";
                mLogger.log(Level.FINE, msg);
            }
        } else {
            msg = "LocalFileProviderImpl.mkdirs(): Failed to create directory ["
                    + dirName + "].";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new LocalFileException(msg);
        }
    }
    
    /**
     * Rename the source file to the destination file.
     * Note: When moving or renaming a file, the destination volume
     *       must be the same as the source volume.
     * @param      srcFileName  The full name of source file.
     * @param     destFileName  The full name of destination file.
     */
    public void rename(String srcFileName,String destFileName) throws LocalFileException {
        String msg = null;
        
        boolean ret = false;
        try {
            File srcFile = new File(srcFileName);
            File destFile = new File(destFileName);
            ret = srcFile.renameTo(destFile);
        } catch (Exception ex) {
            msg = "LocalFileProviderImpl.rename(): Failed to rename file ["
                    + srcFileName + "] to file [" + destFileName + "].";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
                mLogger.log(Level.SEVERE, ex.toString());
            }
            throw new LocalFileException(msg, ex);
        }
        
        if (ret) {
            if (mLogger.isLoggable(Level.FINE)) {
                msg = "LocalFileProviderImpl.rename(): File [" + srcFileName
                        + "] is renamed to file [" + destFileName + "].";
                mLogger.log(Level.FINE, msg);
            }
        } else {
            msg = "LocalFileProviderImpl.rename(): Failed to rename file ["
                    + srcFileName + "] to file [" + destFileName + "].";
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new LocalFileException(msg);
        }
    }
    
    public LocalFileName getFirstFileName(String directory,
            boolean directoryIsPattern, String file, boolean fileIsPattern) {
        if ((directory.length() == 0) || (file.length() == 0)) {
            return null;
        }
        String[] dirs = null;
        if (directoryIsPattern) {
            LocalDirRegExp ldRegEx = new LocalDirRegExp(directory);
            try {
                dirs = (String[]) ldRegEx.getDirs().toArray(new String[0]);
            } catch (Exception ex) {
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, "LocalFileProviderImpl.getFirstFileName: Got exception while processing directory name regular expression "
                            + directory + " The exception is: " + ex.toString());
                }
            }
            if (dirs != null) {
                Arrays.sort(dirs);
            }
        } else {
            File dir = new File(directory);
            if (dir.isDirectory()) {
                dirs = new String[1];
                dirs[0] = directory;
            } else {
                dirs = null;
            }
        }
        
        if (dirs == null) {
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, "LocalFileProviderImpl.getFirstFileName: Unable to find an existing directory matching the regular expression "
                        + directory);
            }
            return null;
        }
        
        LocalFileName lfn = null;
        // try to locate the first file name in the listed directories
        for (int i = 0; i < dirs.length; i++) {
            File dir = new File(dirs[i]);
            FileNameFilter fnf = null;
            try {
                fnf = new FileNameFilter(file, fileIsPattern);
            } catch (Exception ex) {
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, "LocalFileProviderImpl.getFirstFileName: Unable to create FileName filter with the regular expression "
                            + file + " The exception is " + ex.toString());
                }
                // Break out of the loop and return null.
                // The caller handles null return value properly.
                break;
            }
            File[] files = dir.listFiles(fnf);
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "LocalFileProviderImpl.getFirstFileName: Checking for file match in directory " +
                        dir.getAbsolutePath());
            }
            if ((files != null) && (files.length > 0)) {
                Arrays.sort(files);
                File f = files[0];
                // we got our file name
                lfn = new LocalFileName(f.getAbsolutePath(), f.getParent(), f.getName());
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "LocalFileProviderImpl.getFirstFileName: The match is " +
                            lfn.toString());
                }
                break;
            }
        }
        
        return lfn;
    }
    
    public FileInputStream getFileInputStream(String name)
    throws FileNotFoundException {
        return new FileInputStream(name);
    }
    
    public FileOutputStream getFileOutputStream(String name, boolean append)
    throws FileNotFoundException, LocalFileException {
        // Perform size validation, becuse of the following FileOutputStream bug.
        // http://developer.java.sun.com/developer/bugParade/bugs/4481333.html
        File file = new File(name);
        if (append && (file.length() > maxInitialAppendSize)) {
            String msg = "LocalFileProviderImpl.getFileOutputStream: "
                    + "Users of the BatchLocal in the Java Batch e*Way may experience problems opening very large files for writing in append mode (typically the limit is reached at 4 GB). "
                    + "Please contact the support desk for more information (QA 50291). "
                    + "The default file size limit is " + SP_MAX_INITIAL_APPEND_SIZE_DEFAULT + " bytes. "
                    + "The enforced file size limit is " + maxInitialAppendSize + " bytes. "
                    + "The file in question is " + name;
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg);
            }
            throw new LocalFileException(msg);
        }
        file = null;
        return new FileOutputStream(name, append);
    }
    
    protected class FileNameFilter implements FileFilter {
        private String fn = null;
        private boolean isRegEx = false;
        //private RE re = null;
        private Pattern pattern;
        private Matcher match;
        
        public FileNameFilter(String fn, boolean isRegEx) throws Exception /*REException*/ {
            this.fn = fn;
            this.isRegEx = isRegEx;
            if (this.isRegEx) {
                //this.re = new RE(fn);
                this.pattern = Pattern.compile(fn);
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "FileNameFilter.<init>: Created a RegEx with the string " + fn);
                }
            }
        }
        public boolean accept(File fx) {
            if (!fx.isFile()) {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "FileNameFilter.accept: It is not a file " + fx.getName());
                }
                return false;
            }
            boolean bRet = false;
            if (isRegEx) {
                // Changed to accept partial match as in the Monk version
                // and to be the same as the FtpFileProviderImpl:390
                // in listfiles(). If complete match is necessary then
                // the commented line can be used instead.
                //bRet = re.isMatch(fx.getName());
                //bRet = (re.getMatch(fx.getName()) != null);
                match = pattern.matcher(fx.getName());
                //~ Changed back to partial match (find()) so it is consistent 
                //~ with older versions including Monk version and java 45x, etc. 
                //~ matches() is for entire match.
                //~bRet = match.matches();
                bRet = match.find();
            } else {
                bRet = fx.getName().equals(fn);
            }
            if (bRet) {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "FileNameFilter.accept: It is a match " + fx.getName());
                }
            } else {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "FileNameFilter.accept: It is not a match " + fx.getName());
                }
            }
            return bRet;
        }
    }
}
