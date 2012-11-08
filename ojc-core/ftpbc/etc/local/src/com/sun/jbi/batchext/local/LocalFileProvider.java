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
 * @(#)LocalFileProvider.java 
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

// Note: I just clone the monk file functions here because
//       1. I don't know why those functions are requested. Actually, users
//          can operate files easily just using standard java.io package. ??!!
//       2. In implementation of BatchLocal, I don't use any method in 
//          this class, so I don't know what methods are really needed.
//          Let's assume users just want the same functions as monk. !!

import com.sun.jbi.batchext.streaming.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;

/**
 * It is used to expose a group of local file functions.
 */
public interface LocalFileProvider {
    public void initialize(BatchLocal etd);
    public boolean reset();
    
    /**
     * Append the contents of the source file to the destination file. 
     * If the destination file does not exist, it is created.
     * @param      srcFileName  The full name of source file.
     * @param     destFileName  The full name of destination file.
     * @return     True if successfully completed, false if not.
     */
    public boolean append(String srcFileName, String destFileName);
    
    /**
     * Copy the contents of the source file to the destination file. 
     * If the destination file does not exist, it is created.
     * @param      srcFileName  The full name of source file.
     * @param     destFileName  The full name of destination file.
     */
    public void copy(String srcFileName, String destFileName)
        throws LocalFileException;

    /**
     * Delete the file. 
     * @param      fileName  The full file name.
     */
    public void delete(String fileName) throws LocalFileException;

    /**
     * Create the named directory, returning true if and only if 
     * the directory was created; false otherwise. See another method
     * mkdirs(String dirName).
     * @param      dirName  A valid directory name to be created.
     */
    public void mkdir(String dirName) throws LocalFileException;

    /**
     * Create the named directory, including any necessary but
     * nonexistent parent directories. Note that if this operation
     * fails it may have succeeded in creating some of the necessary
     * parent directories. See another method mkdir(String dirName).
     * @param      dirName  A valid directory name to be created.
     */
    public void mkdirs(String dirName) throws LocalFileException;

    /**
     * Rename the source file to the destination file.
     * Note: When moving or renaming a file, the destination volume
     *       must be the same as the source volume.
     * @param      srcFileName  The full name of source file.
     * @param     destFileName  The full name of destination file.
     */
    public void rename(String srcFileName, String destFileName)
        throws LocalFileException;

    /**
     * Get the first file name matching the specified parameters.
     * @param   directory  The directory name or RegEx.
     * @param   directoryIsPattern  Indicates whether the directory parameter is a RegEx.
     * @param   file  The file name or RegEx.
     * @param   fileIsPattern  Indicates whether the file parameter is a RegEx.
     * @return  The file name found or null if none.
     */
    public LocalFileName getFirstFileName(String directory, boolean directoryIsRegEx,
        String file, boolean fileIsRegEx);

    public FileInputStream getFileInputStream(String name)
        throws FileNotFoundException;

    public FileOutputStream getFileOutputStream(String name, boolean append)
        throws FileNotFoundException, LocalFileException;
}
