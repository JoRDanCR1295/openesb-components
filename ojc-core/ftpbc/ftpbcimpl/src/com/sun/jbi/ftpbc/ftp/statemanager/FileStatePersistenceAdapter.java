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
 * @(#)FileStatePersistenceAdapter.java 
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
package com.sun.jbi.ftpbc.ftp.statemanager;

import com.sun.jbi.internationalization.Messages;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.logging.Logger;

/**
 * Persistence adapter utilizing a local file as the persistent store in non-transactional mode.
 * Requires one file per instance. For use in non-transactional mode.
 */
public class FileStatePersistenceAdapter implements StatePersistenceAdapter {

    private static final Messages mMessages =
            Messages.getMessages(FileStatePersistenceAdapter.class);
    private static final Logger mLogger =
            Messages.getLogger(FileStatePersistenceAdapter.class);
    protected String directory = null;
    protected String filename = null;
    protected Serializable objectCache = null;
    protected final static String FILE_SEPARATOR = System.getProperty("file.separator");

    /**
     * Constructs a file state persistence adapter which utilizes
     * a local file, specified under a specific directory, as a
     * persistent store.
     *
     * @param       directory   The local directory where the file store can be found.
     * @param       filename    The name of the local file used for the perisistent store.
     *
     * @throws      StatePersistenceException upon error.
     */
    public FileStatePersistenceAdapter(String directory, String filename) throws StatePersistenceException {
        this.objectCache = null;

        // Ensure that both directory and filename are non-null
        if (directory == null) {
            throw new StatePersistenceException(mMessages.getString("FTPBC-E006088.ERR_STAT_MGR_NO_DIR",
                    new Object[]{"FileStatePersistenceAdapter(String directory, String filename)"
                    }));
        }

        if (filename == null) {
            throw new StatePersistenceException(mMessages.getString("FTPBC-E006089.ERR_STAT_MGR_NO_FILE",
                    new Object[]{"FileStatePersistenceAdapter(String directory, String filename)"
                    }));
        }

        // Ensure that the directory specified is indeed a directory
        File dirFile = new File(directory);
        if (dirFile.exists()) {
            if (!dirFile.isDirectory()) {
                throw new StatePersistenceException(mMessages.getString("FTPBC-E006090.ERR_STAT_MGR_DIR_INVALID",
                        new Object[]{"FileStatePersistenceAdapter(String directory, String filename)",
                            directory
                        }));
            }
        } else {
            boolean status = false;
            try {
                status = createDirectory(dirFile);
            } catch (Exception ex) {
                throw new StatePersistenceException(mMessages.getString("FTPBC-E006091.ERR_STAT_MGR_CREATE_DIRS_EXCEPTION",
                        new Object[]{"FileStatePersistenceAdapter(String directory, String filename)",
                            directory,
                            ex
                        }));
            } finally {
                if (!status) {
                    throw new StatePersistenceException(mMessages.getString("FTPBC-E006092.ERR_STAT_MGR_CREATE_DIRS_ERROR",
                            new Object[]{"FileStatePersistenceAdapter(String directory, String filename)",
                                directory
                            }));
                }
            }
        }

        this.directory = directory;
        this.filename = filename;
    }

    /**
     * Called to save the contents of serializable object (consisting
     * of some state information).
     *
     * @param       state   A java.io.Serializable object to save.
     *
     * @throws      StateManagerException when save fails.
     */
    public void save(Serializable state) throws StatePersistenceException {
        writeStateToFile(directory + FILE_SEPARATOR + filename, state);
        this.objectCache = state;
    }

    /**
     * Called to restore the contents of a Serializable object previously
     * saved.  If no state persistence file exists, then a null state will
     * be returned.
     *
     * @returns     A java.io.Serializable object restored from persistent store
     *              or null if no state persistence file.
     *
     * @throws      StateManagerException when restore fails.
     */
    public Serializable restore() throws StatePersistenceException {
        // DO NOT enable unless a 'deep copy' can be stored!
        //if (this.objectCache != null)
        //    return objectCache;

        this.objectCache = this.readStateFromFile(directory + FILE_SEPARATOR + filename);
        return this.objectCache;
    }

    /**
     * Close the store persistence adapter and clean up resources.
     * The adapter should not be used again once close is called.
     *
     * @throws      StatePersistenceException when close fails.
     *
     */
    public void close() throws StatePersistenceException {
    }

    /**
     * Create the requested directory.
     * The existence of the full path will be confirmed and any missing
     * directories will be created. Potential conflicts during concurrent
     * invocations from multiple threads with overlapping paths are
     * addressed and resolved.
     *
     * @param dir The directory to be created.
     *
     * @return true if the directory and all parent directories were created.
     *
     * @throws SecurityException - If a security manager exists and its
     * SecurityManager.checkWrite(java.io.FileDescriptor) method does not
     * permit the named directory and all necessary parent directories and
     * to be created.
     */
    public static boolean createDirectory(File dir) throws SecurityException {
        // Check if it already exists.
        if (dir.exists()) {
            return dir.isDirectory();
        }

        // Attempt to create the parent directory
        File parentDir = dir.getParentFile();
        boolean parentCreated = true;
        if (parentDir != null) {
            parentCreated = createDirectory(parentDir);
        }

        // Attempt to create the last directory in the path.
        dir.mkdir();

        // Check if it was created by this or any other thread.
        if (dir.exists()) {
            return dir.isDirectory();
        } else {
            return false;
        }
    }

    ///////////////////////////////////////////////////////////////
    // Protected Members
    ///////////////////////////////////////////////////////////////
    protected void writeStateToFile(String fullPathFilename, Serializable state) throws StatePersistenceException {
        ObjectOutputStream oos = null;
        FileOutputStream fos = null;
        try {
            fos = new FileOutputStream(fullPathFilename);
            oos = new ObjectOutputStream(fos);
        } catch (Exception ex) {
            throw new StatePersistenceException(mMessages.getString("FTPBC-E006093.ERR_STAT_MGR_EXCEPTION_OPEN_OR_CREATE",
                    new Object[]{"FileStatePersistenceAdapter.writeStateToFile(String fullPathFilename, Serializable state)",
                        directory + FILE_SEPARATOR + filename,
                        ex
                    }));
        }

        try {
            oos.writeObject(state);
            oos.flush();
            fos.flush();
            fos.getFD().sync(); // force udpate to disk
        } catch (Exception ex) {
            throw new StatePersistenceException(mMessages.getString("FTPBC-E006094.ERR_STAT_MGR_EXCEPTION_SAVE",
                    new Object[]{"FileStatePersistenceAdapter.writeStateToFile(String fullPathFilename, Serializable state)",
                        directory + FILE_SEPARATOR + filename,
                        ex
                    }));
        }

        try {
            oos.close();
        } catch (Exception ex) {
            throw new StatePersistenceException(mMessages.getString("FTPBC-E006095.ERR_STAT_MGR_EXCEPTION_CLOSE",
                    new Object[]{"FileStatePersistenceAdapter.writeStateToFile(String fullPathFilename, Serializable state)",
                        directory + FILE_SEPARATOR + filename,
                        ex
                    }));
        }
    }

    protected Serializable readStateFromFile(String fullPathFilename) throws StatePersistenceException {
        ObjectInputStream ois = null;
        Serializable state = null;

        // If state persistence file does not exist, return a null state.
        if (!(new File(fullPathFilename)).exists()) {
            return null;
        }

        try {
            ois = new ObjectInputStream(new FileInputStream(fullPathFilename));
        } catch (Exception ex) {
            throw new StatePersistenceException(mMessages.getString("FTPBC-E006096.ERR_STAT_MGR_EXCEPTION_OPEN_4_READ",
                    new Object[]{"FileStatePersistenceAdapter.readStateFromFile(String fullPathFilename)",
                        fullPathFilename,
                        ex
                    }));
        }

        try {
            state = (Serializable) ois.readObject();
        } catch (Exception ex) {
            throw new StatePersistenceException(mMessages.getString("FTPBC-E006097.ERR_STAT_MGR_EXCEPTION_READ",
                    new Object[]{"FileStatePersistenceAdapter.readStateFromFile(String fullPathFilename)",
                        fullPathFilename,
                        ex
                    }));
        }

        try {
            ois.close();
        } catch (Exception ex) {
            throw new StatePersistenceException(mMessages.getString("FTPBC-E006095.ERR_STAT_MGR_EXCEPTION_CLOSE",
                    new Object[]{"FileStatePersistenceAdapter.readStateFromFile(String fullPathFilename)",
                        fullPathFilename,
                        ex
                    }));
        }

        return state;
    }
}
