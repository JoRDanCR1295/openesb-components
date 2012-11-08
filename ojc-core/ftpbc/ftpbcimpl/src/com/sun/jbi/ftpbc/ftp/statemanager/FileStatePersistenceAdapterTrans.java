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
 * @(#)FileStatePersistenceAdapterTrans.java 
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

import java.io.File;
import java.io.FileInputStream;
import java.io.ObjectInputStream;
import java.io.Serializable;

/**
 * Persistence adapter utilizing a local file as the persistent store in transactional mode.
 * Requires one file per instance. For use in transactional mode.
 */
public class FileStatePersistenceAdapterTrans extends FileStatePersistenceAdapter implements StatePersistenceAdapterTrans {

    private Serializable prepObject = null;
    private String tempFileName = null;
    private String prepareFileName = null;

    /**
     * Constructs a transactional file state persistence adapter which
     * utilizes a local file, specified under a specific directory, as a
     * persistent store.
     *
     * @param       directory   The local directory where the file store can be found.
     * @param       filename    The name of the local file used for the perisistent store.
     *
     * @throws      StatePersistenceException upon error.
     */
    public FileStatePersistenceAdapterTrans(String directory, String filename) throws StatePersistenceException {
        super(directory, filename);
        tempFileName = directory + this.FILE_SEPARATOR + filename + ".temp";
        prepareFileName = directory + this.FILE_SEPARATOR + filename + ".prep";
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
        ObjectInputStream ois = null;
        Serializable state = null;

        // DO NOT enable unless a 'deep copy' can be stored!
        //if (this.objectCache != null)
        //    return objectCache;

        String stateFileToLoadFrom = directory + FILE_SEPARATOR + filename;
        if ((new File(this.prepareFileName)).exists()) {
            stateFileToLoadFrom = this.prepareFileName;
        } else if ((new File(this.tempFileName)).exists()) {
            stateFileToLoadFrom = this.tempFileName;
        } else if (!(new File(stateFileToLoadFrom)).exists()) {
            // If state persistence file does not exist, return a null state.
            return null;
        }

        try {
            ois = new ObjectInputStream(new FileInputStream(stateFileToLoadFrom));
        } catch (Exception ex) {
            throw new StatePersistenceException("FileStatePersistenceAdapter - Failed to open file '" + stateFileToLoadFrom + "' for reading. Exception thrown : " + ex.toString(), ex);
        }

        try {
            state = (Serializable) ois.readObject();
        } catch (Exception ex) {
            throw new StatePersistenceException("FileStatePersistenceAdapter - Failed to read from file '" + directory + FILE_SEPARATOR + filename + "'. Exception thrown : " + ex.toString(), ex);
        }

        try {
            ois.close();
        } catch (Exception ex) {
            throw new StatePersistenceException("FileStatePersistenceAdapter - Failed to close file '" + directory + FILE_SEPARATOR + filename + "' after reading from it. Exception thrown : " + ex.toString(), ex);
        }

        objectCache = state;

        return state;
    }

    /**
     * Called to prepare the save of state information. Upon
     * calling this method successfully, the state information
     * can be committed by calling the commit method.
     *
     * @throws      StatePersistenceException when prepare fails.
     */
    public void prepare() throws StatePersistenceException {
        File prepareFile = new File(this.prepareFileName);
        if (prepareFile.exists()) {
            return;
        }

        File tempStateFile = new File(this.tempFileName);
        if (tempStateFile.exists()) {
            try {
                tempStateFile.renameTo(prepareFile);
            } catch (Exception ex) {
                throw new StatePersistenceException("FileStatePersistenceAdapterTrans - failed to prepare.", ex);
            }
        }
        // Otherwise, do nothing - initial start up with no actions
    }

    /**
     * Called to save of state information. Upon
     * calling this method successfully, the state information
     * can be committed by calling the save method.
     *
     * @param       state   A java.io.Serializable object to save.
     *
     * @throws      StatePersistenceException when prepare fails.
     */
    public void save(java.io.Serializable state) throws StatePersistenceException {
        try {
            File tempStateFile = new File(tempFileName);
            if (tempStateFile.exists()) {
                String tempTempFileName = directory + this.FILE_SEPARATOR + filename + ".temptemp";
                File tempTempFile = new File(tempTempFileName);
                writeStateToFile(tempTempFile.getAbsolutePath(), state);
                tempStateFile.delete();
                tempTempFile.renameTo(tempStateFile);
            } else {
                writeStateToFile(tempStateFile.getAbsolutePath(), state);
            }
        } catch (Exception ex) {
            throw new StatePersistenceException("FileStatePersistenceAdapterTrans - failed to save state.", ex);
        }

        this.objectCache = state;
    }

    /**
     * Called to commit the prepared state information. The prepare
     * method must be called prior to committing the state information.
     *
     * @see         #prepare
     *
     * @throws      StatePersistenceException if prepare was not called
     *              or the save was not successful.
     */
    public void commit() throws StatePersistenceException {
        File commitFile = new File(directory + FILE_SEPARATOR + filename);
        try {
            // Check existence of prepare file,
            //	if exist, just rename it from prepare to commit file,
            //	else, just ignore and do nothing.
            //
            File prepareFile = new File(this.prepareFileName);
            if (prepareFile.exists()) {
                if (commitFile.exists()) {
                    commitFile.delete();
                }

                prepareFile.renameTo(commitFile);
            }
        } catch (Exception ex) {
            throw new StatePersistenceException("FileStatePersistenceAdapterTrans - Failed to commit the prepare file; unable to rename the prepare file. Exception thrown : " + ex.toString(), ex);
        }
    }

    /**
     * Called to rollback any work done in prepare. Upon successfully
     * returning from rollback, the state of this object will be as
     * if prepare did not get called.  If prepare was never called
     * before rollback then nothing is done.
     *
     * @see         #prepare
     *
     * @throws      StatePersistenceException if prepare was not called
     *              called or rollback failed.
     */
    public void rollback() throws StatePersistenceException {
        try {
            File prepFile = new File(this.prepareFileName);
            if (prepFile.exists()) {
                prepFile.delete();
            }
            File tempFile = new File(this.tempFileName);
            if (tempFile.exists()) {
                tempFile.delete();
            }
        } catch (Exception ex) {
            throw new StatePersistenceException("FileStatePersistenceAdapterTrans - Failed to rollback; unable to delete the prepare file. Exception thrown : " + ex.toString(), ex);
        } finally {
            prepObject = null;
            objectCache = null;
        }
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
}
