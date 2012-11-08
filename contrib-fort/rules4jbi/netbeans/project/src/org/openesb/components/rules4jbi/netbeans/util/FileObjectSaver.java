/*
 * @(#)FileObjectSaver.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
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

import java.io.IOException;
import java.io.OutputStream;
import java.util.logging.Logger;

import org.openide.filesystems.FileLock;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.Mutex;
import org.openide.util.MutexException;

import org.netbeans.api.project.ProjectManager;

import org.openesb.components.rules4jbi.shared.config.Saveable;

/**
 * Utility class for properly saving <code>Saveable</code>s into <code>FileObject</code>s.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
 * 
 * @since 0.1
 */
public final class FileObjectSaver {
    
    private static final Logger logger = Logger.getLogger(FileObjectSaver.class.getName());

    private FileObjectSaver() {}
    
    public static void save(final Saveable saveable, final FileObject destination) throws IOException {
        try {
            ProjectManager.mutex().writeAccess(new Mutex.ExceptionAction<Void>() {

                public Void run() throws IOException {
                    final String saveableName = saveable.getClass().getSimpleName();
                    
                    logger.fine("Saving " + saveableName + " into " + FileUtil.getFileDisplayName(destination));
                    
                    FileLock lock = null;
                    OutputStream outputStream = null;

                    try {
                        lock = destination.lock();

                        outputStream = destination.getOutputStream(lock);

                        saveable.save(outputStream);
                        
                        logger.fine(saveableName + " saved successfully");

                    } catch (Exception e) {
                        logger.warning("Failed to save " + saveableName + ": " + e.getMessage());

                        throw new IOException("Failed to save " + saveableName, e);

                    } finally {
                        if (outputStream != null) {
                            try {
                                outputStream.close();

                            } catch (IOException e) {
                                logger.fine("Failed to properly close the output stream: " + e.getMessage());
                            }
                        }

                        if (lock != null) {
                            lock.releaseLock();
                        }
                    }

                    return null;
                }
            });

        } catch (MutexException e) {
            throw (IOException) e.getException();
        }        
    }
}
