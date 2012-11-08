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
 * @(#)FtpFileStatePersistenceAdapter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ftpbc.ftp;

import com.sun.jbi.ftpbc.ftp.statemanager.FileStatePersistenceAdapter;
import com.sun.jbi.ftpbc.ftp.statemanager.StatePersistenceAdapter;
import com.sun.jbi.ftpbc.ftp.statemanager.StatePersistenceException;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Serializable;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;

/*
 * A new FileStatePersistenceAdapter fitting into the existing 
 * StateManager/FileStatePersistenceAdapter framework.
 * The *state* is a java string so the file is a flat file.
 * 
 * 
 * 
 * @author Harry Liu (harry.liu@sun.com)
 * @author jfu (jim.fu@sun.com)
 * Copyright 2006 Sun Microsystems, Inc. All Rights Reserved.
 */
public class FtpFileStatePersistenceAdapter extends FileStatePersistenceAdapter 
        implements StatePersistenceAdapter {
    private static final Messages mMessages =
            Messages.getMessages(FtpFileStatePersistenceAdapter.class);
    private static final Logger mLogger =
            Messages.getLogger(FtpFileStatePersistenceAdapter.class);
    
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
    public FtpFileStatePersistenceAdapter(String directory, String filename) throws StatePersistenceException {
        super(directory, filename);
    }
    
    ///////////////////////////////////////////////////////////////
    // Protected Members to overwrite
    ///////////////////////////////////////////////////////////////
    
    protected void writeStateToFile(String fullPathFilename, Serializable state) throws StatePersistenceException {
        BufferedWriter writer = null;
        try {
            writer = new BufferedWriter(new FileWriter(fullPathFilename));
        } catch (IOException ex) {
            throw new StatePersistenceException(mMessages.getString("FTPBC-E006041.ERR_EXT_FTP_STATE_OPEN", 
            		new Object[] {
            		"writeStateToFile(String fullPathFilename, Serializable state)",
            		fullPathFilename,
            		ex
            }));
        }
        
        FtpFileState ftpState = null;
        try {
            ftpState = (FtpFileState) state;
        } catch (Exception ex) {
            throw new StatePersistenceException(mMessages.getString("FTPBC-E006042.ERR_EXT_FTP_INVALID_STATE", 
            		new Object[] {
            		"writeStateToFile(String fullPathFilename, Serializable state)",
            		state,
            		ex
            }));
        }
        
        String stateValue = null;
        for (int i = 0; i < ftpState.getSequenceNo().length; i++) {
            stateValue = "%" + i + "=" + ftpState.getSequenceNo(i);
            try {
                if (i != 0) {
                    writer.newLine();
                }
                writer.write(stateValue);
                writer.flush();
            } catch (IOException ex) {
                throw new StatePersistenceException(mMessages.getString("FTPBC-E006043.ERR_EXT_FTP_STATE_WRITE", 
                		new Object[] {
                		"writeStateToFile(String fullPathFilename, Serializable state)",
                		fullPathFilename,
                		ex
                }));
            }
        }
        
        try {
            writer.close();
        } catch (IOException ex) {
            throw new StatePersistenceException(mMessages.getString("FTPBC-E006044.ERR_EXT_FTP_STATE_CLOSE", 
            		new Object[] {
            		"writeStateToFile(String fullPathFilename, Serializable state)",
            		fullPathFilename,
            		ex
            }));
        }
    }
    
    protected Serializable readStateFromFile(String fullPathFilename) throws StatePersistenceException {
        // If state persistence file does not exist, return a null state.
        if (! (new File(fullPathFilename)).exists()) {
            return null;
        }
        
        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new FileReader(fullPathFilename));
        } catch (Exception ex) {
            throw new StatePersistenceException(mMessages.getString("FTPBC-E006041.ERR_EXT_FTP_STATE_OPEN", 
            		new Object[] {
            		"readStateFromFile(String fullPathFilename)",
            		fullPathFilename,
            		ex
            }));
        }

        String stateValue = null;
        long seqNo;
        FtpFileState ftpState = new FtpFileState();
        for (int i = 0; i < ftpState.getSequenceNo().length; i++) {
            try {
                stateValue = reader.readLine(); // String applies here - we can also accept multiple lines if needed
            } catch (Exception ex) {
                throw new StatePersistenceException(mMessages.getString("FTPBC-E006045.ERR_EXT_FTP_STATE_READ", 
                		new Object[] {
                		"readStateFromFile(String fullPathFilename)",
                		fullPathFilename,
                		ex
                }));
            }

            if (null == stateValue) {
                // EOF
                break;
            }
            
            try {
                seqNo = Long.parseLong(stateValue.trim().substring(3)); // remove leading %9=, we also can make it as property key if needed.
            } catch (Exception ex) {
                continue;
            }

            ftpState.setSequenceNo(i, seqNo);
        }

        try {
            reader.close();
        } catch (Exception ex) {
            throw new StatePersistenceException(mMessages.getString("FTPBC-E006044.ERR_EXT_FTP_STATE_CLOSE", 
            		new Object[] {
            		"readStateFromFile(String fullPathFilename)",
            		fullPathFilename,
            		ex
            }));
        }

        return ftpState;
        
    }
}
