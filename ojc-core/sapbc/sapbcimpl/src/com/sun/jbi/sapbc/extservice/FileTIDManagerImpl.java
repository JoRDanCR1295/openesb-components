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
 * @(#)FileTIDManagerImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**************************************************************************
 *
 *          Copyright (c) 2002, SeeBeyond Technology Corporation,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ***************************************************************************/

package com.sun.jbi.sapbc.extservice;

import com.sap.mw.jco.JCO.AbapException;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.sapbc.Utils;
import javax.jbi.JBIException;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.io.SyncFailedException;
import java.util.logging.Logger;
import java.util.logging.Level;


/**
 * A File-based implementation of a SAP Transactional ID (TID) Manager.
 * @author Rajesh Dhingra
 * @version 
 */
public class FileTIDManagerImpl extends Object implements TIDManager {
    
    private static final Messages mMessages = Messages.getMessages(FileTIDManagerImpl.class);
    private static final Logger mLogger = Messages.getLogger(FileTIDManagerImpl.class);
    
    /**
     * Line Feed
     */
    protected static final char LF          = '\n';     // Record separator
    
    /**
     * Const RESERVED, state of the TID
     */
    protected static final char RESERVED    = 'R';      // Reserved or pending
    
    /**
     * Const COMMITTED, state of the TID
     */
    protected static final char COMMITTED   = 'C';      // Committed
    
    /**
     * Const UNPROCESSED, state of the TID
     */
    protected static final char UNPROCESSED = 'U';      // Unprocessed or rolled back
    
    /**
     * Const ENCODING
     */
    protected static final String ENCODING  = "UTF-8";
    
    /**
     * Const INBOUND_STATE_OFFSET
     */
    // |<--TID(24)-->|<--State(1)-->|LF(1)
    protected static final int INBOUND_STATE_OFFSET      = TIDManager.TID_SIZE;  //24
    
    /**
     * Const INBOUND_RECORD_SEP_OFFSET
     */
    protected static final int INBOUND_RECORD_SEP_OFFSET = INBOUND_STATE_OFFSET + 1;  //25
    
    /**
     * Const INBOUND_TID_RECORD_SIZE
     */
    protected static final int INBOUND_TID_RECORD_SIZE   = INBOUND_RECORD_SEP_OFFSET + 1; //26
    
    /**
     * Const OUTBOUND_TID_MAX_ROWS
     * maximum number of rows which can be in the client TID database
     */
    // |<--TID(24)-->|<--EID(variable)-->|LF(1) - Outbound TID EID Format
    private int outBoundTidMaxRow = 200;
    
    /**
     * Const OUTBOUND_EID_OFFSET
     */
    public static final int OUTBOUND_EID_OFFSET = TIDManager.TID_SIZE; //24
    
    /**
     * Const LOCK_ATTEMPT_SLEEP, sleep interval
     */
    private static final long LOCK_ATTEMPT_SLEEP = 10;
    
    /**
     * Const LOCK_ATTEMPT_MAX
     */
    private static final int LOCK_ATTEMPT_MAX = 500;  // 5 secs: 300 x 10 millisecs
    
    private SAPBCConnectorClient myConnector = null;
    private File tidF = null;
    private String tidFFullPathName = null;
    private int tidCount = 0;
    private File flock = null;
    private boolean inTestMode = false;
    private PrintfFormat tidFmt = new PrintfFormat("%024d");
    private static int localTID = 1;
    private boolean lockErr = false;
    
    //private ObjectReference mMonitor;
    
    /**
     * Creates a new FileTIDManagerImpl object for local testing.
     */
    protected FileTIDManagerImpl() {
        super();
        //tidF = new File(System.getProperty("user.home"), "svrtest.tid");
        tidF = new File("C:\\JavaCAPS51\\data\\svrtest.tid");
        tidFFullPathName = tidF.getAbsolutePath();
        init();
        inTestMode = true;
    }
    
    
    /**
     * Creates a new FileTIDManagerImpl object.
     *
     * @param   connector   SAPBCConnectorClient object associated with this File TID Manager.
     * @param   isServer   boolean value to indicate client or server mode of operation
     */
    public FileTIDManagerImpl(SAPBCConnectorClient connector, boolean isServer) {
        super();
        myConnector = connector;
        
        if (myConnector.isTrfcEnabled()) {
            tidF = this.getFullPath(myConnector.getTidDatabase());
        } else {
            Utils.checkLog(mLogger, Level.FINE, "FileTIDManagerImpl.Set_tRFCEnabled");
        }
        init();
    }
    /*
     * To get and set the lockErr - variable which denotes error during locking the tid database file
     */
    public boolean getLockErr() {
        return lockErr;
    }
    private void setLockErr(boolean le) {
        lockErr = le;
    }
    
    /**
     * Does initialization of lock files for Client mode
     *
     */
    private void init() {
        File lockFd = new File(tidF.getParent(), ".lockdir");
        lockFd.mkdirs();
        flock = new File(lockFd, tidF.getName() + ".~lock~");
        String mr = System.getProperty("bapi.outTIDMaxRow");
        if (mr != null) {
            outBoundTidMaxRow = Integer.parseInt(mr);
        } else {
            if(myConnector instanceof com.sun.jbi.sapbc.extservice.SAPBCClient) {
                outBoundTidMaxRow = myConnector.getClientMaxDBRows();
                //outBoundTidMaxRow = 10;
            }
        }
        Utils.checkLog(mLogger, Level.FINE, "FileTIDManagerImpl.TID_Location", new Object[]{tidF.getAbsolutePath(), outBoundTidMaxRow});
    }
    
    
    /**
     * Creates a new lock file to simulate a locking protocol
     *
     * @throws private
     */
    private synchronized void lockFile()
    throws Exception {
        boolean canLock = false;
        boolean retryAllowed = false;
        
        try {
            do {
                retryAllowed = !retryAllowed;
                for (int i = 0; i < LOCK_ATTEMPT_MAX; i++) {
                    try {
                        if ((canLock = flock.createNewFile())) {
                            break;
                        }
                    } catch (java.io.IOException e) {
                        String errMsg = mMessages.getString("ExceptionThrown", new Object[]{e.getClass().getName(), "lockFile()", e.getMessage()});
                        mLogger.log(Level.WARNING, errMsg);
                        //throw new JBIException(errMsg);
                    }
                    
                    Thread.sleep(LOCK_ATTEMPT_SLEEP);
                }
                if (canLock) {
                    Utils.checkLog(mLogger, Level.INFO, "FileTIDManagerImpl.Aquired_Lock_File", flock.getAbsolutePath());
                } else if (retryAllowed) {
                    // Maybe the owner of the lockfile died so try deleting it...
                    Utils.checkLog(mLogger, Level.INFO, "FileTIDManagerImpl.Cannot_Create_Lock_File_Delete", new Object[]{flock.getAbsolutePath(), LOCK_ATTEMPT_MAX});
                    try {
                        unlockFile();
                    } catch (Exception e) {
                        Utils.checkLog(mLogger, Level.WARNING, e.getMessage());
                    }
                }
            } while (!canLock && retryAllowed);
            
            if (!canLock) {
                Utils.checkLog(mLogger, Level.WARNING, "FileTIDManagerImpl.Could_Not_Create_Access_Find_TID");
                //SAPBAPIAlertCodes.sendAlert(SAPBAPIAlertCodes.SAPBAPI_TIDFILE_NOTAVAIL,
                //        new String[] {tidF.getAbsolutePath()},
                //        "Unable to lock TID file " + flock.getAbsolutePath() + ". Please ensure that the given path is valid, accessible and has appropriate permissions.",
                //        NotificationEvent.SEVERITY_TYPE_CRITICAL, getMonitor());
                mLogger.log(Level.SEVERE, "FileTIDManagerImpl.Unable_To_Access_TID_Lock_File",flock.getAbsolutePath());
                if(myConnector instanceof com.sun.jbi.sapbc.extservice.SAPBCClient) {
                    Utils.checkLog(mLogger, Level.INFO, "FileTIDManagerImpl.Cannot_Create_Lock_File_IDoc", flock.getAbsolutePath());
                    setLockErr(true);
                }else{
//[begin.jca.1.0@
                        /* Server not yet implemented
                    Utils.checkLog(mLogger, Level.INFO, "FileTIDManagerImpl.Cannot_Create_Lock_File_Stop_Bapi", flock.getAbsolutePath());
                    ((com.sun.jbi.sapbc.extservice.SAPBCServer) myConnector).server.getThread().interrupt();
                    ((com.sun.jbi.sapbc.extservice.SAPBCServer) myConnector).stopServer();
                         */
//[end.jca.1.0@
                }
                String errMsg = mMessages.getString("FileTIDManagerImpl.Cannot_Create_Lock_File", new Object[]{flock.getAbsolutePath(), LOCK_ATTEMPT_MAX});
                mLogger.log(Level.SEVERE, errMsg);
                throw new JBIException(errMsg);
            }
        } catch (InterruptedException e) {
            String errMsg = mMessages.getString("FileTIDManagerImpl.Cannot_Thread_Sleep_Lock_File", flock.getAbsolutePath());
            mLogger.log(Level.SEVERE, errMsg);
            throw new JBIException(errMsg,e);
        }
    }
    
    /**
     * Deletes the file used for locking
     *
     */
    private synchronized void unlockFile()
    throws Exception {
        boolean canUnlock = false;
        try {
            for (int i = 0; i < LOCK_ATTEMPT_MAX && !(canUnlock = flock.delete()); i++) {
                Thread.sleep(LOCK_ATTEMPT_SLEEP);
            }
            
            if (canUnlock) {
                Utils.checkLog(mLogger, Level.FINE, "FileTIDManagerImpl.Released_Lock_File", flock.getAbsolutePath());
            } else {
                String errMsg = mMessages.getString("FileTIDManagerImpl.Cannot_Delete_Unlock_File", flock.getAbsolutePath());
                mLogger.log(Level.SEVERE, errMsg);
                throw new JBIException(errMsg);
            }
        } catch (InterruptedException e) {
            String errMsg = mMessages.getString("FileTIDManagerImpl.Cannot_Thread_Sleep_Unlock_File", flock.getAbsolutePath());
            mLogger.log(Level.SEVERE, errMsg);
            throw new JBIException(errMsg,e);
        }
    }
    
    private int findInBoundTID(StringBuffer tidTbl, String tid) {
        int beginIdx = 0;
        int endIdx = INBOUND_STATE_OFFSET;
        int bufSize = tidTbl.length();
        
        while (endIdx < bufSize) {
            String testTID = tidTbl.substring(beginIdx, endIdx);
            Utils.checkLog(mLogger, Level.FINE, "FileTIDManagerImpl.Inbound_Test_Known_TID", new Object[] {testTID, tid});
            if (testTID.equals(tid)) {
                return beginIdx;
            }
            beginIdx += this.INBOUND_TID_RECORD_SIZE;
            endIdx = beginIdx + INBOUND_STATE_OFFSET;
        }
        return -1;
    }
    
    /**
     * Finds the Outbound TID for a given EID
     * @param tidTbl    the TID buffer
     * @param eid       the given eid
     * @return the TID
     */
    private int findOutBoundTIDForEID(StringBuffer tidTbl, String eid) {
        int beginIdx = 0;
        int endIdx = -1;
        int bufSize = tidTbl.length();
        
        // |<--TID(24)-->|<--EID(variable)-->|LF(1)
        while (beginIdx < bufSize && (endIdx = indexOf(tidTbl, LF, beginIdx)) != -1) {
            String testEID = tidTbl.substring(beginIdx + OUTBOUND_EID_OFFSET, endIdx);
            Utils.checkLog(mLogger, Level.FINE, "FileTIDManagerImpl.Outbound_Test_Known_TID", new Object[] {testEID, eid});
            if (testEID.equals(eid)) {
                return beginIdx;
            }
            beginIdx = endIdx + 1;
        }
        return -1;
    }
    
    
    /**
     * Reads the TID file in a buffer
     * @param raf       the handle to the TID file
     * @return the TID string buffer
     * @throws IOException  IOException
     */
    protected StringBuffer readTIDFileFully(RandomAccessFile raf)
    throws IOException {
        try {
            int bufSz = (int) raf.length();
            Utils.checkLog(mLogger, Level.FINE, "FileTIDManagerImpl.RAF_Size", new Integer(bufSz));
            StringBuffer sb = null;
            if (bufSz > 0) {
                byte[] buf = new byte[bufSz];
                raf.readFully(buf);
                sb = new StringBuffer(new String(buf, ENCODING));
            } else {
                sb = new StringBuffer();
            }
            return sb;
        } catch (IOException sfe) {
            throw ((IOException) sfe);
        }
        
    }
    
    
    /**
     * Writes TID buffer to file
     * @param raf    the file handle to write to
     * @param sb     the TID buffer to be written to file
     * @throws IOException  IOException
     */
    protected void writeTIDFileFully(RandomAccessFile raf, StringBuffer sb)
    throws IOException {
        writeTIDFileFully(raf, sb.toString());
    }
    
    
    /**
     * Writes TID buffer to file
     * @param raf    the file handle to write to
     * @param s      the TID buffer to be written to file
     * @throws IOException  IOException
     */
    protected void writeTIDFileFully(RandomAccessFile raf, String s)
    throws IOException {
        raf.seek(0);
        raf.writeBytes(s);  // writes lower 8 bits == UTF-8 for ASCII
        raf.setLength(s.length());     // necessary to truncate file
        try {
            raf.getFD().sync();
        } catch (SyncFailedException sfe) {
            throw ((IOException) sfe);
        }
    }
    
    
    /**
     * Sets the mbean object for alerts
     *
     * @param mbean     The mbean object.
    public void setMonitor(ObjectReference mbean) {
      mMonitor = mbean;
    }
     */
    
    /**
     * Gets the mbean object for alerts
     *
     * @return the mbean object
    public ObjectReference getMonitor() {
      return mMonitor;
    }
     */
    
    /**
     * Checks if the TID has been reserved (R) or committed (C).  It not, the TID
     * will be stored persistently and marked as reserved (R).
     *
     * @param       tid       The incoming tRFC Transaction ID (TID).
     * @return      <code>true</code> if TID has not been Reserved (R) nor
     *              committed (C). <code>false</code> otherwise.
     */
    public boolean onCheckTID(String tid)
    throws com.sap.mw.jco.JCO.Exception{
        boolean bret = true;
        Utils.checkLog(mLogger, Level.FINE, "InsideMethod", "onCheckTID(String tid)");
        if (tid.length() != TIDManager.TID_SIZE) {
            String errMsg = mMessages.getString("FileTIDManagerImpl.Bad_TID_Length", new Object[]{"onCheckTID", tid, TIDManager.TID_SIZE});
            mLogger.log(Level.SEVERE, errMsg);
            throw new AbapException("NOT_SUPPORTED", errMsg);
        }
        
        try {
            lockFile();
            RandomAccessFile tidRf = new RandomAccessFile(tidF, "rw");
            
            int tidPos = -1;
            StringBuffer tidBuf = readTIDFileFully(tidRf);
            if (tidBuf.length() > 0) {
                tidPos = findInBoundTID(tidBuf, tid);
            }
            
            if (tidPos < 0) {
                tidBuf.append(tid).append(RESERVED).append(LF);
                writeTIDFileFully(tidRf, tidBuf);
                Utils.checkLog(mLogger, Level.FINE, "FileTIDManagerImpl.Reserved_TID", tid);
            } else {
                char thetidState = tidBuf.charAt(tidPos + INBOUND_STATE_OFFSET);
                if (COMMITTED == thetidState) {
                    bret = false;
                } else {
                    //the state is R or U due to communication break down
                    tidBuf.setCharAt(tidPos + INBOUND_STATE_OFFSET, RESERVED);
                    writeTIDFileFully(tidRf, tidBuf);
                }
            }
            
            tidRf.close();
            unlockFile();
        } catch (IOException e) {
            String errMsg = mMessages.getString("ExceptionThrown", new Object[]{e.getClass().getName(), "onCheckTID()", e.getMessage()});
            mLogger.log(Level.SEVERE, errMsg);
            throw new AbapException("CHECK_TID_ERROR", errMsg);
        } catch (Exception e) {
            String errMsg = mMessages.getString("ExceptionThrown", new Object[]{e.getClass().getName(), "onCheckTID()", e.getMessage()});
            mLogger.log(Level.SEVERE, errMsg);
            throw new AbapException("CHECK_TID_ERROR", errMsg);
        }
        return bret;
    }
    
    
    /**
     * Commits the TID into the persistent database.
     *
     * @param       tid       The TID to commit.
     */
    public void onCommit(String tid) {
        Utils.checkLog(mLogger, Level.FINE, "InsideMethod", "onCommit(String tid)");
        if (tid.length() != TIDManager.TID_SIZE) {
            String errMsg = mMessages.getString("FileTIDManagerImpl.Bad_TID_Length", new Object[]{"onCommmit", tid, TIDManager.TID_SIZE});
            mLogger.log(Level.SEVERE, errMsg);
            throw new AbapException("NOT_SUPPORTED", errMsg);
        }
        
        try {
            lockFile();
            RandomAccessFile tidRf = new RandomAccessFile(tidF, "rw");
            
            StringBuffer tidBuf = readTIDFileFully(tidRf);
            int tidPos = findInBoundTID(tidBuf, tid);
            if (tidPos >= 0) {
                char thetidState = tidBuf.charAt(tidPos + INBOUND_STATE_OFFSET);
                if (RESERVED == thetidState) {
                    tidBuf.setCharAt(tidPos + INBOUND_STATE_OFFSET, COMMITTED);
                    writeTIDFileFully(tidRf, tidBuf);
                    Utils.checkLog(mLogger, Level.FINE, "FileTIDManagerImpl.onCommitted_TID", tid);
                } else {
                    Utils.checkLog(mLogger, Level.FINE, "FileTIDManagerImpl.onCommitted_TID_Not_Reserved", new Object[]{tid, new Character(thetidState)});
                }
            } else {
                Utils.checkLog(mLogger, Level.FINE, "FileTIDManagerImpl.onCommitted_Cannot_Find_TID", tid);
            }
            
            tidRf.close();
            unlockFile();
        } catch (IOException e) {
            String errMsg = mMessages.getString("ExceptionThrown", new Object[]{e.getClass().getName(), "onCommit()", e.getMessage()});
            mLogger.log(Level.SEVERE, errMsg);
        } catch (Exception e) {
            String errMsg = mMessages.getString("ExceptionThrown", new Object[]{e.getClass().getName(), "onCommit()", e.getMessage()});
            mLogger.log(Level.SEVERE, errMsg);
        }
    }
    
    
    /**
     * Confirms the TID in the persistent database.  From SAP's standpoint, this
     * means the TID can be removed from the database, since it will never be sent
     * again by SAP.
     *
     * @param       tid       The TID to confirm.
     */
    public void onConfirmTID(String tid) {
        Utils.checkLog(mLogger, Level.FINE, "InsideMethod", "onConfirmTID(String tid)");
        if (tid.length() != TIDManager.TID_SIZE) {
            String errMsg = mMessages.getString("FileTIDManagerImpl.Bad_TID_Length", new Object[]{"onConfirmTID", tid, TIDManager.TID_SIZE});
            mLogger.log(Level.SEVERE, errMsg);
            throw new AbapException("NOT_SUPPORTED", errMsg);
        }
        
        try {
            lockFile();
            RandomAccessFile tidRf = new RandomAccessFile(tidF, "rw");
            
            StringBuffer tidBuf = readTIDFileFully(tidRf);
            int tidPos = findInBoundTID(tidBuf, tid);
            if (tidPos >= 0) {
                char thetidState = tidBuf.charAt(tidPos + INBOUND_STATE_OFFSET);
                
                if (COMMITTED == thetidState) {
                    tidBuf.delete(tidPos, tidPos + INBOUND_TID_RECORD_SIZE);
                    writeTIDFileFully(tidRf, tidBuf);
                    Utils.checkLog(mLogger, Level.FINE, "FileTIDManagerImpl.onConfirmed_TID", tid);
                } else {
                    Utils.checkLog(mLogger, Level.FINE, "FileTIDManagerImpl.onConfirmed_TID_Not_Committed", new Object[]{tid, new Character(thetidState)});
                }
            } else {
                Utils.checkLog(mLogger, Level.FINE, "FileTIDManagerImpl.onConfirmed_Cannot_Find_TID", tid);
            }
            
            tidRf.close();
            unlockFile();
        } catch (IOException e) {
            String errMsg = mMessages.getString("ExceptionThrown", new Object[]{e.getClass().getName(), "onConfirmTID()", e.getMessage()});
            mLogger.log(Level.SEVERE, errMsg);
        } catch (Exception e) {
            String errMsg = mMessages.getString("ExceptionThrown", new Object[]{e.getClass().getName(), "onConfirmTID()", e.getMessage()});
            mLogger.log(Level.SEVERE, errMsg);
        }
    }
    
    
    /**
     * Rollbacks the TID from the persistent database (marks it as unprocessed, U).
     *
     * @param       tid       The TID to confirm.
     */
    public void onRollback(String tid) {
        Utils.checkLog(mLogger, Level.FINE, "InsideMethod", "onRollback(String tid)");
        if (tid.length() != TIDManager.TID_SIZE) {
            String errMsg = mMessages.getString("FileTIDManagerImpl.Bad_TID_Length", new Object[]{"onRollback", tid, TIDManager.TID_SIZE});
            mLogger.log(Level.SEVERE, errMsg);
            throw new AbapException("NOT_SUPPORTED", errMsg);
        }
        
        try {
            lockFile();
            RandomAccessFile tidRf = new RandomAccessFile(tidF, "rw");
            
            StringBuffer tidBuf = readTIDFileFully(tidRf);
            int tidPos = findInBoundTID(tidBuf, tid);
            if (tidPos >= 0) {
                char thetidState = tidBuf.charAt(tidPos + INBOUND_STATE_OFFSET);
                if (RESERVED == thetidState) {
                    tidBuf.setCharAt(tidPos + INBOUND_STATE_OFFSET, UNPROCESSED);
                    writeTIDFileFully(tidRf, tidBuf);
                    Utils.checkLog(mLogger, Level.FINE, "FileTIDManagerImpl.onRollback_TID", tid);
                } else {
                    Utils.checkLog(mLogger, Level.FINE, "FileTIDManagerImpl.onRollback_TID_Not_Reserved", new Object[]{tid, new Character(thetidState)});
                    
                }
            } else {
                Utils.checkLog(mLogger, Level.FINE, "FileTIDManagerImpl.onRollback_Cannot_Find_TID", tid);
            }
            
            tidRf.close();
            unlockFile();
        } catch (IOException e) {
            String errMsg = mMessages.getString("ExceptionThrown", new Object[]{e.getClass().getName(), "onRollback()", e.getMessage()});
            mLogger.log(Level.SEVERE, errMsg);
        } catch (Exception e) {
            String errMsg = mMessages.getString("ExceptionThrown", new Object[]{e.getClass().getName(), "onRollback()", e.getMessage()});
            mLogger.log(Level.SEVERE, errMsg);
        }
    }
    
    
    /**
     * Reads the outbound EID/TID database file.
     *
     * @param       tidRf      RandomAccessFile pointer to EID/TID database file
     * @return      The string buffer representing the max limit of rows read.
     * @throws      IOException     When IO errors occur
     */
    protected StringBuffer readOutboundTIDFile(RandomAccessFile tidRf)
    throws IOException {
        Utils.checkLog(mLogger, Level.FINE, "InsideMethod", "readOutboundTIDFile(RandomAccessFile tidRf)");
        StringBuffer sb = readTIDFileFully(tidRf);
        tidCount = 0;
        int sbLen = sb.length();
        Utils.checkLog(mLogger, Level.FINE, "LengthIs", new Object[]{"stringbuf", new Integer(sbLen)});
        for (int i = 0; i < sbLen; i++) {
            if (sb.charAt(i) == LF) {
                tidCount++;
            }
        }
        return sb;
    }
    
    /**
     * For test mode.
     * @return  Next TID.
     */
    private synchronized int createTID() {
        return localTID++;
    }
    
    
    /**
     * Creates a TID (using SAP's method) and stores it persistently, marking it
     * as reserved (R).
     *
     * @param   eid     The eid for which a tid will be created.
     * @return  String  The TID created for the eid.
     */
    public String createTID(String eid) {
        String tid = null;
        Utils.checkLog(mLogger, Level.FINE, "InsideMethod", "createTID(String eid)");
        Utils.checkLog(mLogger, Level.FINE, "ValueIs", new Object[]{"eid", eid});
        try {
            lockFile();
            RandomAccessFile tidRf = new RandomAccessFile(tidF, "rw");
            
            StringBuffer tidBuf = readOutboundTIDFile(tidRf);
            
            // Find the TID corresponding to the given EID
            int tidRowPos;
            
            //mLogger.debug(" *** tidRowPos is - " + tidRowPos);
            if ((tidRowPos = findOutBoundTIDForEID(tidBuf, eid)) < 0) {
                // such TID was never created for the eid
                if (inTestMode) {
                    tid = tidFmt.sprintf(createTID());
                } else {
                    com.sap.mw.jco.JCO.Client client = myConnector.getClient().getJCOClient();
                    tid = client.createTID();
                    myConnector.setLastActivityTime(System.currentTimeMillis());
                }
                
                // |<--TID(24)-->|<--EID(variable)-->|LF(1)
                // add at the end of buffer
                tidBuf.append(tid).append(eid).append(LF);
                writeTIDFileFully(tidRf, tidBuf);
                Utils.checkLog(mLogger, Level.FINE, "FileTIDManagerImpl.Created_TID", new Object[]{tid, eid});
            }
            
            tidRf.close();
            unlockFile();
        } catch (IOException e) {
            String errMsg = mMessages.getString("ExceptionThrown", new Object[]{e.getClass().getName(), "createTID()", e.getMessage()});
            mLogger.log(Level.SEVERE, errMsg);
        } catch (Exception e) {
            String errMsg = mMessages.getString("ExceptionThrown", new Object[]{e.getClass().getName(), "createTID()", e.getMessage()});
            mLogger.log(Level.SEVERE, errMsg);
        }
        
        return tid;
    }
    
    /**
     * Returns you an index of the char ch given.
     *
     * @param   sb     the buffer of characters
     * @param   ch     the char whose index is to be located
     * @param   fromIndex   the fromIndex
     * @return  int     The index of the given char
     */
    protected int indexOf(StringBuffer sb, char ch, int fromIndex) {
        int retIdx = -1;
        int sbLen = sb.length();
        for (int i = fromIndex; i < sbLen; i++) {
            if (sb.charAt(i) == ch) {
                retIdx = i;
                break;
            }
        }
        return retIdx;
    }
    
    
    /**
     * Confirms the TID in SAP's database.  SAP will then remove the TID from its
     * tracking since e*Gate is basically guaranteeing that the TID will never be
     * sent again.
     *
     */
    public void confirmTID() {
        //if not many record in the table, do not confirm the oldest one
        Utils.checkLog(mLogger, Level.FINE, "InsideMethod", "confirmTID()");
        try {
            lockFile();
            RandomAccessFile tidRf = new RandomAccessFile(tidF, "rw");
            
            StringBuffer tidBuf = readOutboundTIDFile(tidRf);
            Utils.checkLog(mLogger, Level.FINE, "ValueIs", new Object[]{"tidCount", tidCount});
            Utils.checkLog(mLogger, Level.FINE, "ValueIs", new Object[]{"tidBuf", tidBuf.toString()});
            if (tidCount > outBoundTidMaxRow) {
                // confirm the oldest TID's
                int nConfirm = tidCount - outBoundTidMaxRow;
                Utils.checkLog(mLogger, Level.FINE, "ValueIs", new Object[]{"nConfirm", nConfirm});
                int rowBeginIdx = 0;
                
                for (int i = 0; i < nConfirm; i++, rowBeginIdx = indexOf(tidBuf, LF, rowBeginIdx) + 1) {
                    Utils.checkLog(mLogger, Level.FINE, "ValueIs", new Object[]{"rowBeginIdx", rowBeginIdx});
                    // |<--TID(24)-->|<--EID(variable)-->|LF(1)
                    String tid = tidBuf.substring(rowBeginIdx, rowBeginIdx + OUTBOUND_EID_OFFSET);
                    Utils.checkLog(mLogger, Level.FINE, "ValueIs", new Object[]{"tid", tid});
                    // Have SAP confirm the TID
                    if (inTestMode) {
                        Utils.checkLog(mLogger, Level.FINE, "Simulated_Confirm",tid);
                    } else {
                        myConnector.getClient().getJCOClient().confirmTID(tid);
                        String eid = tidBuf.substring(rowBeginIdx + OUTBOUND_EID_OFFSET,
                                indexOf(tidBuf, LF, rowBeginIdx));
                        Utils.checkLog(mLogger, Level.FINE, "Confirm_TID", new Object[]{tid, eid});
                    }
                }
                
                String tidBufStr = tidBuf.substring(rowBeginIdx);
                writeTIDFileFully(tidRf, tidBufStr);
            }
            
            tidRf.close();
            unlockFile();
        } catch (IOException e) {
            String errMsg = mMessages.getString("ExceptionThrown", new Object[]{e.getClass().getName(), "confirmTID()", e.getMessage()});
            mLogger.log(Level.SEVERE, errMsg);
        } catch (Exception e) {
            String errMsg = mMessages.getString("ExceptionThrown", new Object[]{e.getClass().getName(), "confirmTID()", e.getMessage()});
            mLogger.log(Level.SEVERE, errMsg);
        }
    }
    
    /**
     * Converts a partial (maybe) pathname to a fully qualified abstract
     * <code>File</code> object, normalized relative to e*Gate.
     *
     * @param     fname        The partial pathname.
     * @return    The fully qualified file
     */
    public static File getFullPath(String fname) {
        File retFile = null;
        if (fname != null) {
            fname = fname.replace('/', File.separatorChar);
            retFile = new File(fname);
        }
        return retFile;
    }
    
    
    /**
     * A standalone tester helper
     */
    public static class TestFileTIDmanagerImpl extends Thread {
        private String tid;
        private String eid;
        private String act;
        private FileTIDManagerImpl tidMgr;
        private Logger mLogger = Messages.getLogger(TestFileTIDmanagerImpl.class);
        
        /**
         * creates a new TestFileTIDmanagerImpl, Constructor
         *
         * @param   yourtid     the TID
         * @param   youreid     the EID
         * @param   yourAct     the Account
         */
        public TestFileTIDmanagerImpl(String yourtid, String youreid, String yourAct) {
            this.tid = yourtid;
            this.eid = youreid;
            this.act = yourAct;
            this.tidMgr = new FileTIDManagerImpl();
        }
        
        /**
         * Run method to execute the TestFileTIDmanagerImpl Thread
         *
         *
         */
        public void run() {
            Utils.checkLog(mLogger, Level.FINE, "InsideMethod", "run() of TestFileTIDmanagerImpl");
            try {
                if (this.act.equals("onCheckTID")) {
                    Utils.checkLog(mLogger, Level.FINE, "ValueIs", new Object[]{"tidMgr.onCheckTID(this.tid)", tidMgr.onCheckTID(this.tid)});
                } else if (this.act.equals("onCommit")) {
                    tidMgr.onCommit(this.tid);
                } else if (this.act.equals("onConfirmTID")) {
                    tidMgr.onConfirmTID(this.tid);
                } else if (this.act.equals("onRollback")) {
                    tidMgr.onRollback(this.tid);
                } else if (this.act.equals("createTID")) {
                    tidMgr.createTID(this.eid);
                } else if (this.act.equals("confirmTID")) {
                    tidMgr.confirmTID();
                }
            } catch (Exception e) {
                String errMsg = mMessages.getString("ExceptionThrown", new Object[]{e.getClass().getName(), "run()", e.getMessage()});
                mLogger.log(Level.SEVERE, errMsg);
            }
        }
    }
    
    
    /**
     * Standalone tester for the FileTIDManagerImpl
     * @param  args     command line arguments
     */
    public static void main(String[] args) {
        //String[] tids = {"012345678901234567890121", "012345678901234567890122", "012345678901234567890123","012345678901234567890124", "012345678901234567890125", "012345678901234567890126", "012345678901234567890127"};
        String[] tids = {"012345678901234567890121"};
        String usage = "\nUsage: FileTIDManagerImpl {onCheckTID | onCommit | onConfirmTID | onRollback | createTID | "
                + " confirmTID [maxOutboundKeep]}";
        if (args.length == 0) {
            System.out.println(usage);
            System.exit(0);
        }
        
        if (("onCheckTID|onCommit|onConfirmTID|onRollback").indexOf(args[0]) != -1) {
            for (int i = 0; i < tids.length; i++) {
                (new TestFileTIDmanagerImpl(tids[i], "", args[0])).start();
            }
        } else if (("createTID|confirmTID").indexOf(args[0]) != -1) {
            if (args.length > 1) {
                int outBoundTidMaxRow = Integer.parseInt(args[1]);
            }
            
            for (int i = 0; i < tids.length; i++) {
                (new TestFileTIDmanagerImpl("", tids[i], args[0])).start();
            }
        } else {
            System.out.println("Unknown test command");
            System.out.println(usage);
        }
    }
}
