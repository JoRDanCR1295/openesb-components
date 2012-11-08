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
 * @(#)OutputFilenameFormatter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc.util;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.filebc.SequenceRegistry;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.UUID;
import java.io.File;
import java.io.RandomAccessFile;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

/** This is a utility class to derive the next available
 * output file name.
 *
 * @author Sherry Weng
 * @author Qian Fu jim.fu@sun.com
 * 
 */
public class OutputFilenameFormatter {

    private static final int MAX_INDEX = 2147483646;
    private static final Messages mMessages = Messages.getMessages(OutputFilenameFormatter.class);
    private static HashMap mSequencedFileNames = new HashMap();
    private static HashMap mPersistedSequencedFileNames = new HashMap();
    private static HashMap mTimedFileNames = new HashMap();
    private static HashMap mGuidFileNames = new HashMap();

    private OutputFilenameFormatter() {}
    
    /**
     *  for junit only
     **/
    public synchronized static String getNextOutputFileName(String fileNamePattern) throws Exception {
        String[] decomposedName = new String[3];
        FileNamePatternType type = FileNamePatternUtil.validatePattern(fileNamePattern, decomposedName);

        if (type == FileNamePatternType.NAME_INVALID) {
            throw new Exception(mMessages.getString("FILEBC-E00660.OFF_Invalid_pattern", fileNamePattern));
        }

        int seqnum = 0;
        String filename = null;
        String prefix = decomposedName[0];
        String suffix = decomposedName[2];
        String key = prefix + suffix;

        switch (type) {
            case NAME_WITH_TIMESTAMP:
                Date date = new Date(System.currentTimeMillis());
                SimpleDateFormat formatter = new SimpleDateFormat("yyyyMMdd-HH-mm-ss-SSS");
                filename = prefix + formatter.format(date) + suffix;
                mTimedFileNames.put(key, filename);
                break;
            case NAME_WITH_SEQ:
                if (mSequencedFileNames.containsKey(key)) {
                    String oldname = (String) mSequencedFileNames.get(key);
                    seqnum = getIndex(oldname, prefix, suffix);
                    int next = getOneUp(seqnum);
                    if (next >= MAX_INDEX) {
                        next = 0;
                    }
                    filename = prefix + next + suffix;
                    mSequencedFileNames.put(key, filename);
                } else {
                    filename = prefix + seqnum + suffix;
                    mSequencedFileNames.put(key, filename);
                }
                break;
            case NAME_WITH_UUID:
                String uuid = UUID.randomUUID().toString();
                filename = prefix + uuid + suffix;
                mGuidFileNames.put(key, filename);
                break;
            case NAME_WITH_PERSISTED_SEQ:
                // no need to test this in junit
                break;
            default:
                throw new Exception(mMessages.getString("FILEBC-E00660.OFF_Invalid_pattern", fileNamePattern));
        }

        return filename;
    }

    public synchronized static String getNextOutputFileName(String fileNamePattern, String aKey, File seqPersistDir) throws Exception {
        // seqPersistDir - if given - is suppose to be a resolved path pointing to the directory where
        // a persisted sequence store resides
        String[] decomposedName = new String[3];
        FileNamePatternType type = FileNamePatternUtil.validatePattern(fileNamePattern, decomposedName);

        if (type == FileNamePatternType.NAME_INVALID) {
            throw new Exception(mMessages.getString("FILEBC-E00660.OFF_Invalid_pattern", fileNamePattern));
        }

        int seqnum = 0;
        String filename = null;
        String prefix = decomposedName[0];
        String suffix = decomposedName[2];

        switch (type) {
            case NAME_WITH_TIMESTAMP:
                Date date = new Date(System.currentTimeMillis());
                SimpleDateFormat formatter = new SimpleDateFormat("yyyyMMdd-HH-mm-ss-SSS");
                filename = prefix + formatter.format(date) + suffix;
                mTimedFileNames.put(aKey, filename);
                break;
            case NAME_WITH_SEQ:
                if (mSequencedFileNames.containsKey(aKey)) {
                    String oldname = (String) mSequencedFileNames.get(aKey);
                    seqnum = getIndex(oldname, prefix, suffix);
                    int next = getOneUp(seqnum);
                    if (next >= MAX_INDEX) {
                        next = 0;
                    }
                    filename = prefix + next + suffix;
                    mSequencedFileNames.put(aKey, filename);
                } else {
                    filename = prefix + seqnum + suffix;
                    mSequencedFileNames.put(aKey, filename);
                }
                break;
            case NAME_WITH_UUID:
                filename = prefix + UUID.randomUUID().toString() + suffix;
                mGuidFileNames.put(aKey, filename);
                break;
            case NAME_WITH_PERSISTED_SEQ:
                // fetch seq from the seq store
                // which is lock protected
                String seq = fetchSeq(seqPersistDir, aKey, decomposedName[1]);
                if (seq == null || seq.trim().length() == 0) {
                    throw new Exception(mMessages.getString("FILEBC-E00661.OFF_Error_can_not_resolve_persisted_seq", fileNamePattern));
                }
                filename = prefix + seq + suffix;
                break;
            default:
                throw new Exception(mMessages.getString("FILEBC-E00660.OFF_Invalid_pattern", fileNamePattern));
        }

        return filename;
    }

    /**
     * for junit only
     **/
    public synchronized static String getOutputFileName(String fileNamePattern) throws Exception {
        String[] decomposedName = new String[3];
        FileNamePatternType type = FileNamePatternUtil.validatePattern(fileNamePattern, decomposedName);

        if (type == FileNamePatternType.NAME_INVALID) {
            throw new Exception(mMessages.getString("FILEBC-E00660.OFF_Invalid_pattern", fileNamePattern));
        }

        String filename = null;
        String key = decomposedName[0] + decomposedName[2];

        switch (type) {
            case NAME_WITH_TIMESTAMP:
                if (mTimedFileNames.containsKey(key)) {
                    filename = (String) mTimedFileNames.get(key);
                } else {
                    filename = getNextOutputFileName(fileNamePattern, key, null);
                }
                break;
            case NAME_WITH_SEQ:
                if (mSequencedFileNames.containsKey(key)) {
                    filename = (String) mSequencedFileNames.get(key);
                } else {
                    filename = getNextOutputFileName(fileNamePattern, key, null);
                }
                break;
            case NAME_WITH_UUID:
                if (mGuidFileNames.containsKey(key)) {
                    filename = (String) mGuidFileNames.get(key);
                } else {
                    filename = getNextOutputFileName(fileNamePattern, key, null);
                }
                break;
            case NAME_WITH_PERSISTED_SEQ:
                // no need to test this in junit
                break;
            default:
                throw new Exception(mMessages.getString("FILEBC-E00660.OFF_Invalid_pattern", fileNamePattern));
        }

        return filename;
    }

    public synchronized static String getOutputFileName(String fileNamePattern, String aKey, File seqPersistDir) throws Exception {
        // seqPersistDir - if given - is suppose to be a resolved path pointing to the directory where
        // a persisted sequence store resides
        String[] decomposedName = new String[3];
        FileNamePatternType type = FileNamePatternUtil.validatePattern(fileNamePattern, decomposedName);

        if (type == FileNamePatternType.NAME_INVALID) {
            throw new Exception(mMessages.getString("FILEBC-E00660.OFF_Invalid_pattern", fileNamePattern));
        }

        String filename = null;

        switch (type) {
            case NAME_WITH_TIMESTAMP:
                if (mTimedFileNames.containsKey(aKey)) {
                    filename = (String) mTimedFileNames.get(aKey);
                } else {
                    filename = getNextOutputFileName(fileNamePattern, aKey, seqPersistDir);
                }
                break;
            case NAME_WITH_SEQ:
                if (mSequencedFileNames.containsKey(aKey)) {
                    filename = (String) mSequencedFileNames.get(aKey);
                } else {
                    filename = getNextOutputFileName(fileNamePattern, aKey, seqPersistDir);
                }
                break;
            case NAME_WITH_UUID:
                if (mGuidFileNames.containsKey(aKey)) {
                    filename = (String) mGuidFileNames.get(aKey);
                } else {
                    filename = getNextOutputFileName(fileNamePattern, aKey, seqPersistDir);
                }
                break;
            case NAME_WITH_PERSISTED_SEQ:
                // fetch seq from the seq store
                // lock protected
                if (mPersistedSequencedFileNames.containsKey(aKey)) {
                    filename = (String) mPersistedSequencedFileNames.get(aKey);
                } else {
                    filename = getNextOutputFileName(fileNamePattern, aKey, seqPersistDir);
                    mPersistedSequencedFileNames.put(aKey, filename);
                }
                break;
            default:
                throw new Exception(mMessages.getString("FILEBC-E00660.OFF_Invalid_pattern", fileNamePattern));
        }

        return filename;
    }

    public synchronized static void removeOutputFileNamePattern(String key) {
        mSequencedFileNames.remove(key);
        mTimedFileNames.remove(key);
        mGuidFileNames.remove(key);
    }

    private static int getOneUp(Integer index) {
        return index.intValue() + 1;
    }

    private static int getIndex(String filename, String prefix, String suffix) {
        int start = prefix.length();
        int end = filename.indexOf(suffix);
        String aIndex = filename.substring(start, end);
        return Integer.parseInt(aIndex);
    }

    private static String fetchSeq(File seqPersistDir, String epKey, String seqVarName) throws Exception {
        String seqNum = "";
        int seqVal = 0;
        if (!seqPersistDir.exists()) {
            seqPersistDir.mkdirs();
        } else if (!seqPersistDir.isDirectory()) {
            throw new Exception(mMessages.getString("FILEBC-E00660.OFF_Invalid_pattern", seqPersistDir.getAbsolutePath()));
        }
        File seqFile = new File(seqPersistDir, seqVarName);
        seqFile.createNewFile();
        // obtain the lock associated with the seq
        ReentrantLock l = SequenceRegistry.register(seqFile.getCanonicalPath(), epKey, new ReentrantLock());
        if (l.tryLock(50000, TimeUnit.MILLISECONDS)) {
            RandomAccessFile randomSeqFile = new RandomAccessFile(seqFile, "rw");
            FileChannel channel = null;
            FileLock fl = null;
            try {
                channel = randomSeqFile.getChannel();
                int attempted = 0;
                while (true) {
                    fl = channel.tryLock();

                    if (fl != null) {
                        // read the seq number from the seq file
                        // empty file is equivalent to 0 seeded
                        if (randomSeqFile.length() > 0) {
                            seqNum = randomSeqFile.readLine();
                            try {
                                seqVal = Integer.parseInt(seqNum);
                            } catch (Exception ex) {
                                throw new Exception(mMessages.getString("FILEBC-E00662.OFF_Invalid_sequence_value_from_persistence", new Object[]{seqNum, ex}));
                            }
                        } else {
                            seqNum = "0";
                        }
                        seqVal++;
                        String nv = "" + seqVal;
                        randomSeqFile.seek(0L);

                        randomSeqFile.write(nv.getBytes());
                        randomSeqFile.close();
                        break;
                    } else {
                        if (attempted > 5) {
                            throw new Exception(mMessages.getString("FILEBC-E00663.OFF_Fail_acquire_seq_file_lock", seqFile.getAbsolutePath()));
                        }
                        attempted++;
                        Thread.sleep(1000);
                    }
                }
            } catch (Exception ex) {
                ex.printStackTrace();
            } finally {
                if (randomSeqFile != null) {
                    try {
                        randomSeqFile.close();
                    } catch (Exception ex) {
                        // ignore on purpose, we have tried
                    }
                }
                if (channel != null) {
                    try {
                        channel.close();
                    } catch (Exception ex) {
                        // ignore on purpose, we have tried
                    }
                }
                if (fl != null) {
                    try {
                        fl.release();
                    } catch (Exception ex) {
                        // ignore on purpose
                    }
                }
                l.unlock();
            }
        } else {
            // can not fetch seq because lock acquire failed
            throw new Exception(mMessages.getString("FILEBC-E00664.OFF_Timeout_acquire_exclusive_lock", seqFile.getAbsolutePath()));
        }
        return seqNum;
    }
}
