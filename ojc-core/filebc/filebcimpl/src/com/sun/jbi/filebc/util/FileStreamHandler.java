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
 * @(#)FileStreamHandler.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.PushbackInputStream;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;

/**
 * Input stream handler.
 *
 * @author Sherry Weng
 **/
public class FileStreamHandler {

    public static final int DEFAULT_BUFFER_LENGTH = 1024;
    private static final int READ_CHUNK_SIZE = 51200;
    public static final char CARRIAGE_RETURN = '\r';
    public static final char LINE_FEED = '\n';
    private static final Messages mMessages = Messages.getMessages(FileStreamHandler.class);
    private static Logger mLogger = Messages.getLogger(FileStreamHandler.class);
    private long mMaxRecordSize;
    private long mMaxStreamLength;
    private boolean mDropEOL;
    private byte[] mEOR;
    private PushbackInputStream mStream;
    private boolean multipleRecords = false;
    private boolean donotClose = false;
    private boolean closed = false;

    public FileStreamHandler() {
    }

    public FileStreamHandler(boolean aDropEOL,
            byte[] aEOR,
            long aMaxRecordSize,
            long aMaxStreamLength) {
        mDropEOL = aDropEOL;
        mMaxRecordSize = aMaxRecordSize;
        mMaxStreamLength = aMaxStreamLength;
        mEOR = copyByteArray(aEOR);
    }

    public FileStreamHandler(InputStream aStream,
            boolean aDropEOL,
            byte[] aEOR,
            long aMaxRecordSize,
            long aMaxStreamLength) {
        this(aDropEOL, aEOR, aMaxRecordSize, aMaxStreamLength);

        mStream = new PushbackInputStream(aStream,
                (int) (mMaxRecordSize > 0 ? mMaxRecordSize
                : DEFAULT_BUFFER_LENGTH));

    }

    public byte[] getAllContentsAsBytes() throws IOException {
        return getAllContentsAsBytes(mStream);
    }

    public byte[] getAllContentsAsBytes(InputStream aStream) throws IOException {
        byte[] lData = null;

        if (aStream == null) {
            mLogger.log(Level.SEVERE, mMessages.getString("FILEBC-E00620"));
            throw new IOException(mMessages.getString("FILEBC-E00620"));
        }

        if (mMaxStreamLength == 0) {
            return null;
        }

        try {
            if (aStream.available() > 0) {
                int lSize = 0;
                if (mMaxRecordSize > 0 && mMaxRecordSize < mMaxStreamLength) {
                    mLogger.log(Level.SEVERE, mMessages.getString("FILEBC-E00621", new Object[]{mMaxStreamLength, mMaxRecordSize}));
                    lData = new byte[(int) mMaxRecordSize];
                    lSize = aStream.read(lData);
                } else {
                    lData = new byte[(int) mMaxStreamLength];
                    lSize = aStream.read(lData);
                }

                if (lSize < 0) {
                    // no data
                    return null;
                }

//                // All done, close the stream
//                aStream.close();

                if (mDropEOL) {
                    lData = removeEOL(lData);
                }
            }
        } catch (IOException e) {
            mLogger.log(Level.SEVERE, e.getLocalizedMessage());
            throw e;
        }

        return lData;
    }

    public boolean hasMoreRecords() throws IOException {
        return hasMoreRecords(mStream);
    }

    public boolean hasMoreRecords(InputStream aStream) throws IOException {
        if (closed) {
            return false;
        }

        boolean lReturn = false;
        if (aStream == null) {
            mLogger.log(Level.SEVERE, mMessages.getString("FILEBC-E00620"));
            throw new IOException(mMessages.getString("FILEBC-E00620"));
        }

        if (aStream.available() > 0) {
            lReturn = true;
        }

        return lReturn;
    }

    public byte[] readNextRecord() throws IOException {
        return readNextRecord(mStream);
    }

    public byte[] readNextRecord(PushbackInputStream aStream) throws IOException {
        if (aStream == null) {
            mLogger.log(Level.SEVERE, mMessages.getString("FILEBC-E00620"));
            throw new IOException(mMessages.getString("FILEBC-E00620"));
        }

        int lBufferSize = (mMaxRecordSize == -1) ? DEFAULT_BUFFER_LENGTH : (int) mMaxRecordSize;
        int lReadSize = 0;
        byte[] lData = null;
        byte[] lTemp = null;
        byte[] lBuffer = new byte[lBufferSize];

        /**
         * If record delimiter is not defined and
         * there is a max record size, then the assumption
         * is that the data is fixed length'ed and
         * maxRecordSize is considered the data length
         */
        if ((mEOR == null ||
                mEOR.length == 0) &&
                mMaxRecordSize > 0) {
            int lLength = aStream.read(lBuffer, 0, lBufferSize);
            if (lLength < 0) {
                // All done with reading
                return null;
            }

            lData = getByteArrayChunks(lBuffer, 0, lLength);
            if (mDropEOL) {
                lData = removeEOL(lData);
            }
            return lData;
        }

        if (mEOR == null || mEOR.length == 0) {
            mLogger.log(Level.WARNING, mMessages.getString("FILEBC-W00620"));
            mEOR = new byte[]{LINE_FEED};  // Windows vs. Unix??
        }

        while (true) {
            int lLength = aStream.read(lBuffer, 0, lBufferSize);

            if (lLength < 0) {
                /**
                 * Reached the end of stream
                 * Return what's in the data buffer
                 */
                return lData;
            }

            lReadSize += lLength;
            if (lData != null && lData.length > 0) {
                // Data buffer is not empty
                lData = appendBytes(lBuffer, 0, lData, lLength);
            } else {
                // Populate the data buffer with what's read
                lData = getByteArrayChunks(lBuffer, 0, lLength);
            }

            int lEOR = checkEndOfRecord(lBuffer, 0, lLength);
            if (lEOR == -1) {   // EOR not found
                // Check to see if the EOR is on the edge of 2 buffers
                int lStart = checkPartialEndOfRecord(lBuffer, lLength - mEOR.length, lLength);
                if (lStart != -1) {
                    /** We did find the first byte of the EOR sequence.
                     * Read subsequent bytes for the length of EOR defined
                     * to see if indeed lStart is the beginning index of EOR
                     */
                    int lRest = mEOR.length - (lLength - lStart);
                    byte[] lRestBytes = new byte[lRest];
                    int lSize = aStream.read(lRestBytes, 0, lRest);

                    /**
                     * Get the bytes starting from the last occurrence
                     * of the starting byte of EOR
                     */
                    lTemp = getByteArrayChunks(lBuffer, lStart, lLength);
                    lTemp = appendBytes(lRestBytes, 0, lTemp, lRest);

                    if (byteArrayEquals(lTemp, mEOR)) {
                        // Found EOR. Return the appropriate number of bytes in the data buffer
                        lData = getByteArrayChunks(lData, 0, lData.length - (lLength - lStart));
                        if (mDropEOL) {
                            lData = removeEOL(lData);
                        }
                        break;
                    } else {
                        if (mMaxRecordSize > 0) {
                            if (lLength < mMaxRecordSize) {
                                lData = getByteArrayChunks(lData, 0, lLength);
                            } else {
                                lData = getByteArrayChunks(lData, 0, (int) mMaxRecordSize);
                            }
                            if (mDropEOL) {
                                lData = removeEOL(lData);
                            }

                            // Push back lRestBytes so that subsequent read
                            // resumes at the correct starting position
                            aStream.unread(lRestBytes, 0, lRest);
                            break;
                        } else {
                            // Append to the data buffer what we have just read
                            lData = appendBytes(lRestBytes, 0, lData, lRest);
                            continue;
                        }
                    }
                } else {
                    if (mMaxRecordSize > 0) {
                        if (lLength < mMaxRecordSize) {
                            lData = getByteArrayChunks(lData, 0, lLength);
                        } else {
                            lData = getByteArrayChunks(lData, 0, (int) mMaxRecordSize);
                        }
                        if (mDropEOL) {
                            lData = removeEOL(lData);
                        }
                        break;
                    } else {
                        continue;
                    }
                }
            } else if (lEOR >= 0) {
                // Found the EOR sequence!
                lData = getByteArrayChunks(lData, 0, lData.length - (lLength - lEOR));

                // lEOR indicates the index of the first byte of EOR in the data buffer
                if (lEOR < lLength) {
                    // Push back the stream for subsequent read
                    aStream.unread(lBuffer, lEOR + mEOR.length, lLength - lEOR - mEOR.length);
                }

                break;
            }
        }

        return lData;
    }

    private int checkEndOfRecord(byte[] pBuffer, int pStart, int pLength) {
        int lReturn = -1; // Not found by default

        for (int i = pStart; i < pLength; i++) {
            if (pBuffer[i] == mEOR[ 0]) {
                if (i + mEOR.length > pLength) { // unable to compare the whole length of EOR
                    return lReturn;
                }
                boolean lFound = true;
                // check the following bytes for the length of EOR
                for (int j = 1; j < mEOR.length; j++) {
                    if (pBuffer[i + j] != mEOR[j]) {
                        lFound = false;
                        break;
                    }
                }
                if (lFound) {
                    lReturn = i;
                    break;
                }
            }
        }

        return lReturn;
    }

    private int checkPartialEndOfRecord(byte[] pBuffer, int pStart, int pLength) {
        int lReturn = -1;   // Not found by default

        if (pStart < 0 || pLength <= 0 || pBuffer.length == 0) {
            return lReturn;
        }

        for (int i = pStart; i < pLength; i++) {
            if (pBuffer[i] == mEOR[ 0]) {
                lReturn = i;
                break;
            }
        }

        return lReturn;
    }

    private boolean byteArrayEndsWith(byte[] pArray, byte[] pEndsWith) {
        if (pArray == null) {
            // Return true if the both arrays are null
            return pEndsWith == null;
        } else {
            if (pEndsWith == null) {
                // Return true if ends with is null because then it is nothing to check against
                return true;
            } else {
                if (pArray.length < pEndsWith.length) {
                    // Return false if the endsWith array is bigger
                    return false;
                } else {
                    for (int i = 0; i < pEndsWith.length; i++) {
                        byte lTarget = pArray[pArray.length - pEndsWith.length + i];
                        byte lCheck = pEndsWith[i];
                        if (lTarget != lCheck) {
                            // If a character is different the return false
                            return false;
                        }
                    }
                    // If all character are equal then it ends with the given byte array
                    return true;
                }
            }
        }
    }

    private byte[] appendBytes(byte[] pSource, int pSrcPosition, byte[] pTarget, int pLength) {
        byte[] lReturn = null;
        if (pTarget != null) {
            if (pSource != null && pSource.length > 0) {
                pLength = (pLength > pSource.length ? pSource.length : pLength);
                lReturn = new byte[pTarget.length + pLength];
                System.arraycopy(pTarget, 0, lReturn, 0, pTarget.length);
                System.arraycopy(pSource, pSrcPosition, lReturn, pTarget.length, pLength);
                return lReturn;
            } else {
                return pTarget;
            }
        }
        return new byte[]{};
    }

    private boolean byteArrayEquals(byte[] pSource, byte[] pTarget) {
        return byteArrayEquals(pSource, pTarget, pTarget.length);
    }

    private boolean byteArrayEquals(byte[] pSource, byte[] pTarget, int pLength) {
        boolean lEquals = true;
        for (int ii = 0; ii < pLength; ii++) {
            if (pSource[ii] != pTarget[ii]) {
                lEquals = false;
            }
        }

        return lEquals;
    }

    private byte[] removeEOL(byte[] pData) {
        if (byteArrayEndsWith(pData, new byte[]{CARRIAGE_RETURN, LINE_FEED})) { // Windows
            return truncateBytes(pData, 2);
        } else if (byteArrayEndsWith(pData, new byte[]{LINE_FEED}) || // Unix, other...
                byteArrayEndsWith(pData, new byte[]{CARRIAGE_RETURN})) {
            return truncateBytes(pData, 1);
        }

        return pData;
    }

    private byte[] truncateBytes(byte[] pArray, int pNumberOfBytes) {
        return truncateBytes(pArray, pArray.length, pNumberOfBytes);
    }

    private byte[] truncateBytes(byte[] pArray, int pLength, int pNumberOfBytes) {
        byte[] lReturn = new byte[]{};
        if (pArray != null) {
            if (pLength > pNumberOfBytes) {
                lReturn = new byte[pLength - pNumberOfBytes];
                System.arraycopy(pArray, 0, lReturn, 0, pLength - pNumberOfBytes);
            }
        }
        return lReturn;
    }

    private byte[] getByteArrayChunks(byte[] pSource, int pStart, int pEnd) {
        int lLength = pEnd - pStart;
        byte[] lReturn = new byte[lLength];
        System.arraycopy(pSource, pStart, lReturn, 0, lLength);

        return lReturn;
    }

    private byte[] copyByteArray(byte[] pSource) {
        byte[] lTarget = new byte[pSource.length];
        System.arraycopy(pSource, 0, lTarget, 0, pSource.length);

        return lTarget;
    }

    public InputStream getInpuStream() {
        return mStream;
    }

    public boolean isMultipleRecords() {
        return multipleRecords;
    }

    public void setMultipleRecords(boolean multipleRecords) {
        this.multipleRecords = multipleRecords;
    }

    public synchronized void close() {
        if (closed) {
            return;
        }
        try {
            mStream.close();
            closed = true;
        } catch (Throwable t) {
        }
    }

    public boolean isDonotClose() {
        return donotClose;
    }

    public void setDonotClose(boolean donotClose) {
        this.donotClose = donotClose;
    }

    public long getSize() {
        return mMaxStreamLength;
    }
}
