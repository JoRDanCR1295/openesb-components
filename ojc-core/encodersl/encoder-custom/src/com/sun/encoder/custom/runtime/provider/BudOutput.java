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
 * @(#)BudOutput.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.custom.runtime.provider;

import java.io.File;
import java.io.IOException;
import java.util.Stack;

import javax.xml.namespace.QName;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.sax.SAXResult;
import javax.xml.transform.stream.StreamSource;

import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;

import com.sun.encoder.runtime.CoderFactory;
import com.sun.encoder.runtime.MarshalException;
import com.sun.encoder.runtime.OtdOutputStream;
import com.sun.encoder.runtime.StringCoder;
import com.sun.encoder.custom.runtime.provider.Nodes.Node;
import com.sun.encoder.custom.runtime.provider.OtdDelim.DelimLevel;
import com.sun.encoder.runtime.provider.ByteBuilder;
import com.sun.encoder.runtime.provider.Misc;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.bind.DatatypeConverter;
import org.apache.xmlbeans.SchemaType;

/**
 * BudOutput is a run-time class used to marshal BUD (Basic User Defined)
 * event structure contents. Part of the eGate 5.0 core product.
 * Based on Montana/Java/com/stc/jcsre/ssc/SscOutBuffer.java  1.1.4.1.
 *
 * @author Michael Libourel, Jun Xu
 * @version
 */
public final class BudOutput implements ContentHandler {

    /**
     * Initial size (in bytes) for the output buffer, {@link mBuff}.
     */
    private static final int INITIAL_SIZE = 1000;

    Logger mLog = Logger.getLogger(getClass().getName());

    static final String LN = System.getProperty("line.separator");

    private final Nodes mNodes;
    private final OtdDelim mOtdDelim;

    /**
     * The flush threshold for output buffer (in bytes); 0=none, i.e. no flush.
     * Note that only committed data is flushed; cf. commit().
     */
    private int mFlushThreshold = 0;

    /**
     * The OTD output stream.  This class only writes to the stream in a linear
     * fashion, i.e. no reverse seeks.
     */
    private OtdOutputStream mOutStream = null;

    /**
     * A buffer for a copy of the slice of output data that
     * contains the data for the embedded delimiters.
     * It will be null when there are no embedded delimiters.
     */
    private byte[] mEmbedDelimBytes = null;

    /**
     * A list of flags that parallel the OtdDelim.mSlots[], to signal
     * for each slot whether its data is available yet.
     */
    private final boolean[] mSlotsSeen;

    /**
     * The current output is accumulated in a byte array buffer.
     * Invariant relation:
     *  0 <= mConfirmedSize <= mCursor <= mEmittedSize <= mAllocatedSize.
     */
    private byte[] mBuff = null;

    /** Confirmed data size in mBuff. */
    private int mConfirmedSize = 0;

    /** the current position in mBuff for tenuous data (pending delimiters). */
    private int mCursor = 0;

    /** byte [0..mEmittedSize-1] contain emitted data in mBuff. */
    private int mEmittedSize = 0;

    /** The allocated size in mBuff. */
    private int mAllocatedSize = 0;

    /** offset into output stream for mBuff[0]. */
    private long mOffset = 0;

    /** if >= 0, then it represents cut-off limit for output. */
    private long mOutputLimit = -1;

    /** bytes of space used for default padding. */
    private final byte[] DEFAULT_PAD_BYTES = new byte[]{0x20};

    /** bytes of encoded zero. */
    private byte[] mZeroPadding;

    /**
     * A stack of context objects. <p>
     * Imagine the following XML data starting from root node:
     * <pre>
     * <Root>
     *  <A>
     *      <A1>XX</A1>
     *      <A2></A2>
     *  </A>
     *  <B>
     *      <B1>YY</B1>
     *      <B2>ZZ</B2>
     *  </B>
     * </Root>
     * </pre>
     * <p>
     * There would be the following 4 Context objects generated with the
     * corresponding initial XML start tag as follows:
     * <li>
     * <ol>Context@Root</ol>
     * <ol>Context@A<ol>
     * <ol>Context@A1<ol>
     * <ol>Context@B1<ol>
     * </li>
     * When walking though each of the XML events (let's ignore character data
     * event from a non-leaf node), the corresponding current
     * context ould be as follows:
     * <pre>
     *   Root start tag -- new Context@Root (mIsOpen = true)
     *   A start tag -- new Context@A (pushed above to stack, mIsOpen = true)
     *   A1 start tag -- new Context@A1 (pushed above to stack, mIsOpen = true)
     *   XX character data -- Context@A1 (mIsOpen = true)
     *   A1 end tag -- Context@A1 (mIsOpen = false)
     *   A2 start tag -- Context@A1 (mIsOpen = true)
     *   A2 end tag -- Context@A1 (mIsOpen = false)
     *   A end tag -- Context@A (popped from stack, mIsOpen = false)
     *   B start tag -- Context@A (mIsOpen = true)
     *   B1 start tag -- new Context@B1 (pushed above to stack, mIsOpen = true)
     *   YY character data -- Context@B1 (mIsOpen = true)
     *   B1 end tag -- Context@B1 (mIsOpen = false)
     *   B2 start tag -- Context@B1 (mIsOpen = true)
     *   ZZ character data -- Context@B1 (mIsOpen = true)
     *   B2 end tag -- Context@B1 (mIsOpen = false)
     *   B end tag -- Context@A (popped from stack, mIsOpen = false)
     *   Root end tag -- Context@Root (popped from stack, mIsOpen = false)
     * </pre>
     */
    private final Stack<Context> mContextStack = new Stack<Context>();

    /**
     * Current Context.
     */
    private Context mContext = null;

    private int mTransientLevel = -1;

    private boolean mIsEndDelimCurrent = false;

    /**
     * Character encoder used to encode string into byte array.
     */
    private StringCoder mEncoder;

    /**
     * Creates from the global default delimiter lists and default encoding.
     * The default encoding is used for any field that does not have an
     * explicit "encoding" attribute value in its description in the XSC file.
     *
     * @param nodes  the node descriptor set
     * @param otdDelim  the delimiter descriptor set
     */
    public BudOutput(final Nodes nodes, final OtdDelim otdDelim) {
        mNodes = nodes;
        mOtdDelim = otdDelim;
        if (otdDelim.mSlots == null || otdDelim.mSlots.length == 0) {
            mSlotsSeen = null;
        } else {
            mSlotsSeen = new boolean[otdDelim.mSlots.length];
        }
        if (mLog.isLoggable(Level.FINEST)) {
            mLog.finest("Prepare to output data. Nodes::" + LN + mNodes + LN
                + " OtdDelim::" + mOtdDelim);
        }
    }

    /**
     * Returns the flush threshould.
     * @return the flush threshould.
     */
    public int getFlushThreshold() {
        return mFlushThreshold;
    }

    /**
     * Sets the output threshold for flushing to the output stream.
     *
     * @param threshold the output threshold value (in bytes); 0=none
     */
    public void setFlushThreshold(final int threshold) {
        if (threshold < 0) {
            throw new IllegalArgumentException("Threshold is negative.");
        }
        mFlushThreshold = threshold;
    }

    /**
     * Resets the embedded slot data.
     */
    private void initSlotSeen() {
        if (mSlotsSeen != null) {
            for (int i = mSlotsSeen.length; i-- > 0;) {
                mSlotsSeen[i] = false;
            }
        }
        if (mOtdDelim.mSlotsLength > 0) {
            mEmbedDelimBytes = new byte[mOtdDelim.mSlotsLength];
        }
    }

    /**
     * Makes room for given additional byte size, from current position.
     *
     * @param addedSize the number of bytes to make room for.
     */
    private void makeRoom(final int addedSize) {
        if (addedSize < 0) {
            return;
        }
        int totalSize = mCursor + addedSize;
        if (totalSize > mAllocatedSize) {
            // Grow the buffer.
            int newSize = mAllocatedSize * 3 / 2;
            if (newSize < totalSize) {
                newSize = totalSize;
            }
            byte[] newBuff = new byte[newSize];
            if (mBuff != null) {
                // populate into newBuff
                System.arraycopy(mBuff, 0, newBuff, 0, mAllocatedSize);
            }
            mAllocatedSize = newSize;
            mBuff = newBuff;
        }
    }

    /**
     * Forwards the current position by given delta size.
     *
     * @param delta forward how much
     */
    private void forward(final int delta) {
        if (mLog.isLoggable(Level.FINEST)) {
            mLog.finest("Move forward for " + delta);
        }
        makeRoom(delta);
        mCursor += delta;
        if (mCursor < 0) {
            throw new MarshalException("Move back to content that has"
                    + " been flushed out is not allowed");
        }
    }

    /**
     * Adds data to buffer directly.
     * Throws exception if this exceed the output limit.
     * NYI: Maybe truncation should be silent...
     * NYI: Exception should only happen on commit(), not here...
     *
     * @param data  the data, or null
     * @param offset in data[] to 1st valid byte
     * @param length  number of bytes to add
     */
    public void addRawData(final byte[] data, final int offset,
        final int length) {
        if (mLog.isLoggable(Level.FINEST)) {
            mLog.finest("Add raw bytes=\""
                + Misc.bytes2str(data, offset, length) + "\".");
        }
        if (data == null) {
            return;
        }
        // check if exceeds the output limit
        if (mOutputLimit >= 0 && mOffset + mCursor + length > mOutputLimit) {
            throw new MarshalException("Truncation by parent.");
        }
        makeRoom(length);
        // arraycopy(Object src, int srcPos, Object dest, int destPos, int len)
        System.arraycopy(data, offset, mBuff, mCursor, length);
        mCursor += length;
        mIsEndDelimCurrent = false;
    }

    /**
     * Adds data to buffer directly based on absolute position.
     * Throws exception if this exceed the output limit.
     * NYI: Maybe truncation should be silent...
     * NYI: Exception should only happen on commit(), not here...
     *
     * @param absPos - absolute position.
     * @param data  the data, or null
     * @param offset in data[] to 1st valid byte
     * @param length  number of bytes to add
     */
    public void addRawDataAtAbsPos(final long absPos, final byte[] data,
        final int offset, final int length) {
        if (mLog.isLoggable(Level.FINEST)) {
            mLog.finest("Add raw bytes at absolute position=" + absPos);
        }
        if (data != null) {
            int pos = mCursor;
            mCursor = (int) (absPos - mOffset);
            if (mCursor < 0) {
                throw new MarshalException(
                    "Can not overwrite content that has been flushed out.");
            }
            addRawData(data, offset, length);
            if (pos > mCursor) {
                mCursor = pos;
            }
        }
    }

    /**
     * Adds data to buffer directly.
     * Throws exception if this exceed the output limit.
     * NYI: Maybe truncation should be silent...
     * NYI: Exception should only happen on commit(), not here...
     *
     * @param data  the data, or null
     */
    public void addRawData(final byte[] data) {
        int offset = 0;
        addRawData(data, offset, data.length);
    }

    /**
     * Adds a begin delimiter to buffer directly.
     *
     * @param delim the delimiter descriptor whose begin bytes is to be added.
     */
    public void addBeginDelim(final Delim delim) {
        if (mLog.isLoggable(Level.FINE)) {
            if (mLog.isLoggable(Level.FINEST)) {
                mLog.finest("Add begin delimiter=[" + delim + "]");
            } else {
                mLog.fine("Add begin delimiter=[" + delim.dump() + "]");
            }
        }
        if (delim.isBeginDelimEmbedded()) {
            // Get delimiter from slot.
            Delim.Slot slot = mOtdDelim.mSlots[delim.mBeginSlotIndex];
            if (!mSlotsSeen[delim.mBeginSlotIndex]) {
                if (mOffset + mConfirmedSize < slot.mOffset + slot.mLength) {
                    throw new MarshalException("Output embedded delimiter ["
                        + slot.mOffset + ":" + slot.mLength + "] at "
                        + (mOffset + mConfirmedSize)
                        + ", before its defining data.");
                }
                mSlotsSeen[delim.mBeginSlotIndex] = true;
            }
            long offset = slot.mOffset - mOtdDelim.mSlotsOffset;
            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine("Output begin embedded " + delim.mBeginSlotIndex
                    + ": " + slot.mOffset + ", " + slot.mLength + "; slice "
                    + offset + ", " + slot.mLength + " of <"
                    + Misc.printable(mEmbedDelimBytes) + "> ].");
            }
            addRawData(mEmbedDelimBytes, (int) offset, slot.mLength);
        } else {
            // Delimiter is constant.
            addRawData(delim.mBeginDelim);
        }
    }

    /**
     * Adds a delimiter (end delimiter or array delimiter) to buffer directly.
     *
     * @param delim the delimiter descriptor whose delim bytes is to be added.
     */
    public void addDelim(final Delim delim) {
        if (mLog.isLoggable(Level.FINE)) {
            if (mLog.isLoggable(Level.FINEST)) {
                mLog.finest("Add end delimiter=[" + delim + "]");
            } else {
                mLog.fine("Add end delimiter=[" + delim.dump() + "]");
            }
        }
        if (delim.isEmbedded()) {
            // Get delimiter from slot.
            Delim.Slot slot = mOtdDelim.mSlots[delim.mSlotIndex];
            if (!mSlotsSeen[delim.mSlotIndex]) {
                if (mOffset + mConfirmedSize < slot.mOffset + slot.mLength) {
                    throw new MarshalException("Output embedded delimiter ["
                        + slot.mOffset + ":" + slot.mLength + "] at "
                        + (mOffset + mConfirmedSize)
                        + ", before its defining data");
                }
                mSlotsSeen[delim.mSlotIndex] = true;
            }
            long offset = slot.mOffset - mOtdDelim.mSlotsOffset;
            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine("Output embedded " + delim.mSlotIndex
                    + ": " + slot.mOffset + ", " + slot.mLength + "; slice "
                    + offset + ", " + slot.mLength + " of <"
                    + Misc.printable(mEmbedDelimBytes) + "> ].");
            }
            addRawData(mEmbedDelimBytes, (int) offset, slot.mLength);
        } else {
            // Delimiter is constant.
            addRawData(delim.mBytes);
        }
        mIsEndDelimCurrent = true;
    }

    /**
     * Adds data to buffer directly, with given byte count and default
     * padding. Truncate data if too long, otherwise right-pad with the
     * given or padding bytes. If count is &lt; 1, ignore the count.
     *
     * @param data raw data bytes
     * @param count byte count.
     * @throws MarshalException when padding does not fit into space left
     */
    private void addRawDataWithDefaultPadding(byte[] data, int count)
        throws MarshalException {
        addRawDataWithPadding(data, count, DEFAULT_PAD_BYTES);
    }

    /**
     * Adds data to buffer directly, with given byte count. Truncate data
     * if too long, otherwise right-pad with the given or padding bytes.
     * If count is &lt; 1, ignore the count.
     *
     * @param data raw data bytes
     * @param count byte count.
     * @param padBytes padding bytes
     * @throws MarshalException when padding does not fit into space left
     */
    private void addRawDataWithPadding(byte[] data, int count, byte[] padBytes)
        throws MarshalException {
        if (mLog.isLoggable(Level.FINEST)) {
            mLog.finest("Add raw data=" + Misc.bytes2str(data) + ", count="
                + count + ", padBytes=" + Misc.bytes2str(padBytes) + ".");
        }
        if (count < 1) {
            addRawData(data);
            return;
        }
        if (data == null) {
            data = new byte[0];
        }
        int len = data.length;
        if (len > count) {
            len = count;
        }
        // check if exceeds the output limit
        if (mOutputLimit >= 0 && mOffset + mCursor + len > mOutputLimit) {
            throw new MarshalException("Truncation by parent.");
        }
        makeRoom(count);
        System.arraycopy(data, 0, mBuff, mCursor, len);
        mCursor += len;
        int pi = 0;
        for (int i = len; i < count; i++) {
            mBuff[mCursor++] = padBytes[pi++];
            if (pi >= padBytes.length) { pi = 0; }
        }
        if (pi != 0) {
            throw new MarshalException("Padding sequence truncated.");
        }
        mIsEndDelimCurrent = false;
    }

    /**
     * Adds data to buffer directly, with given byte count, and based on
     * absolute position.
     * Truncate data if too long, otherwise right-pad with
     * the given or padding bytes.
     * If count is &lt; 1, ignore the count.
     *
     * @param absPos absolute position.
     * @param data data bytes to add.
     * @param count byte count.
     * @throws MarshalException when padding does not fit into space left
     */
    private void addRawDataAtAbsPosWithDefaultPadding(long absPos, byte[] data,
        int count) {
        addRawDataAtAbsPosWithPadding(absPos, data, count, DEFAULT_PAD_BYTES);
    }

    /**
     * Adds data to buffer directly, with given byte count, and based on
     * absolute position.
     * Truncate data if too long, otherwise right-pad with
     * the given or padding bytes.
     * If count is &lt; 1, ignore the count.
     *
     * @param absPos absolute position.
     * @param data data bytes to add.
     * @param count byte count.
     * @param padBytes  padding bytes
     * @throws MarshalException when padding does not fit into space left
     */
    private void addRawDataAtAbsPosWithPadding(long absPos, byte[] data,
        int count, byte[] padBytes) {
        if (mLog.isLoggable(Level.FINEST)) {
            mLog.finest("Add raw data at absolute position=" + absPos
                + ", data=" + Misc.bytes2str(data) + ", count=" + count
                + ", padBytes=" + Misc.bytes2str(padBytes) + ".");
        }
        int pos = mCursor;
        mCursor = (int) (absPos - mOffset);
        if (mCursor < 0) {
            throw new MarshalException(
                "Overwriting content that has been flushed out is prohibited.");
        }
        addRawDataWithPadding(data, count, padBytes);
        if (pos > mCursor) {
            mCursor = pos;
        }
    }

    /**
     * Flushes all committed data to output stream.
     * @throws IOException i/o error.
     */
    public void flush()
        throws IOException {
        //!!! Currently flush is not enabled. If flush needs to be enabled,
        //then mParentStart must be adjusted when flushing.
        if (mConfirmedSize > 0) {
            if (mOutStream == null) {
                throw new RuntimeException("No output stream defined.");
            }
            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine("Flush to output steam bytes=" + mConfirmedSize
                    + ", offset=" + mOffset);
            }
            mOutStream.write(mBuff, 0, mConfirmedSize);
            mOffset += mConfirmedSize;
            if (mConfirmedSize < mCursor) {
                // Copy uncommitted data back to buffer start.
                for (int i = 0; mConfirmedSize < mCursor; i++) {
                    mBuff[i] = mBuff[mConfirmedSize++];
                }
            }
            mCursor -= mConfirmedSize;
            mConfirmedSize = 0;
        }
    }

    /**
     * Commits all pending data. Flushes to output stream if over threshold;
     * @see #setFlushThreshold(int). Also updates the mConfirmedSize.
     * If the OTD uses embedded delimiter, it also copies committed data to
     * {@link mEmbedDelimBytes} if in the range used by the delimiter slots.
     * Note that this method is not only invoked in startElement() method
     * (1) to make up delimiters and default values for all skipped delimited
     * nodes, (2) to add array delimiter, it is also invoked in characters()
     * and endElement() methods to add character data, delimiters, etc.
     *
     * @param what what stuff to commit.
     * @throws SAXException wrapped on i/o error during .
     */
    public void commit(String what)
        throws SAXException {
        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder sb = new StringBuilder("Commit after adding ");
            sb.append(what).append(", cursor=").append(mCursor);
            sb.append(" (was ").append(mConfirmedSize).append(") at offset=");
            sb.append(mOffset).append(".").append(LN);
            if (mLog.isLoggable(Level.FINEST)) {
                sb.append(getContextDetails(Level.FINEST));
                mLog.finest(sb.toString());
            } else {
                sb.append(getContextDetails(Level.FINE));
                mLog.fine(sb.toString());
            }
        }
        if (mEmbedDelimBytes != null) {
            /** Has embedded delimiters. We'll copy a slice of committed
             * data from the "mBuff" data buffer to the "mEmbedDelimBytes"
             * slot data buffer: the slice is "size" bytes, offset at "from"
             * in mBuff to offset "into" in mEmbedDelimBytes.
             */
            int size = mCursor;
            long from = 0;
            long into = mOffset - mOtdDelim.mSlotsOffset;
            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine("Copy to embed: from=" + from + ", into="
                    + into + ", size=" + size + ".");
            }
            if (into < mOtdDelim.mSlotsLength && 0 <= into + size) {
                // There is really some data to copy.
                if (into < 0) {
                    // Committed data partially precedes slots.
                    size += into;
                    from -= into;
                    into = 0;
                }
                if (mOtdDelim.mSlotsLength < into + size) {
                    // Committed data partially follows slots.
                    size = (int) (mOtdDelim.mSlotsLength - into);
                }
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.fine("Truly embed: from=" + from
                        + ", into=" + into + ", size=" + size + ".");
                }
                while (size-- > 0) {
                    mEmbedDelimBytes[(int) into++] = mBuff[(int) from++];
                }
            }
        }
        mConfirmedSize = mCursor;
        if (mConfirmedSize >= mFlushThreshold && mFlushThreshold > 0) {
            //!!! Currently flush is not enabled. If flush needs to be enabled,
            //then mParentStart must be adjusted when flushing.
            try {
                flush();
            } catch (IOException ioex) {
                throw new SAXException(ioex);
            }
        }
    }

    /**
     * Rolls back changes to the last commit, but not past the given mark.
     * @param limit a given limit.
     */
    public void rollback(long limit) {
        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder sb = new StringBuilder("Roll back changes");
            sb.append(" with limit=").append(limit).append(". ");
            if (mLog.isLoggable(Level.FINEST)) {
                sb.append(getContextDetails(Level.FINEST));
                mLog.finest(sb.toString());
            } else {
                sb.append(getContextDetails(Level.FINE));
                mLog.fine(sb.toString());
            }
        }
        if (mOffset + mCursor < limit) {
            throw new RuntimeException("Invalid limit " + limit
                + ", past offset/cursor " + mOffset + "/" + mCursor);
        }
        int oldCursorPos = mCursor;
        mCursor = Math.max(mConfirmedSize, (int) (limit - mOffset));
        if (oldCursorPos != mCursor) {
            mIsEndDelimCurrent = false;
        }
    }

    /**
     * Rolls back changes to the last commit state. It changes the mCursor to
     * mConfirmedSize, also updates the mIsEndDelimCurrent flag.
     * This method is invoked in 2 places: (1) startElement() method right
     * before adding and committing array delimiter, and (2) elementElement()
     * method for a delim/array node before adding and committing regular
     * and array delimiter(s).
     *
     * @see #commit().
     */
    public void rollback() {
        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder sb = new StringBuilder();
            if (mConfirmedSize == mCursor) {
                sb.append("No uncommited changes to roll back, ");
                sb.append("cursor=confirmed=").append(mCursor);
            } else {
                sb.append("Roll back uncommited changes, cursor=");
                sb.append(mCursor).append(" (was ").append(mConfirmedSize);
                sb.append("). ");
                if (mLog.isLoggable(Level.FINEST)) {
                    sb.append(getContextDetails(Level.FINEST));
                } else {
                    sb.append(getContextDetails(Level.FINE));
                }
            }
            if (mLog.isLoggable(Level.FINEST)) {
                mLog.finest(sb.toString());
            } else {
                mLog.fine(sb.toString());
            }
        }
        if (mConfirmedSize != mCursor) {
            mIsEndDelimCurrent = false;
            mCursor = mConfirmedSize;
        }
    }

    /**
     * Returns the current absolute position.
     * @return the current absolute position.
     */
    public long getCurrAbsPos() {
        return mOffset + mCursor;
    }

    /**
     * Adds the given number of the given or padding bytes.
     *
     * @param n  the number of bytes
     * @param padBytes  padding bytes, or null for default
     * @throws MarshalException when padding does not fit into space left
     */
    public void pad(int n, byte[] padBytes) {
        if (padBytes == null) {
            padBytes = DEFAULT_PAD_BYTES;
        }
        if (n > 0) {
            makeRoom(n);
            int padIdx = 0;
            while (n-- > 0) {
                mBuff[mCursor++] = padBytes[padIdx++];
                if (padIdx >= padBytes.length) {
                    padIdx = 0;
                }
            }
            if (padIdx != 0) {
                throw new MarshalException("Padding sequence truncated.");
            }
            if (mEmittedSize < mCursor) {
                mEmittedSize = mCursor;
            }
        }
    }

    /**
     * Given a starting position, pads to given length past it.
     *
     * @param from a starting position
     * @param n length
     * @param padBytes  padding bytes, or null for default
     */
    public void pad(int from, int n, byte[] padBytes) {
        pad(from + n - mCursor, padBytes);
    }

    /**
     * Clears all data from the buffer.
     */
    public void clear() {
        mCursor = 0;
        mConfirmedSize = 0;
        mEmittedSize = 0;
        mAllocatedSize = 0;
        mBuff = null;
    }

    /**
     * Fetches the buffer contents as raw data.
     * @return the buffer contents as raw data.
     */
    public byte[] getBufferContentBytes() {
        byte[] out = new byte[mConfirmedSize];
        if (mConfirmedSize > 0) { // ...and hence mBuff != null
            System.arraycopy(mBuff, 0, out, 0, mConfirmedSize);
        }
        if (mLog.isLoggable(Level.FINE)) {
            mLog.fine("Content of " + mConfirmedSize + " bytes="
                + Misc.printable(out));
        }
        clear();
        return out;
    }

    public StringCoder getEncoder() {
        // IN=114051 Open-ESB-Issue-560, check and use mNodes' encoder first
        if (mEncoder == null) {
            mEncoder = mNodes.getEncoder();
        }
        if (mEncoder == null) {
            mEncoder = CoderFactory.getCoder(Nodes.DEFAULT_ENCODING);
        }
        return mEncoder;
    }

    public byte[] getZeroPadding() {
        if (mZeroPadding == null) {
            mZeroPadding = getEncoder().encode("0");
        }
        return mZeroPadding;
    }

    public void setEncoder(StringCoder encoder) {
        mEncoder = encoder;
    }

    /**
     * Receive an object for locating the origin of SAX document events.
     *
     * @param locator an object that can return the location of
     *                any SAX document event
     * @see org.xml.sax.Locator
     */
    public void setDocumentLocator(Locator locator) {
        //no-op
    }

    /**
     * Receive notification of the beginning of a document.
     *
     * <p>The SAX parser will invoke this method only once, before any
     * other event callbacks (except for {@link #setDocumentLocator
     * setDocumentLocator}).</p>
     *
     * @throws org.xml.sax.SAXException any SAX exception, possibly
     *            wrapping another exception
     * @see #endDocument
     */
    public void startDocument()
        throws SAXException {
        if (mLog.isLoggable(Level.FINE)) {
            if (mOutStream != null) {
                mLog.fine("Enter: startDocument and output to a stream.");
            } else {
                mLog.fine("Enter: startDocument and output to a byte array.");
            }
        }
        mContextStack.clear();
        mContext = new Context(mNodes.get(mNodes.getRootNodeIndex()).mName);
        if (mLog.isLoggable(Level.FINER)) {
            mLog.finer("Created a new context={" + mContext.mId + "}.");
        }
        mContext.mNodeIndex = mNodes.getRootNodeIndex();
        mContext.mChildIndex = -1;
        mContext.mRepeatIndex = -1;
        mContext.mNode = null;
        mContext.mParentNode = null;
        mContext.mStartPos = 0;
        mContext.mDelimLevel = null;
        mContext.mPlainDelim = null;
        mContext.mArrayDelim = null;
        mContext.mHasContent = false;
        mContext.mIsOpen = false;

        mCursor = 0;
        mConfirmedSize = 0;
        mEmittedSize = 0;
        makeRoom(INITIAL_SIZE);
        if (mOutStream != null) {
            try {
                mOutStream.begin();
            } catch (IOException e) {
                throw new SAXException(e);
            }
        } else {
            mFlushThreshold = 0;
        }
        initSlotSeen();
        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder sb = new StringBuilder("Exit: startDocument. ");
            if (mLog.isLoggable(Level.FINEST)) {
                sb.append(getContextDetails(Level.FINEST));
                mLog.finest(sb.toString());
            } else {
                sb.append(getContextDetails(Level.FINE));
                mLog.fine(sb.toString());
            }
        }
    }

    /**
     * Receive notification of the end of a document.
     *
     * @throws org.xml.sax.SAXException any SAX exception, possibly
     *            wrapping another exception
     * @see #startDocument
     */
    public void endDocument() throws SAXException {
        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder sb = new StringBuilder();
            if (mOutStream != null) {
                sb.append("Enter: endDocument and output to a stream. ");
            } else {
                sb.append("Enter: endDocument and output to a byte array. ");
            }
            if (mLog.isLoggable(Level.FINEST)) {
                sb.append(getContextDetails(Level.FINEST));
                mLog.finest(sb.toString());
            } else {
                sb.append(getContextDetails(Level.FINE));
                mLog.fine(sb.toString());
            }
        }
        if (mOutStream != null) {
            try {
                flush();
                mOutStream.end();
            } catch (IOException e) {
                throw new SAXException(e);
            }
        }
        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder sb = new StringBuilder();
            if (mOutStream != null) {
                sb.append("Exit: endDocument and output to a stream. ");
            } else {
                sb.append("Exit: endDocument and output to a byte array. ");
            }
            if (mLog.isLoggable(Level.FINEST)) {
                sb.append(getContextDetails(Level.FINEST));
                mLog.finest(sb.toString());
            } else {
                sb.append(getContextDetails(Level.FINE));
                mLog.fine(sb.toString());
            }
        }
    }

    /**
     * Begin the scope of a prefix-URI Namespace mapping.
     *
     * @param prefix the Namespace prefix being declared.
     * An empty string is used for the default element namespace,
     * which has no prefix.
     * @param uri the Namespace URI the prefix is mapped to
     * @throws org.xml.sax.SAXException the client may throw
     *            an exception during processing
     * @see #endPrefixMapping
     * @see #startElement
     */
    public void startPrefixMapping(String prefix, String uri)
        throws SAXException {
        //no-op
    }

    /**
     * End the scope of a prefix-URI mapping.
     *
     * @param prefix the prefix that was being mapped.
     *	This is the empty string when a default mapping scope ends.
     * @throws org.xml.sax.SAXException the client may throw
     *            an exception during processing
     * @see #startPrefixMapping
     * @see #endElement
     */
    public void endPrefixMapping(String prefix)
        throws SAXException {
        //no-op
    }

    /**
     * Receive notification of the beginning of an element.
     *
     * @param uri the Namespace URI, or the empty string if the
     *        element has no Namespace URI or if Namespace
     *        processing is not being performed
     * @param localName the local name (without prefix), or the
     *        empty string if Namespace processing is not being
     *        performed
     * @param qName the qualified name (with prefix), or the
     *        empty string if qualified names are not available
     * @param atts the attributes attached to the element.  If
     *        there are no attributes, it shall be an empty
     *        Attributes object.  The value of this object after
     *        startElement returns is undefined
     * @throws org.xml.sax.SAXException any SAX exception, possibly
     *            wrapping another exception
     * @see #endElement
     * @see org.xml.sax.Attributes
     * @see org.xml.sax.helpers.AttributesImpl
     */
    public void startElement(String uri, String localName, String qName,
        Attributes atts) throws SAXException {
        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder sb = new StringBuilder();
            sb.append("Enter: startElement '").append(localName).append("'");
            if (!localName.equals(qName)) {
                sb.append(", qName='").append(qName).append("'");
            }
            if (mLog.isLoggable(Level.FINER)) {
                if (atts.getLength() > 0) {
                    sb.append(",").append(LN).append(" attributes={");
                }
                for (int i = 0; i < atts.getLength(); i++) {
                    sb.append(" attribute[").append(i).append("]=[");
                    sb.append(atts.getQName(i)).append("=");
                    sb.append(atts.getValue(i)).append("]");
                }
                if (atts.getLength() > 0) {
                    sb.append("}");
                }
            }
            sb.append(". ");
            if (mLog.isLoggable(Level.FINEST)) {
                sb.append(getContextDetails(Level.FINEST));
                mLog.finest(sb.toString());
            } else {
                sb.append(getContextDetails(Level.FINE));
                mLog.fine(sb.toString());
            }
        }
        if (mContext.mIsOpen && mContext.mNode != null
            && mContext.mNode.isTrans()) {
            mTransientLevel++;
            return;
        }
        //Preserve the child index since mContext.mChildIndex
        //will be mutated in match(...)
        int oldChildIndex = mContext.mChildIndex;
        if (mContext.mIsOpen) {
            oldChildIndex = -1;
        }
        // lookup node metadata in current context based on given uri and
        // localName, and update current context if match is found.
        boolean isMatched = match(uri, localName);
        if (!isMatched) {
            throw new SAXException("Unexpected element={" + uri + "}"
                + localName);
        }
        if (mContext.mParentNode != null && mContext.mParentNode.isOrdered()) {
            // need to make up delimiters and default values for all skipped
            // delimited nodes
            Node node;
            boolean toCommit = false;
            for (int i = oldChildIndex + 1; i < mContext.mChildIndex; i++) {
                node = mNodes.get(mContext.mParentNode.mSubnodes[i]);
                // check if node type is 'delimited' or 'array'
                if (node.isDelim() || node.isArray()) {
                    /* if (mContext.mDelimLevel == null) { */
                    // IN=114320 Open-ESB-Issue-705, check on node's delimLevel
                    // first, if null, then check on context's delimLevel
                    DelimLevel delimLevel = node.mDelimLevel;
                    if (delimLevel == null) {
                        delimLevel = mContext.mDelimLevel;
                    }
                    if (delimLevel == null) {
                        if (mLog.isLoggable(Level.SEVERE)) {
                            mLog.severe("No delimiter level defined for node=["
                                + node + "].");
                        }
                        throw new SAXException("No delimiter level available");
                    }
                    /* if (mContext.mPlainDelim == null) { */
                    // IN=114320 Open-ESB-Issue-705, check on node's (preferred
                    // over context's) delimLevel's plainDelim
                    Delim delim = delimLevel.mPlainDelim;
                    if (delim == null) {
                        if (mLog.isLoggable(Level.SEVERE)) {
                            mLog.severe("No plain delimiter defined for node=["
                                + node + "].");
                        }
                        throw new SAXException("No plain delimiter defined.");
                    }
                    /* boolean isFavor = mContext.mPlainDelim != null &&
                        mContext.mPlainDelim.mOptMode.compareTo(Delim.Mode.FAVOR) >= 0;
                     if (isFavor || node.mDefaultValue != null) {
                     IN=114320 Open-ESB-Issue-705 */
                    if (delim.mOptMode.compareTo(Delim.Mode.FAVOR) >= 0
                        || node.mDefaultValue != null) {
                        // node has default value -or- optMode is FAVOR/FORCE
                        if (delim.hasBeginBytes()) {
                            if (node.mOutput1stChar
                                && node.mScavengerBytes != null) {
                                addRawData(node.mScavengerBytes, 0, 1);
                            }
                            addBeginDelim(delim);
                        }
                        if (node.mDefaultValue != null && !node.mIsOptional) {
                            byte[] d = getEncoder().encode(node.mDefaultValue);
                            addRawData(d);
                        }
                        addDelim(delim);
                        toCommit = true;
                    }
                } else if (node.isFixed() && node.mLength > 0) {
                    // i.e. node type is 'fixedLength'
                    if (node.isLeaf() && node.mDefaultValue != null
                        && !node.mIsOptional) {
                        byte[] d = getEncoder().encode(node.mDefaultValue);
                        addRawDataWithDefaultPadding(d, node.mLength);
                        toCommit = true;
                    }
                }
            }
            if (toCommit) {
                commit("scavenger char, required default value, begin/end"
                    + " delimiters for skipped nodes");
            }
        }
        if (mContext.mNode.isTrans()) {
            mTransientLevel = 0;
            return;
        }
        // check if current node is delim/array type
        if (mContext.mNode.isDelim() || mContext.mNode.isArray()) {
            /* Consume one level from the current delimiter list.
             * Use the first available regular delimiter for our output.
             */
            if (mContext.mDelimLevel == null) {
                if (mLog.isLoggable(Level.SEVERE)) {
                    mLog.severe("No delimiter level defined in current context={"
                        + mContext + "}.");
                }
                throw new SAXException("No delimiter level available.");
            }
            if (mContext.mNode.isDelim() && mContext.mPlainDelim == null) {
                if (mLog.isLoggable(Level.SEVERE)) {
                    mLog.severe("No plain delimiter defined in current context={"
                        + mContext + "}.");
                }
                throw new SAXException("No plain delimiter defined for delimited node.");
            }
            if (mContext.mNode.isArray() && mContext.mArrayDelim == null) {
                if (mLog.isLoggable(Level.SEVERE)) {
                    mLog.severe("No array delimiter defined in current context={"
                        + mContext + "}.");
                }
                throw new SAXException("No array delimiter defined for array node.");
            }
            // check if delimited node has begin delimiter
            if (mContext.mNode.isDelim()
                && mContext.mPlainDelim.hasBeginBytes()) {
                if (mContext.mNode.mOutput1stChar
                    && mContext.mNode.mScavengerBytes != null) {
                    addRawData(mContext.mNode.mScavengerBytes, 0, 1);
                }
                addBeginDelim(mContext.mPlainDelim);
            }
        } // end-- if (mContext.mNode.isDelim() || mContext.mNode.isArray())

        //long start = mOffset + mCursor;
        if (!mContext.mNode.mIsRepeatable) {
            // i.e. this is a single child.
            if (mContext.mRepeatIndex > 0) {
                throw new SAXException("Too many elements for {" + uri + "}"
                    + localName);
            }
            if (mContext.mNode.isLeaf()
                && mContext.mNodeIndex == mNodes.getRootNodeIndex()) {
                throw new SAXException("Simple root node is not allowed.");
            }
        } else {
            // Repeated child, iterate over occurrences.
            //NYI: what about "optional" here?
            if (mContext.mNodeIndex == mNodes.getRootNodeIndex()) {
                throw new SAXException("Repeated root node is not allowed.");
            }
            if (mContext.mNode.mMaxOccurs != -1 &&
                mContext.mRepeatIndex > mContext.mNode.mMaxOccurs) {
                throw new SAXException("Too many elements for: {" + uri + "}"
                    + localName);
            }
            if (mContext.mNode.isArray() && mContext.mRepeatIndex > 0) {
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.fine("Separate array node child index="
                        + mContext.mChildIndex + ", repeatIndex="
                        + mContext.mRepeatIndex);
                }
                rollback();
                addDelim(mContext.mArrayDelim);
                commit("after adding array delimieter");
            }
        }
        if (mContext.mNode.isFixedAndLeaf()) {
            // initialize mContext.mVal -- ByteBuilder object.
            if (mContext.mVal == null) {
                mContext.mVal = new ByteBuilder();
            } else {
                mContext.mVal.reset();
            }
        }
        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder sb = new StringBuilder("Exit: startElement '");
            sb.append(localName).append("'. ");
            if (mLog.isLoggable(Level.FINEST)) {
                sb.append(getContextDetails(Level.FINEST));
                mLog.finest(sb.toString());
            } else {
                sb.append(getContextDetails(Level.FINE));
                mLog.fine(sb.toString());
            }
        }
    }

    /**
     * Receive notification of the end of an element. It's the place to take
     * care of default value, begin/end delimiters, and scavenger characters.
     *
     * @param uri the Namespace URI, or the empty string if the
     *        element has no Namespace URI or if Namespace
     *        processing is not being performed
     * @param localName the local name (without prefix), or the
     *        empty string if Namespace processing is not being
     *        performed
     * @param qName the qualified XML name (with prefix), or the
     *        empty string if qualified names are not available
     * @throws org.xml.sax.SAXException any SAX exception, possibly
     *            wrapping another exception
     */
    public void endElement(String uri, String localName, String qName)
        throws SAXException {
        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder sb = new StringBuilder("Enter: endElement '");
            sb.append(localName).append("'");
            if (!localName.equals(qName)) {
                sb.append(", qName='").append(qName).append("'");
            }
            sb.append(". ");
            if (mLog.isLoggable(Level.FINEST)) {
                sb.append(getContextDetails(Level.FINEST));
                mLog.finest(sb.toString());
            } else {
                sb.append(getContextDetails(Level.FINE));
                mLog.fine(sb.toString());
            }
        }
        // IN=114500 Open-ESB-Issue-722
        boolean isContextPopped = false;
        // check to see if last seen XML tag is an end tag.
        if (!mContext.mIsOpen) {
            Node node;
            final int kidsCount = mContext.mParentNode.mSubnodes.length;
            final boolean isFavor = (mContext.mPlainDelim != null)
                && (mContext.mPlainDelim.mOptMode.compareTo(Delim.Mode.FAVOR) >= 0);
            boolean toCommit = false;
            for (int i = mContext.mChildIndex + 1; i < kidsCount - 1; i++) {
                //Takes care of the optional nodes at the end.
                node = mNodes.get(mContext.mParentNode.mSubnodes[i]);
                if (node.isDelim() || node.isArray()) {
                    if (mContext.mDelimLevel == null) {
                        if (mLog.isLoggable(Level.SEVERE)) {
                            mLog.severe("No delimiter level defined in current context={"
                                + mContext + "}" + LN
                                + " for current node=[" + node + "].");
                        }
                        throw new SAXException("No delimiter level available");
                    }
                    if (mContext.mPlainDelim == null) {
                        if (mLog.isLoggable(Level.SEVERE)) {
                            mLog.severe("No plain delimiter defined in current context={"
                                + mContext + "}" + LN
                                + " for current node=[" + node + "].");
                        }
                        throw new SAXException("No plain delimiter defined.");
                    }
                    if (isFavor || node.mDefaultValue != null) {
                        if (mContext.mPlainDelim.hasBeginBytes()) {
                            if (node.mOutput1stChar
                                && node.mScavengerBytes != null) {
                                addRawData(node.mScavengerBytes, 0, 1);
                            }
                            addBeginDelim(mContext.mPlainDelim);
                        }
                        if (node.mDefaultValue != null && !node.mIsOptional) {
                            byte[] d = getEncoder().encode(node.mDefaultValue);
                            addRawData(d);
                        }
                        addDelim(mContext.mPlainDelim);
                        toCommit = true;
                    }
                } else if (node.isFixedAndLeaf()
                    && node.mDefaultValue != null && !node.mIsOptional) {
                    if (node.mLength > 0) {
                        byte[] d = getEncoder().encode(node.mDefaultValue);
                        addRawDataWithDefaultPadding(d, node.mLength);
                    } else {
                        byte[] d = getEncoder().encode(node.mDefaultValue);
                        addRawData(d);
                    }
                    toCommit = true;
                }
            }
            if (toCommit) {
                commit("after adding scavenger char, begin and/or end"
                    + " delimiters and default value");
                toCommit = false;
            }
            // IN=114075 Open-ESB-Issue-535
            /* if (mContext.mChildIndex < kidsCount - 1) { */
            if (mContext.mChildIndex < kidsCount - 1
                && !mContext.mParentNode.isAlter()) {
                node = mNodes.get(mContext.mParentNode.mSubnodes[kidsCount - 1]);
                if (node.isDelim() || node.isArray()) {
                    //  rollback();
                    if (mContext.mDelimLevel == null) {
                        if (mLog.isLoggable(Level.SEVERE)) {
                            mLog.severe("No delimiter level defined in current context={"
                                + mContext + "}" + LN + " for current node=["
                                + node + "].");
                        }
                        throw new SAXException("No delimiter level available.");
                    }
                    if (mContext.mPlainDelim == null) {
                        if (mLog.isLoggable(Level.SEVERE)) {
                            mLog.severe("No plain delimiter defined in current context={"
                                + mContext + "}" + LN + " for current node=["
                                + node + "].");
                        }
                        throw new SAXException("No plain delimiter defined.");
                    }
                    if (mContext.mPlainDelim.hasBeginBytes()) {
                        if (node.mOutput1stChar
                            && node.mScavengerBytes != null) {
                            addRawData(node.mScavengerBytes, 0, 1);
                        }
                        addBeginDelim(mContext.mPlainDelim);
                    }
                    if (node.mDefaultValue != null && !node.mIsOptional) {
                        byte[] d = getEncoder().encode(node.mDefaultValue);
                        addRawData(d);
                        commit("after adding default value"
                            + " (and scavenger char and delimiter)");
                    }
                    addDelim(mContext.mPlainDelim);
                    if (mContext.mPlainDelim.mTermMode.compareTo(
                        Delim.Mode.FAVOR) >= 0) {
                        // only commit if delimiter termMode is FAVOR/FORCE
                        commit("after adding delimiter"
                            + " (and scavenger char and delimiter)");
                    }
                } else if (node.isFixedAndLeaf()
                    && node.mDefaultValue != null && !node.mIsOptional) {
                    byte[] d = getEncoder().encode(node.mDefaultValue);
                    if (node.mLength > 0) {
                        addRawDataWithDefaultPadding(d, node.mLength);
                    } else {
                        addRawData(d);
                    }
                    commit("default value for required fixed-length leaf node");
                }
            }
            mContext = mContextStack.pop();
            // IN=114500 Open-ESB-Issue-722
            isContextPopped = true;
            if (mLog.isLoggable(Level.FINER)) {
                mLog.finer("Pop context from stack, popped context={"
                    + mContext.mId + "}.");
            }
        } else {
            // i.e. mContext.mIsOpen == true
            if (mContext.mNode.isTrans()) {
                if (mTransientLevel > 0) {
                    mTransientLevel--;
                    return;
                }
                mTransientLevel = -1;
            } else if (mContext.mNode.isFixedAndLeaf()) {
                if (mContext.mNode.mPosition >= 0) {
                    // i.e. Encoded length field, write the length
                    int curPos = mCursor; //  getOffset();
                    forward(mContext.mNode.mPosition);

                    String encodedLength = String.valueOf(mContext.mVal.size());
                    byte[] d = getEncoder().encode(encodedLength);
                    byte[] padBytes = getZeroPadding();
                    addRawDataWithPadding(d, mContext.mNode.mLength, padBytes);

                    if (mContext.mNode.mOffset >= 0) {
                        long parentStartPos = 0;
                        if (!mContextStack.isEmpty()) {
                            parentStartPos = mContextStack.peek().mStartPos;
                        }
                        long absPos = parentStartPos + mContext.mNode.mOffset;
                        d = mContext.mVal.getBytes();
                        addRawDataAtAbsPos(absPos, d, 0, mContext.mVal.size());
                    } else {
                        d = mContext.mVal.getBytes();
                        addRawDataAtAbsPos(curPos, d, 0, mContext.mVal.size());
                    }
                } else {
                    // i.e. mContext.mNode.mPosition < 0
                    if (mContext.mNode.mOffset >= 0) {
                        long parentStartPos = 0;
                        if (!mContextStack.isEmpty()) {
                            parentStartPos = mContextStack.peek().mStartPos;
                        }
                        long absPos = parentStartPos + mContext.mNode.mOffset;
                        byte[] d = mContext.mVal.toByteArray();
                        addRawDataAtAbsPosWithDefaultPadding(absPos, d,
                            mContext.mNode.mLength);
                    } else {
                        byte[] d = mContext.mVal.toByteArray();
                        addRawDataWithDefaultPadding(d, mContext.mNode.mLength);
                    }
                }
                commit("after adding data for fixed-length leaf node");
            }
        } // end-- if (!mContext.mIsOpen) & else if

        // sets the mIsOpen flag to false
        mContext.mIsOpen = false;
        if (mLog.isLoggable(Level.FINER)) {
            mLog.finer("Set context's isOpen=false.");
        }

        if (mContext.mNode.isDelim() || mContext.mNode.isArray()) {
            // i.e. 'delimited' or 'array' node type
            if (!mContext.mHasContent
                && mContext.mChildIndex < mContext.mParentNode.mSubnodes.length - 1) {
                // IN=114383 Open-ESB-Issue-728
                // there is empty node which is non-last sibling in its parent
                commit("empty data for an empty node");
            } else {
                rollback();
            }
            // check if to output 1st of scavenger character
            if (mContext.mNode.mOutput1stChar
                && mContext.mNode.mScavengerBytes != null
                && mIsEndDelimCurrent) {
                addRawData(mContext.mNode.mScavengerBytes, 0, 1);
            }
            Delim delim = mContext.mPlainDelim;
            // LT: if no mPlainDelim, then use mArrayDelim here. Is this right?
            if (delim == null && mContext.mNode.isArray()) {
                delim = mContext.mArrayDelim;
            }
            addDelim(delim);
            if (delim.mTermMode.compareTo(Delim.Mode.FAVOR) >= 0) {
                // only do commit if delim's termMode is FAVOR/FORCE
                commit("(scavenger char and) delimiter");
            } else {
                if (mLog.isLoggable(Level.FINEST)) {
                    mLog.finest("Not commit (scavenger char and) delimiter.");
                }
            }
        }
        // IN=114500 Open-ESB-Issue-722
        if (isContextPopped && mContext.mDelimOverridden) {
            if (mLog.isLoggable(Level.FINEST)) {
                mLog.finest("Restore delimiters in the popped context={"
                    + mContext.getId() + "}");
            }
            mContext.mDelimLevel = mContext.mPrevDelimLevel;
            mContext.mPlainDelim = mContext.mPrevPlainDelim;
            mContext.mArrayDelim = mContext.mPrevArrayDelim;
            mContext.mDelimOverridden = false;
        }
        if (mLog.isLoggable(Level.FINE)) {
            StringBuilder sb = new StringBuilder("Exit: endElement '");
            sb.append(localName).append("'. ");
            if (mLog.isLoggable(Level.FINEST)) {
                sb.append(getContextDetails(Level.FINEST));
                mLog.finest(sb.toString());
            } else {
                sb.append(getContextDetails(Level.FINE));
                mLog.fine(sb.toString());
            }
        }
    }

    /**
     * Receive notification of character data.
     *
     * @param chars the characters from the XML document
     * @param start the start position in the array
     * @param length the number of characters to read from the array
     * @throws org.xml.sax.SAXException any SAX exception, possibly
     *            wrapping another exception
     * @see #ignorableWhitespace
     * @see org.xml.sax.Locator
     */
    public void characters(char[] chars, int start, int length)
        throws SAXException {
        if (mLog.isLoggable(Level.FINE)) {
            mLog.fine("Receive: character data=\""
                + new String(chars, start, length) + "\".");
        }
        if (!mContext.mNode.isLeaf()) {
            //ignore character data from a non-leaf node
            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine("Ignore the received non-leaf node character data.");
            }
            return;
        }
        if (!mContext.mIsOpen) {
            // ignore character data from a node following an end tag
            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine("Ignore the received character data after an end tag.");
            }
            return;
        }
        StringCoder coder = getEncoder();
        String str = new String(chars, start, length);
        byte[] d = null;
        if (mContext.mNode.mElmType == SchemaType.BTC_BASE_64_BINARY) {
            d = DatatypeConverter.parseBase64Binary(str);
        } else if (mContext.mNode.mElmType == SchemaType.BTC_HEX_BINARY) {
            d = DatatypeConverter.parseHexBinary(str);
        } else {
            d = coder.encode(str);
        }
        if (d == null) {
            // nothing to do
            return;
        }
        if (mContext.mNode.isFixed()) {
            mContext.mVal.write(d);
        } else {
            addRawData(d);
            commit("characters data");
        }
        // update mHasContent to true for all previous contexts pushed in stack
        for (int i = mContextStack.size() - 1; i >= 0; i--) {
            mContextStack.get(i).mHasContent = true;
        }
        // also update mHasContent in the current context to true.
        mContext.mHasContent = true;
    }

    /**
     * Receive notification of ignorable whitespace in element content.
     *
     * @param ch the characters from the XML document
     * @param start the start position in the array
     * @param length the number of characters to read from the array
     * @throws org.xml.sax.SAXException any SAX exception, possibly
     *            wrapping another exception
     * @see #characters
     */
    public void ignorableWhitespace(char[] ch, int start, int length)
        throws SAXException {
        //no-op
    }

    /**
     * Receive notification of a processing instruction.
     *
     * @param target the processing instruction target
     * @param data the processing instruction data, or null if
     *        none was supplied.  The data does not include any
     *        whitespace separating it from the target
     * @throws org.xml.sax.SAXException any SAX exception, possibly
     *            wrapping another exception
     */
    public void processingInstruction(String target, String data)
        throws SAXException {
        //no-op
    }

    /**
     * Receive notification of a skipped entity.
     *
     * @param name the name of the skipped entity.  If it is a
     *        parameter entity, the name will begin with '%', and if
     *        it is the external DTD subset, it will be the string
     *        "[dtd]"
     * @throws org.xml.sax.SAXException any SAX exception, possibly
     *            wrapping another exception
     */
    public void skippedEntity(String name)
        throws SAXException {
        //no-op
    }

    public OtdOutputStream getOutputStream() {
        return mOutStream;
    }

    public void setOutputStream(OtdOutputStream out) {
        mOutStream = out;
    }

    /**
     * Match given namespace and localName against the node metadata in current
     * context. This method is only invoked in startElement() method.
     * @see #startElement(java.lang.String, java.lang.String, java.lang.String,
     * org.xml.sax.Attributes). Once matched, it sets the mIsOpen to true.
     * @see Context#mIsOpen
     *
     * @param namespace
     * @param localName
     * @return true if matched successfully, or false otherwise.
     * @throws org.xml.sax.SAXException
     */
    private boolean match(String namespace, String localName)
        throws SAXException {
        if (namespace == null) {
            namespace = "";
        }
        if (mLog.isLoggable(Level.FINER)) {
            mLog.finer("Match node: '" + localName + "', namespace='"
                + namespace + "'.");
        }
        Node node = null;
        if (!mContext.mIsOpen) {
            // check on previous node
            if (mContext.mNode == null) {
                // i.e. must have just started from the root node start tag
                node = mNodes.get(mNodes.getRootNodeIndex());
                //TODO: consider use intern() for efficient string comparison.
                if (localName.equals(node.mName)
                    && namespace.equals(node.mNamespace)) {
                    // matched by name and ns, so populate mContext
                    mContext.mNode = node;
                    mContext.mDelimLevel = node.mDelimLevel;
                    if (mContext.mDelimLevel != null) {
                        if (mContext.mDelimLevel.mPlainIdx >= 0) {
                            mContext.mPlainDelim = mOtdDelim.getDelim(
                                mContext.mDelimLevel.mPlainIdx);
                        }
                        if (mContext.mDelimLevel.mArrayIdx >= 0) {
                            mContext.mArrayDelim = mOtdDelim.getDelim(
                                mContext.mDelimLevel.mArrayIdx);
                        }
                    }
                    mContext.mNodeIndex = mNodes.getRootNodeIndex();
                    mContext.mChildIndex = 0;
                    mContext.mRepeatIndex = 0;
                    mContext.mIsOpen = true;
                    mContext.mHasContent = false;
                    mContext.mStartPos = getCurrAbsPos();
                    if (mLog.isLoggable(Level.FINER)) {
                        mLog.finer("Matched root, context={" + mContext + "}.");
                    }
                    return true;
                }
            } else {
                // just got start tag of a non-root element.
                //TODO: consider use intern() for efficient string comparison.
                if (localName.equals(mContext.mNode.mName)
                    && namespace.equals(mContext.mNode.mNamespace)) {
                    // matched up by name and ns, so populate mContext
                    mContext.mIsOpen = true;
                    mContext.mHasContent = false;
                    mContext.mStartPos = getCurrAbsPos();
                    // increase repetition index
                    mContext.mRepeatIndex++;
                    // IN=114500 Open-ESB-Issue-722
                    // sync up the delimiters in context with node's delims
                    // because in the case of delimiter backup and restore due
                    // to locally defined delimiter override they may be
                    // out of sync.
                    node = mContext.mNode;
                    if (node.mDelimLevel != null) {
                        mContext.mDelimLevel = node.mDelimLevel;
                        mContext.mPlainDelim = null;
                        mContext.mArrayDelim = null;
                        if (node.mDelimLevel.mPlainIdx >= 0) {
                            mContext.mPlainDelim = mOtdDelim.getDelim(
                                node.mDelimLevel.mPlainIdx);
                        }
                        if (node.mDelimLevel.mArrayIdx >= 0) {
                            mContext.mArrayDelim = mOtdDelim.getDelim(
                                node.mDelimLevel.mArrayIdx);
                        }
                    }
                    if (mLog.isLoggable(Level.FINER)) {
                        mLog.finer("Matched '" + localName + "', context={"
                            + mContext + "}.");
                    }
                    return true;
                }
                if (mContext.mParentNode.isAlter()) {
                    return false;
                }
                // loop through each un-examined siblings
                for (int i = mContext.mChildIndex + 1;
                    i < mContext.mParentNode.mSubnodes.length; i++) {
                    node = mNodes.get(mContext.mParentNode.mSubnodes[i]);
                    //TODO: consider use intern() for efficient string comparison.
                    if (localName.equals(node.mName)
                        && namespace.equals(node.mNamespace)) {
                        // matched up by name and ns, so populate mContext
                        mContext.mNodeIndex = mContext.mParentNode.mSubnodes[i];
                        mContext.mChildIndex = i;
                        mContext.mNode = node;
                        mContext.mIsOpen = true;
                        mContext.mHasContent = false;
                        // set repetition index to 0
                        mContext.mRepeatIndex = 0;
                        if (node.mDelimLevel != null) {
                            // i.e. has local delimiters defined for node which
                            // take precedence. Need to backup current delims.
                            if (mLog.isLoggable(Level.FINEST)) {
                                mLog.finest("Backup current delimiters in context before local delimiters override.");
                            }
                            // IN=114500 Open-ESB-Issue-722
                            mContext.mPrevDelimLevel = mContext.mDelimLevel;
                            mContext.mPrevPlainDelim = mContext.mPlainDelim;
                            mContext.mPrevArrayDelim = mContext.mArrayDelim;
                            mContext.mDelimOverridden = true;

                            mContext.mDelimLevel = node.mDelimLevel;
                            mContext.mPlainDelim = null;
                            mContext.mArrayDelim = null;
                            if (node.mDelimLevel.mPlainIdx >= 0) {
                                mContext.mPlainDelim = mOtdDelim.getDelim(
                                    node.mDelimLevel.mPlainIdx);
                            }
                            if (node.mDelimLevel.mArrayIdx >= 0) {
                                mContext.mArrayDelim = mOtdDelim.getDelim(
                                    node.mDelimLevel.mArrayIdx);
                            }
                        }
                        mContext.mStartPos = getCurrAbsPos();
                        if (mLog.isLoggable(Level.FINER)) {
                            mLog.finer("Matched '" + localName + "', context={"
                                + mContext + "}.");
                        }
                        return true;
                    }
                } // end-- for loop
            } // end-- if & else (mContext.mNode == null)
            // if arrive here, means no match found
            return false;
        } // end-- if (!mContext.mIsOpen)

        // if reach heer, that means the start tag is open
        if (mContext.mNode.isLeaf()) {
            return false;
        }
        // now loop through each child node meta to see if any one matches
        for (int i = 0; i < mContext.mNode.mSubnodes.length; i++) {
            node = mNodes.get(mContext.mNode.mSubnodes[i]);
            //@TODO consider use intern() for efficient string comparison.
            if (localName.equals(node.mName)
                && namespace.equals(node.mNamespace)) {
                // this child node matches, so create a new context
                // because it starts an XML tag after a previous start tag.
                Context newCtxt = new Context(node.mName);
                // assign to this child node's local delimiter level
                newCtxt.mDelimLevel = node.mDelimLevel;
                // check if this child node has local delim level defined.
                if (newCtxt.mDelimLevel != null) {
                    // i.e. this child node has local delimiters defined
                    if (newCtxt.mDelimLevel.mPlainIdx >= 0) {
                        newCtxt.mPlainDelim = mOtdDelim.getDelim(
                            newCtxt.mDelimLevel.mPlainIdx);
                    }
                    if (newCtxt.mDelimLevel.mArrayIdx >= 0) {
                        newCtxt.mArrayDelim = mOtdDelim.getDelim(
                            newCtxt.mDelimLevel.mArrayIdx);
                    }
                } else {
                    if (mContext.mNode.isDelim() || mContext.mNode.isArray()) {
                        // i.e. parent node (mContext.mNode) is delim/array type
                        // then assign to parent node's next delimiter level
                        newCtxt.mDelimLevel = mContext.mDelimLevel.mNextDelimLevel;
                        if (newCtxt.mDelimLevel != null
                            && newCtxt.mDelimLevel.mPlainIdx >= 0) {
                            newCtxt.mPlainDelim = mOtdDelim.getDelim(
                                newCtxt.mDelimLevel.mPlainIdx);
                        }
                        if (newCtxt.mDelimLevel != null
                            && newCtxt.mDelimLevel.mArrayIdx >= 0) {
                            newCtxt.mArrayDelim = mOtdDelim.getDelim(
                                newCtxt.mDelimLevel.mArrayIdx);
                        }
                    } else {
                        // i.e. parent node is group/fixed/transient type
                        // then transfer parent node's delimLevel to this child
                        newCtxt.mDelimLevel = mContext.mDelimLevel;
                        newCtxt.mPlainDelim = mContext.mPlainDelim;
                        newCtxt.mArrayDelim = mContext.mArrayDelim;
                    }
                }
                // update mHasContent to true since it encloses a new
                // child context which should contain data.
                mContext.mHasContent = true;
                // populates more on this newCtxt before becomes mContext
                newCtxt.mNode = node;
                newCtxt.mParentNode = mContext.mNode;
                newCtxt.mStartPos = getCurrAbsPos();
                newCtxt.mNodeIndex = mContext.mNode.mSubnodes[i];
                newCtxt.mChildIndex = i;
                newCtxt.mRepeatIndex = 0;
                newCtxt.mIsOpen = true;
                newCtxt.mHasContent = false;
                if (mLog.isLoggable(Level.FINER)) {
                    mLog.finer("Matched '" + localName + "'. Push context={"
                        + mContext.mId + "} to stack and created"
                        + LN + " new context={" + newCtxt + "}.");
                }
                mContextStack.push(mContext);
                mContext = newCtxt;
                return true;
            }
        }
        // so far, nothing matched. bad.
        return false;
    }

    /**
     * Tests the output buffer logic with a simple case.
     * @param args arguments.
     */
    public static void main(final String[] args) {
        try {
            System.out.println("--BEGIN--");
            String dirname = System.getProperty("ENCODER_SHAREDLIBRARY_DIR");
            if (dirname == null || dirname.length() == 0) {
                dirname = "C:/OpenSourceProjects/open-jbi-components/ojc-core/encodersl";
            }
            File encoderBase = new File(dirname);
            File xsdFile = new File(encoderBase, "xsdextension-custom/src/customencoder-sample.xsd");
            QName rootElementName =
                new QName("http://xml.netbeans.org/schema/ud1sample", "root");
            Nodes nodes = NodesFactory.loadFromXSD(xsdFile, rootElementName);
            nodes.verify();
            BudOutput marshaller = new BudOutput(nodes, nodes.mOtdDelim);
            Transformer transformer =
                TransformerFactory.newInstance().newTransformer();
            StreamSource streamSource =
                new StreamSource(
                new File(encoderBase,
                "xsdextension-custom/src/customencoder-sample-decoded.xml"));
            SAXResult saxResult = new SAXResult(marshaller);
            transformer.transform(streamSource, saxResult);
            byte[] outputBytes = marshaller.getBufferContentBytes();
            System.out.println("Output is: " + Misc.printable(outputBytes));
            System.out.println("Should be: 'a,b|0123456789abcde0123456789'");
            System.out.println("--END--");
        } catch (Exception ex) {
            ex.printStackTrace();
            System.exit(1);
        }
    }

    private String getContextDetails(Level logLevel) {
        StringBuilder sb = new StringBuilder(" Details: buffer=");
        if (Level.FINEST.equals(logLevel)) {
            sb.append(Misc.printable(Misc.byteslice(mBuff, 0, mCursor)));
        } else {
            boolean allowLeadingEclipsis = true;
            int maxShowSize = -1; // using default
            boolean alignedAtFromPos = false;
            sb.append("\"").append(Misc.showFragment(mBuff, 0, mCursor,
                allowLeadingEclipsis, maxShowSize, alignedAtFromPos))
                .append("\"");
        }
        sb.append(LN).append(" cursor=").append(mCursor);
        sb.append(" confirmedSize=").append(mConfirmedSize);
        sb.append(" isEndDelimCurrent=").append(mIsEndDelimCurrent);
        if (mEmbedDelimBytes != null) {
            sb.append(" embedDelim=[");
            sb.append(Misc.printable(mEmbedDelimBytes)).append("]");
        }
        if (Level.FINEST.equals(logLevel)) {
            sb.append(LN).append(" context={").append(mContext).append("}");
            if (mContextStack != null && mContextStack.size() > 0) {
                sb.append(LN).append(" contextStack:");
                for (int i = 0; i < mContextStack.size(); i++) {
                    sb.append(" [").append(i).append("]=");
                    sb.append(mContextStack.get(i).mId);
                }
            }
        }
        return sb.toString();
    }
}
