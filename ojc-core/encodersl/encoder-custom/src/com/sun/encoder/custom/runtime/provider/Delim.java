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
 * @(#)Delim.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.custom.runtime.provider;

import com.sun.encoder.runtime.provider.Misc;
import java.nio.ByteBuffer;

/**
 * Class to represent a single delimiter for input data parsing.
 * A delimiter is a byte sequence that will be matched in input data
 * to find the end of a field.  Each delimiter has a precedence to
 * resolve conflicts where one delimiter is a prefix of another one.
 * Instances of this class are immutable.
 *
 * @author Michael Libourel
 * @author Jun Xu
*/
public final class Delim implements com.sun.encoder.custom.runtime.Delim {
    /**
     * The delimiter type, a constant from Delim.Type.
     */
    public final Delim.Type mType;

    /**
     * The use of the delimiter as a terminater; see Delim.Mode.
     */
    public final Delim.Mode mTermMode;

    /**
     * The use of the delimiter for absent fields; see Delim.Mode.
     */
    public final Delim.Mode mOptMode;

    /**
     * For delimiters derived from the input message (a.k.a. embedded
     * delimiters) this contains an index into an OTD instance specific
     * run-time set of delimiter strings found in the input.  For constant
     * delimiters, it is -1.
     */
    public short mSlotIndex;

    /**
     * The bytes that make up the delimiter.  Minimum length is 1.
     * For embedded delimiters, this will be null, and the sequence is taken
     * from the input slot given by mSlotIndex instead.
     */
    public byte[] mBytes;

    /**
     * The delimiter precedence. In case of conflict (i.e. input will
     * match two or more delimiters), the one with the highest precedence
     * wins.  Precedence being equal, the innermost (last addition) wins.
     */
    public final byte mPrecedence;

    /**
     * If multiple consecutive delimiters should be skipped.  For example,
     * assuming data is "   AAA ", the delimiter is ' ' (Space character) and
     * the current position is at 0, if this flag is true, the match will not
     * stop at position 0, but for begin delimiter it will stop at position 3
     * (all consecutive begin delimiters will be skipped) and for end delimiter
     * it will stop at position 6 (all consecutive leading end delimiters will
     * be skipped and the data will be parsed and delimiter scan stops at the
     * first delimiter after the data).
     *
     *      |0123456|
     *      |   AAA |
     */
    public final boolean mIsSkipLeading;

    /**
     * If multiple end delimiters should be collapsed into one.  For example,
     * assuming data is "AAA   XYZ    ", the delimiter is ' ' (Space) and the
     * current position is at 0, if this flag is on, the delimiter scan will
     * not stop at position 3, but at position 5. Next scan will stop at
     * position 12 instead of 9.
     *
     *      |0123456789012|
     *      |AAA   XYZ    |
     */
    public final boolean mIsCollapse;

    /**
     * The slot index of the begin delimiter if it is embedded.
     */
    public short mBeginSlotIndex;

    /**
     * The bytes of begin delimiter.
     */
    public byte[] mBeginDelim;

    /**
     * Flag indicates if the begin delimiter must be anchored.
     */
    public boolean mIsBeginDelimAnchored;

    /**
     * Flag indicates if the end delimiter must be anchored.
     */
    public boolean mEndDelimAnchored;

    /**
     * An embedded delimiter slot describes a delimiter that is taken from
     * the input message itself, at a fixed absolute offset, preceding
     * any use of the delimiter in the message.
     * Instances of this class are immutable.
     */
    public static class Slot {
        public final long mOffset;
        public final int mLength;

        /**
         * Creates from offset and length.
         *
         * @param offset  the absolute offset, in bytes
         * @param length  the length, a positive number
         */
        public Slot(final long offset, final int length) {
            if (offset < 0) {
                throw new IllegalArgumentException("negative offset: "
                    + offset);
            }
            if (length <= 0) {
                throw new IllegalArgumentException("non-positive length: "
                    + length);
            }
            mOffset = offset;
            mLength = length;
        }

        @Override
        public String toString() {
            StringBuffer buff = new StringBuffer("Slot@")
                .append(Integer.toHexString(hashCode()));
            buff.append(" offset=").append(mOffset);
            buff.append(" length=").append(mLength);
            return buff.toString();
        }
    }

    /**
     * Contructs from delimiter data and precedence level.
     *
     * @param type  the delimiter type, from Delim.Type
     * @param precedence  non-negative precedence value
     * @param termMode  terminator use, from Delim.Mode
     * @param optMode  use en lieu of absent field, from Delim.Mode
     * @param slotIndex  the OtdDelim.mSlots[] index, for embedded delimiters, or -1
     * @param data  the non-empty delimiter, for constant delimiters, or null
     * @param isSkipLeading if multiple consecutive begin delimiters should be
     *                    skipped
     * @param isCollapse if multiple consecutive end delimiters should be collapsed
     * @param beginSlotIndex the OtdDelim.mSlots[] index, for embedded begin
     *                  delimiters, or -1
     * @param beginDelim the non-empty begin delimiter, for constant delimiters,
     *              or null
     * @param beginDelimAnchored if begin delimiter is anchored
     * @param endDelimAnchored if end delimiter is anchored
     */
    public Delim(Delim.Type type, byte precedence, Delim.Mode termMode,
        Delim.Mode optMode, short slotIndex, byte[] data, boolean isSkipLeading,
        boolean isCollapse, short beginSlotIndex, byte[] beginDelim,
        boolean beginDelimAnchored, boolean endDelimAnchored) {

        if (slotIndex < 0 && beginSlotIndex < 0 && data == null && beginDelim == null) {
            throw new NullPointerException("No delimiter data provided.");
        }
        if (data != null && data.length == 0) {
            throw new IllegalArgumentException("End delimiter must have at least 1 byte.");
        }
        if (beginDelim != null && beginDelim.length == 0) {
            throw new IllegalArgumentException("begin delimiter must have at least 1 byte.");
        }
        if (precedence < 0) {
            throw new IllegalArgumentException("Negative precedence is not allowed.");
        }
        mType = type;
        mTermMode = termMode;
        mOptMode = optMode;
        mSlotIndex = slotIndex;
        mBytes = data;
        mPrecedence = precedence;
        mIsSkipLeading = isSkipLeading;
        mIsCollapse = isCollapse;
        mBeginSlotIndex = beginSlotIndex;
        mBeginDelim = beginDelim;
        mIsBeginDelimAnchored = beginDelimAnchored;
        mEndDelimAnchored = endDelimAnchored;
    }

    @Override
    public String toString() {
        StringBuffer buf = new StringBuffer("Delim@")
            .append(Integer.toHexString(hashCode()));
        buf.append(" bytes=").append(Misc.printable(mBytes));
        if (mType != null) {
            buf.append(" type=").append(mType);
        }
        if (mTermMode != null) {
            buf.append(" termMode=").append(mTermMode);
        }
        if (mOptMode != null) {
            buf.append(" optMode=").append(mOptMode);
        }
        if (mSlotIndex >= 0) {
            buf.append(" slotIndex=").append(mSlotIndex);
        }
        buf.append(" precedence=").append(mPrecedence);
        buf.append(" isSkipLeading=").append(mIsSkipLeading);
        buf.append(" isCollapse=").append(mIsCollapse);
        if (mBeginSlotIndex >= 0) {
            buf.append(" beginSlotIndex=").append(mBeginSlotIndex);
        }
        if (mBeginDelim != null && mBeginDelim.length > 0) {
            buf.append(" beginDelim=").append(Misc.printable(mBeginDelim));
            buf.append(" beginDelimAnchored=").append(mIsBeginDelimAnchored);
        }
        buf.append(" endDelimAnchored=").append(mEndDelimAnchored);
        return buf.toString();
    }

    /**
     * Dump as shorter description of this delimiter.
     * @return shorter description of this delimiter.
     */
    public String dump() {
        StringBuffer buf = new StringBuffer("bytes=");
        if (isEmbedded()) {
            buf.append("embed#").append(mSlotIndex);
        } else {
            buf.append(Misc.printable(mBytes));
        }
        buf.append(" type=").append(mType);
        buf.append(" prec=").append(mPrecedence);
        buf.append(" termMode=").append(termMode());
        buf.append(" optMode=").append(optMode());
        if (mBeginDelim != null && mBeginDelim.length > 0) {
            buf.append(" begDelim=").append(Misc.printable(mBeginDelim));
        }
        return buf.toString();
    }

    /**
     * Constructs from delimiter type, data and precedence level.
     * This defaults to regular separator delimiters.
     *
     * @param type  the delimiter type, from Delim.Type.
     * @param precedence  non-negative precedence value
     * @param data  the non-empty delimiter, embedded as ISO-8859-1 string
     */
    public Delim(final Delim.Type type, final byte precedence,
        final byte[] data) {
        this(type, precedence, Mode.NEVER, Mode.NEVER, (short) -1, data,
                false, false, (short) -1, null, true, true);
    }

    /**
     * Constructs from delimiter data and precedence level.
     * This defaults to regular separator delimiters.
     *
     * @param precedence  non-negative precedence value
     * @param data  the non-empty delimiter, embedded as ISO-8859-1 string
     */
    public Delim(final byte precedence, final byte[] data) {
        this(Type.NORMAL, precedence, data);
    }

    /**
     * Constructs from delimiter data and precedence level.
     * This defaults to regular separator delimiters.
     *
     * @param precedence  non-negative precedence value
     * @param data  the non-empty delimiter, embedded as ISO-8859-1 string
     */
    public Delim(final byte precedence, final String data) {
        this(precedence, Misc.str2bytes(data));
    }

    /**
     * Returns this Delim's type.
     * @return this Delim's type.
     */
    public Delim.Type type() {
        return mType;
    }

    /**
     * Returns this Delim's terminator mode.
     * @return this Delim's terminator mode.
     */
    public Delim.Mode termMode() {
        return mTermMode;
    }

    /**
     * Returns this Delim's optional mode.
     * @return this Delim's optional mode.
     */
    public Delim.Mode optMode() {
        return mOptMode;
    }

    /**
     * Tests whether delimiter is embedded.
     * Convenience functions for testing mSlotIndex.
     *
     * @return true if embedded, false if constant
     */
    public boolean isEmbedded () {
        return mSlotIndex >= 0;
    }

    /**
     * Tests whether leading delimiters should be skipped.
     *
     * @return <code>true</code> if should skip,
     *          otherwise <code>false</code>
     */
    public boolean isSkipLeading() {
        return mIsSkipLeading;
    }

    /**
     * Tests whether should collapse multiple consecutive
     * delimiters into one.
     *
     * @return <code>true</code> if should isCollapse,
     *          otherwise <code>false</code>
     */
    public boolean isCollapse() {
        return mIsCollapse;
    }

    /**
     * Tests if the delimiter has beginning bytes.
     *
     * @return <code>true</code> if has beginning bytes,
     *          otherwise <code>false</code>
     */
    public boolean hasBeginBytes() {
        return mBeginDelim != null || mBeginSlotIndex >= 0;
    }

    /**
     * Tests if the delimiter has ending bytes.
     *
     * @return <code>true</code> if has ending bytes,
     *          otherwise <code>false</code>
     */
    public boolean hasEndBytes() {
        return mBytes != null || mSlotIndex >= 0;
    }

    /**
     * Tests whether the begin delimiter is embedded.
     *
     * @return true if embedded, false if constant
     */
    public boolean isBeginDelimEmbedded() {
        return mBeginSlotIndex >= 0;
    }

    /**
     * Tests whether the Delim's terminator mode is <code>FORCE</code>.
     * @return true if the Delim's terminator mode is <code>FORCE</code>.
     */
    public boolean isForceTerm() {
        return mTermMode == com.sun.encoder.custom.runtime.Delim.Mode.FORCE;
    }

    /**
     * Tests whether the Delim's type is <code>ESCAPE_NEXT</code>.
     * @return true if the Delim's type is <code>ESCAPE_NEXT</code>.
     */
    public boolean isEscapeNext() {
        return mType == com.sun.encoder.custom.runtime.Delim.Type.ESCAPE_NEXT;
    }
}
