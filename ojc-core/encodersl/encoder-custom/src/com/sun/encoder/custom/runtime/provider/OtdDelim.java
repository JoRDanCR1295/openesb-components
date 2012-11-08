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
 * @(#)OtdDelim.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.custom.runtime.provider;

import java.util.Hashtable;
import java.util.Map;

import com.sun.encoder.custom.runtime.provider.Delim.Slot;
import com.sun.encoder.runtime.provider.Misc;
import java.util.Set;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * An instance of the OtdDelim class contains the complete delimiter information
 * for a single BUD OTD.  This includes a numbered set of all delimiters in the
 * OTD nodes, including the closure of its (non-embedded) template references.
 * It also includes a synchronized cache of all delimiter contexts that do not
 * involve embedded delimiters, i.e. all contexts that are not input message
 * specific.
 *
 * Note that OtdDelim makes no effort to eliminate duplicate delimiter entries
 * or otherwise optimize the delimiter set, since this would impose run-time
 * overhead.  The intelligence for this is assumed instead in the BUD code
 * generator, where it represents config-time overhead.
 *
 * NYI: Perhaps we should split this into an interface and a provider class;
 *  the same goes for most other classes here.
 *
 * @author Michael Libourel
 * @author Jun Xu
 */
public final class OtdDelim {
    /**
     * The assumed precedence level for fixed-length nodes.
     * NYI: We'd like to expand this to allow fixed fields any precedence,
     * but that will require mapping given precedence levels to a dense
     * sequienmce, and using that to index a transition table much like
     * DelimStack.mPushed[].
     */
    public static final byte FIXED_FIELD_PRECEDENCE = 10;

    /**
     * Flags used in <code>DelimStack.mDelimIndexRefs</code> to indicate
     * whether the entry is for begin delimiter or (regular) end delimiter.
     */
    public static final int END_DELIM_FLAG = 0;
    public static final int BEG_DELIM_FLAG = 1;

    /**
     * Field indices used to access the least significant dimension in
     * <code>DelimStack.mDelimIndexRefs</code>.
     */
    public static final int DELIM_FLAG_IDX = 0;
    public static final int DELIM_REF_IDX = 1;

    /**
     * The list of delimiters that exist in this context.
     */
    public final Delim[] mDelims;

    /**
     * The list of mDelims[] indices to embedded delimiters, preferably
     * in ascending order of input offset.  May be null if there are none.
     */
    public final Slot[] mSlots;

    // Offset to first slot, or -1 if no slots.
    public final long mSlotsOffset;

    // Length from first slot to end of last, or -1 if no slots.
    public final int mSlotsLength;

    private final int ONE_BYTE_MASK = 0xFF;
    private final int FULL_BYTE = 0x100;

    /**
     * The delimiter context cache for constant cases, i.e. not including
     * any delimiters defined in the input message itself. (i.e. no embedded)
     */
    private final DelimStackCache mConstStackCache = new DelimStackCache();

    /** Empty DelimStack object. */
    public final DelimStack mEmptyStack;

    /** Logger object. */
    Logger mLog = Logger.getLogger(getClass().getName());

    static final String LN = System.getProperty("line.separator");

    /**
     * Creates from the given set of delimiters.
     *
     * @param delims  the list of delimiters, may be null
     * @param slots  the list of embedded delimiter slots, may be null
     */
    public OtdDelim(Delim[] delims, final Slot[] slots) {
        if (delims == null) {
            delims = new Delim[0];
        }
        mDelims = delims;
        mSlots = slots;
        if (slots == null || slots.length == 0) {
            mSlotsOffset = -1;
            mSlotsLength = 0;
        } else {
            // Compute area containing embedded delimiter info.
            long offset = slots[0].mOffset;
            long end = offset + slots[0].mLength;
            for (int i = 1; i < slots.length; i++) {
                offset = Math.min(offset, slots[i].mOffset);
                end = Math.max(end, slots[i].mOffset + slots[i].mLength);
            }
            mSlotsOffset = offset;
            mSlotsLength = (int) (end - offset);
        }

        // Seed the stack cache.
        mEmptyStack = new DelimStack();
        if (delims.length > 0 && delims[0] != null && delims[0].isEscapeNext()
                && delims[0].mBytes != null && delims[0].mBytes.length > 0) {
            // i.e. Have global escape next sequence.
            // Load the empty stack with that
            int i = delims[0].mBytes[0] & ONE_BYTE_MASK;
            mEmptyStack.mRefs[i] = new int[][]{new int[]{END_DELIM_FLAG, 0}};
        }
        mConstStackCache.put(mEmptyStack);
    }

    /**
     * Gets a delimiter from the list.
     *
     * @param idx index to the OtdDelim.mDelims[]
     * @return the Delim.
     */
    public Delim getDelim(final int idx) {
        if (idx < 0 || mDelims.length <= idx) {
            throw new IllegalArgumentException("Delimiter index out of range="
                + idx);
        }
        if (mDelims[idx] == null) {
            throw new RuntimeException("No referenced Delim object at=" + idx);
        }
        return mDelims[idx];
    }

    @Override
    public String toString() {
        StringBuffer sb = new StringBuffer();
        sb.append("OtdDelim@").append(Integer.toHexString(hashCode()));
        if (mDelims != null) {
            for (int i = 0; i < mDelims.length; i++) {
                Delim delim = mDelims[i];
                sb.append(" delimiter#").append(i).append("=[");
                sb.append(delim).append("]").append(LN);
            }
        }
        sb.append(" constStackCache=[").append(mConstStackCache);
        sb.append("]").append(LN);
        if (mSlots != null) {
            for (int i = 0; i < mSlots.length; i++) {
                Slot slot = mSlots[i];
                sb.append("[").append(i).append("] EMBED: off=");
                sb.append(slot.mOffset).append(", len=").append(slot.mLength);
            }
        }
        return sb.toString();
    }

    /**
     * This class represents mutable DelimLevel.
     */
    public final class MutableDelimLevel {

        @Override
        public String toString() {
            StringBuffer sb = new StringBuffer();
            sb.append("MutableDelimLevel@").append(
                Integer.toHexString(hashCode()));
            if (mDelimIndexRefs != null && mDelimIndexRefs.length > 0) {
                sb.append(" delimIndexRefs={");
                for (int i = 0; i < mDelimIndexRefs.length; i++) {
                    sb.append(mDelimIndexRefs[i]);
                    if (i < mDelimIndexRefs.length - 1) {
                        sb.append(",");
                    }
                }
                sb.append("}");
            }
            if (mPlainIndex >= 0) {
                sb.append(" plainIndex=").append(mPlainIndex);
            }
            if (mArrayIndex >= 0) {
                sb.append(" arrayIndex=").append(mArrayIndex);
            }
            sb.append(LN).append(" next=[");
            sb.append(mNext == null ? "null" : mNext).append("].");

            return sb.toString();
        }

        public MutableDelimLevel mNext;

        public int[] mDelimIndexRefs;

        /** plain output delimiter index, -1 if none. */
        public int mPlainIndex;

        /** array output delimiter index, -1 if none */
        public int mArrayIndex;

        /** Disable no-arg constructor from outside. */
        private MutableDelimLevel() {
        }

        /**
         * Inherits and populates this MutableDelimLevel object
         * from a specified DelimLevel instance.
         * Remember that target (this object) takes precedence over the passed
         * source DelimLevel object. i.e. if target has data, then don't
         * override from source. We only populate target when target has no data
         * and source has data.
         *
         * @param level specified source DelimLevel instance.
         */
        public void inheritFrom(final DelimLevel level) {
            // (1) check on plain delimiter(s)
            if (level.mPlainIdx != -1) {
                // i.e. source has plain delimiter(s)
                if (mPlainIndex != -1) {
                    // i.e. target also has plain delimiter(s)
                    // source delimiter
                    Delim sd = mDelims[level.mPlainIdx];
                    // target delimiter
                    Delim td = mDelims[mPlainIndex];
                    // populate target only when target has no data
                    if (sd.mBeginDelim != null && td.mBeginDelim == null) {
                        td.mBeginDelim = sd.mBeginDelim;
                        td.mIsBeginDelimAnchored = sd.mIsBeginDelimAnchored;
                    } else if (sd.mBeginSlotIndex != -1
                        && td.mBeginSlotIndex == -1) {
                        td.mBeginSlotIndex = sd.mBeginSlotIndex;
                        td.mIsBeginDelimAnchored = sd.mIsBeginDelimAnchored;
                    }
                    if (sd.mBytes != null && td.mBytes == null) {
                        td.mBytes = sd.mBytes;
                        td.mEndDelimAnchored = sd.mEndDelimAnchored;
                    } else if (sd.mSlotIndex != -1 && td.mSlotIndex == -1) {
                        td.mSlotIndex = sd.mSlotIndex;
                        td.mEndDelimAnchored = sd.mEndDelimAnchored;
                    }
                } else {
                    // i.e. target doesn't have plain delimiter(s)
                    // so expand mDelimIndexRefs array to hold the plainIdx
                    // at the end.
                    if (mDelimIndexRefs != null) {
                        int[] refs = new int[mDelimIndexRefs.length + 1];
                        for (int i = mDelimIndexRefs.length - 1; i >= 0; i--) {
                            refs[i] = mDelimIndexRefs[i];
                        }
                        mDelimIndexRefs = refs;
                    } else {
                        mDelimIndexRefs = new int[1];
                    }
                    mDelimIndexRefs[mDelimIndexRefs.length - 1] = level.mPlainIdx;
                    mPlainIndex = level.mPlainIdx;
                }
            }
            // (2) check on repeat/array delimiter(s) in the same way
            if (level.mArrayIdx != -1) {
                // i.e. source has array delimiter(s)
                if (mArrayIndex != -1) {
                    // i.e. target also has array delimiter(s)
                    // source delimiter
                    Delim sd = mDelims[level.mArrayIdx];
                    // target delimiter
                    Delim td = mDelims[mArrayIndex];
                    // populate target only when target has no data
                    if (sd.mBeginDelim != null && td.mBeginDelim == null) {
                        td.mBeginDelim = sd.mBeginDelim;
                        td.mIsBeginDelimAnchored = sd.mIsBeginDelimAnchored;
                    } else if (sd.mBeginSlotIndex != -1
                        && td.mBeginSlotIndex == -1) {
                        td.mBeginSlotIndex = sd.mBeginSlotIndex;
                        td.mIsBeginDelimAnchored = sd.mIsBeginDelimAnchored;
                    }
                    if (sd.mBytes != null && td.mBytes == null) {
                        td.mBytes = sd.mBytes;
                        td.mEndDelimAnchored = sd.mEndDelimAnchored;
                    } else if (sd.mSlotIndex != -1 && td.mSlotIndex == -1) {
                        td.mSlotIndex = sd.mSlotIndex;
                        td.mEndDelimAnchored = sd.mEndDelimAnchored;
                    }
                } else {
                    // i.e. target has no array delimiter(s)
                    // so expand mDelimIndexRefs array to hold the arrayIdx
                    // at the end.
                    if (mDelimIndexRefs != null) {
                        int[] refs = new int[mDelimIndexRefs.length + 1];
                        for (int i = mDelimIndexRefs.length - 1; i >= 0; i--) {
                            refs[i] = mDelimIndexRefs[i];
                        }
                        mDelimIndexRefs = refs;
                    } else {
                        mDelimIndexRefs = new int[1];
                    }
                    mDelimIndexRefs[mDelimIndexRefs.length - 1] = level.mArrayIdx;
                    mArrayIndex = level.mArrayIdx;
                }
            }
        }
    }

    /**
     * An instance of this class represents a single delimiter list level.
     * The levels in a list are chained one way through the mNextDelimLevel field.
     * We refer to a delimiter list by referring to its first DelimLevel.
     * Instances of this class are immutable.
     * This class belongs inside OtdDelim, because it contains indices
     * of the mDelims[] array, so it is only meaningful w.r.t. to an
     * OtdDelim instance.
     */
    public final class DelimLevel {

        /** a level down, maybe null. */
        public final DelimLevel mNextDelimLevel;

        /**
         * A series of delimiters that need to be pushed for this level.
         * The integer values are indices into OtdDelim.mDelims[].
         * @see OtdDelim#mDelims[]
         */
        public final int[] mDelimIndexRefs;

        /**
         * plain output delimiter index in OtdDelim.mDelims, -1 if none.
         */
        public final int mPlainIdx;

        /**
         * array output delimiter index in OtdDelim.mDelims, -1 if none.
         */
        public final int mArrayIdx;

        /**
         * Flag for: does input require terminator?
         */
        public final boolean mIsTerm;

        /**
         * Flag for: is delimiter required for optional?
         */
        public final boolean mIsLieu;

        /**
         * Plain delimiter.
         */
        public final Delim mPlainDelim;

        /**
         * Flag indicating are all delimiters at this level must have
         * begin delimiters each?
         */
        public final boolean mMustBegin;

        /**
         * Flag indicating does any of the contained delimiters at this
         * level have a begin delimiter?
         */
        public final boolean mHasBegin;

        /**
         * Construct an immutable instance from a mutable one.
         *
         * @param mutable the mutable instance
         * @param nextLevel a level down, may be null
         */
        public DelimLevel(MutableDelimLevel mutable, DelimLevel nextLevel) {
            this(mutable.mDelimIndexRefs.clone(), mutable.mPlainIndex,
                mutable.mArrayIndex, nextLevel);
        }

        /**
         * Constructs immutable instance.
         *
         * @param delimIndexRefs  list of delimiters to define at this level
         * @param plainIdx  index of plain output delimiter, -1 if none
         * @param arrayIdx  index of array output delimiter, -1 if none
         * @param nextDelimLevel level down, may be null
         */
        public DelimLevel(int[] delimIndexRefs, int plainIdx,
            int arrayIdx, DelimLevel nextDelimLevel) {
            if (delimIndexRefs == null) {
                throw new NullPointerException("No delimiter references.");
            }
            boolean isTerm = true;
            boolean isLieu = true;
            boolean allDelimHasBeginBytes = true;
            boolean anyDelimHasBeginBytes = false;
            Delim delim = null;
            for (int i = delimIndexRefs.length; i-- > 0;) {
                delim = getDelim(delimIndexRefs[i]);
                if (delim.mType == Delim.Type.NORMAL) {
                    isTerm &= (delim.mTermMode == Delim.Mode.FORCE);
                    isLieu &= (delim.mOptMode == Delim.Mode.FORCE);
                    allDelimHasBeginBytes &= delim.hasBeginBytes();
                    anyDelimHasBeginBytes = (anyDelimHasBeginBytes
                        || delim.hasBeginBytes());
                }
            }
            mDelimIndexRefs = delimIndexRefs;
            mPlainIdx = plainIdx;
            mArrayIdx = arrayIdx;
            mIsTerm = isTerm;
            mIsLieu = isLieu;
            mNextDelimLevel = nextDelimLevel;
            mMustBegin = allDelimHasBeginBytes;
            mHasBegin = anyDelimHasBeginBytes;
            if (mPlainIdx >= 0) {
                mPlainDelim = getDelim(mPlainIdx);
            } else {
                mPlainDelim = null;
            }
        }

        /**
         * Constructs immutable instance.
         * The plain and array delimiters are defaulted from the input delims.
         *
         * @param delimIndexRefs  list of delimiters to define at this level
         * @param nextDelimLevel level down, may be null
         */
        public DelimLevel(final int[] delimIndexRefs,
            final DelimLevel nextDelimLevel) {
            if (delimIndexRefs == null) {
                throw new NullPointerException("No delimiter references.");
            }
            boolean isTerm = true;
            boolean isLieu = true;
            boolean allDelimHasBeginBytes = true;
            boolean anyDelimHasBeginBytes = false;
            Delim delim;
            int plainIdx = -1;
            int arrayIdx = -1;
            // Pick first occurrence in list as default.
            for (int i = delimIndexRefs.length; i-- > 0;) {
                delim = getDelim(delimIndexRefs[i]);
                switch (delim.mType) {
                    case NORMAL:
                        plainIdx = delimIndexRefs[i];
                        isTerm &= (delim.mTermMode == Delim.Mode.FORCE);
                        isLieu &= (delim.mOptMode == Delim.Mode.FORCE);
                        allDelimHasBeginBytes &= delim.hasBeginBytes();
                        anyDelimHasBeginBytes = (anyDelimHasBeginBytes
                            || delim.hasBeginBytes());
                        break;
                    case REPEAT:
                        arrayIdx = delimIndexRefs[i];
                        // added following
                        isTerm &= (delim.mTermMode == Delim.Mode.FORCE);
                        isLieu &= (delim.mOptMode == Delim.Mode.FORCE);
                        allDelimHasBeginBytes &= delim.hasBeginBytes();
                        anyDelimHasBeginBytes = (anyDelimHasBeginBytes
                            || delim.hasBeginBytes());
                        break;
                    default:
                        // no-op
                }
                // Don't trust codegen...
                int idx = 0;
                if (mSlots != null) {
                    idx = mSlots.length;
                }
                if (idx < delim.mSlotIndex) {
                    throw new RuntimeException("Invalid slot index="
                        + delim.mSlotIndex + " in delim #" + delimIndexRefs[i]);
                }
            }
            mDelimIndexRefs = delimIndexRefs;
            mPlainIdx = plainIdx;
            mArrayIdx = arrayIdx;
            mIsTerm = isTerm;
            mIsLieu = isLieu;
            mMustBegin = allDelimHasBeginBytes;
            mHasBegin = anyDelimHasBeginBytes;
            mNextDelimLevel = nextDelimLevel;
            if (mPlainIdx >= 0) {
                mPlainDelim = getDelim(mPlainIdx);
            } else {
                mPlainDelim = null;
            }
        }

        /**
         * Clones this delimiter level and set its next to <code>null</code>.
         */
        @Override
        public DelimLevel clone() {
            return new DelimLevel(mDelimIndexRefs.clone(), mPlainIdx,
                mArrayIdx, null);
        }

        @Override
        public String toString() {
            StringBuffer sb = new StringBuffer("DelimLevel@");
            sb.append(Integer.toHexString(hashCode()));
            if (mDelimIndexRefs != null && mDelimIndexRefs.length > 0) {
                sb.append(" delimIndexRefs={");
                for (int i = 0; i < mDelimIndexRefs.length; i++) {
                    sb.append(mDelimIndexRefs[i]);
                    if (i < mDelimIndexRefs.length - 1) {
                        sb.append(",");
                    }
                }
                sb.append("}");
            }
            if (mPlainDelim != null) {
                sb.append(" plainDelimBytes=")
                    .append(Misc.printable(mPlainDelim.mBytes));
            }
            if (mPlainIdx >= 0) {
                sb.append(" plainIdx=").append(mPlainIdx);
            }
            if (mArrayIdx >= 0) {
                sb.append(" arrayIdx=").append(mArrayIdx);
            }
            sb.append(" isTerm=").append(mIsTerm);
            sb.append(" isLieu=").append(mIsLieu);
            sb.append(" mustBegin=").append(mMustBegin);
            sb.append(" hasBegin=").append(mHasBegin);
            if (mNextDelimLevel != null) {
                sb.append(LN);
                sb.append(" next=[").append(mNextDelimLevel).append("].");
            }
            return sb.toString();
        }

        /**
         * Clones a mutable delimiter level.
         *
         * @return a mutable delimiter level.
         */
        public MutableDelimLevel cloneMutable() {
            MutableDelimLevel mutable = new MutableDelimLevel();
            mutable.mNext = null;
            mutable.mDelimIndexRefs = mDelimIndexRefs.clone();
            mutable.mPlainIndex = mPlainIdx;
            mutable.mArrayIndex = mArrayIdx;
            return mutable;
        }

        /**
         * Checks whether the given delimiter occurs on this level.
         *
         * @param delim  the delimiter to find
         * @return true if found on this level, else false
         */
        public boolean contains(final Delim delim) {
            // NYI: figure out more efficient way of checking whether
            //  delim is local or not...
            for (int i = 0; i < mDelimIndexRefs.length; i++) {
                if (mDelims[mDelimIndexRefs[i]] == delim) {
                    return true;
                }
            }
            return false;
        }

        /**
         * Tests if all non-array delimiters on this level are terminators
         * on input.  If so, an input field cannot be terminated by end-of-data
         * or a parent delimiter.
         *
         * @return true if termination required
         */
        public boolean isTerminal() {
            return mIsTerm;
        }

        /**
         * Tests if all non-array delimiters on this level have an en-lieu
         * level FORCE.  If so, previous sibling cannot terminate the parent's
         * data.
         *
         * @return true if absent field must occur
         */
        public boolean isExplicit() {
            return mIsLieu;
        }

        /**
         * Tests if all delimiters at this delimter level must have begin
         * delimiters each.
         *
         * @return true if all delimiters at this delimter level must have begin
         * delimiters each, or false otherwise.
         */
        public boolean mustBegin() {
            return mMustBegin;
        }

        /**
         *
         * Tests if any of the contained delimiters at this delimiter level
         * have a begin delimiter.
         *
         * @return true if any of the contained delimiters at this delimiter
         * level have a begin delimiter.
         */
        public boolean hasBegin() {
            return mHasBegin;
        }
    }

    /**
     * Builds entire delimiter level list from a matrix.
     * Example: given input {{ 3 }, { 4, 5 }} it will create a list of
     * 2 levels. The 1st level contains a single delimiter, described in
     * detail by OtdDelim.mDelims[3].  The 2nd level contains two
     * delimiters, with mDelims indices 4 and 5.  When parsing a delimited
     * node with this list, the top node needs delimiter 3, its (delimited)
     * children will need delimiter 4 or 5.
     *
     * @param delimIndexRefs  list of list of delimiters, 1st is highest level
     * @return the list
     */
    public DelimLevel build(int[][] delimIndexRefs) {
        if (delimIndexRefs == null) {
            return null;
        }
        DelimLevel delimLevel = null;
        for (int i = delimIndexRefs.length; i-- > 0;) {
            delimLevel = new DelimLevel(delimIndexRefs[i], delimLevel);
        }
        return delimLevel;
    }

    /**
     * Builds an immutable delim level list from a reversed mutable delim
     * level list.
     *
     * @param mutableLevel the reversed mutable list
     * @return the immutable list
     */
    public DelimLevel buildDelimLevel(MutableDelimLevel mutableLevel) {
        DelimLevel delimLevel = null;
        while (mutableLevel != null) {
            delimLevel = new DelimLevel(mutableLevel, delimLevel);
            mutableLevel = mutableLevel.mNext;
        }
        return delimLevel;
    }

    /**
     * This class implements a mapping from delimiter contexts IDs to
     * contexts, to use as a cache.  The string ID for a stack is computed
     * as the sequence of delimiters on the stack, top element last, with
     * each delimiter represented by its index in mDelims[] as a char.
     */
    public final class DelimStackCache {

        private Map<String, DelimStack> mMap
            = new Hashtable<String, DelimStack>();

        /**
         * Clears the delimiter stack cache.
         */
        public void clear() {
            mMap.clear();
        }

        /**
         * Gets the stack with the given ID, if in the cache.
         *
         * @param id  the stack ID
         * @return the stack in the cache, or null if unknown
         */
        public DelimStack get(final String id) {
            if (id == null) {
                throw new NullPointerException("No key given."); //I18N
            }
            return mMap.get(id);
        }

        /**
         * Puts the stack with the given ID into the cache.
         *
         * @param stack  the stack to enter
         */
        public void put(final DelimStack stack) {
            if (stack == null) {
                throw new NullPointerException("No stack.");
            }
            if (mMap.containsKey(stack.mId)) {
                throw new IllegalArgumentException("Duplicate entry.");
            }
            mMap.put(stack.mId, stack);
        }

        @Override
        public String toString() {
            Set<String> keySet = mMap.keySet();
            TreeSet<String> keySet2 = new TreeSet(keySet);
            StringBuffer sb = new StringBuffer("DelimStackCache@");
            sb.append(Integer.toHexString(hashCode()));
            sb.append(" cacheSize=").append(keySet2.size()).append(":");
            int i = 0;
            if (!mMap.isEmpty()) {
                for (String key : keySet2) {
                    DelimStack delimStack = mMap.get(key);
                    sb.append(LN).append(" (").append(++i).append(") ");
                    sb.append(Misc.printable(key));
                    sb.append(" -> (").append(delimStack).append(")");
                }
            }
            return sb.toString();
        }
    }

    /**
     * Interface for DelimStack.match() to communicate back its results.
     */
    public interface MatchReply {

        /**
         * Assuming data is "   AAA   XYZ", the delimiter is ' ' and the
         * current position is at 0, as below:
         *
         *   |012345678901|
         *   |   AAA   XYZ|
         *
         * If isSkipLeading flag is true and isCollapse flag is true, then
         * setMatchPos() will be called with value 8 (the position of the last
         * matched delimiter), setDataStartPos() will be called with value 3
         * and setDataLength() will be called with value 3.
         */

        /**
         * Sets the position where the delimiter match was found at.
         *
         * @param pos  the position, in the buffer passed to match()
         */
        void setMatchPos(int pos);

        /**
         * Gets the position where the delimiter match was found at.
         *
         * @return the position, in the buffer passed to match()
         */
        int getMatchPos();

        /**
         * Sets the start position where the data was found at.
         *
         * @param pos  the position, in the buffer passed to match()
         */
        void setDataStartPos(int pos);

        /**
         * Sets the length of data.  May be zero.
         *
         * @param length the data length
         */
        void setDataLength(int length);

        /**
         * Sets the flag that indicates a begin delimiter is matched.
         *
         * @param isBeginDelimMatched <code>true</code> is a begin delimiter
         * is matched, otherwise <code>false</code>.
         */
        void setBeginDelimMatched(boolean isBeginDelimMatched);
    }

    /**
     * An instance of this class describes the lexer state needed to match
     * delimiters for a specific node context.  Conceptually it is a frozen
     * snapshot of the stack of delimiters pushed down while coming down
     * its chain of ancestor nodes. The stack is not entirely in ancestor
     * order, because delimiters "sink down" to their given precedence level.
     * Delimiters are matched in the order of the stack.
     *
     * Instances of this class are immutable, except for the reference lists
     * in mPushed[] and mHide.
     *
     * NYI: should change constructors to methods, as push/hide can yield
     * same stack snapshot as before, which would make duplicate in cache.
     */
    public final class DelimStack {

        /**
         * References to contexts resulting from pushing a delimiter
         * with the given index.  If an entry is null, that means it has
         * not been used yet, and the context must be computed or retrieved
         * from a cache, and then stored in this table so a second use
         * will find it here.
         */
        public final DelimStack[] mPushed = new DelimStack[mDelims.length];

        /**
         * Reference to a context with all delimiters below a given
         * precedence cut out.  Currently, we only support this for
         * cutting at FIXED_FIELD_PRECEDENCE.
         */
        public DelimStack mHide = null;

        /**
         * The key used to cache this context. An example Id is
         * "\u0001\u0002\u0003" composed of 3 bytes with values 1, 2, and 3.
         */
        public final String mId;

        /**
         * The delimiter stack is stored as an array of sub-stacks indexed by
         * the first byte of the delimiter. Each individual sub-stack is
         * stored as an array of pairs of mDelims[] index and begin/end delim
         * flag, top of stack first.
         * @see #mDelims
         */
        public final int[][][] mRefs;

        /**
         * The embedded delimiter data used for the mDynamStackCache entries.
         * Null means it has not been initialized yet, or there are no
         * embedded delimiters. Passed in from OtdDelimInst.
         */
        private byte[][] mSlotData = null;

        /**
         * Whether this context refers to constant delimiters only.
         */
        public final boolean mOnlyConstDelims;

        /**
         * Creates an empty stack.
         */
        private DelimStack() {
            mId = "";
            mRefs = new int[FULL_BYTE][][];
            mOnlyConstDelims = true;
        }

        /**
         * Creates a new DelimStack by pushing the given delimiter reference.
         * Note that we do not bother to check for and remove a prior
         * occurrence of the given delimiter, since it will be obscured
         * by a second push anyway.
         *
         * @param delimStack original delimiter stack.
         * @param delimIdx the mDelims[] index of the delimiter to push
         * @param slotData - embedded delimiter data.
         */
        private DelimStack(DelimStack delimStack, int delimIdx, byte[][] slotData) {
            if (delimStack == null) {
                throw new NullPointerException("No delimiter stack.");
            }
            if (delimIdx < 0 || mDelims.length <= delimIdx) {
                throw new IllegalArgumentException("Invalid delimiter index="
                    + delimIdx); //I18N
            }
            mSlotData = slotData;
            mId = delimStack.getIdForPush(delimIdx);
            mRefs = delimStack.mRefs.clone();

            Delim delim = mDelims[delimIdx];
            byte[] data;
            if (delim.hasEndBytes()) {
                if (delim.isEmbedded()) {
                    // Embedded delimiter.
                    if (slotData == null) {
                        throw new RuntimeException("No SlotData set."); //I18N
                    }
                    data = slotData[delim.mSlotIndex];
                } else {
                    // Constant delimiter.
                    data = delim.mBytes;
                }
                int delimFirstByteAsInt = (data[0] & ONE_BYTE_MASK);
                int[][] refStack;
                int[][] subStack = mRefs[delimFirstByteAsInt];
                if (subStack == null) {
                    // Easy case: first one with this initial.
                    refStack = new int[][]{new int[]{END_DELIM_FLAG, delimIdx}};
                } else {
                    // Insert the new index into a copy of the sub-stack.
                    refStack = new int[subStack.length + 1][];
                    int j = 0;
                    for (; j < subStack.length; j++) {
                        refStack[j] = subStack[j];
                        if (mDelims[subStack[j][DELIM_REF_IDX]].mPrecedence
                            <= delim.mPrecedence) {
                            break;
                        }
                    }
                    refStack[j] = new int[2];
                    refStack[j][DELIM_REF_IDX] = delimIdx;
                    refStack[j][DELIM_FLAG_IDX] = END_DELIM_FLAG;
                    for (; j < subStack.length; j++) {
                        refStack[j + 1] = subStack[j];
                    }
                }
                mRefs[delimFirstByteAsInt] = refStack;
            } // end if (delim.hasEndBytes())

            if (delim.hasBeginBytes()) {
                if (delim.isBeginDelimEmbedded()) {
                    // Embedded delimiter.
                    if (slotData == null) {
                        throw new RuntimeException("No SlotData set."); //I18N
                    }
                    data = slotData[delim.mBeginSlotIndex];
                } else {
                    // Constant delimiter.
                    data = delim.mBeginDelim;
                }
                int firstByte = (data[0] & ONE_BYTE_MASK);
                int[][] refStack;
                int [][] subStack = mRefs[firstByte];
                if (subStack == null) {
                    // Easy case: first one with this initial.
                    refStack = new int[][]{new int[]{BEG_DELIM_FLAG, delimIdx}};
                } else {
                    // Insert the new index into a copy of the sub-stack.
                    refStack = new int[subStack.length + 1][];
                    int pos = 0;
                    for (; pos < subStack.length; pos++) {
                        refStack[pos] = subStack[pos];
                        if (mDelims[subStack[pos][DELIM_REF_IDX]].mPrecedence
                            <= delim.mPrecedence) {
                            break;
                        }
                    }
                    refStack[pos] = new int[2];
                    refStack[pos][DELIM_REF_IDX] = delimIdx;
                    refStack[pos][DELIM_FLAG_IDX] = BEG_DELIM_FLAG;
                    for (; pos < subStack.length; pos++) {
                        refStack[pos + 1] = subStack[pos];
                    }
                }
                mRefs[firstByte] = refStack;
            }
            mOnlyConstDelims = (delimStack.mOnlyConstDelims
                && (!delim.hasBeginBytes() || !delim.isBeginDelimEmbedded())
                && !delim.isEmbedded());
        }

        /**
         * Creates a new stack, by hiding all delimiters below the given
         * precedence level.  This is how we implement fixed nodes in a
         * delimited context.
         *
         * NYI: We want to allow more than one precedence level for fixed
         * nodes some day.
         *
         * @param delimStack original delimiter stack.
         * @param precedence the precedence level: FIXED_FIELD_PRECEDENCE now
         */
        private DelimStack(DelimStack delimStack, int precedence) {
            if (delimStack == null) {
                throw new NullPointerException("No delimiter stack."); //I18N
            }
            if (precedence != FIXED_FIELD_PRECEDENCE) {
                throw new IllegalArgumentException("Precedence must be="
                    + FIXED_FIELD_PRECEDENCE); //I18N
            }
            mId = delimStack.getIdForHide(precedence);
            mRefs = (int[][][]) delimStack.mRefs.clone();
            boolean change = false;
            boolean onlyConstDelims = true;
            int[][] tmp = new int[mDelims.length][];
            for (int i = 0; i < FULL_BYTE; i++) {
                int[][] sub = mRefs[i];
                if (sub != null) {
                    // We got a sub-stack.
                    int k = 0;
                    for (int j = 0; j < sub.length; j++) {
                        Delim delim = mDelims[sub[j][DELIM_REF_IDX]];
                        if (delim.mPrecedence > precedence) {
                            // Delimiter has precedence, keep it.
                            tmp[k++] = sub[j];
                            onlyConstDelims = onlyConstDelims && !delim.isEmbedded()
                                && (!delim.hasBeginBytes() || !delim.isBeginDelimEmbedded());
                        }
                    }
                    if (k == 0) {
                        // All delimiters in this sub-stack hidden.
                        mRefs[i] = null;
                        change = true;
                    } else if (k < sub.length) {
                        // Sub-stack has shrunk, partially hidden.
                        for (sub = new int[k][]; k-- > 0;) {
                            sub[k] = tmp[k];
                        }
                        mRefs[i] = sub;
                        change = true;
                    }
                }
            }
            mOnlyConstDelims = onlyConstDelims;
            //- if (!change) return stack;
        }

        /**
         * Computes the ID for a stack that will result from pushing delimiter
         * "index" (in mDelims[]) onto the current stack.
         *
         * @param delimIdx index to mDelims with the delimiter to push
         * @return the new stack's ID
         */
        public String getIdForPush(final int delimIdx) {
            if (delimIdx < 0) {
                throw new IllegalArgumentException("Negative delimiter index"); //I18N
            } else if (delimIdx >= 0xFFFE) {
                throw new RuntimeException("Delimiter index bigger than 65534"); //I18N
            }
            String ret = null;
            // check to see if mId contains delimIdx as a single char.
            int pos = mId.indexOf((char) delimIdx);
            if (pos < 0) {
                // not exist, so append it to mId.
                ret = mId + (char) delimIdx;
            } else {
                // already exists, so move it to end of mId.
                ret = mId.substring(0, pos) + mId.substring(pos + 1)
                    + (char) delimIdx;
            }
            if (mLog.isLoggable(Level.FINEST)) {
                mLog.finest("Id for pushing delim of Idx="
                    + delimIdx + " onto id=" + Misc.printable(mId) + " is "
                    + Misc.printable(ret));
            }
            return ret;
        }

        /**
         * Computes the ID for a stack that will result from hiding all
         * delimiters below or at the given precedence level.
         *
         * @param precedence  the precedence cut-off
         * @return new ID to new stack with delimiters of cutoff precedence
         * hidden.
         */
        public String getIdForHide(int precedence) {
            if (precedence < 0) {
                throw new IllegalArgumentException("Negative precedence."); //I18N
            }
            StringBuffer sb = new StringBuffer();
            int len = mId.length();
            char c;
            for (int i = 0; i < len; i++) {
                c = mId.charAt(i);
                if (mDelims[(int) c].mPrecedence > precedence) {
                    // keep it.
                    sb.append(c);
                } else {
                    // hide/discard it.
                }
            }
            String id = sb.toString();
            if (mLog.isLoggable(Level.FINEST)) {
                mLog.finest("Computed id for hiding delims with precedence <="
                    + precedence + " is =" + Misc.printable(id));
            }
            return id;
        }

        /**
         * Returns an identifier for this object.
         * @return an identifier for this object.
         */
        public String getIdnt() {
            StringBuffer sb = new StringBuffer("DelimStack@");
            sb.append(Integer.toHexString(hashCode())).append(" Id=");
            if (mId == null) {
                sb.append("null");
            } else {
                sb.append(Misc.printable(mId));
            }
            return sb.toString();
        }

        @Override
        public String toString() {
            StringBuffer sb = new StringBuffer(getIdnt());
            if (!mOnlyConstDelims) {
                sb.append(" hasEmbeddedDelims=true");
            }
            if (mHide != null) {
                sb.append(" hideDelimStack=(").append(mHide.getIdnt())
                    .append(")");
            }
            sb.append(" pushedDelimStacks=[");
            if (mPushed.length > 0) {
                for (int i = 0; i < mPushed.length; i++) {
                    sb.append(i).append("=");
                    if (mPushed[i] != null) {
                        sb.append("(").append(mPushed[i].getIdnt()).append(")");
                    }
                    sb.append(", ");
                }
            }
            sb.append("]").append(LN).append(" DelimRefs=[");
            int count = 0;
            for (int i = 0; i < FULL_BYTE; i++) {
                int[][] sub = mRefs[i];
                if (sub == null) {
                    continue;
                }
                count++;
                for (int j = 0; j < sub.length; j++) {
                    Delim delim = mDelims[sub[j][DELIM_REF_IDX]];
                    //buf.append(LN).append(" delim=[").append(delim).append("]").append(LN);
                    sb.append(" ref[").append(i).append("]");
                    sb.append("[").append(j).append("](");
                    if (sub[j][DELIM_FLAG_IDX] == END_DELIM_FLAG) {
                        sb.append("END");
                    } else {
                        sb.append("BEG");
                    }
                    sb.append("-").append(delim.type());
                    sb.append("-").append(delim.mPrecedence).append(")=");
                    if (delim.hasBeginBytes()) {
                        if (delim.isBeginDelimEmbedded()) {
                            sb.append("begin embed#").append(delim.mBeginSlotIndex);
                        } else {
                            sb.append(Misc.printable(delim.mBeginDelim));
                        }
                        sb.append(", ");
                    }
                    if (delim.isEmbedded()) {
                       sb.append("end embed#" + delim.mSlotIndex);
                    } else {
                       sb.append(Misc.printable(delim.mBytes));
                    }
                }
            }
            sb.append("].");
            return sb.toString();
        }

        /**
         * Tries to find the first current delimiter in the given input.
         * If any is matched, return the delimiter, and deliver its
         * offset through matchReply.setMatchPos(). If no delimiter was found,
         * deliver the offset where a delimiter could still start (with
         * more input).
         *
         * @param data  the input buffer to look in
         * @param pos  where the input data starts
         * @param end  where the input data ends (first position past end)
         * @param matchReply  the reply for setMatchPos()
         * @return the delimiter if found, else null
         */
        public Delim scan(final byte[] data, int pos, final int end,
            MatchReply matchReply) {
            if (mLog.isLoggable(Level.FINE)) {
                String msg = "Scan data=\"" + Misc.showFragment(data, pos, end, true) + "\", start=" + pos + ", end="+ end;
                if (mLog.isLoggable(Level.FINEST)) {
                    mLog.finest(msg + ", delimStack=[" + this + "].");
                } else {
                    mLog.fine(msg + ".");
                }
            }
            if (pos < 0) {
                throw new IllegalArgumentException("Negative start position.");
            }
            if (data.length < end) {
                throw new IllegalArgumentException("End position past data length.");
            }
            int[][] ref;
            int startPos = pos;
            int dataStart = -1;
            int dataEnd = -1;
            Delim quotDelim = null; // use to keep track if open quote delim is seen
dataLoop:
            for (; pos < end; pos++) {
                // find if any of the data byte is part of delimiter bytes
                int bVal = (data[pos] & ONE_BYTE_MASK);
                ref = mRefs[bVal];
                if (mLog.isLoggable(Level.FINEST)) {
                    mLog.finest("Examine position=" + pos + ", byte=" + bVal + "(" + Misc.printable((char)bVal) + ")");
                }
                if (ref != null) {
                    if (mLog.isLoggable(Level.FINEST)) {
                        mLog.finest("Found " + ref.length + " potential delimiter match(es) in delimRefs.");
                    }
delimLoop:
                    for (int j = 0; j < ref.length; j++) {
                        Delim delim = mDelims[ref[j][DELIM_REF_IDX]];
                        if (mLog.isLoggable(Level.FINEST)) {
                            mLog.finest("Check on delimiter[" + j + "]=" + Misc.printable(delim.mBytes));
                        }
                        boolean isBeginDelim = (ref[j][DELIM_FLAG_IDX] == BEG_DELIM_FLAG);
                        byte[] delimBytes;
                        if (isBeginDelim) {
                            if (delim.isBeginDelimEmbedded()) {
                                delimBytes = mSlotData[delim.mBeginSlotIndex];
                            } else {
                                delimBytes = delim.mBeginDelim;
                            }
                        } else {
                            if (delim.isEmbedded()) {
                                delimBytes = mSlotData[delim.mSlotIndex];
                            } else {
                                delimBytes = delim.mBytes;
                            }
                        }
                        int delimBytLen = delimBytes.length;
                        if (pos + delimBytLen > end) {
                            /** Cannot check, delimiter would exceed length.
                             * Note 1: Not really true, we could check for a
                             * match failure in the prefix that *is* in the
                             * buffer; that would be an optimization.
                             * Note 2: Not really happy with this logic,
                             * could continue if there are shorter delimiters.
                             */
                            break dataLoop;
                        }
                        // Check rest of bytes of DelimBytes if delim is > 1
                        // byte; note that 1st byte already matches.
                        for (int i = 1; i < delimBytLen; i++) {
                            if (delimBytes[i] != data[pos + i]) {
                                continue delimLoop;
                            }
                        }
                        if (delim.mType == Delim.Type.ESCAPE) {
                            // Escape delimiter, skip in entirety to dataLoop
                            pos += delimBytLen - 1;
                            continue dataLoop;
                        }
                        if (delim.mType == Delim.Type.ESCAPE_NEXT) {
                            // Escape next delimiter, skip in entirety and also
                            // skip the next byte to dataLoop
                            pos += delimBytLen;
                            continue dataLoop;
                        }
                        if (quotDelim != null) {
                            if (delim.mType == Delim.Type.QUOT_ESCAPE
                                && quotDelim == delim) {
                                dataEnd = pos;
                                quotDelim = null;
                            }
                            pos += delimBytLen - 1; // adjust pos if multiple byte delimiter
                            continue dataLoop;
                        } else if (delim.mType == Delim.Type.QUOT_ESCAPE) {
                            dataStart = pos + delimBytLen;
                            quotDelim = delim; // record for seen quote escape delimiter
                            pos += delimBytLen - 1;
                            continue dataLoop;
                        }
                        if (pos == startPos && delim.isSkipLeading()) {
skipLeadingLoop:
                            while ((pos += delimBytLen) <= end - delimBytLen) {
                                for (int i = 0; i < delimBytLen; i++) {
                                    if (delimBytes[i] != data[pos + i]) {
                                        dataStart = pos;
                                        if (isBeginDelim) {
                                            break skipLeadingLoop;
                                        }
                                        continue dataLoop;
                                    }
                                }
                            }
                            // pos already over shot, so adjust here
                            pos -= delimBytLen;
                        }
                        if (!isBeginDelim && delim.isCollapse()) {
                            if (dataEnd == -1) {
                                dataEnd = pos;
                            }
findLastDelimLoop:
                            while ((pos += delimBytLen) <= end - delimBytLen) {
                                for (int i = 0; i < delimBytLen; i++) {
                                    if (delimBytes[i] != data[pos + i]) {
                                        break findLastDelimLoop;
                                    }
                                }
                            }
                            // pos already over shot, so adjust here
                            pos -= delimBytLen;
                        }
                        matchReply.setMatchPos(pos);
                        matchReply.setBeginDelimMatched(isBeginDelim);
                        if (dataStart != -1) {
                            matchReply.setDataStartPos(dataStart);
                            if (isBeginDelim) {
                                matchReply.setDataLength(-1);
                            } else {
                                matchReply.setDataLength(
                                    (dataEnd == -1 ? pos : dataEnd) - dataStart);
                            }
                        } else {
                            if (isBeginDelim) {
                                //set the data start position to be
                                //the first byte after the begin delimiter
                                matchReply.setDataStartPos(pos + delimBytLen);
                                matchReply.setDataLength(-1);
                            } else {
                                matchReply.setDataStartPos(startPos);
                                matchReply.setDataLength(
                                    (dataEnd == -1 ? pos : dataEnd) - startPos);
                            }
                        }
                        if (mLog.isLoggable(Level.FINER)) {
                            mLog.finer("After scan found delimiter match" + (matchReply != null ? " at matchPos=" + matchReply.getMatchPos() : "") + ", delim=[" + delim + "].");
                        }
                        return delim;
                    }
                }
            } // end of dataLoop: for (; pos < end; pos++)

            matchReply.setMatchPos(pos);
            if (dataStart != -1) {
                matchReply.setDataStartPos(dataStart);
                matchReply.setDataLength((dataEnd == -1 ? pos : dataEnd) - dataStart);
            } else {
                matchReply.setDataStartPos(startPos);
                matchReply.setDataLength((dataEnd == -1 ? pos : dataEnd) - startPos);
            }
            if (mLog.isLoggable(Level.FINER)) {
                mLog.finer("After scan found no delimiter match" + (matchReply != null ? ", matchPos="+ matchReply.getMatchPos() : "") + ".");
            }
            return null;
        }
    }

    /**
     * This class holds the cached delimiter contexts for an OTD instance.
     * Any given BUD OTD will have a single instance of OtdDelim at the
     * class level to hold the immutable information and the computed contexts
     * that are independnet of any particular input message.  The actual
     * instance of such an OTD class will have its own OtdDelimInst instance
     * at the instance level, to cache input dependent context.  When a new
     * DelimStack is computed, it is cached at the OTD class level (i.e. in
     * OtdDelim) if constant, else in the OtdDelimInst (in mDynamStackCache).
     */
    public final class OtdDelimInst {

        /**
         * The embedded delimiter data used for the mDynamStackCache entries.
         * Null means it has not been initialized yet, or there are no
         * embedded delimiters.
         */
        private byte[][] mSlotData = null;

        /**
         * The instance level cache. It is only instantiated if required
         * (when there is an embedded delimter).
         */
        private DelimStackCache mDynamStackCache = null;

        /**
         * Gets the common OTD descriptor of which this is the instance.
         *
         * @return the constant OtdDelim.
         */
        public OtdDelim getOtdDelim() {
            return OtdDelim.this;
        }

        @Override
        public String toString() {
            StringBuffer sb = new StringBuffer();
            sb.append("OtdDelimInst@").append(Integer.toHexString(hashCode()));
            sb.append(" dynamStackCache=");
            if (mDynamStackCache != null) {
                sb.append(mDynamStackCache);
            } else {
                sb.append("null");
            }
            sb.append(" otdDelim={").append(getOtdDelim()).append("}");
            return sb.toString();
        }

        /**
         * Sets the new embedded delimiter data.  If null, there are no
         * embedded delimiters.  We need to flush the dynamic delim stack
         * cache if the actual data changed.
         *
         * @param data  a list of embedded delimiter sequences
         * @see OtdDelim#mSlots[]
         */
        public void setSlotData(final byte[][] data) {
            if (data != null) {
                if (mSlots == null) {
                    throw new RuntimeException("no embedded delims to set");
                }
                if (data.length != mSlots.length) {
                    throw new IllegalArgumentException("slot count wrong: need "
                        + mSlots.length + ", but got " + data.length);
                }
                if (mSlotData != null) {
                    // Check if old sequences are the same, else flush cache.
                    boolean isSame = true;
                    for (int i = 0; i < data.length; i++) {
                        if (data[i] == null) {
                            throw new NullPointerException("Null slot #" + i);
                        }
                        int len = data[i].length;
                        isSame &= (len == mSlotData[i].length);
                        if (!isSame) {
                            break;
                        }
                        for (int j = 0; j < len; j++) {
                            isSame &= (data[i][j] == mSlotData[i][j]);
                            if (!isSame) {
                                break;
                            }
                        }
                    }
                    if (!isSame) {
                        mDynamStackCache.clear();
                    }
                }
                mSlotData = data;
            }
        }

        /**
         * Gets the next delimiter context, hiding all delimiters at or below
         * the given precedence level.  This is for fixed node support.
         * The stack snapshot is kept cached, and registered with the
         * previous stack itself.
         *
         * @param delimStack the original stack snapshot
         * @param precedence the precedence level of the fixed node
         * @return the new DelimStack snapshot
         */
        public DelimStack hide(final DelimStack delimStack, final int precedence) {
            if (precedence != FIXED_FIELD_PRECEDENCE) {
                throw new IllegalArgumentException("Precedence to hide must be="
                    + FIXED_FIELD_PRECEDENCE + ", not " + precedence); //N18I
            }
            if (mLog.isLoggable(Level.FINER)) {
                StringBuilder sb = new StringBuilder();
                sb.append("Hide delimiters with precedence <=");
                sb.append(precedence);
                if (mLog.isLoggable(Level.FINEST)) {
                    sb.append(", delimStack=[").append(delimStack);
                    sb.append("], OtdDelimInst=[").append(this).append("].");
                    mLog.finest(sb.toString());
                } else {
                    sb.append(".");
                    mLog.finer(sb.toString());
                }
            }
            DelimStack nextStack = delimStack.mHide;
            if (nextStack == null) {
                // Reference not set yet, update it.
                String id = delimStack.getIdForHide(precedence);
                if (mDynamStackCache != null) {
                    nextStack = mDynamStackCache.get(id);
                } else {
                    if (mLog.isLoggable(Level.FINEST)) {
                        mLog.finest("Nothing in dynamStackCache to lookup.");
                    }
                }
                if (nextStack == null) {
                    // Not found in mDynamStackCache.
                    nextStack = mConstStackCache.get(id);
                    if (mLog.isLoggable(Level.FINEST)) {
                        mLog.finest("Lookup constStackCache found stack=["
                            + (nextStack == null ? "null" : nextStack) + "].");
                    }
                }
                if (nextStack == null) {
                    // Not found in both caches, so create it new.
                    nextStack = new DelimStack(delimStack, precedence);
                    if (mLog.isLoggable(Level.FINEST)) {
                        mLog.finest("Created a new stack conext=" + nextStack);
                    }
                    if (nextStack.mOnlyConstDelims) {
                        // Cache at OTD class level.
                        if (mLog.isLoggable(Level.FINEST)) {
                            mLog.finest("Put new delimStack=(" + nextStack.getIdnt()
                                + ")" + LN + " onto constStackCache=["
                                + mConstStackCache + "].");
                        }
                        mConstStackCache.put(nextStack);
                    } else {
                        // Cache at OTD instance level.
                        if (mDynamStackCache == null) {
                            mDynamStackCache = new DelimStackCache();
                            if (mLog.isLoggable(Level.FINEST)) {
                                mLog.finest("Created for dynamStackCache a new DelimStackCache@"
                                    + Integer.toHexString(mDynamStackCache.hashCode()));
                            }
                        }
                        mDynamStackCache.put(nextStack);
                        if (mLog.isLoggable(Level.FINEST)) {
                            mLog.finest("Put the new conext (" + nextStack.getIdnt()
                                + ") onto dynamStackCache=[" + mDynamStackCache + "].");
                        }
                    }
                }
                delimStack.mHide = nextStack;
            }
            if (mLog.isLoggable(Level.FINER)) {
                StringBuilder sb = new StringBuilder();
                sb.append("After delimiter hiding, delimStack=[");
                sb.append(nextStack).append("]");
                if (mLog.isLoggable(Level.FINEST)) {
                    sb.append(" OtdDelimInst=[").append(this).append("]");
                }
                sb.append(".");
                if (mLog.isLoggable(Level.FINEST)) {
                    mLog.finest(sb.toString());
                } else {
                    mLog.finer(sb.toString());
                }
            }
            return nextStack;
        }

        /**
         * Push the delimiter from mDelims with the given index on the given
         * delimiter stack, and get the next delimiter context.
         * @see OtdDelim#mDelims
         * The stack snapshot is kept cached, and the link is registered with
         * the previous stack itself, so a second call for the same transition
         * is fast.
         *
         * @param delimStack the original stack snapshot, may get mutated.
         * @param delimIdx the delimiter, as index in mDelims
         * @return the new snapshot of DelimStack.
         */
        public DelimStack push(DelimStack delimStack, final int delimIdx) {
            if (delimStack == null) {
                throw new NullPointerException("No base delim stack to push.");
            }
            if (mLog.isLoggable(Level.FINEST)) {
                mLog.finest("Push delimIndex=" + delimIdx + " of delimiter=["
                    + mDelims[delimIdx] + "]" + LN + " onto delimStack=("
                    + delimStack.getIdnt() + ")," + LN + " OtdDelimInst=["
                    + this + "].");
            }
            // check delimStack.mPushed first to see if it's already there.
            DelimStack nextStack = delimStack.mPushed[delimIdx];
            if (nextStack != null) {
                // found in delimStack.mPushed, so return the context.
                if (mLog.isLoggable(Level.FINEST)) {
                    mLog.finest("During push found existing stack=[" + nextStack
                        + "] at delimIndex=" + delimIdx + ", so return it.");
                }
                return nextStack;
            }
            // Reference (context) not set yet, so we update it now.
            synchronized (mConstStackCache) {
                /* The constant stacks may be subject to contention,
                 * as they are shared by all instances of a specific
                 * generated UD-OTD, so re-check availability here.
                 */
                nextStack = delimStack.mPushed[delimIdx];
                if (nextStack == null) {
                    // context not found in delimStack.mPushed,
                    // so we first compute id
                    String id = delimStack.getIdForPush(delimIdx);
                    if (mDynamStackCache != null) {
                        // look up mDynamStackCache first based on id
                        nextStack = mDynamStackCache.get(id);
                    } else {
                        if (mLog.isLoggable(Level.FINEST)) {
                            mLog.finest("Nothing in dynamStackCache to lookup.");
                        }
                    }
                    if (nextStack == null) {
                        // look up mConstStackCache secondly based on id
                        nextStack = mConstStackCache.get(id);
                        if (mLog.isLoggable(Level.FINEST)) {
                            mLog.finest("Lookup constStackCache found stack=["
                                + (nextStack == null ? "null" : nextStack) + "].");
                        }
                    }
                    if (nextStack == null) {
                        // Not in either of above caches, so create it new.
                        nextStack = new DelimStack(delimStack,
                            delimIdx, mSlotData);
                        if (mLog.isLoggable(Level.FINEST)) {
                            mLog.finest("Created a new stack conext=" + nextStack);
                        }
                        if (nextStack.mOnlyConstDelims) {
                            // Cache at OTD class level if no embedded delimiter
                            if (mLog.isLoggable(Level.FINEST)) {
                                mLog.finest("Put new delimStack=(" + nextStack.getIdnt()
                                    + ")" + LN + " onto constStackCache=["
                                    + mConstStackCache + "].");
                            }
                            mConstStackCache.put(nextStack);
                        } else {
                            // Cache at OTD instance level since involves embed
                            if (mDynamStackCache == null) {
                                mDynamStackCache = new DelimStackCache();
                                if (mLog.isLoggable(Level.FINEST)) {
                                    mLog.finest("Created for dynamStackCache a new DelimStackCache@"
                                        + Integer.toHexString(mDynamStackCache.hashCode()));
                                }
                            }
                            mDynamStackCache.put(nextStack);
                            if (mLog.isLoggable(Level.FINEST)) {
                                mLog.finest("Put the new conext (" + nextStack.getIdnt()
                                    + ") onto dynamStackCache=[" + mDynamStackCache + "].");
                            }
                        }
                    }
                    // update the pushed stack to indicate it's retrieved
                    delimStack.mPushed[delimIdx] = nextStack;
                }
            }
            if (mLog.isLoggable(Level.FINEST)) {
                mLog.finest("After push, otdDelimInst=[" + this + "].");
            }
            return nextStack;
        }

        /**
         * Push the given delimiter level on the given delimiter stack,
         * and get the next delimiter context (delimiter stack).
         *
         * NYI: maybe we should keep track of level transitions, not just
         * single pushes, in DelimStack.mPushed-- that would require numbering all
         * the levels in an OTD, and using that as the index.
         *
         * @param delimStack the original stack snapshot
         * @param delimLevel the delimiter list level to push
         * @param isIncludeRepSep whether to include repetition separators
         * @return the new DelimStack snapshot.
         */
        public DelimStack push(DelimStack delimStack, DelimLevel delimLevel,
            boolean isIncludeRepSep) {
            if (delimLevel == null) {
                throw new NullPointerException("No DelimLevel to push."); //I18N
            }
            if (mLog.isLoggable(Level.FINEST)) {
                mLog.finest("Will push delimLevel=[" + delimLevel + "]" + LN
                    + " onto delimStack=[" + delimStack + "].");
            }
            for (int i = 0; i < delimLevel.mDelimIndexRefs.length; i++) {
                int index = delimLevel.mDelimIndexRefs[i];
                if (isIncludeRepSep || mDelims[index].mType != Delim.Type.REPEAT) {
                    if (mLog.isLoggable(Level.FINEST)) {
                        mLog.finest("Will push delimIndex=" + index);
                    }
                    delimStack = push(delimStack, index);
                }
            }
            return delimStack;
        }
    }

    /**
     * A simple MatchReply implementation mainly for testing purpose.
     */
    public static class Pos implements MatchReply {

        public int mMatchPos = 0;
        public int mDataStartPos = 0;
        public int mDataLength = 0;
        public boolean mIsBeginDelimMatched = false;
        private boolean mIsSet = false;

        public void setMatchPos(int pos) {
            mMatchPos = pos;
            mIsSet = true;
        }

        public int getMatchPos() {
            return mMatchPos;
        }

        public int getPos() {
            if (!mIsSet) {
                throw new RuntimeException("pos not set");
            }
            return mMatchPos;
        }

        public void setDataStartPos(int pos) {
            mDataStartPos = pos;
        }

        public void setDataLength(int length) {
            mDataLength = length;
        }

        public void setBeginDelimMatched(boolean begin) {
            mIsBeginDelimMatched = begin;
        }

        @Override
        public String toString() {
            StringBuffer sb = new StringBuffer("Pos@")
                .append(Integer.toHexString(hashCode()));
            sb.append(" matchPosition=").append(mMatchPos);
            sb.append(" dataStartPos=").append(mDataStartPos);
            sb.append(" dataLength=").append(mDataLength);
            sb.append(" isBeginDelimMatched=").append(mIsBeginDelimMatched);
            return sb.toString();
        }
    }

    /**
     * Tests the state stack with some delimiters.
     * @param args arguments.
     */
    public static void main(String[] args) {
        try {
            System.out.println("start");
            String input = "xxA";
            byte[] data = input.getBytes(Nodes.DEFAULT_ENCODING);
            /* new Delim(final byte precedence, final String data) */
            Delim[] delims = new Delim[]{
                new Delim((byte) 5, "A"),
                new Delim((byte) 6, "AA"),
                new Delim((byte) 4, "AAA"),
                new Delim((byte) 5, "A4"),
                new Delim((byte) 7, "B"),
                new Delim((byte) 5, "xx")
            };
            Slot[] slots = null;
            OtdDelim otdDelim = new OtdDelim(delims, slots);
            OtdDelimInst otdDelimInst = otdDelim.new OtdDelimInst();
            DelimStack delimStack = otdDelim.mEmptyStack;
            Pos pos = new Pos();
            for (int i = 0; i < delims.length; i++) {
                System.out.println("Pushing delimStack #" + i);
                delimStack = otdDelimInst.push(delimStack, i);
                System.out.println("After push, delimStack: " + delimStack);
                Delim delim = delimStack.scan(data, 0, data.length, pos);
                if (delim != null) {
                    System.out.println("Found delim: pos=" + pos.getPos()
                        + ", delim=" + new String(delim.mBytes, Nodes.DEFAULT_ENCODING));
                } else {
                    System.out.println("No delim found: pos=" + pos.getPos());
                }
            }
            System.out.println("done.");
        } catch (Exception ex) {
            ex.printStackTrace();
            System.exit(1);
        }
    }
}
