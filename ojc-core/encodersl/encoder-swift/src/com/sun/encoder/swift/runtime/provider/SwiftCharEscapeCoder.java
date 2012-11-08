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
 * @(#)SwiftCharEscapeCoder.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.swift.runtime.provider;

/**
 * This class provides Swift specific implementation for escaping/unescaping
 * character sequences.
 * 
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
public final class SwiftCharEscapeCoder {

    //state of processing
    private static final int PROCESS_UNKNOWN = 0;
    private static final int PROCESS_ESCAPE = 1;
    private static final int PROCESS_UNESCAPE = 2;
    
    //state of escaping
    private static final int ESCAPE_NONE = 0;
    private static final int ESCAPE_UNDET = 1;
    private static final int ESCAPE_FLDSEP = 2;
    private static final int ESCAPE_CMPSEP = 3;
    private static final int ESCAPE_SUBSEP = 4;
    private static final int ESCAPE_REPSEP = 5;
    private static final int ESCAPE_ESPCHR = 6;
    private static final int ESCAPE_HEX = 7;
    private static final int ESCAPE_OTHER = 8;
        
    private final char mFieldSep;
    private final char mCompoSep;
    private final char mSubCompoSep;
    private final char mRepetSep;
    private final char mEscapeChar;
    private final Result mResult = new Result();
    
    private int mState = PROCESS_UNKNOWN;
    private char[][] mEscapeSeqs;
    private char[] mDataBuf;
    private char[] mData;
    private char[] mResData;
    private int mStart;
    private int mCount;
    private int mEnd;
    private int mPos;
    private int mResPos;
    private int mLastResPos;
    private boolean mIsEnd;
    private int mLeftOver = 0;
    private boolean mEscapeOpen = false;
    private int mEscapeState = ESCAPE_NONE;
    private boolean mHasNext = false;
    private boolean mPendingScan;
    
    public SwiftCharEscapeCoder(char fieldSep, char compoSep, char subCompoSep,
            char repetSep, char escapeChar) {
        mFieldSep = fieldSep;
        mCompoSep = compoSep;
        mSubCompoSep = subCompoSep;
        mRepetSep = repetSep;
        mEscapeChar = escapeChar;
    }
    
    public void escape(char[] srcchars, int srcoffset, int count) {
        reset();
        mState = PROCESS_ESCAPE;
        if (count == 0) {
            return;
        }
        ensureEscapeSequence();
        final int end = srcoffset + count;
        char[] seq;
        int pos = srcoffset;
        mResPos = 0;
        int i;
        for (i = srcoffset; i < end; i++) {
            if (mEscapeSeqs[srcchars[i] & 0xff] != null) {
                seq = mEscapeSeqs[srcchars[i] & 0xff];
                ensureResultCapacity((i - pos) + mResPos + seq.length);
                if (i > pos) {
                    System.arraycopy(srcchars, pos, mResData, mResPos, i - pos);
                    mResPos += (i - pos);
                }
                System.arraycopy(seq, 0, mResData, mResPos, seq.length);
                mResPos += seq.length;
                pos = i + 1;
                mHasNext = true;
            }
        }
        if (mHasNext) {
            if (i > pos) {
                ensureResultCapacity((i - pos) + mResPos);
                System.arraycopy(srcchars, pos, mResData, mResPos, i - pos);
                mResPos += (i - pos);
                pos = i;
            }
            mResult.mType = Result.CHARDATA;
            mResult.mData = mResData;
            mResult.mStart = 0;
            mResult.mCount = mResPos;
        }
    }

    public void unescape(char[] srcchars, int srcoffset, int count,
            boolean end) {
        if (mState != PROCESS_UNESCAPE) {
            reset();
        }
        mState = PROCESS_UNESCAPE;
        if (mLeftOver > 0) {
            ensureSourceCapacity(mLeftOver + count);
            System.arraycopy(srcchars, srcoffset, mDataBuf, mLeftOver, count);
            mData = mDataBuf;
            mStart = 0;
            mCount = count + mLeftOver;
        } else {
            mData = srcchars;
            mStart = srcoffset;
            mCount = count;
        }
        mPos = mStart;
        mEnd = mStart + mCount;
        ensureResultCapacity(mCount);
        mResPos = 0;
        mLastResPos = 0;
        mIsEnd = end;
        mPendingScan = true;
    }
    
    public boolean hasResult() {
        if (mState == PROCESS_UNESCAPE) {
            if (mPendingScan) {
                scan();
                mPendingScan = false;
            }
            return mHasNext;
        }
        if (mState == PROCESS_ESCAPE) {
            return mHasNext;
        }
        return false;
    }
    
    public Result getResult() {
        if (mState == PROCESS_UNESCAPE) {
            if (mPendingScan) {
                scan();
                mPendingScan = false;
            }
            if (!mHasNext) {
                return null;
            }
            mPendingScan = true;
            return mResult;
        }
        if (mState == PROCESS_ESCAPE) {
            return mResult;
        }
        return null;
    }
    
    public void reset() {
        mResult.clear();
        mState = PROCESS_UNKNOWN;
        mData = null;
        mStart = -1;
        mCount = 0;
        mEnd = -1;
        mPos = -1;
        mResPos = -1;
        mLastResPos = -1;
        mIsEnd = true;
        mLeftOver = 0;
        mEscapeOpen = false;
        mEscapeState = ESCAPE_NONE;
        mHasNext = false;
        mPendingScan = false;
    }
    
    private void scan() {
        mHasNext = false;
        mResult.clear();
        final int end = mEnd;
        int i;
        if (mLeftOver > 0) {
            i = mLeftOver;
            mLeftOver = 0;
        } else {
            i = mPos;
        }
        mLastResPos = mResPos;
        whileloop: while (i < end) {
            if (mEscapeState == ESCAPE_UNDET) {
                if (mData[i] == mEscapeChar) {
                    //a consecutive escape char.
                    mEscapeState = ESCAPE_NONE;
                    i++;
                    continue whileloop;
                }
                boolean copiedData = false;
                if (i - 1 > mPos) {
                    //copy the data before the escape char.
                    System.arraycopy(mData, mPos, mResData,
                            mResPos, i - 1 - mPos);
                    mResPos += (i - 1 - mPos);
                    mPos = i - 1;
                    copiedData = true;
                }
                switch (mData[i]) {
                case 'F':
                    mEscapeState = ESCAPE_FLDSEP;
                    break;
                case 'S':
                    mEscapeState = ESCAPE_CMPSEP;
                    break;
                case 'T':
                    mEscapeState = ESCAPE_SUBSEP;
                    break;
                case 'R':
                    mEscapeState = ESCAPE_REPSEP;
                    break;
                case 'E':
                    mEscapeState = ESCAPE_ESPCHR;
                    break;
                case 'X':
                    mEscapeState = ESCAPE_HEX;
                    break;
                default:
                    if (copiedData) {
                        //has char. data before the escape char.
                        i--;
                        mEscapeState = ESCAPE_NONE;
                        break whileloop;
                    } else {
                        mEscapeState = ESCAPE_OTHER;
                    }
                }
            } else if (mData[i] == mEscapeChar) {
                if (mEscapeState == ESCAPE_NONE) {
                    //found an escape char., but don't know the type yet
                    mEscapeState = ESCAPE_UNDET;
                    mEscapeOpen = true;
                } else {
                    mEscapeOpen = false;
                    switch (mEscapeState) {
                    case ESCAPE_FLDSEP:
                        mResData[mResPos] = mFieldSep;
                        mResPos++;
                        mEscapeState = ESCAPE_NONE;
                        mPos = i + 1;
                        break;
                    case ESCAPE_CMPSEP:
                        mResData[mResPos] = mCompoSep;
                        mResPos++;
                        mEscapeState = ESCAPE_NONE;
                        mPos = i + 1;
                        break;
                    case ESCAPE_SUBSEP:
                        mResData[mResPos] = mSubCompoSep;
                        mResPos++;
                        mEscapeState = ESCAPE_NONE;
                        mPos = i + 1;
                        break;
                    case ESCAPE_REPSEP:
                        mResData[mResPos] = mRepetSep;
                        mResPos++;
                        mEscapeState = ESCAPE_NONE;
                        mPos = i + 1;
                        break;
                    case ESCAPE_ESPCHR:
                        mResData[mResPos] = mEscapeChar;
                        mResPos++;
                        mEscapeState = ESCAPE_NONE;
                        mPos = i + 1;
                        break;
                    case ESCAPE_HEX:
                        if (i > mPos + 2) {
                            String hexVal =
                                new String(mData, mPos + 2,
                                        i - mPos - 2);
                            char[] chars =
                                Character.toChars(
                                        Integer.parseInt(hexVal, 16));
                            if (chars.length == 1) {
                                mResData[mResPos++] = chars[0];
                            } else {
                                System.arraycopy(chars, 0, mResData,
                                        mResPos, chars.length);
                                mResPos += chars.length;
                            }
                        }
                        mEscapeState = ESCAPE_NONE;
                        mPos = i + 1;
                        break;
                    default:
                        i++;
                        break whileloop;
                    }
                }
            }
            i++;
        }
        if (mEscapeState != ESCAPE_NONE && mEscapeOpen && mIsEnd) {
            //data reaches end, but the second escape char. is not found
            mEscapeState = ESCAPE_NONE;
            mEscapeOpen = false;
        }
        if (mEscapeState == ESCAPE_NONE) {
            if (mPos < i) {
                System.arraycopy(mData, mPos, mResData,
                        mResPos, i - mPos);
                mResPos += (i - mPos);
                mPos = i;
            }
            if (mHasNext = (mResPos > mLastResPos)) {
                mResult.mStart = mLastResPos;
                mResult.mData = mResData;
                mResult.mCount = mResPos - mLastResPos;
                mResult.mType = Result.CHARDATA;
            }
        } else {
            if (mEscapeOpen) {
                //have left over
                ensureSourceCapacity(i - mPos);
                System.arraycopy(mData, mPos, mDataBuf, 0, i - mPos);
                mLeftOver = i - mPos;
                mPos = 0;
            } else {
                if (mHasNext = (i > mPos + 1)) {
                    mResult.mStart = mPos + 1;
                    mResult.mData = mData;
                    mResult.mCount = i - mPos - 2;
                    mResult.mType = Result.ESCAPE;
                }
                mPos = i;
                mEscapeState = ESCAPE_NONE;
            }
        }
    }
    
    private void ensureSourceCapacity(int capacity) {
        if (mDataBuf == null) {
            mDataBuf = new char[capacity];
            return;
        }
        if (mDataBuf.length >= capacity) {
            return;
        }
        char[] buf = new char[capacity];
        System.arraycopy(mDataBuf, 0, buf, 0, mDataBuf.length);
        mDataBuf = buf;
    }
    
    private void ensureResultCapacity(int capacity) {
        if (mResData == null) {
            mResData = new char[capacity];
            return;
        }
        if (mResData.length >= capacity) {
            return;
        }
        char[] buf = new char[capacity];
        System.arraycopy(mResData, 0, buf, 0, mResData.length);
        mResData = buf;
    }
    
    private void ensureEscapeSequence() {
        if (mEscapeSeqs == null) {
            mEscapeSeqs = new char[256][];
            mEscapeSeqs[mFieldSep & 0xff] = new char[] {'\\', 'F', '\\'};
            mEscapeSeqs[mCompoSep & 0xff] = new char[] {'\\', 'S', '\\'};
            mEscapeSeqs[mSubCompoSep & 0xff] = new char[] {'\\', 'T', '\\'};
            mEscapeSeqs[mRepetSep & 0xff] = new char[] {'\\', 'R', '\\'};
            mEscapeSeqs[mEscapeChar & 0xff] = new char[] {'\\', 'E', '\\'};
        }
    }
    
    public static final class Result {
        
        public static final int NONE = 0;
        public static final int CHARDATA = 1;
        public static final int ESCAPE = 2;
        
        public int mType;
        public char[] mData;
        public int mStart;
        public int mCount;
        
        void clear() {
            mType = NONE;
            mData = null;
            mStart = -1;
            mCount = 0;
        }
    }
}
