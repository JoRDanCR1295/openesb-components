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

package com.sun.encoder.hl7.runtime.provider;

import com.sun.encoder.hl7.util.Util;
import com.sun.encoder.hl7.runtime.LexicalException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Arrays;
import org.xml.sax.InputSource;
import com.sun.encoder.hl7.i18n.Messages;
import java.util.logging.Level;
import java.util.logging.Logger;

final class Lexer {

    private static Messages mMessages = Messages.getMessages(Lexer.class);

    /** Logger object.*/
    private static Logger mLog = Logger.getLogger(Lexer.class.getName());

    /**
     * Field Separator, Contains the separator character that will be in effect
     * for the rest of this message.
     */
    private static final int HDR_FIELD_SEP_POS = 1;

    /**
     * Encoding Characters, Contains the component separator, repetition
     * separator, escape character, and sub-component separator, respectively
     * that will be in effect for the rest of this message."^~\&"
     */
    private static final int HDR_ENCODING_CHARS_POS = 2;

    // field ordinal number
    private int mFieldSequence = -1;

    private final String mFullMessage;

    // lexer states
    enum State {
        NOT_STARTED, SEG_END, SEG_NAME_READ, SEG_NAME_READ_EOF, EOF
    }
    // lexer state
    private State mState = State.NOT_STARTED;

    public State getState() {
        return mState;
    }

    /**
     * header segment
     * @see Util#isHeaderSegmentName(java.lang.String)
     */
    enum SegType {
        HDR_SEG, NORM_SEG
    }
    // segment type
    SegType mSegType;

    // segment name size constant, for HL7, it's always 3
    private static final int SEG_NAME_SIZE = 3;

    private final Reader mReader;
    private final int mBufCapacity;
    // holding raw data in char array
    private final char[] mRawCharBuf;

    public HL7CharEscapeCoder mEscapeCoder;

    // encoding characters initialized with default HL7 recommended values
    private char mSegDelim      = Delimiter.DEFAULT_SEG_DELIM;
    private char mFieldSep      = Delimiter.DEFAULT_FIELD_SEP;
    private char mCompoSep      = Delimiter.DEFAULT_COMPO_SEP;
    private char mRepetSep      = Delimiter.DEFAULT_REPET_SEP;
    private char mEscape        = Delimiter.DEFAULT_ESCAPE;
    private char mSubCompoSep   = Delimiter.DEFAULT_SUBCOMPO_SEP;

    /** Serves as a lookup table with array index being a delimiter char and
     * the value being one of the Delimiter type constant value.
     * @see Token#SEG_TERM
     * @see Token#FIELD_SEP
     * @see Token#ESCAPE_CHAR
     */
    private int[] mDelimChar2Type = new int[256];

    // raw data position starting from 0
    int mRawPos = 0;
    // available data buffer size limit, up to 1024 (DEFAULT_BUF_CAPACITY) at a time
    private int mBufLimit = -1;
    // line number starting from 1
    private int mLine = 1;
    // column number starting from 1
    int mCol = 1;
    // flag indicating whether or not to load buffer
    private boolean mIsLoadBuf;
    
    @Override
    public String toString() {
        StringBuffer buff = new StringBuffer("(Lexer@")
            .append(Integer.toHexString(hashCode()));
        buff.append(" state=").append(mState);
        buff.append(" segType=").append(mSegType);
        buff.append(" rawPos=").append(mRawPos);
        buff.append(" line=").append(mLine);
        buff.append(" col=").append(mCol);
        buff.append(" isLoadBuf=").append(mIsLoadBuf).append(")");
        return buff.toString();
    }

    // buffer capacity settings
    static final int DEFAULT_BUF_CAPACITY = 1024;
    static final int MINIMUM_BUF_CAPACITY = 12;
    static final int MAXIMUM_BUF_CAPACITY = 1024 * 128;

    public Lexer(InputSource input)
            throws IOException {
        this(input, DEFAULT_BUF_CAPACITY, false);
    }

    public Lexer(InputSource input, int bufCapacity, boolean preLoadAll)
        throws IOException {
        String errMsg = null;
        if (bufCapacity < MINIMUM_BUF_CAPACITY
            || bufCapacity > MAXIMUM_BUF_CAPACITY) {
            errMsg = mMessages.getString("HL7ENC-E0037.Invalid_buffer_capacity",
                new Object[]{"" + MINIMUM_BUF_CAPACITY, "" + MAXIMUM_BUF_CAPACITY});
            throw new IllegalArgumentException(errMsg);
        }
        mBufCapacity = bufCapacity;
        if (input.getCharacterStream() == null) {
            InputStream byteStream = input.getByteStream();
            if (byteStream == null) {
                throw new IllegalArgumentException(
                    mMessages.getString("HL7ENC-E0038.No_Reader_OR_InputSteram"));
            }
            if (input.getEncoding() != null) {
                mReader = new InputStreamReader(byteStream, input.getEncoding());
            } else {
                mReader = new InputStreamReader(byteStream, "ASCII");
            }
        } else {
            mReader = input.getCharacterStream();
        }
        Arrays.fill(mDelimChar2Type, 0);

        if (preLoadAll) {
            char[] charBuf = new char[DEFAULT_BUF_CAPACITY];
            StringBuilder builder = new StringBuilder();
            while ((mBufLimit = mReader.read(charBuf)) > 0) {
                builder.append(charBuf, 0, mBufLimit);
            }
            mFullMessage = builder.toString();
            mRawCharBuf = mFullMessage.toCharArray();
            mBufLimit = mRawCharBuf.length;
        } else {
            // now read characters into mRawCharBuf, returns number of chars read.
            mRawCharBuf = new char[mBufCapacity];
            mBufLimit = mReader.read(mRawCharBuf);
            if (mBufLimit == -1) {
                mState = State.EOF;
            }
            mIsLoadBuf = false;
            mFullMessage = "";
        }
    }

    /**
     * Load character buffer by reading from the Reader into RawCharBuf.
     * It takes care of the situation where there is still some chars left
     * in the RawCharBuf.
     * 
     * @param token
     * @return true if load char buffer successfully, or false if nothing
     * to load.
     * @throws java.io.IOException
     */
    private boolean loadBuffer(Token token)
            throws IOException {
        mIsLoadBuf = false;
        // reach buffer limit, so load data
        int remain = mBufLimit - mRawPos;
        if (remain != 0) {
            System.arraycopy(mRawCharBuf, mRawPos, mRawCharBuf, 0, remain);
            mBufLimit = mReader.read(mRawCharBuf, remain,
                (mBufCapacity - remain));
            if (mBufLimit == -1) {
                mState = State.SEG_NAME_READ_EOF;
                mBufLimit = remain;
            } else {
                mBufLimit += remain;
            }
        } else {
            mBufLimit = mReader.read(mRawCharBuf);
            if (mBufLimit == -1) {
                mState = State.EOF;
                token.mTokenType = Token.Type.EOF;
                token.mDelimType = Delimiter.Type.SEG_TERM;
                return false;
            }
        }
        mRawPos = 0;
        return true;
    }

    /**
     * Handle those special fields in the header segment.
     * 
     * @param token data token object.
     * @return true if handles successfully, or false otherwise.
     * @throws java.io.IOException
     * @throws com.sun.encoder.hl7.runtime.LexicalException
     */
    public final boolean handleSpecialFieldsInHeaderSegment(Token token)
        throws IOException, LexicalException {
        int nextRawPos;
        int nextCol;
        char c;
        int idx;

        if (mFieldSequence == HDR_FIELD_SEP_POS) {
            // i.e. this is in the field separator single character position
            mFieldSep = mRawCharBuf[mRawPos];
            token.mChars = mRawCharBuf;
            token.mOffset = mRawPos;
            token.mCount = 1;
            token.mDelimType = Delimiter.Type.FIELD_SEP;
            token.mLexerPos = mRawPos;
            token.mTokenType = Token.Type.VALUE;
            token.mLine = mLine;
            token.mCol = mCol;
            token.mToBeContinued = false;
            mFieldSequence++;
            mRawPos++;
            mCol++;
            return true;
        }

        // i.e. mFieldSequence ==  HDR_ENCODING_CHARS_POS:
        // this is in the encoding characters position, parse out all characters
        int delta = mBufLimit - mRawPos;
        if (delta <= 0) {
            throwLexException(mMessages.getString("HL7ENC-E0039.Missing_fld_separtor"),
                mLine, mCol + 4, 1, mRawPos + 4);
        }
        // Open-ESB issue 2218. Here check that if the header segment ID
        // got split here by the buffer, then load the buffer by reading
        // the character array downsteam from the reader - the loadBuffer will
        // take care of the former part of the header segment ID.
        if (delta <= 4) {
            boolean b = loadBuffer(token);
            if (!b) {
                if (mLog.isLoggable(Level.FINEST)) {
                    mLog.finest("After handleSpecialFieldsInHeaderSegment, token=[" + token + "].");
                }
                return false;
            }
        }

        nextRawPos = mRawPos;
        nextCol = mCol;
        idx = 0;
        while (nextRawPos < mBufLimit) {
            c = mRawCharBuf[nextRawPos];
            if (c == mFieldSep) {
                // encounter field separator, finish/stop now
                break;
            }
            switch (idx) {
                case 0: // component separator
                    mCompoSep = c;
                    break;
                case 1: // repetition separator
                    mRepetSep = c;
                    break;
                case 2: // escape character
                    mEscape = c;
                    break;
                case 3: // subcomponent separator
                    mSubCompoSep = c;
                    break;
            }
            nextRawPos++;
            idx++;
            nextCol++;
        }
        if (nextRawPos >= mBufLimit) {
            throwLexException(mMessages.getString("HL7ENC-E0039.Missing_fld_separtor"),
                mLine, mCol + 4, 1, mRawPos + 4);
        }

        // if reach here, means it's stopped by field separator, good
        mDelimChar2Type[mSegDelim & 0xFF] = Delimiter.Type.SEG_TERM;
        // check to see if each separator(s) has been set before
        int val;
        val = mDelimChar2Type[mFieldSep & 0xFF];
        if (val != 0 && val != Delimiter.Type.FIELD_SEP) {
            // conflict
            mDelimChar2Type[mFieldSep & 0xFF] = Delimiter.Type.MULTIPLE_HIT;
        } else {
            mDelimChar2Type[mFieldSep & 0xFF] = Delimiter.Type.FIELD_SEP;
        }
        val = mDelimChar2Type[mCompoSep & 0xFF];
        if (val != 0 && val != Delimiter.Type.COMPO_SEP) {
            // conflict
            mDelimChar2Type[mCompoSep & 0xFF] = Delimiter.Type.MULTIPLE_HIT;
        } else {
            mDelimChar2Type[mCompoSep & 0xFF] = Delimiter.Type.COMPO_SEP;
        }
        val = mDelimChar2Type[mRepetSep & 0xFF];
        if (val != 0 && val != Delimiter.Type.REPET_SEP) {
            // conflict
            mDelimChar2Type[mRepetSep & 0xFF] = Delimiter.Type.MULTIPLE_HIT;
        } else {
            mDelimChar2Type[mRepetSep & 0xFF] = Delimiter.Type.REPET_SEP;
        }
        val = mDelimChar2Type[mSubCompoSep & 0xFF];
        if (val != 0 && val != Delimiter.Type.SUBCOMPO_SEP) {
            // conflict
            mDelimChar2Type[mSubCompoSep & 0xFF] = Delimiter.Type.MULTIPLE_HIT;
        } else {
            mDelimChar2Type[mSubCompoSep & 0xFF] = Delimiter.Type.SUBCOMPO_SEP;
        }
        val = mDelimChar2Type[mEscape & 0xFF];
        if (val != 0 && val != Delimiter.Type.ESCAPE_CHAR) {
            // conflict
            mDelimChar2Type[mEscape & 0xFF] = Delimiter.Type.MULTIPLE_HIT;
        } else {
            mDelimChar2Type[mEscape & 0xFF] = Delimiter.Type.ESCAPE_CHAR;
        }
        // initialize the HL7CharEscapeCoder
        mEscapeCoder = new HL7CharEscapeCoder(mFieldSep,
            mCompoSep, mSubCompoSep, mRepetSep, mEscape);
        // populates token
        token.mChars = mRawCharBuf;
        token.mOffset = mRawPos;
        token.mCount = idx;
        token.mDelimType = Delimiter.Type.FIELD_SEP;
        token.mLexerPos = mRawPos;
        token.mTokenType = Token.Type.VALUE;
        token.mLine = mLine;
        token.mCol = mCol;
        token.mToBeContinued = false;
        mRawPos = nextRawPos + 1;
        mCol = nextCol + 1;
        mFieldSequence++;
        return true;
    }

    /**
     * Returns the full text of the message, if preLoadAll was set to true
     */
    public final String getFullMessageText() {
        return mFullMessage;
    }

    /**
     * Fill in next token.
     * 
     * @param token next token to be filled/populated from next data element
     * @return true if successfully filled next token, or false if no more data
     * left for parsing (token is not filled/updated).
     * 
     * @throws java.io.IOException
     * @throws com.sun.encoder.hl7.runtime.LexicalException
     */
    public final boolean fillNextToken(Token token)
            throws IOException, LexicalException {
        if (mLog.isLoggable(Level.FINEST)) {
            mLog.finest("Before fillNextToken, token=[" + token + "].");
        }
        String errMsg = null;
        // reset mIsConsumed and mHasEscape
        token.mIsConsumed = false;
        token.mHasEscape = false;
        if (mState == State.EOF) {
            token.mTokenType = Token.Type.EOF;
            token.mDelimType = Delimiter.Type.SEG_TERM;
            if (mLog.isLoggable(Level.FINEST)) {
                mLog.finest("After fillNextToken, token=[" + token + "].");
            }
            return false;
        }
        if (mRawPos > mBufLimit) {
            throw new IllegalStateException("rawPos (" + mRawPos
                + ") cannot be greater than bufLimit (" + mBufLimit + ")");
        }
        if (mIsLoadBuf || (mRawPos == mBufLimit)) {
            boolean b = loadBuffer(token);
            if (!b) {
                if (mLog.isLoggable(Level.FINEST)) {
                    mLog.finest("After fillNextToken, token=[" + token + "].");
                }
                return false;
            }
        }

        int nextRawPos;
        int nextCol;
        int delimType;
        char c;
        int ci;
        int i;

        switch (mState) {
            case SEG_NAME_READ:
            case SEG_NAME_READ_EOF:
                if (mSegType == SegType.HDR_SEG && (mFieldSequence == HDR_FIELD_SEP_POS
                    || mFieldSequence == HDR_ENCODING_CHARS_POS)) {
                    boolean b = handleSpecialFieldsInHeaderSegment(token);
                    if (mLog.isLoggable(Level.FINEST)) {
                        mLog.finest("After fillNextToken, token=[" + token + "].");
                    }
                    return b;
                }
                // if reaches here it means either not a header segment, or
                // fields in header segment which require no special handling
                nextRawPos = mRawPos;
                nextCol = mCol;
                delimType = Delimiter.Type.DELIM_UNKNOWN;
look4delimloop:
                while (nextRawPos < mBufLimit) {
                    // check on each char byte to see if matches a delimiter.
                    ci = mRawCharBuf[nextRawPos] & 0xFF;
                    if (mDelimChar2Type[ci] != 0) {
                        // might have found delimiter char, check further
                        delimType = mDelimChar2Type[ci];
                        switch (delimType) {
                            case Delimiter.Type.FIELD_SEP:
                                if (mRawCharBuf[nextRawPos] == mFieldSep) {
                                    break look4delimloop;
                                }
                                // no actual match
                                break;
                            case Delimiter.Type.COMPO_SEP:
                                if (mRawCharBuf[nextRawPos] == mCompoSep) {
                                    break look4delimloop;
                                }
                                // no actual match
                                break;
                            case Delimiter.Type.REPET_SEP:
                                if (mRawCharBuf[nextRawPos] == mRepetSep) {
                                    break look4delimloop;
                                }
                                // no actual match
                                break;
                            case Delimiter.Type.SUBCOMPO_SEP:
                                if (mRawCharBuf[nextRawPos] == mSubCompoSep) {
                                    break look4delimloop;
                                }
                                // no actual match
                                break;
                            case Delimiter.Type.SEG_TERM:
                                if (mRawCharBuf[nextRawPos] == mSegDelim) {
                                    break look4delimloop;
                                }
                                // no actual match
                                break;
                            case Delimiter.Type.ESCAPE_CHAR:
                                if (mRawCharBuf[nextRawPos] == mEscape) {
                                    token.mHasEscape = true;
                                }
                                nextRawPos++;
                                nextCol++;
                                continue look4delimloop;
                            case Delimiter.Type.MULTIPLE_HIT:
                                // conflict hash code, check which one it is.
                                // should rarely happen
                                c = mRawCharBuf[nextRawPos];
                                if (c == mFieldSep) {
                                    delimType = Delimiter.Type.FIELD_SEP;
                                    break look4delimloop;
                                } else if (c == mCompoSep) {
                                    delimType = Delimiter.Type.COMPO_SEP;
                                    break look4delimloop;
                                } else if (c == mRepetSep) {
                                    delimType = Delimiter.Type.REPET_SEP;
                                    break look4delimloop;
                                } else if (c == mSubCompoSep) {
                                    delimType = Delimiter.Type.SUBCOMPO_SEP;
                                    break look4delimloop;
                                } else if (c == mSegDelim) {
                                    delimType = Delimiter.Type.SEG_TERM;
                                    break look4delimloop;
                                } else {
                                    // no actual match
                                    break;
                                }
                        }
                    }
                    nextRawPos++;
                    nextCol++;
                } // end-- look4delimloop: while (nextRawPos < mBufLimit)
                if (nextRawPos < mBufLimit) {
                    // stopped by a delimiter. make sure not following delimiter types
                    assert (delimType != Delimiter.Type.DELIM_UNKNOWN
                        && delimType != Delimiter.Type.MULTIPLE_HIT
                        && delimType != Delimiter.Type.DELIM_NOT_READ);
                    // populate or reset token fields
                    token.mChars = mRawCharBuf; 
                    token.mOffset = mRawPos;
                    token.mCount = nextRawPos - mRawPos;
                    token.mDelimType = delimType;
                    token.mLexerPos = mRawPos;
                    token.mTokenType = Token.Type.VALUE;
                    token.mLine = mLine;
                    token.mCol = mCol;
                    token.mToBeContinued = false;
                    mRawPos = nextRawPos + 1;
                    if (delimType == Delimiter.Type.SEG_TERM) {
                        // i.e. will begin another new line of segment data
                        mLine++;
                        mCol = 1; // new line beginning, starting column 1
                        mFieldSequence = 1; // first field of new line
                        mState = State.SEG_END; // this segment has ended
                    } else {
                        if (delimType == Delimiter.Type.FIELD_SEP) {
                            mFieldSequence++;
                        }
                        mCol = nextCol + 1; // FIELD_SEP len=1
                    }
                } else {
                    token.mChars = mRawCharBuf;
                    token.mOffset = mRawPos;
                    token.mCount = nextRawPos - mRawPos;
                    if (mState == State.SEG_NAME_READ_EOF) {
                        token.mDelimType = Delimiter.Type.SEG_TERM;
                        mState = State.EOF;
                    } else {
                        token.mDelimType = Delimiter.Type.DELIM_NOT_READ;
                    }
                    token.mLexerPos = mRawPos;
                    token.mTokenType = Token.Type.VALUE;
                    token.mLine = mLine;
                    token.mCol = mCol;
                    token.mToBeContinued = true;
                    mRawPos = nextRawPos;
                    mCol = nextCol;
                }
                if (mLog.isLoggable(Level.FINEST)) {
                    mLog.finest("After fillNextToken, token=[" + token + "].");
                }
                return true;
            case SEG_END:
                nextRawPos = mRawPos;
                nextCol = mCol;
                i = 0;
                while (nextRawPos < mBufLimit && i <= SEG_NAME_SIZE) {
                    c = mRawCharBuf[nextRawPos];
                    if (c == mFieldSep) {
                        break; // break-- while (nextRawPos < mBufLimit &&...
                    }
                    nextRawPos++;
                    i++;
                    nextCol++;
                } // end-- while (nextRawPos < mBufLimit &&...
                if (i > SEG_NAME_SIZE) {
                    throwLexException(mMessages.getString("HL7ENC-E0040.SEG_Name_has_more_characters"),
                        mLine, mCol, 4, mRawPos);
                }
                // populate token
                token.mChars = mRawCharBuf;
                token.mOffset = mRawPos;
                token.mCount = i;
                token.mLexerPos = mRawPos;
                token.mTokenType = Token.Type.SEG_NAME;
                token.mLine = mLine;
                token.mCol = mCol;
                if (nextRawPos < mBufLimit) {
                    token.mDelimType = Delimiter.Type.FIELD_SEP;
                    token.mToBeContinued = false;
                    mState = State.SEG_NAME_READ;
                    // check if header segment
                    String segName = new String(token.mChars, token.mOffset,
                        token.mCount);
                    if (Util.isHeaderSegmentName(segName)) {
                        mSegType = SegType.HDR_SEG;
                        mRawPos = nextRawPos;
                        mCol = nextCol;
                    } else {
                        mSegType = SegType.NORM_SEG;
                        mRawPos = nextRawPos + 1; // delimiter len=1
                        mCol = nextCol + 1; // delimiter len=1
                    }
                } else {
                    mIsLoadBuf = true;
                    token.mDelimType = Delimiter.Type.DELIM_NOT_READ;
                    token.mToBeContinued = true;
                    mRawPos = nextRawPos;
                    mCol = nextCol;
                }
                if (mLog.isLoggable(Level.FINEST)) {
                    mLog.finest("After fillNextToken, token=[" + token + "].");
                }
                return true;
            case NOT_STARTED:
                nextRawPos = mRawPos + SEG_NAME_SIZE;
                if (nextRawPos > mBufLimit) {
                    errMsg = mMessages.getString("HL7ENC-E0041.Incomplete_segment_name",
                        new Object[]{new String(mRawCharBuf, 0, mBufLimit)});
                    throwLexException(errMsg, mLine, mCol, mBufLimit, mBufLimit);
                }
                // populate token
                token.mChars = mRawCharBuf;
                token.mOffset = 0;
                token.mCount = SEG_NAME_SIZE;
                token.mDelimType = Delimiter.Type.FIELD_SEP;
                token.mLexerPos = 0;
                token.mTokenType = Token.Type.SEG_NAME;
                token.mLine = mLine;
                token.mCol = mCol;
                token.mToBeContinued = false;
                mFieldSequence = 1;
                // a header segment as starting segment
                mSegType = SegType.HDR_SEG;
                mState = State.SEG_NAME_READ;
                mRawPos += SEG_NAME_SIZE;
                mCol += SEG_NAME_SIZE;
                if (mLog.isLoggable(Level.FINEST)) {
                    mLog.finest("After fillNextToken, token=[" + token + "].");
                }
                return true;
            default:
                // Not possible unless programming error
                throw new IllegalStateException(mMessages.getString("HL7ENC-E0042.Illegal_state",
                    new Object[]{"" + mState}));
        }
    }

    private void throwLexException(String msg, int line, int col, int count,
            int pos) throws LexicalException {
        throw new LexicalException(msg, line, col, count);
    }
}
