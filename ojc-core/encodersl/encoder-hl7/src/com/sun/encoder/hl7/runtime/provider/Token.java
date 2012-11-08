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

public final class Token {

    // token types
    enum Type {SEG_NAME, VALUE, EOF}
    Type mTokenType;

    // delimiter type
    public int mDelimType;

    public char[] mChars;
    public int mOffset;
    // count/size of chars of this token
    public int mCount;
    public int mLine;
    public int mCol;
    public int mLexerPos;
    public boolean mToBeContinued;
    public boolean mHasEscape;
    // flag indicating whether no more data left (data already consumed)
    public boolean mIsConsumed;

    @Override
    public String toString() {
        StringBuffer buff = new StringBuffer("(Token@")
            .append(Integer.toHexString(hashCode()));
        if (mChars != null && mOffset >= 0 && mCount >= 0) {
            buff.append(" data='").append(mChars, mOffset, mCount).append("'");
        } else {
            buff.append(" data=<null>");
        }
        buff.append(" tokenType=").append(mTokenType);
        buff.append(" offset=").append(mOffset);
        buff.append(" count=").append(mCount);
        buff.append(" delimType=").append(Delimiter.Type.getDelimiterTypeDesc(mDelimType));
        buff.append(" line=").append(mLine);
        buff.append(" col=").append(mCol);
        buff.append(" lexerPos=").append(mLexerPos);
        buff.append(" toBeContinued=").append(mToBeContinued);
        buff.append(" hasEscape=").append(mHasEscape);
        buff.append(" isConsumed=").append(mIsConsumed).append(")");
        return buff.toString();
    }
}
