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
 * @(#)CocoToken.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.coco.model;


/**
 * Class of objects that {@link com.sun.encoder.coco.model.CocoLexer CocoLexers} produce. CocoToken
 * objects represent lexemes extracted from Cobol Copybook data.

 * @author  Noel Ang
 *
 */
public final class CocoToken {

    public final String mValue;
    public final CocoTokenTypes mType;
    public final int mFoundCol;
    public final int mFoundRow;
    private boolean mIsEOL;

    /**
     * Create a token.
     *
     * @param  value Value of the token, may be null
     * @param  type  Type of the token
     */
    public CocoToken(String value, CocoTokenTypes type) {
        this(value, type, -1, -1);
    }

    /**
     * Create a token.
     *
     * @param  value Value for the token, may be null
     * @param  type  Token type
     * @param  row   Row (in some frame of reference) where the token occurs; use
     *               -1 to convey no row information
     * @param  col   Column in a Cobol source line where the token occurs; use
     *               -1 to convey no column information
     *
     * @throws java.lang.IndexOutOfBoundsException if row or col are zero, or a negative
     *         number other than -1
     */
    public CocoToken(String value, CocoTokenTypes type, int row, int col)
        throws IndexOutOfBoundsException {

        if (row < -1 || row == 0) {
            throw new IndexOutOfBoundsException();
        }
        if (col < -1 || col == 0) {
            throw new IndexOutOfBoundsException();
        }
        mValue    = value;
        mType     = type;
        mFoundRow = row;
        mFoundCol = col;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("CocoToken@")
                .append(Integer.toHexString(hashCode()));
        sb.append(" value='").append(mValue).append("'");
        sb.append(" type=").append(mType);
        sb.append(" row=").append(mFoundRow);
        sb.append(" col=").append(mFoundCol);
        if (mIsEOL) {
        sb.append(" IsEOL");
        }
        return sb.toString();
    }

    /**
     * Retrieve the type of the token.
     *
     * @return type the token
     * @see    CocoTokenTypes
     */
    public CocoTokenTypes getType() {
        return mType;
    }

    /**
     * Retrieve the value of the token.
     *
     * @return Value of token, or null if token has no value
     */
    public String getStringValue() {
        return mValue;
    }

    /**
     * Retrieve the value of the token.
     *
     * @return Value of token, or null if token has no value
     */
    public char[] getCharsValue() {
        char[] value = null;
        if (mValue != null) {
            value = mValue.toCharArray();
        }
        return value;
    }

    /**
     * Retrieve the value of the token. The byte array representation of the
     * value will be two times the size of the value in characters; each character
     * is a 2-byte value preserved across two byte array elements.
     *
     * @return Value of token, or null if token has no value
     */
    public byte[] getBytesValue() {
        byte[] value = null;
        if (mValue != null) {
            value = new byte[mValue.length() * 2];
            for (int i = 0, b = 0; i < value.length; i += 2, b++) {
                char ch = mValue.charAt(b);
                value[i] = (byte) ((ch & 0xFFFF0000) >> 8);
                value[i + 1] = (byte) (ch & 0x0000FFFF);
            }
        }
        return value;
    }

    /**
     * Retrieve row position of the token, or the line number of the
     * Cobol source line from which it was scanned.
     *
     * @return -1 if the token was constructed without position information; or
     *            a non-zero positive number otherwise
     */
    public int getRow() {
        return mFoundRow;
    }

    /**
     * Retrieve column position of the token, in the originating Cobol source line
     * from which it was scanned.
     *
     * @return -1 if the token was constructed without position information; or
     *            a column position number otherwise
     */
    public int getColumn() {
        return mFoundCol;
    }

    /**
     * Retrieve the length of the token.
     *
     * @return Token length; can be 0 if token has no value
     */
    public int getLength() {
        return (mValue == null ? 0 : mValue.length());
    }

    public void setIsEOL(boolean b) {
        mIsEOL = b;
    }

    public boolean isEOL() {
        return mIsEOL;
    }

    /**
     * Check this token is a EOF_TOKEN derived from EOF;
     * @return true if this token is a EOF_TOKEN derived from EOF, or false
     *         otherwise.
     */
    public boolean isEOF() {
        if (mType == CocoTokenTypes.EOF_TOKEN) {
            return true;
        }
        return false;
    }
}
