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
 * @(#)CocoPicture.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.coco.model;

import com.sun.encoder.coco.runtime.messages.ErrorManager;
import com.sun.encoder.coco.runtime.messages.Message;
import com.sun.encoder.coco.runtime.messages.MessageCatalog;

/**
 * Represents the information and realization of a Cobol PICTURE clause.
 *
 * @author  Noel Ang
 *
 * @version $Revision: 1.2 $
 */
public class CocoPicture {

    /**
     * Picture Data Categories
     */
    public enum Category {
        /*
        public static final int ERR = 0;
        public static final int ALPHABETIC = 1;
        public static final int NUMERIC = 2;
        public static final int NUMERIC_EDITED = 3;
        public static final int ALPHANUMERIC = 4;
        public static final int ALPHANUMERIC_EDITED = 5;
        public static final int DBCS = 6;
        public static final int EX_FLOAT = 7; */
        ERR, ALPHABETIC, NUMERIC, NUMERIC_EDITED, ALPHANUMERIC,
        ALPHANUMERIC_EDITED, DBCS, EX_FLOAT
    }

    private static final ErrorManager cErrorMgr =
            ErrorManager.getManager("STC.eWay.converter.COBOLCopybook."
                                    + CocoPicture.class.getName());
    private String mOriginalPicture;
    private String mPicture;
    private int mDecimalPos;
    private int mDecimalScalePos;
    private Category mDataCategory;

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("CocoPicture@")
                .append(Integer.toHexString(hashCode()));
        sb.append(" picture=").append(mPicture);
        sb.append(" originalPicture=").append(mOriginalPicture);
        sb.append(" decimalPos=").append(mDecimalPos);
        sb.append(" decimalScalePos=").append(mDecimalScalePos);
        sb.append(" dataCategory=").append(mDataCategory);
        return sb.toString();
    }

    /**
     * Create a picture object from a picture character string.
     *
     * @param  picStr Picture character string
     * @throws java.lang.IllegalArgumentException if the specified character string is
     *         malformed or possesses unsupported and unignorable symbols.
     */
    public CocoPicture(String picStr) throws IllegalArgumentException {
        if (picStr == null || picStr.length() == 0) {
            Message msg = MessageCatalog.getMessage("CCCB4123");
            String text = msg.toString();
            cErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
            throw new IllegalArgumentException(text);
        }
        mOriginalPicture = picStr;
        picStr = normalizePicString(picStr);
        validatePicString(picStr);
        mPicture = picStr.toUpperCase();
        computeCategory();
        computeDecimalPosition();
    }

    /**
     * Retrieve "raw" Picture character string.
     *
     * @return Picture character string
     */
    public String getPicture() {
        return mPicture;
    }

    public String getOriginalPicture() {
        return mOriginalPicture;
    }

    /**
     * Retrieve the position of the Picture's decimal.
     *
     * @return a non-negative integer equal to the number of digits to the right
     *         of the decimal point.  A picture character-string with a 'V' as the
     *         last character computes a zero (0); a 'V' as the first character
     *         in the picture computes the value equal to the number of digits
     *         in the picture.  A picture with trailing P symbols will return
     *         a negative value representing the number of zeroes to append to
     *         the number (to scale up the value). A picture which cannot have a
     *         decimal position (either implied or explicit) returns a value of 0.
     */
    public int getDecimalPosition() {
        return mDecimalPos;
    }

    /**
     * Retrieve the number of decimal scaling positions of the Picture.
     *
     * @return a non-negative integer equal to the number of P symbols in the
     * picture.
     */
    public int getDecimalScalingPositions() {
        return mDecimalScalePos;
    }

    /**
     * Retrieve picture data category. See {@link CocoPicture.Category}
     *
     * @return Picture data category as a type ordinal.
     *
     * @see CocoPicture.Category
     */
    public Category getCategory() {
        return mDataCategory;
    }

    /**
     * Determine number of digits in the picture
     *
     * @return the number of digits character positions in the picture
     */
    public int countDigits() {

        int digitCount = 0;

        for (int i = 0; i < mPicture.length(); i++) {
            char ch = mPicture.charAt(i);
            if ("Z90".indexOf(ch) != -1) {
                digitCount++;
            }
        }
        return digitCount;
    }

    /**
     * Determine whether the picture has a sign indicator.
     */
    public boolean hasSign() {
        return (mPicture.indexOf('S') != -1);
    }

    /**
     * Determine whether the picture contains decimal point information; that is,
     * it contains any of the symbols P, V, and period (.).
     */
    public boolean hasDecimalPoint() {
        for (int i = 0; i < mPicture.length(); i++) {
            char ch = mPicture.charAt(i);
            if ("PV.".indexOf(ch) != -1) {
                return true;
            }
        }
        return false;
    }

    /**
     * Determine whether a string is usable as a Picture clause character-string.
     *
     * @throws java.lang.IllegalArgumentException if the string is not usable as a Picture
     *         character-string
     */
    private static void validatePicString(String str) throws IllegalArgumentException {

        str = str.toUpperCase();

        /*
         * Picture character strings have many and explicit rules concerning
         * what characters can appear to the left of what characters.
         *
         * The full format requirements specified in the COBOL language reference
         * is in Part 5 of the document, figure 6.
         *
         * What I have done here is to check the symbol-specific restrictions listed
         * in table 11 of said document section, but not the full-blown adjacency
         * rules of each symbol.
         */

        /* +/-/CR/DB are mutually exclusive */
        {
        boolean foundCR    = (str.indexOf("CR") != -1);
        boolean foundDB    = (str.indexOf("DB") != -1);
        boolean foundPlus  = (str.indexOf("+") != -1);
        boolean foundMinus = (str.indexOf("-") != -1);
        int cntFound = (foundCR || foundDB ? 1 : 0);
        cntFound += (foundPlus || foundMinus ? 1 : 0);
        if (cntFound > 1) { // 0 is valid
            Message msg = MessageCatalog.getMessage("CCCB4124");
            String text = msg.formatText(new Object[] {
                str
            });
            cErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
            throw new IllegalArgumentException(text);
        }
        }

        /* V must only occur once */
        {
        int idxV1 = str.indexOf("V");
        int idxV2 = str.lastIndexOf("V");
        if (idxV1 != idxV2) {
            Message msg = MessageCatalog.getMessage("CCCB4125");
            String text = msg.formatText(new Object[] {
                str
            });
            cErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
            throw new IllegalArgumentException(text);
        }
        }

        /* S must be left most character */
        {
        int idxS = str.indexOf("S");
        if (idxS != -1 && idxS != 0) {
            Message msg = MessageCatalog.getMessage("CCCB4126");
            String text = msg.formatText(new Object[] {
                str
            });
            cErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
            throw new IllegalArgumentException(text);
        }
        }

        /*
         * P must be leftmost (except to S) XOR rightmost, and must be continuous
         * if repeating
         */
        {
        int idxP = str.indexOf("P");
        if (idxP != -1) {
            boolean foundStop = false;
            for (int i = idxP + 1; i < str.length(); i++) {
                if (str.charAt(i) != 'P') {
                    foundStop = true;
                } else if (foundStop) {
                    Message msg = MessageCatalog.getMessage("CCCB4127");
                    String text = msg.formatText(new Object[] {
                        str
                    });
                    cErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
                    throw new IllegalArgumentException(text);
                }
            }
            idxP = str.lastIndexOf("P");
            int idxS = str.indexOf("S");
            if ((idxS != -1) && (idxP < idxS)) {
                Message msg = MessageCatalog.getMessage("CCCB4128");
                String text = msg.formatText(new Object[] {
                    str
                });
                cErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
                throw new IllegalArgumentException(text);
            }
            if ((str.indexOf("P") != 0) && (idxP < str.length() - 1)) {
                Message msg = MessageCatalog.getMessage("CCCB4129");
                String text = msg.formatText(new Object[] {
                    str
                });
                cErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
                throw new IllegalArgumentException(text);
            }
        }
        }

        /* certain characters can only occur once in a pic string */
        {
        String msg = null;
        int pos;
        if ((pos = str.indexOf("CR")) != -1 && pos != str.lastIndexOf("CR")) {
            Message err = MessageCatalog.getMessage("CCCB4130");
            msg = err.formatText(new Object[] {str});
        }
        if ((pos = str.indexOf("DB")) != -1 && pos != str.lastIndexOf("DB")) {
            Message err = MessageCatalog.getMessage("CCCB4131");
            msg = err.formatText(new Object[] {str});
        }
        if ((pos = str.indexOf("S")) != -1 && pos != str.lastIndexOf("S")) {
            Message err = MessageCatalog.getMessage("CCCB4132");
            msg = err.formatText(new Object[] {str});
        }
        if ((pos = str.indexOf("V")) != -1 && pos != str.lastIndexOf("V")) {
            Message err = MessageCatalog.getMessage("CCCB4124");
            msg = err.formatText(new Object[] {str});
        }
        if ((pos = str.indexOf("E")) != -1 && pos != str.lastIndexOf("E")) {
            Message err = MessageCatalog.getMessage("CCCB4133");
            msg = err.formatText(new Object[] {str});
        }
        if ((pos = str.indexOf(".")) != -1 && pos != str.lastIndexOf(".")) {
            Message err = MessageCatalog.getMessage("CCCB4134");
            msg = err.formatText(new Object[] {str});
        }
        if (msg != null) {
            cErrorMgr.log(ErrorManager.Severity.ERROR, null, msg);
            throw new IllegalArgumentException(msg);
        }
        }

        /* if external floating point, check format */
        {
        int pos = str.indexOf('E');
        if (pos != -1) {

            String exfloat = str.trim();

            pos = exfloat.indexOf('E');
            if (pos != exfloat.length() - 4) {
                Message msg = MessageCatalog.getMessage("CCCB4135");
                String text = msg.formatText(new Object[] {str});
                cErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
                throw new IllegalArgumentException(text);
            }

            boolean ok = true;
            int vCount = 0;
            int dotCount = 0;
            ok |= ("+-".indexOf(exfloat.charAt(0)) != -1);
            ok |= ("+-".indexOf(exfloat.charAt(pos + 1)) != -1);
            ok |= ((exfloat.charAt(pos + 2) == '9') && (exfloat.charAt(pos + 3) == '9'));
            for (int i = 1; i < pos; i++) {
                ok |= ("9.V".indexOf(exfloat.charAt(i)) != -1);
                if (exfloat.charAt(i) == '.') {
                    dotCount++;
                } else if (exfloat.charAt(i) == 'V') {
                    vCount++;
                }
            }
            ok |= ((dotCount + vCount) < 2);

            if (!ok) {
                Message msg = MessageCatalog.getMessage("CCCB4135");
                String text = msg.formatText(new Object[] {str});
                cErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
                throw new IllegalArgumentException(text);
            }
        }
        }
    }

    /**
     * Clean up the picture string.
     *
     * @param  pic Picture character-string in consideration
     *
     * @return normalized picture string
     *
     * @throws java.lang.IllegalArgumentException if any error in the picture string prevents
     *         successful processing.
     */
    private static String normalizePicString(String pic) throws IllegalArgumentException {
        pic = expandOccurence(pic);
        return pic;
    }

    /**
     * Expands X(n) short-hand notations in the picture string; e.g., 9(2) becomes
     * 99.
     *
     * @param  pic  Picture character-string in consideration
     *
     * @throws java.lang.IllegalArgumentException if any error in the picture string prevents
     *         successful processing.
     */
    private static String expandOccurence(String pic) throws IllegalArgumentException {

        int len = pic.length();
        int prevCh  = -1;
        StringBuffer buffer = new StringBuffer(len);

        for (int i = 0; i < len; i++) {

            char ch = pic.charAt(i);

            if (ch == '(') {

                if (prevCh == -1) {
                    Message msg = MessageCatalog.getMessage("CCCB4136");
                    String text = msg.formatText(new Object[] {
                        String.valueOf(ch),
                        pic
                    });
                    cErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
                    throw new IllegalArgumentException(text);
                }

                int rparenPos = pic.indexOf(')', i);
                if (rparenPos == -1) {
                    Message msg = MessageCatalog.getMessage("CCCB4137");
                    String text = msg.formatText(new Object[] {pic});
                    cErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
                    throw new IllegalArgumentException(text);
                }

                try {
                    int times = Integer.parseInt(pic.substring(i + 1, rparenPos));
                    times -= 1; // prevCh added the first one
                    i = rparenPos;
                    for (int j = 0; j < times; j++) {
                        buffer.append((char) prevCh);
                    }
                } catch (NumberFormatException nfe) {
                    throw new IllegalArgumentException(nfe.getLocalizedMessage());
                }
            } else if (ch == ')') {
                Message msg = MessageCatalog.getMessage("CCCB4138");
                String text = msg.formatText(new Object[] {pic});
                cErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
                throw new IllegalArgumentException(text);
            } else {
                prevCh = (int) ch;
                buffer.append(ch);
            }
        }

        return buffer.toString();
    }

    /**
     * Computes decimal position (implied or explicit) of the picture.
     *
     * Preconditions: mPicture is initialized and normalized
     *
     * Postconditions: mDecimalPos is updated
     */
    private void computeDecimalPosition() {

        mDecimalPos = 0;
        mDecimalScalePos = 0;

        switch (mDataCategory) {

        case EX_FLOAT: {
            int ePos = mPicture.indexOf("E");
            int digitCnt = 0;
            for (int i = ePos - 1; i >= 0; i--) {
                char ch = mPicture.charAt(i);
                if (ch == '9') {
                    digitCnt++;
                } else if ("V.".indexOf(ch) != -1) {
                    mDecimalPos = digitCnt;
                    break;
                }
            }
            break;
        }

        case NUMERIC: {
            boolean haveV = (mPicture.indexOf('V') != -1);
            boolean haveP = (mPicture.indexOf('P') != -1);
            boolean leftP = (mPicture.lastIndexOf('P') != (mPicture.length() - 1));
            if (haveV) {
                int digitCnt = 0;
                for (int i = mPicture.length() - 1; i >= 0; i--) {
                    char ch = mPicture.charAt(i);
                    if (ch == '9') {
                        digitCnt++;
                    } else if (ch == 'V') {
                        mDecimalPos = digitCnt;
                        break;
                    }
                }
            } else if (haveP) {
                mDecimalScalePos = mPicture.lastIndexOf('P') - mPicture.indexOf('P') + 1;
                mDecimalPos = (!leftP ? 0 : mPicture.length() - (hasSign() ? 1 : 0));
            } else {
                mDecimalPos = 0;
            }
            break;
        }

        case NUMERIC_EDITED: {
            boolean haveDot = (mPicture.indexOf('.') != -1);
            boolean haveV = (mPicture.indexOf('V') != -1);
            boolean haveP = (mPicture.indexOf('P') != -1);
            boolean leftP = (mPicture.lastIndexOf('P') != (mPicture.length() - 1));
            if (haveV || haveDot) {
                int digitCnt = 0;
                for (int i = mPicture.length() - 1; i >= 0; i--) {
                    char ch = mPicture.charAt(i);
                    if ("90Z".indexOf(ch) != -1) {
                        digitCnt++;
                    } else if (".V".indexOf(ch) != -1) {
                        mDecimalPos = digitCnt;
                        break;
                    }
                }
            } else if (haveP) {
                mDecimalScalePos = mPicture.lastIndexOf('P') - mPicture.indexOf('P') + 1;
                mDecimalPos = (!leftP ? 0 : mPicture.length() - (hasSign() ? 1 : 0));
            } else {
                mDecimalPos = 0;
            }
            break;
        }

        default:
            mDecimalPos = 0;

        }
    }

    /**
     * Determine the data category of the picture.
     *
     * Preconditions: mPicture is initialized and normalized.
     *
     * Postconditions: initializes mDataCategory.
     */
    private void computeCategory() {

        if (checkAlphabetic()) {
            mDataCategory = Category.ALPHABETIC;
        } else if (checkNumeric()) {
            mDataCategory = Category.NUMERIC;
        } else if (checkAlphanumeric()) {
            mDataCategory = Category.ALPHANUMERIC;
        } else if (checkDbcs()) {
            mDataCategory = Category.DBCS;
        } else if (checkAlphanumericEdited()) {
            mDataCategory = Category.ALPHANUMERIC_EDITED;
        } else if (checkExFloat()) {
            mDataCategory = Category.EX_FLOAT;
        } else if (checkNumericEdited()) {
            mDataCategory = Category.NUMERIC_EDITED;
        } else {
            mDataCategory = Category.ERR;
        }
    }

    /**
     * Determine whether or not the picture is alphabetic.
     *
     * @return true if the pic is alphabetic, false otherwise
     */
    private boolean checkAlphabetic() {
        for (int i = 0; i < mPicture.length(); i++) {
            if (mPicture.charAt(i) != 'A') {
                return false;
            }
        }
        return true;
    }

    /**
     * Determine whether or not the picture is numeric
     *
     * @return true if the pic is numeric, false otherwise
     */
    private boolean checkNumeric() {

        int digitCount = 0;

        for (int i = 0; i < mPicture.length(); i++) {
            char ch = mPicture.charAt(i);
            if ("9PSV".indexOf(ch) == -1) {
                return false;
            } else if (ch == '9') {
                digitCount++;
            }
        }
        return (digitCount > 0);
    }

    /**
     * Determine whether or not the picture is alphanumeric.
     *
     * @return true if the pic is alphanumeric, false otherwise
     */
    private boolean checkAlphanumeric() {

        int countA = 0;
        int count9 = 0;
        int len = mPicture.length();

        for (int i = 0; i < len; i++) {
            char ch = mPicture.charAt(i);
            if ("9XA".indexOf(ch) == -1) {
                return false;
            } else if (ch == '9') {
                count9++;
            } else if (ch == 'A') {
                countA++;
            }
        }
        return ((countA != len) && (count9 != len));
    }

    /**
     * Determine whether or not the picture is DBCS.
     *
     * @return true if the pic is DBCS, false otherwise
     */
    private boolean checkDbcs() {

        boolean hasG = false;
        boolean hasB = false;
        boolean hasN = false;

        for (int i = 0; i < mPicture.length(); i++) {
            char ch = mPicture.charAt(i);
            if ("GBN".indexOf(ch) != -1) {
                switch (ch) {
                    case 'G':
                        hasG = true;
                        break;
                    case 'B':
                        hasB = true;
                        break;
                    case 'N':
                        hasN = true;
                        break;
                }
            } else {
                return false;
            }
        }
        return ((!hasN) || (!hasG && !hasB));
    }

    /**
     * Determine whether or not the picture is an external floating-point.
     *
     * @return true if the pic is an external floating-point, false otherwise
     */
    private boolean checkExFloat() {

        if (mPicture.length() < 4) {
            return false;
        }

        int mantissaEndPos = -1;
        int exponentEndPos = -1;
        int exponentDelimitPos = -1;

        for (int i = 0; i < mPicture.length(); i++) {

            char ch = mPicture.charAt(i);

            if (ch == '+' || ch == '-') {
                if (i != 0) {
                    if (mantissaEndPos == -1) {
                        return false;
                    } else if (i != (mantissaEndPos + 2)) {
                        return false;
                    }
                }

            } else if (".V".indexOf(ch) != -1) {
                if (exponentDelimitPos != -1) {
                    return false;
                }
                mantissaEndPos = i;

            } else if (ch == 'E') {
                if (mantissaEndPos == -1) {
                    return false;
                } else if (i != (mantissaEndPos + 1)) {
                    return false;
                }
                exponentDelimitPos = i;

            } else if (ch == '9') {
                if (exponentDelimitPos == -1) {
                    mantissaEndPos = i;
                } else {
                    exponentEndPos = i;
                }

            } else {
                return false;
            }
        }

        if (exponentEndPos == (mPicture.length() - 1)) {
            return (mPicture.charAt(exponentEndPos) == '9' && mPicture.charAt(exponentEndPos - 1) == '9');
        } else {
            return false;
        }

    }

    /**
     * Determine whether or not the picture is a numeric-edited item
     *
     * @return true if the pic is numeric-edited, false otherwise
     */
    private boolean checkNumericEdited() {
        for (int i = 0; i < mPicture.length(); i++) {
            if (mPicture.indexOf("CR") != -1) {
                continue;
            } else if (mPicture.indexOf("DB") != -1) {
                continue;
            } else if ("BPVZ90/,.+-*$".indexOf(mPicture.charAt(i)) == -1) {
                return false;
            }
        }
        return true;
    }

    /**
     * Determine whether or not the picture is a alphanumeric-edited item
     *
     * @return true if the pic is alphanumeric-edited, false otherwise
     */
    private boolean checkAlphanumericEdited() {

        boolean hasA_X = false;
        boolean hasB_0_Slant = false;

        for (int i = 0; i < mPicture.length(); i++) {
            char ch = mPicture.charAt(i);
            if ("AX9B0/".indexOf(ch) != -1) {
                switch (ch) {

                    /* order of cases is significant */

                    /* has A or X */
                    case 'A':
                    case 'X':
                        hasA_X = true;
                        break;

                    /* has B or 0 or slant */
                    case 'B':
                    case '0':
                    case '/':
                        hasB_0_Slant = true;
                }
            } else {
                return false;
            }
        }
        return (hasA_X && hasB_0_Slant);
    }

}
/* EOF $RCSfile: CocoPicture.java,v $ */
