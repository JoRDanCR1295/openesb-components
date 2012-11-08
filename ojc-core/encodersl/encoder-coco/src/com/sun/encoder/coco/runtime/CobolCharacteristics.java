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
 * @(#)CobolCharacteristics.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.encoder.coco.runtime;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;
import com.sun.encoder.coco.runtime.messages.ErrorManager;
import com.sun.encoder.coco.runtime.messages.Message;
import com.sun.encoder.coco.runtime.messages.MessageCatalog;

/**
 * Characteristics of Cobol items stemming from the clauses and
 * pictures used to describe them.
 *
 * @author Noel Ang, Jun Xu
 */
public class CobolCharacteristics {

    /**
     * Creates an alphabetic, display-usage item description wih all boolean
     * characteristics set to false, and all numerical characteristics set to
     * 0.
     */
    public CobolCharacteristics() {
    }

    /**
     * Retrieve an integral representation of the characteristics. The value
     * returned is suitable for initializing {@link CobolCharacteristics} thru
     * {@link #fromString(String)}.
     * <p/>
     * The generated specification is encoded:
     * <p/>
     * <code>
     *  p ! u ! d,P ! L ! e,B,J ! S,s,t ! R,r
     *
     *  Symbols:
     *      p  picture category (internal object)
     *      u  usage type (internal object)
     *      d  decimal position integer
     *      P  decimal scaling positions integer
     *      L  item size (logical unit, not bytes) integer
     *      B  is-blank-when-zero boolean (1 or 0)
     *      J  is-justified boolean (1 or 0)
     *      S  is-signed boolean (1 or 0)
     *      s  sign-is-leading boolean (set == 1;
     *                  unset == sign-is-trailing == 0)
     *      t  sign-is-separate boolean (1 or 0)
     * </code>
     *
     * @return string repressentation of the characteristics
     */
    @Override
    public String toString() {
        return serialize();
    }

    /**
     * Initialize the characteristics represented by this object using an
     * exported description.
     *
     * <p/>
     * The expected encoding of the specification is:
     * <p/>
     * <code>
     *  p ! u ! d,P ! L ! e,B,J ! S,s,t ! R,r
     *
     *  Symbols:
     *      p  picture category (internal object)
     *      u  usage type (internal object)
     *      d  decimal position integer
     *      P  decimal scaling positions integer
     *      L  item size (logical unit, not bytes) integer
     *      B  is-blank-when-zero boolean (1 or 0)
     *      J  is-justified boolean (1 or 0)
     *      S  is-signed boolean (1 or 0)
     *      s  sign-is-leading boolean (set == 1;
     *                  unset == sign-is-trailing == 0)
     *      t  sign-is-separate boolean (1 or 0)
     * </code>
     * 
     * @param spec Value obtained from a {@link CobolCharacteristics} object
     *             thru {@link #toString()}.
     * 
     * @throws NullPointerException If <code>spec</code> is <code>null</code>
     * @throws IllegalArgumentException if the characteristics description is
     *                                  not well-formed
     */
    public void fromString(String spec) {
        initialize(spec);
    }

    // Used to write out the object state in exported form
    private String serialize() {
        final StringBuffer strbuf = new StringBuffer(STRBUF_INITIAL_SIZE);
        serializeCategory(strbuf);
        strbuf.append("!");
        serializeUsage(strbuf);
        strbuf.append("!");
        serializeScaling(strbuf);
        strbuf.append("!");
        serializeSize(strbuf);
        strbuf.append("!");
        serializeBasicFlags(strbuf);
        strbuf.append("!");
        serializeSignFlags(strbuf);
        return strbuf.toString();
    }

    // Used to initialize/synchronized internal object state to exported specs
    private void initialize(String spec) {
        final StringTokenizer tokr;

        if (spec == null) {
            throw new NullPointerException();
        }

        tokr = new StringTokenizer(spec, "!");
        try {
            initializeCategory(tokr);
            initializeUsage(tokr);
            initializeScaling(tokr);
            initializeSize(tokr);
            initializeBasicFlags(tokr);
            initializeSignFlags(tokr);
            initializeRedefinitionFlags(tokr);
        } catch (Exception e) {
            Message msg = MessageCatalog.getMessage("CCCD4005");
            mErrorMgr.log(ErrorManager.Severity.ERROR,
                    e,
                    msg.formatText(new Object[]{e.getLocalizedMessage()}));
        }
    }

    private void initializeCategory(StringTokenizer tokr) {
        if (!tokr.hasMoreTokens()) {
            throw new IllegalArgumentException("Category unreadable");
        }

        String categoryToken = tokr.nextToken();
        Integer category;
        if ((category = PIC_MAP.get(categoryToken)) == null) {
            throw new IllegalArgumentException(
                    "Invalid category information; scalar: " + categoryToken);
        }

        mPicture = category.intValue();
    }

    private void serializeCategory(StringBuffer buf) {
        buf.append(PIC_ARRAY[mPicture]);
    }

    private void initializeUsage(StringTokenizer tokr) {
        if (!tokr.hasMoreTokens()) {
            throw new IllegalArgumentException("Usage unreadable");
        }

        String usageToken = tokr.nextToken();
        Integer usage;
        if ((usage = USAGE_MAP.get(usageToken)) == null) {
            throw new IllegalArgumentException(
                    "Invalid usage information; scalar: " + usageToken);
        }

        mUsage = usage;
    }

    private void serializeUsage(StringBuffer buf) {
        buf.append(USAGE_ARRAY[mUsage]);
    }

    private void initializeScaling(StringTokenizer tokr) {
        if (!tokr.hasMoreTokens()) {
            throw new IllegalArgumentException("Scaling unreadable");
        }

        StringTokenizer tupleTokr;
        String tuple;

        tuple = tokr.nextToken();
        tupleTokr = new StringTokenizer(tuple, ","); // no i18n
        if (tupleTokr.countTokens() < 2) {
            throw new IllegalArgumentException("Scaling 2-tuple unreadable");
        }

        // scale
        Integer val = new Integer(tupleTokr.nextToken()); // may raise IAE
        mScale = val.intValue();

        // decimal scaling positions
        val = new Integer(tupleTokr.nextToken()); // may raise IAE
        mScalingPositions = val.intValue();
    }

    private void serializeScaling(StringBuffer buf) {
        buf.append(mScale);
        buf.append(',');
        buf.append(mScalingPositions);
    }

    private void initializeSize(StringTokenizer tokr) {
        if (!tokr.hasMoreTokens()) {
            throw new IllegalArgumentException("Size unreadable");
        }

        Integer val = new Integer(tokr.nextToken()); // may raise IAE
        mSize = val.intValue();
    }

    private void serializeSize(StringBuffer buf) {
        buf.append(mSize);
    }

    private void initializeBasicFlags(StringTokenizer tokr) {
        if (!tokr.hasMoreTokens()) {
            throw new IllegalArgumentException("Basic flags unreadable");
        }

        StringTokenizer flagTokr;

        flagTokr = new StringTokenizer(tokr.nextToken(), ","); // no i18n
        if (flagTokr.countTokens() < 3) {
            throw new IllegalArgumentException("Basic flag tuple unreadable");
        }
        mIsBlankWhenZero = flagToBoolean(flagTokr.nextToken());
        mIsJustified = flagToBoolean(flagTokr.nextToken());
    }

    private void serializeBasicFlags(StringBuffer buf) {
        buf.append(booleanToFlag(mIsBlankWhenZero));
        buf.append(',');
        buf.append(booleanToFlag(mIsJustified));
    }

    private void initializeSignFlags(StringTokenizer tokr) {
        if (!tokr.hasMoreTokens()) {
            throw new IllegalArgumentException("Sign flags unreadable");
        }

        StringTokenizer signTokr;

        signTokr = new StringTokenizer(tokr.nextToken(), ","); // no i18n
        if (signTokr.countTokens() < 3) {
            throw new IllegalArgumentException("Sign flag tuple unreadable");
        }
        mIsSigned = flagToBoolean(signTokr.nextToken());
        mIsSignLeading = flagToBoolean(signTokr.nextToken());
        mIsSignSeparate = flagToBoolean(signTokr.nextToken());
    }

    private void serializeSignFlags(StringBuffer buf) {
        buf.append(booleanToFlag(mIsSigned));
        buf.append(',');
        buf.append(booleanToFlag(mIsSignLeading));
        buf.append(',');
        buf.append(booleanToFlag(mIsSignSeparate));
    }

    private void initializeRedefinitionFlags(StringTokenizer tokr) {
        if (!tokr.hasMoreTokens()) {
            throw new IllegalArgumentException(
                    "Redefinition flags unreadable");
        }

        StringTokenizer redefTokr;

        redefTokr = new StringTokenizer(tokr.nextToken(), ","); // no i18n
        if (redefTokr.countTokens() < 2) {
            throw new IllegalArgumentException(
                    "Redefinition flag tuple unreadable");
        }
    }

    private boolean flagToBoolean(String val) {
        // ESR 104580
        // It appears the following String comparison
        // is very expensive since this function is
        // called many times by other functions.
        // Switch to the optimized code. 
        //if ("0".equals(val)) { // no i18n
        //    return Boolean.FALSE.booleanValue();
        //}
        //else if ("1".equals(val)) { // no i18n
        //    return Boolean.TRUE.booleanValue();
        //}

        switch (val.charAt(0)) {
            case '0':
                return false;
            case '1':
                return true;
            default:
                throw new IllegalArgumentException("Invalid flag value " + val);
        }
    }

    private char booleanToFlag(boolean bool) {
        if (bool) {
            return '1'; // no i18n
        } else {
            return '0'; // no i18n
        }
    }

    public void setPicCategory(int category) {
        if (category < 0 || category >= PIC_ARRAY.length) {
            throw new ArrayIndexOutOfBoundsException(category);
        }
        mPicture = category;
    }

    public int getPicCategory() {
        return mPicture;
    }

    public void setUsage(int usage) {
        if (usage < 0 || usage >= USAGE_ARRAY.length) {
            throw new ArrayIndexOutOfBoundsException(usage);
        }
        mUsage = usage;
    }

    public int getUsage() {
        return mUsage;
    }

    public String descUsage() {
        switch (mUsage) {
            case USAGE_DEGENERATE:
                return "DEGENERATE";
            case USAGE_BINARY:
                return "BINARY";
            case USAGE_COMP:
                return "COMP";
            case USAGE_COMP4:
                return "COMP4";
            case USAGE_PACKED:
                return "PACKED";
            case USAGE_COMP3:
                return "COMP3";
            case USAGE_COMP5:
                return "COMP5";
            case USAGE_COMP1:
                return "COMP1";
            case USAGE_COMP2:
                return "COMP2";
            case USAGE_DISPLAY:
                return "DISPLAY";
            case USAGE_DISPLAY1:
                return "DISPLAY1";
            case USAGE_INDEX:
                return "INDEX";
            default:
                return "<UNKNOWN USAGE>";
        }
    }

    public String descCategory() {
        switch (mPicture) {
            case PIC_DEGENERATE:
                return "DEGENERATE";
            case PIC_ALPHA:
                return "ALPHA";
            case PIC_ALPHANUM:
                return "ALPHANUM";
            case PIC_NUM:
                return "NUM";
            case PIC_NUME:
                return "NUME";
            case PIC_DBCS:
                return "DBCS";
            case PIC_EXFLOAT:
                return "EXFLOAT";
            default:
                return "<UNKNOWN CATEGORY>";
        }
    }

    public String descUsageCategory() {
        String desc = descUsage();
        if (mUsage == USAGE_DISPLAY || mUsage == USAGE_DISPLAY1) {
            desc += "-" + descCategory();
        }
        return desc;
    }

    public void setDecimalPosition(int pos) {
        pos = Math.max(0, pos);
        mScale = pos;
    }

    public int getDecimalPosition() {
        return mScale;
    }

    public void setDecimalScalingPositions(int posses) {
        posses = Math.max(0, posses);
        mScalingPositions = posses;
    }

    public int getDecimalScalingPositions() {
        return mScalingPositions;
    }

    public void setSize(int size) {
        size = Math.max(0, size);
        mSize = size;
    }

    public int getSize() {
        return mSize;
    }

    public void setJustified(boolean val) {
        mIsJustified = val;
    }

    public boolean isJustified() {
        return mIsJustified;
    }

    public void setSignSeparate(boolean val) {
        mIsSignSeparate = val;
    }

    public boolean isSignSeparate() {
        return mIsSignSeparate;
    }

    public void setSignLeading(boolean val) {
        mIsSignLeading = val;
    }

    public boolean isSignLeading() {
        return mIsSignLeading;
    }

    public void setSigned(boolean val) {
        mIsSigned = val;
    }

    public boolean isSigned() {
        return mIsSigned;
    }

    public void setBlankWhenZero(boolean val) {
        mIsBlankWhenZero = val;
    }

    public boolean isBlankWhenZero() {
        return mIsBlankWhenZero;
    }
    private static final int STRBUF_INITIAL_SIZE = 80;
    // Bit mask for obtaining picture category information
    public static final int PIC_DEGENERATE = 0;
    public static final int PIC_ALPHA = 1;
    public static final int PIC_ALPHANUM = 2;
    public static final int PIC_ALPHANUME = 3;
    public static final int PIC_NUM = 4;
    public static final int PIC_NUME = 5;
    public static final int PIC_DBCS = 6;
    public static final int PIC_EXFLOAT = 7;
    public static final int MAX_PIC_VALUE = 7;

    // Bit mask for obtaining usage information
    public static final int USAGE_DEGENERATE = 0;
    public static final int USAGE_DISPLAY = 1;
    public static final int USAGE_BINARY = 2;
    public static final int USAGE_COMP = 3;
    public static final int USAGE_COMP4 = 4;
    public static final int USAGE_PACKED = 5;
    public static final int USAGE_COMP3 = 6;
    public static final int USAGE_COMP5 = 7;
    public static final int USAGE_COMP1 = 8;
    public static final int USAGE_COMP2 = 9;
    public static final int USAGE_DISPLAY1 = 10;
    public static final int USAGE_INDEX = 11;
    public static final int MAX_USAGE_VALUE = 11;
    private static Map<String, Integer> PIC_MAP;
    private static String[] PIC_ARRAY = new String[MAX_PIC_VALUE + 1];
    private static Map<String, Integer> USAGE_MAP;
    private static String[] USAGE_ARRAY = new String[MAX_USAGE_VALUE + 1];
    private static final String DEGENERATE_PIC = "Degenerate.PIC"; // no i18n
    private static final String ALPHA_PIC = "Alphabetic"; // no i18n
    private static final String ALPHANUM_PIC = "Alphanumeric"; // no i18n
    private static final String ALPHANUME_PIC = "Alphanumeric-edited"; // no i18n
    private static final String NUM_PIC = "Numeric"; // no i18n
    private static final String NUME_PIC = "Numeric-edited"; // no i18n
    private static final String DBCS_PIC = "DBCS"; // no i18n
    private static final String EXFLOAT_PIC = "External floating point"; // no i18n


    static {
        PIC_MAP = new HashMap<String, Integer>();
        PIC_MAP.put(DEGENERATE_PIC, new Integer(PIC_DEGENERATE));
        PIC_MAP.put(ALPHA_PIC, new Integer(PIC_ALPHA));
        PIC_MAP.put(ALPHANUM_PIC, new Integer(PIC_ALPHANUM));
        PIC_MAP.put(ALPHANUME_PIC, new Integer(PIC_ALPHANUME));
        PIC_MAP.put(NUM_PIC, new Integer(PIC_NUM));
        PIC_MAP.put(NUME_PIC, new Integer(PIC_NUME));
        PIC_MAP.put(DBCS_PIC, new Integer(PIC_DBCS));
        PIC_MAP.put(EXFLOAT_PIC, new Integer(PIC_EXFLOAT));
        PIC_MAP = Collections.unmodifiableMap(PIC_MAP);

        PIC_ARRAY[PIC_DEGENERATE] = DEGENERATE_PIC;
        PIC_ARRAY[PIC_ALPHA] = ALPHA_PIC;
        PIC_ARRAY[PIC_ALPHANUM] = ALPHANUM_PIC;
        PIC_ARRAY[PIC_ALPHANUME] = ALPHANUME_PIC;
        PIC_ARRAY[PIC_NUM] = NUM_PIC;
        PIC_ARRAY[PIC_NUME] = NUME_PIC;
        PIC_ARRAY[PIC_DBCS] = DBCS_PIC;
        PIC_ARRAY[PIC_EXFLOAT] = EXFLOAT_PIC;
    }
    private static final String DEGENERATE_USAGE = "Degenerate.USAGE"; // no i18n
    private static final String DISPLAY_USAGE = "Display"; // no i18n
    private static final String BINARY_USAGE = "Binary"; // no i18n
    private static final String COMP_USAGE = "Comp";
    private static final String COMP4_USAGE = "Comp4";
    private static final String PACKED_USAGE = "Packed"; // no i18n
    private static final String COMP3_USAGE = "Comp3";
    private static final String COMP5_USAGE = "Comp5"; // no i18n
    private static final String COMP1_USAGE = "Comp1"; // no i18n
    private static final String COMP2_USAGE = "Comp2"; // no i18n
    private static final String DISPLAY1_USAGE = "Display1"; // no i18n
    private static final String INDEX_USAGE = "Index"; // no i18n


    static {
        USAGE_MAP = new HashMap<String, Integer>();
        USAGE_MAP.put(DEGENERATE_USAGE, new Integer(USAGE_DEGENERATE));
        USAGE_MAP.put(DISPLAY_USAGE, new Integer(USAGE_DISPLAY));
        USAGE_MAP.put(BINARY_USAGE, new Integer(USAGE_BINARY));
        USAGE_MAP.put(COMP_USAGE, new Integer(USAGE_COMP));
        USAGE_MAP.put(COMP4_USAGE, new Integer(USAGE_COMP4));
        USAGE_MAP.put(PACKED_USAGE, new Integer(USAGE_PACKED));
        USAGE_MAP.put(COMP3_USAGE, new Integer(USAGE_COMP3));
        USAGE_MAP.put(COMP5_USAGE, new Integer(USAGE_COMP5));
        USAGE_MAP.put(COMP1_USAGE, new Integer(USAGE_COMP1));
        USAGE_MAP.put(COMP2_USAGE, new Integer(USAGE_COMP2));
        USAGE_MAP.put(DISPLAY1_USAGE, new Integer(USAGE_DISPLAY1));
        USAGE_MAP.put(INDEX_USAGE, new Integer(USAGE_INDEX));
        USAGE_MAP = Collections.unmodifiableMap(USAGE_MAP);

        USAGE_ARRAY[USAGE_DEGENERATE] = DEGENERATE_USAGE;
        USAGE_ARRAY[USAGE_DISPLAY] = DISPLAY_USAGE;
        USAGE_ARRAY[USAGE_BINARY] = BINARY_USAGE;
        USAGE_ARRAY[USAGE_COMP] = COMP_USAGE;
        USAGE_ARRAY[USAGE_COMP4] = COMP4_USAGE;
        USAGE_ARRAY[USAGE_PACKED] = PACKED_USAGE;
        USAGE_ARRAY[USAGE_COMP3] = COMP3_USAGE;
        USAGE_ARRAY[USAGE_COMP5] = COMP5_USAGE;
        USAGE_ARRAY[USAGE_COMP1] = COMP1_USAGE;
        USAGE_ARRAY[USAGE_COMP2] = COMP2_USAGE;
        USAGE_ARRAY[USAGE_DISPLAY1] = DISPLAY1_USAGE;
        USAGE_ARRAY[USAGE_INDEX] = INDEX_USAGE;
    }
    private final ErrorManager mErrorMgr =
            ErrorManager.getManager("STC.eWay.converter.COBOLCopybook." + CobolCharacteristics.class.getName());
    private int mUsage = USAGE_DISPLAY;
    private int mPicture = PIC_ALPHA;
    private int mSize;
    private int mScale;
    private int mScalingPositions;
    private boolean mIsSigned;
    private boolean mIsSignLeading;
    private boolean mIsSignSeparate;
    private boolean mIsBlankWhenZero;
    private boolean mIsJustified;
}

// FINIS $RCSfile: CobolCharacteristics.java,v $
