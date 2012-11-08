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

import java.util.Arrays;
//import static com.sun.encoder.hl7.runtime.provider.SemLevel.*;

public final class Delimiter {

    static final char DEFAULT_SEG_DELIM = '\r';
    static final char DEFAULT_FIELD_SEP = '|';
    static final char DEFAULT_COMPO_SEP = '^';
    static final char DEFAULT_REPET_SEP = '~';
    static final char DEFAULT_ESCAPE = '\\';
    static final char DEFAULT_SUBCOMPO_SEP = '&';

    public static class Type {

        /**
         * Delimiter type constants.
         *
         * !!!**** Attention: if any of the following values get changed or new
         * values are added, please also modify the mDelimType2Level array in
         * UnmarshalAdaptor class. Also, please make sure they are exponents of 2
         * and do NOT change the order of these values.
         *
         * DELIM_NOT_READ - indicates that the value of the field is very long, a
         * delimiter is not read before the input buffer boundary is reached.
         *
         * @see UnmarshalAdaptor#mDelimType2Level
         */
        public static final int DELIM_NOT_READ = 1;
        public static final int SEG_TERM = 2;
        public static final int FIELD_SEP = 4;
        public static final int REPET_SEP = 8;
        public static final int COMPO_SEP = 16;
        public static final int SUBCOMPO_SEP = 32;
        /**
         * Escape char is not a delimiter, but we need this value for
         * performance optimization
         */ 
        public static final int ESCAPE_CHAR = 64;
        /**
         * special delimiter state constants.
         */
        static final int DELIM_UNKNOWN = -1;
        static final int MULTIPLE_HIT = -2;

        public static String getDelimiterTypeDesc(int i) {
            switch (i) {
                case SEG_TERM:
                    return "SEG_TERM";
                case FIELD_SEP:
                    return "FIELD_SEP";
                case REPET_SEP:
                    return "REPET_SEP";
                case COMPO_SEP:
                    return "COMPO_SEP";
                case SUBCOMPO_SEP:
                    return "SUBCOMPO_SEP";
                case ESCAPE_CHAR:
                    return "ESCAPE_CHAR";
                case DELIM_UNKNOWN:
                    return "DELIM_UNKNOWN";
                case MULTIPLE_HIT:
                    return "MULTIPLE_HIT";
                default:
                    return "";
            }
        }

    }
    
    /**
     * Qualified delimiters mask.  It indicates what the valid tokens used to
     * terminate the components are at each semantic level. For example, at
     * FIELD semantic level, the following are valid tokens to terminate the
     * components (fields):
     * (1) DELIM_NOT_READ token (which indicates a delimiter is not read before
     * the input buffer boundary is reached.  The next action should be keeping
     * reading next token and cancatenate all data read until reach a delimiter)
     * (2) SEG_TERM ('\r')
     * (3) FIELD_SEP ('|')
     * (4) REPET_SEP, i.e. repeating field seperator ('~')
     *
     * @see Type#DELIM_NOT_READ
     */
    static int[] mDelimQualiMask = new int[SemLevel.MAX_SEMANTIC_LEVELS];
    static {
        mDelimQualiMask[SemLevel.FIELD]
                = Type.DELIM_NOT_READ | Type.SEG_TERM | Type.FIELD_SEP | Type.REPET_SEP;
        mDelimQualiMask[SemLevel.COMPONENT]
                = mDelimQualiMask[SemLevel.FIELD] | Type.COMPO_SEP;
        mDelimQualiMask[SemLevel.SUBCOMPONENT]
                = mDelimQualiMask[SemLevel.COMPONENT] | Type.SUBCOMPO_SEP;
    }

    // terminator for each semantic level
    static int[] mDelimEndMask = new int[SemLevel.MAX_SEMANTIC_LEVELS];
    static{
        mDelimEndMask[SemLevel.SEGMENT] = Type.SEG_TERM;
        mDelimEndMask[SemLevel.FIELD]
                = mDelimEndMask[SemLevel.SEGMENT] | Type.FIELD_SEP;
        mDelimEndMask[SemLevel.COMPONENT]
                = mDelimEndMask[SemLevel.FIELD] | Type.REPET_SEP | Type.COMPO_SEP;
        mDelimEndMask[SemLevel.SUBCOMPONENT]
                = mDelimEndMask[SemLevel.COMPONENT] | Type.SUBCOMPO_SEP;
    }

    /**
     * map between delimiter type and semantic level.
     * @see Type#SEG_TERM
     * @see SemLevel#SEGMENT
     * @see SemLevel#FIELD
     * @see SemLevel#COMPONENT
     * @see SemLevel#SUBCOMPONENT
     */
    static int[] mDelimType2Level = new int[Type.SUBCOMPO_SEP + 1];
    static {
        Arrays.fill(mDelimType2Level, -1);
        mDelimType2Level[Type.SEG_TERM] = SemLevel.SEGMENT;
        mDelimType2Level[Type.FIELD_SEP] = SemLevel.FIELD;
        mDelimType2Level[Type.REPET_SEP] = SemLevel.FIELD;
        mDelimType2Level[Type.COMPO_SEP] = SemLevel.COMPONENT;
        mDelimType2Level[Type.SUBCOMPO_SEP] = SemLevel.SUBCOMPONENT;
    }


}
