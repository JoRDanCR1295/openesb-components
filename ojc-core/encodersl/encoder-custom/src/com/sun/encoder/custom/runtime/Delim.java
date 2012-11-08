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

package com.sun.encoder.custom.runtime;

/**
 * Interface to represent a single delimiter for input data parsing.
 * A delimiter is a byte sequence that will be matched in input data
 * to find the end of a field.  Each delimiter has a precedence to
 * resolve conflicts where one delimiter is a prefix of another one.
 * Instances of this class are immutable.
 *
 * @author Michael Libourel
 */
public interface Delim {

    /**
     * Constants for the delimiter type.
     */
    public enum Type {
        /**
         * Simple delimiter. Normal delimiters terminate a singleton field,
         * and may separate repetition instances (depending on the field type).
         */
        NORMAL,

        /**
         * Repetition separator. Repetition separator delimiters are only
         * used between repeated instances of the same node.
         */
        REPEAT,

        /**
         * Escape sequence. Escape sequences are skipped in its entirety
         * when matched.
         */
        ESCAPE,

        /**
         * Escape next sequence. Escape next sequences are used to skip the next
         * byte right after them when matched. Both escape sequence and escape
         * next sequence exist to make it possible to embed delimiter byte
         * sequences inside field data.
         */
        ESCAPE_NEXT,

        /**
         * Escape using quotation style.
         */
        QUOT_ESCAPE
    }

    /**
     * Constants for the occurrence of a delimiter.
     * This determines exactly when a delimiter occurs in input and output.
     * The values are numerically ordered by increasing terminality.
     *
     * A delimiter occurrence is considered a separator between two fields
     * on the same level, i.e. after a field occurrence succeeded by the next
     * repetition of the same field or the first occurrence of its successor
     * sibling below its parent.  It is considered a terminator instead if
     * followed by the separator of its parent (this includes end of input,
     * and end of data limit for fixed parents).
     */
    public enum Mode {
        /**
         * Pure separator, only occurs between peer fields.
         */
        AVOID,

        /**
         * May occur in input as terminator, but don't use as a terminator
         * when marshalling.
         */
        ALLOW,

        /**
         * Marshal as terminator, but allow its omission on input before
         * a parent's delimiter.
         */
        FAVOR,

        /**
         * Pure terminator, must occur after field.
         */
        FORCE;

        /**
         * Backward compatible alias for AVOID.
         * @deprecated use AVOID instead.
         */
        @Deprecated
        public static final Mode NEVER = AVOID;

        /**
         * Backward compatible alias for FAVOR.
         * @deprecated use FAVOR instead.
         */
        @Deprecated
        public static final Mode CHEER = FAVOR;
    }

    /**
     * Tests whether delimiter is embedded.
     * Convenience functions for testing mSlot.
     *
     * @return true if embedded, false if constant
     */
    boolean isEmbedded();

    /**
     * Tests whether leading delimiters should be skipped.
     *
     * @return <code>true</code> if should skip,
     *          otherwise <code>false</code>
     */
    boolean isSkipLeading();

    /**
     * Tests whether should collapse multiple consecutive
     * delimiters into one.
     *
     * @return <code>true</code> if should collapse,
     *          otherwise <code>false</code>
     */
    boolean isCollapse();
}
