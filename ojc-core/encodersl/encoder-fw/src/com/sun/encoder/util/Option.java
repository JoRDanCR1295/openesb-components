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
 * @(#)Option.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.util;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Class to implement normal Unix-style getopt(). Supports single-letter
 * options, optional arguments, "--" as option list terminator, and multiple
 * options in a single argument.
 *
 * @author Michael Libourel
 * @version 
 */
public class Option {
    /**
     * Value returned by getOpt() to indicate the end of the options.
     */
    public static final int EOF = -1;

    Logger mLog = Logger.getLogger(getClass().getName());

    /**
     * The argument of the current option.
     */
    private String mOptArg = null;

    private String mArgs[]; // the arguments to process
    private boolean mEnd; // reached end of arguments?
    private int mOptInd; // index of current argument
    private int mOptPos; // offset into current argument string
    private String mValid;

    /**
     * Constructs from a set of input arguments, plus a string describing the
     * legal one-letter options. In the "valid" string, each legal optional
     * letter must be present (in any order), optionally followed by a ":" to
     * indicate that it takes an argument.
     *
     * @param args  the command line arguments
     * @param valid  a string of option letters
     */
    public Option(String args[], String valid) {
        if (args == null) {
            throw new NullPointerException("no arguments");
        }
        if (valid == null) {
            throw new NullPointerException("no valid options");
        }
        mArgs = args;
        mValid = valid;
        mOptInd = 0;
        mOptPos = 0;
        mEnd = false;
    }

    /**
     * Returns next option letter found, or EOF for end. If the option takes an
     * argument, it is return via "mOptArg". Options that do not take arguments
     * may be followed immediately by another option letter, and those that do
     * take arguments can take either the remainder of the argument string or
     * the next argument. So, assuming a "valid" string "ab:c", the following
     * are equivalent: -a -b hello -c -a -bhello -c -abhello -c -ab hello -c
     *
     * @return a letter value (char) from "valid", or EOF.
     *
     */
    public int getOpt() {
        if (mEnd || (mOptInd >= mArgs.length)) {
            return EOF;
        }

        // There's something left.
        int len = mArgs[mOptInd].length();
        if (len <= mOptPos) {
            mOptInd++;
            mOptPos = 0;
            return getOpt();
        }
        if (mOptPos == 0) {
            // New argument.
            if (len == 0) {
                return EOF;
            }
            if (mArgs[mOptInd].charAt(0) == '-') {
                // Argument is option.
                if ((len == 1)
                    || ((len == 2) && (mArgs[mOptInd].charAt(1) == '-'))) {
                    // Final option: solitary "-" or "--".
                    if (mLog.isLoggable(Level.FINE)) {
                        mLog.log(Level.FINE, "[ option end ]");
                    }
                    mEnd = true;
                    mOptInd++;
                    return EOF;
                }
                mOptPos = 1;
            } else {
                // Not an option.
                return EOF;
            }
        }

        // Consume one option character.
        char c = mArgs[mOptInd].charAt(mOptPos);
        int  i = mValid.indexOf(c);
        mOptPos++;

        if (i < 0) {
            StringBuffer sb =
                new StringBuffer(
                    "unknown option '-" + c + "' (in argument " + mOptInd
                    + "), valid options are:");

            for (int j = 0; j < mValid.length(); j++) {
                char v = mValid.charAt(j);
                sb.append((v != ':')
                    ? (" -" + v)
                    : "#");
            }

            throw new RuntimeException(sb.toString());
        }

        if (((i + 1) < mValid.length()) && (mValid.charAt(i + 1) == ':')) {
            // Option requires parameter.
            if (mOptPos < len) {
                // Parameter is remainder of argument: -xPar
                mOptArg = mArgs[mOptInd].substring(mOptPos, len);
                mOptInd++;
                mOptPos = 0;
            } else {
                // Parameter is next argument: -x Par
                if ((mOptInd + 1) >= mArgs.length) {
                    throw new RuntimeException("missing final parameter");
                }
                mOptArg = mArgs[mOptInd + 1];
                mOptInd += 2;
                mOptPos = 0;
            }
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, "[ option '-" + c + "' <" + mOptArg + "> ]");
            }
        } else {
            // No parameter.
            mOptArg = null;
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, "[ option '-" + c + "' ]");
            }
        }
        return c;
    }

    /**
     * When getOpt() finds an option that takes an argument, the argument is
     * made available here. If called for an option without an argument, we
     * complain.
     *
     * @return the option argument
     *
     */
    public String getOptArg() {
        if (mOptArg == null) {
            throw new RuntimeException("can't get argument for this option");
        }
        return mOptArg;
    }

    /**
     * Gets options index of next argument.
     *
     * @return the index, range 0...N-1
     */
    public int getOptInd() {
        return mOptInd;
    }
}
