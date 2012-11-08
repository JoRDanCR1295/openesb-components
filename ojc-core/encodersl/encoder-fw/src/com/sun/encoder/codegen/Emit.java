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
 * @(#)Emit.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.codegen;

import java.io.IOException;
import java.io.Writer;

/**
 * Class to provide an output stream with indentation.
 * It maintains the current indentation as state.
 * Part of the eGate 4.5.1 JCS, ported to eGate 5.0.
 *
 * This class builds on a text writer supplied by the user, and is used
 * to write lines to that writer with nested indentation.  Indentation
 * is defined by a number: output lines will be indented this many
 * spaces from the left margin by default.  We use tabs instead of spaces
 * where possible to conserve file space.
 * This class provides the line termination; text passed to emit()
 * and other methods should not contain newline characters.
 *
 * @author Michael Libourel
 * @version 
 */
public class Emit {
    // General debugging.
    public boolean debug = false; //NYI: EgateConfig.getFlag("Emit.debug", false);

    private int mTabSize; // assumed tabstop size in output
    private int mDentStep; // indentation step size in output
    private IOException mErr = null; // pending writer error
    private Writer mOut = null; // current output stream

    /**
     * Current indentation level.  Output lines will be indented this many
     * spaces from the left margin.
     */
    private int mDentSize = 0;

    /**
     * Constructs from given writer and indentation parameters.
     * The first indentation parameter is the assumed tabstop size; if set
     * to 0, then the indentation will not use tab (ASCII code 9), else we
     * try to replace the leftmost spaces by tabs to save space.
     * The second indentation parameter is the step size; we add this many
     * spaces for each extra indentation nesting level.
     * The initial indentation level is zero (no indentation).
     *
     * @param writer  output stream
     * @param tabs  assumed tabstop size
     * @param dent  indentation step size
     */
    public Emit (Writer writer, int tabs, int dent) {
        mTabSize = tabs;
        mDentStep = dent;
        mOut = writer;
    }

    /**
     * Constructs from given writer, with defaulted indentation parameters.
     * The defaults are obtained from the EgateConfig flags "Emit.tabs"
     * for the assumed tabstop size, and "Emit.dent" for the number of
     * spaces to add for each nesting level.
     *
     * @param writer  output stream
     * @see com.sun.encoder.util.EgateConfig
     */
    public Emit (Writer writer) {
        this(writer,
            8, //NYI: EgateConfig.getInteger("Emit.tabs", 8),
            4); //NYI: EgateConfig.getInteger("Emit.dent", 4));
    }

    /**
     * Creates with the same tabstop and indentation step size as the
     * given instance, but with a new writer.
     *
     * @param writer  output stream
     * @return an Emit instance with the same tabstop and indentation step size 
     * as the given instance, but with a new writer.
     */
    public Emit like (Writer writer) {
        return new Emit(writer, mTabSize, mDentStep);
    }

    /**
     * Explicitly (re-)sets the writer.  We need this for constructors in
     * derived classes.  The writer is flushed before the change.  Any error
     * caught and preserved until the close() call on the Emit class.
     * @param writer a writer.
     */
    public void setOut (Writer writer) {
        if (mOut != null) {
            try {
                mOut.flush();
            } catch (IOException io) { mErr = io; }
        }
        mOut = writer;
        setBegin(true);
    }

    /**
     * Resets the current indentation to zero (no indetation).
     */
    public void redent () { redent(0); }

    /**
     * Resets the current indentation to the given number of spaces.
     *
     * @param at  number of spaces (non-negative)
     */
    public void redent (int at) {
        if (at < 0) {
            throw new IllegalArgumentException("indentation must be positive");
        }
        mDentSize = at;
        mDent = null;
    }

    /**
     * Retrieves the current indentation level.
     *
     * @return the number of spaces
     */
    public int isdent () { return mDentSize; }

    // Increments/decrements the current indentation level.
    public void indent () { mDentSize += mDentStep; mDent = null; }
    public void undent () { mDentSize -= mDentStep; mDent = null; }

    /**
     * A cached string, representing the current indentation prefix.
     */
    private String mDent = null;

    // Flag: at beginning of line?
    private boolean mBegin = true;

    /**
     * Emits a line to the current output, with no indentation at all.
     *
     * @param text  the line to emit
     */
    public void emi0 (String text) {
        if (mOut == null) {
            throw new RuntimeException("Emit: use after close()?");
        }
        try {
            if (debug) { System.out.println("[ emit: print <" + text + "> ]"); }
            mOut.write(text);
            mOut.write('\n');
            setBegin(true);
        } catch (IOException io) { mErr = io; }
    }

    /**
     * Emits a text string to the current output.  If this is the start
     * of a line, indent the text with the current indentation.
     * This method does not terminate the line.
     *
     * @param text  the string to emit
     */
    public void part (String text) {
        if (mOut == null) {
            throw new RuntimeException("Emit: use after close()?");
        }

        // Create and cache the indentation prefix.
        if (mDent == null) {
            if (debug) { System.out.println("[ emit: recomput indent ]"); }
            StringBuffer b = new StringBuffer();
            int space = mDentSize;
            if (mTabSize > 0) {
                while (space >= mTabSize) {
                    b.append('\t');
                    space -= mTabSize;
                }
            }
            while (space-- > 0) {
                b.append(' ');
            }
            mDent = b.toString();
        }
        try {
            if (mBegin) {
                if (debug) { System.out.println("[ emit: write indent ]"); }
                mOut.write(mDent);
                setBegin(false);
            }
            if (debug) { System.out.println("[ emit: write <" + text + "> ]"); }
            mOut.write(text);
        } catch (IOException io) { mErr = io; }
    }

    /**
     * Emits a line with the current indentation.
     * This method terminates the line.
     *
     * @param text  the line to emit
     */
    public void emit (String text) {
        part(text);
        try {
            if (debug) { System.out.println("[ emit: newline ]"); }
            mOut.write('\n');
        } catch (IOException io) { mErr = io; }
        setBegin(true);
    }

    /**
     * Emits an empty line to the current output.
     */
    public void emit () {
        emi0("");
    }

    /**
     * Emits a line, with the current indentation plus one.
     * Convenient abbreviation since we need this combination so often.
     *
     * @param text  the line to emit
     */
    public void emi1 (String text) {
        indent();
        emit(text);
        undent();
    }

    /**
     * Emits a line, then increments the indentation.
     * This is useful to emit the beginning of an indented block.
     *
     * @param text  the line to emit
     */
    public void down (String text) {
        emit(text);
        indent();
    }

    /**
     * Decrements the current indentation, and then emits a line.
     * This is useful to emit the end of an indented block.
     *
     * @param text  the line to emit
     */
    public void done (String text) {
        undent();
        emit(text);
    }

    /**
     * Flush the writer output, if buffered.
     * Should be called before the caller fiddles with the writer
     * outside of the Emit instance that uses the writer.
     * @throws IOException IOException occurred.
     */
    public void flush ()
        throws IOException {
        if (mOut != null) { mOut.flush(); }
    }

    /**
     * Frees all resources, and reports any pending errors.
     * We postpone all error handling until this point, to avoid having
     * to deal with exceptions at all the calls to Emit methods.
     * @throws IOException IOException occurred.
     */
    public void close ()
        throws IOException {
        if (mErr != null) { throw mErr; }
        if (mOut != null) { mOut.close(); mOut = null; }
    }

    /**
     * Sets the begin flag to the given value.
     * This flag determines if the next line needs indentation.
     *
     * @param val  the flag value
     */
    private void setBegin (boolean val) {
        mBegin = val;
    }
}
