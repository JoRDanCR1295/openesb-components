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
 * @(#)CocoParser.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.coco.model;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.List;
import java.util.ListIterator;
import com.sun.encoder.coco.runtime.messages.ErrorManager;
import com.sun.encoder.coco.runtime.messages.Message;
import com.sun.encoder.coco.runtime.messages.MessageCatalog;

/**
 * Reads Cobol Copybook files and creates a data model
 *
 * @author  Noel Ang
 *
 */
public class CocoParser {

    /**
     * Length of Cobol source lines
     */
    public static final int SOURCE_LINE_LENGTH = 82;
    private static Set cSourceAreas;

    private ArrayList mCols;
    private ArrayList mRows;
    private CocoLexer mLexer;
    private CocoDataModel mModel;
    private CocoDescriptionEntry mCurrentEntry;
    private CocoDescriptionEntry mLastEntry;
    private int mCol;
    private int mRow;
    private boolean mReservedWordCheck = true;
    private final ErrorManager mErrorMgr =
            ErrorManager.getManager("STC.eWay.converter.COBOLCopybook."
                                    + getClass().getName());


    static {
        String pkg = CocoParser.class.getPackage().getName();

        HashSet areas = new HashSet();
        areas.add(CobolSourceSequenceArea.getArea());
        areas.add(CobolSourceIndicatorArea.getArea());
        areas.add(CobolSourceAreaA.getArea());
        areas.add(CobolSourceAreaB.getArea());
        cSourceAreas = Collections.unmodifiableSet(areas);
    }

    /**
     * Create a parser.
     *
     * @param  lexer CocoLexer object to use as the parser's input source
     */
    public CocoParser(CocoLexer lexer) {
        mLexer = lexer;
        mModel = new CocoDataModel();
        mCol = 1;
        mRow = 1;
        mCols = new ArrayList();
        mRows = new ArrayList();
    }

    /**
     * Parse Copybook data and create a data model.
     *
     * @throws CocoParseException
     * @throws java.io.IOException if an I/O error occurs
     */
    public CocoDataModel parse() throws CocoParseException, IOException {
        createModel();
        // resolve OCCURS with DEPENDING ON <data-name> etc
        mModel.validate();
        mModel.resolveOccurs();
        // Name mangling/resolution suspended.
        // First, this is not a parser concern, so it should be addressed elsewhere.
        // Second, COCOCO relaunch support requires the name mangling process to
        // modify its behaviour based on whether or not a relaunch is underway.
        // Name collision handling will be handled where and when it matters:
        // during OTD creation.
        //mModel.makeBaseNamesUnique();
        return mModel;
    }

    /**
     * Parse Copybook data and populate data model.  If the supplied model is
     * null, a new model will be allocated instead.
     *
     * @param  model Data model to fill with information
     * @throws CocoParseException
     * @throws java.io.IOException if an I/O error occurs
     */
    public CocoDataModel parse(CocoDataModel model)
            throws CocoParseException, IOException {

        if (model != null) {
            mModel = model;
        }
        return parse();
    }

    /**
     * Disable reserved word checking for item names. By default, reserved word
     * checking is enabled.  The consequence of disabling it is that the parser
     * will not be able to parse item descriptions without names
     * (implied 'FILLER' items).
     *
     * @param disable <code>true</code> to disable reserved word checking.
     */
    public void disableItemNameReservedWordChecking(boolean disable) {
        mReservedWordCheck = !disable;
    }

    /**
     * Create the data model from parsed Cobol Copybook input.
     *
     * @throws CocoParseException
     * @throws java.io.IOException if an I/O error occurs
     * @see CocoDataModel
     */
    protected void createModel() throws CocoParseException, IOException {
        processEntries();
    }

    /**
     * Process parsed entries to make the data model.
     *
     * @see CocoDataModel
     * @see CocoDescriptionEntry
     */
    private void processEntries() throws CocoParseException, IOException {

        CocoDescriptionEntry entry = null;
        do {
            entry = getEntry();
            if (entry != null) {
                if (entry.getLevel() != -1) {
                    mLastEntry = entry;

                    /* special case: don't add 88 entries */
                    if (entry.getLevel() == 88) {
                        continue;
                    }

                    try {
                        mModel.addEntry(entry);
                    } catch (IllegalArgumentException iae) {
                        logAndThrow("CCCB4201",
                                new Object[]{iae.getLocalizedMessage()},
                                ErrorManager.Severity.ERROR,
                                iae,
                                null);
                    }
                }
            }
        } while (entry != null);
    }

    /**
     * Read one item description from the input source, and create a
     * CocoDescriptionEntry object for it.
     *
     * @return a CocoDescriptionEntry object, or null if there is no available
     *         input.  This method also has the side-effect of designating
     *         the returned object as the "current" object for this parser
     *         object.
     * @throws CocoParseException if the input cannot be parsed successfully
     * @throws java.io.IOException if an I/O error occurs
     */
    private CocoDescriptionEntry getEntry()
            throws CocoParseException,
            IOException {
        CocoDescriptionEntry entry = null;
        if (skipSequenceArea()) {
            entry = mCurrentEntry = new CocoDescriptionEntry();
            if (parseIndicatorArea(entry)) {
                // EOL
                consumeEOL();
            } else {
                // indicator already parsed
                boolean isEOLReached = parseAreaA(entry);
                if (isEOLReached) {
                    consumeEOL();
                }
                if (!isEOLReached || entry.isContinuation()) {
                    // either a blank but continue line or a line that has B area
                    parseAreaB(entry);
                    skipLine();
                }
            }
        } else {
            // no token following - but check EOL and EOF
            // an entry to be skipped
            CocoToken token = getNextToken();
            ungetToken(token);
            if (token != null) {
                if (token.isEOL()) {
                    // return a dummy entry so getEntries() will continue
                    entry = new CocoDescriptionEntry();
                    // consume it
                    consumeEOL();
                } else if (token.isEOF()) {
                    // THE END
                    entry = null;
                }
            } else {
                // unlikely
            }
        }
        return entry;
    }

    private void consumeEOL() throws IOException {
        CocoToken token = getNextToken();
        if (token != null && token.isEOL()) {
            // consume EOL
            updatePosition(token);
        } else {
            ungetToken(token);
        }
    }

    /**
     * Process the Indicator Area of the current Cobol source line.
     *
     * @param  entry Description entry to initialize/populate with information
     *         from Indicator Area.
     * @return true if EOL is reached, or false otherwise.
     * @throws CocoParseException if a parsing error occurs
     * @throws java.io.IOException if an I/O error occurs
     */
    private boolean parseIndicatorArea(CocoDescriptionEntry entry)
            throws CocoParseException, IOException {
        CocoToken token = getNextToken();

        if (!isInIndicatorArea(token)) {
            return false;
        } else {
            if (token.isEOL()) {
                ungetToken(token);
                return true;
            }

            /* Ignore entries with no indicators */
            if (isSpace(token)) {
                skipIndicatorArea();
                return false;
            }
            /* Ignore comments by consuming rest of area */
            if (isComment(token)) {
                entry.setComment(true);
                skipIndicatorArea();
                return false;
            }

            String value = token.getStringValue().toUpperCase();

            /* Ignore debugging lines */
            if (value.equals(CocoLanguage.DEBUGGING_INDICATOR)) {
                entry.setIndicatorValue(value);
                return false;
            }
            /* Process continuation entries */
            if (value.equals(CocoLanguage.HYPHEN)) {
                /*
                 * This is not a conclusive indication; primary checking
                 * is done during parsing of Area A
                 */
                entry.setContinuation(true);
                return false;
            }
            /* Unrecognized indicator */
            logAndThrow("CCCB4202", new Object[]{token.getStringValue()},
                    ErrorManager.Severity.ERROR, null, null);
            return false;
        }
    }

    /**
     * Process the Area A of the current Cobol source line.
     *
     * @param  entry Description entry to initialize/populate with information
     *         from Area A
     * @return true if EOL is reached, or false otherwise.
     * @throws CocoParseException if a parsing error occurs
     * @throws java.io.IOException if an I/O error occurs
     */
    private boolean parseAreaA(CocoDescriptionEntry entry)
            throws CocoParseException, IOException {
        boolean eodEncountered = false;

        if (entry.isComment()) {
            skipAreaA();
            return eodEncountered;
        }

        CocoToken token = null;

        while (true) {
            token = getNextToken();

            if (token == null || token.isEOF()) {
                ungetToken(token);
                break;
            }

            // scan until the first non blank char - break for level number parsing
            if (!CocoLanguage.SPACE.equals(token.getStringValue())) {
                ungetToken(token);
                break;
            } else {
                if (token.isEOL()) {
                    ungetToken(token);
                    return true;
                }
                if (!isInAreaA(token) && !isPartiallyInAreaA(token)) {
                    ungetToken(token);
                    break;
                }
            }
        }

        /* per 89392 a EOL is possible in area A, B, and indicator area
         */

        if (token == null || token.isEOF()) {
            ungetToken(token);
            return false;
        }

        /*
         * If Area A is blank, depending on what I find in Area B later,
         * it's either a blank line, a continuation line, or a free-standing
         * line whose level number is in Area B.
         *
         * Right now, assume this is a continuation line.
         *
         * If Area A is NOT blank, but the Indicator Area marked this as a
         * continuation line, it's an error.
         */
        if (!isInAreaA(token) && !isPartiallyInAreaA(token)) {
            entry.setContinuation(true);
            return false;
        } else if (entry.isContinuation()) {
            logAndThrow("CCCB4204", null, ErrorManager.Severity.ERROR, null, token);
        }

        parseLevelNumber(entry);

        skipSpacesOrPeriods(true);

        token = getNextToken();
        if (token != null && !token.isEOL()) {
            ungetToken(token);
            parseDataName(entry);
        }
        return false;
    }

    /**
     * Process the Area B of the current Cobol source line.
     *
     * @param  entry Description entry to initialize/populate with information
     *         from Area B
     * @throws CocoParseException if a parsing error occurs
     * @throws java.io.IOException if an I/O error occurs
     */
    private void parseAreaB(CocoDescriptionEntry entry)
            throws CocoParseException, IOException {

        if (entry.isComment()) {
            skipAreaB();
            return;
        }

        /* The entry being processed may be a blank line */
        skipOptionalSpaces(false);
        CocoToken token = getNextTokenAreaB(true, true);

        ungetToken(token);

        if (token == null || token.isEOF() || !isInAreaB(token)) {
            entry.setContinuation(false);
            return;
        }

        if ( entry.getLevel() == -1
                && ( entry.getName() == null || entry.getName().trim().length() == 0 )
                && token != null
                && token.isEOL() )
            return;
    
        /* I never found a level number while scanning Area A */
        if (entry.getLevel() == -1) {
            entry.setContinuation(false);
            parseLevelNumber(entry);
        }

        /* a level number and data name already been parsed before parseAreaB() */
        if (entry.getLevel() > 0
                && entry.getName() != null
                && entry.getName().length() > 0) {
            // already got a data name in A or A across B
        } else {
            parseDataName(entry);
        }

        parseRedefinesClause(entry);

        boolean gotBlankClause = false;
        boolean gotExternalClause = false;
        boolean gotGlobalClause = false;
        boolean gotJustifiedClause = false;
        boolean gotOccursClause = false;
        boolean gotPictureClause = false;
        boolean gotSignClause = false;
        boolean gotSynchronizedClause = false;
        boolean gotUsageClause = false;
        boolean gotValueClause = false;
        boolean gotDateFormatClause = false;
        boolean done = false;
        int prevCount = 0;

        while (!done) {
            int passCount = 0;

            if (!gotBlankClause) {
                gotBlankClause = parseBlankWhenZeroClause(entry);
                passCount += (gotBlankClause ? 1 : 0);
            }

            if (!gotExternalClause) {
                gotExternalClause = parseExternalClause(entry);
                passCount += (gotExternalClause ? 1 : 0);
            }

            if (!gotGlobalClause) {
                gotGlobalClause = parseGlobalClause(entry);
                passCount += (gotGlobalClause ? 1 : 0);
            }

            if (!gotJustifiedClause) {
                gotJustifiedClause = parseJustifiedClause(entry);
                passCount += (gotJustifiedClause ? 1 : 0);
            }

            if (!gotOccursClause) {
                gotOccursClause = parseOccursClause(entry); //also indexed by...
                passCount += (gotOccursClause ? 1 : 0);
            }

            if (!gotPictureClause) {
                gotPictureClause = parsePictureClause(entry);
                passCount += (gotPictureClause ? 1 : 0);
            }

            if (!gotSignClause) {
                gotSignClause = parseSignClause(entry);
                passCount += (gotSignClause ? 1 : 0);
            }

            if (!gotSynchronizedClause) {
                gotSynchronizedClause = parseSynchronizedClause(entry);
                passCount += (gotSynchronizedClause ? 1 : 0);
            }

            if (!gotUsageClause) {
                gotUsageClause = parseUsageClause(entry);
                passCount += (gotUsageClause ? 1 : 0);
            }

            if (!gotValueClause) {
                gotValueClause = parseValueClause(entry);
                passCount += (gotValueClause ? 1 : 0);
            }

            if (!gotDateFormatClause) {
                gotDateFormatClause = parseDateFormatClause(entry);
                passCount += (gotDateFormatClause ? 1 : 0);
            }

            skipOptionalSpaces(true);
            token = getNextTokenAreaB(false, true);
            ungetToken(token);
            if (token == null || token.isEOF()) {
                logAndThrow("CCCB4203", new Object[]{entry.getName()},
                        ErrorManager.Severity.ERROR, null, null);
            }

            String value = token.getStringValue();
            done = (value.equals(CocoLanguage.PERIOD)
                    || value.equals(CocoLanguage.PERIOD_S));
            if (!done) {
                if (passCount <= prevCount) {
                    logAndThrow("CCCB4205", new Object[]{value},
                            ErrorManager.Severity.ERROR, null, token, entry);
                }
            }
            prevCount = passCount;
        }
        skipLine();
    }

    /**
     * Read parser input for a level number and assign it to a description entry.
     *
     * @param  entry CocoDescriptionEntry to initialize with the level number
     * @throws CocoParseException if a parsing error occurs
     * @throws java.io.IOException if an I/O error occurs
     */
    private void parseLevelNumber(CocoDescriptionEntry entry)
            throws CocoParseException, IOException {

        CobolSourceArea area = getCurrentArea();
        CocoToken token = null;

        skipOptionalSpaces(true);

        if (area instanceof CobolSourceAreaB) {
            token = getNextTokenAreaB(false, true);
        } else {
            token = getNextToken();
        }
        if (token != null && !token.isEOF()) {
            if (token.getType() == CocoTokenTypes.NUM_TOKEN) {
                int level = -1;
                try {
                    level = Integer.parseInt(token.getStringValue());
                } catch (NumberFormatException nfe) {
                    logAndThrow("CCCB4108", new Object[]{
                                entry.getName(),
                                String.valueOf(token.getStringValue())},
                            ErrorManager.Severity.ERROR, nfe, token);
                }
                try {
                    entry.setLevel(level);
                } catch (IllegalArgumentException e) {
                    logAndThrow("CCCB4108", new Object[]{
                                entry.getName(),
                                String.valueOf(token.getStringValue())},
                            ErrorManager.Severity.ERROR, e, token);
                }
            } else {
                logAndThrow("CCCB4108", new Object[]{
                            entry.getName(),
                            String.valueOf(token.getStringValue())},
                        ErrorManager.Severity.ERROR, null, token);
            }
        } else {
            logAndThrow("CCCB4206", new Object[]{entry.getName()},
                    ErrorManager.Severity.ERROR, null, null);
        }
    }

    /**
     * Attempt to read parser input for a data name, and make it the name of a
     * description entry. If no valid name can be obtained, the name of the
     * description entry is set to null (to nothing).
     *
     * @param  entry CocoDescriptionEntry to initialize with the name
     * @throws CocoParseException if a parsing error occurs
     * @throws java.io.IOException if an I/O error occurs
     */
    private void parseDataName(CocoDescriptionEntry entry)
            throws CocoParseException, IOException {

        try {
            String name = parseCobolWord(mReservedWordCheck);
            entry.setName(name);
            entry.setOriginalName(name);
            skipClauseSeparator();
        } catch (ReservedWordEncounteredException rwe) {
            // since the data name could be FILLER or empty (blank)
            // we should let the parsing continue
            if (mReservedWordCheck) {
                entry.setReservedWordAfterLevel(rwe);
            //throw rwe;
            }
            // the following code is unlikely be reached
            // but put it here in case - fall back to old logic;
            String name = formWord("FILLER", false);
            if (name != null && !"".equals(name.trim())) {
                entry.setName(name);
                entry.setOriginalName(name);
            } else {
                entry.setName("BLANK");
                entry.setOriginalName("");
            }
        } catch (CocoParseException cpe) {
            String name = formWord("FILLER", false);
            if (name != null && !"".equals(name.trim())) {
                entry.setName(name);
                entry.setOriginalName(name);
            } else {
                entry.setName("BLANK");
                entry.setOriginalName("");
            }
        }
    }

    /**
     * Process 'Redefines' clause from the parser input, and initialize a
     * description entry with its information. If a Redefines clause is not
     * available, the entry is unchanged.
     *
     * @param  entry CocoDescriptionEntry to initialize with the clause information
     * @throws CocoParseException if a parsing error occurs
     * @throws java.io.IOException if an I/O error occurs
     */
    private void parseRedefinesClause(CocoDescriptionEntry entry)
            throws CocoParseException, IOException {

        if (expectWord("REDEFINES")) {
            skipOptionalSpaces(true);
            // if EOL encountered - should continue to next B area
            // we must be in area B - so should call getNextTokenAreaB to jump to the
            // next point;
            CocoToken token = this.getNextTokenAreaB(false, true);
            if (token == null || token.isEOF()) {
                // error
                logAndThrow("CCCB4203",
                        new Object[]{entry.getInfo()},
                        ErrorManager.Severity.ERROR,
                        null,
                        null);
            }
            if (token.isEOL()) {
                // go to next line and scan for a word as redefined target
                token = this.getNextTokenAreaB(false, true);
                if (token == null || token.isEOF()) {
                    logAndThrow("CCCB4203",
                            new Object[]{entry.getInfo()},
                            ErrorManager.Severity.ERROR,
                            null,
                            null);
                }
            }

            ungetToken(token);

            String name = parseCobolWord(mReservedWordCheck);

            CocoDescriptionEntry redefineTarget = mModel.findRedefineTarget(entry, name);

            if (redefineTarget == null) {
                logAndThrow("CCCB4207", new Object[]{
                            name,
                            entry.getName()},
                        ErrorManager.Severity.ERROR, null, null);
            } else if (redefineTarget == entry) {
                logAndThrow("CCCB4114", new Object[]{entry.getName()},
                        ErrorManager.Severity.ERROR, null, null);
            } else if (redefineTarget.getLevel() != entry.getLevel()) {
                logAndThrow("CCCB4116", new Object[]{
                            entry.getName(),
                            redefineTarget.getName(),
                            String.valueOf(entry.getLevel()),
                            String.valueOf(redefineTarget.getLevel())},
                        ErrorManager.Severity.ERROR, null, null);
            } else {
                try {
                    entry.setRedefinedTarget(redefineTarget);
                } catch (IllegalArgumentException e) {
                } catch (IllegalStateException e) {
                    // TODO - implement
                }
            }
            skipClauseSeparator();
        }
    }

    /**
     * Process 'Blank When Zero' clause from the parser input, and initialize a
     * description entry with its information.  If a Blank clause is not available,
     * the entry is also updated with that information.
     *
     * @param  entry CocoDescriptionEntry to initialize with the clause information
     * @return true if a 'blank when zero' clause was consumed, false otherwise.
     * @throws CocoParseException if a parsing error occurs
     * @throws java.io.IOException if an I/O error occurs
     */
    private boolean parseBlankWhenZeroClause(CocoDescriptionEntry entry)
            throws CocoParseException, IOException {

        boolean gotClause = true;

        if (expectWord("BLANK")) {
            expectWord("WHEN");
            if (!expectWord("ZERO")) {
                if (!expectWord("ZEROS")) {
                    if (!expectWord("ZEROES")) {
                        logAndThrow("CCCB4208", null, ErrorManager.Severity.ERROR, null, null);
                    } else {
                        entry.setBlankWhenZero(true);
                        skipClauseSeparator();
                    }
                } else {
                    entry.setBlankWhenZero(true);
                    skipClauseSeparator();
                }
            } else {
                entry.setBlankWhenZero(true);
                skipClauseSeparator();
            }
        } else {
            entry.setBlankWhenZero(false);
            gotClause = false;
        }
        return gotClause;
    }

    /**
     * Process 'External' clause from the parser input, and initialize a
     * description entry with its information. If a External clause is not
     * available, the entry is unchanged.
     *
     * @param  entry CocoDescriptionEntry to initialize with the clause information
     * @return true if a 'external' clause was consumed, false otherwise.
     * @throws CocoParseException if a parsing error occurs
     * @throws java.io.IOException if an I/O error occurs
     */
    private boolean parseExternalClause(CocoDescriptionEntry entry)
            throws CocoParseException, IOException {

        boolean gotClause = false;

        /* EXTERNAL clause not supported (ignored) */
        if (expectWord("EXTERNAL")) {
            skipClauseSeparator();
            gotClause = true;
        }
        return gotClause;
    }

    /**
     * Process 'Global' clause from the parser input, and initialize a
     * description entry with its information. If a Global clause is not
     * available, the entry is unchanged.
     *
     * @param  entry CocoDescriptionEntry to initialize with the clause information
     * @return true if a 'global' clause was consumed, false otherwise.
     * @throws CocoParseException if a parsing error occurs
     * @throws java.io.IOException if an I/O error occurs
     */
    private boolean parseGlobalClause(CocoDescriptionEntry entry)
            throws CocoParseException, IOException {

        boolean gotClause = false;

        /* GLOBAL clause not supported (ignored) */
        if (expectWord("GLOBAL")) {
            skipClauseSeparator();
            gotClause = true;
        }
        return gotClause;
    }

    /**
     * Process 'Justified' clause from the parser input, and initialize a
     * description entry with its information.
     *
     * @param  entry CocoDescriptionEntry to initialize with the clause information
     * @return true if a 'justified' clause was consumed, false otherwise.
     * @throws CocoParseException if a parsing error occurs
     * @throws java.io.IOException if an I/O error occurs
     */
    private boolean parseJustifiedClause(CocoDescriptionEntry entry)
            throws CocoParseException, IOException {

        boolean gotClause = false;

        if (expectWord("JUSTIFIED") || expectWord("JUST")) {
            expectWord("RIGHT");
            entry.setJustified(true);
            skipClauseSeparator();
            gotClause = true;
        } else {
            entry.setJustified(false);
        }
        return gotClause;
    }

    /**
     * Process 'Occurs' clause from the parser input, and initialize a
     * description entry with its information.
     *
     * @param  entry CocoDescriptionEntry to initialize with the clause information
     * @return true if a 'occurs' clause was consumed, false otherwise.
     * @throws CocoParseException if a parsing error occurs
     * @throws java.io.IOException if an I/O error occurs
     */
    private boolean parseOccursClause(CocoDescriptionEntry entry)
            throws CocoParseException, IOException {

        boolean gotClause = false;

        if (expectWord("OCCURS")) {

            /* OCCURS cannot occur for level 01, 66, 77, or 88 entries */
            int level = entry.getLevel();
            if (level == 1 || level == 66 || level == 77 || level == 88) {
                logAndThrow("CCCB4209", new Object[]{
                            entry.getName()
                        }, ErrorManager.Severity.ERROR, null, null);
            }

            CocoToken token = null;

            if (skipOptionalSpaces(true)) {
                token = this.getNextToken();
            }

            int count1 = 0;
            int count2 = 0;


            /* "OCCURS n"... */
            token = getNextTokenAreaB(false, true);

            if (token == null || token.isEOF()) {
                logAndThrow("CCCB4203",
                        new Object[]{entry.getInfo()},
                        ErrorManager.Severity.ERROR,
                        null,
                        null);
            }

            if (token.isEOL()) {
                // next token after EOL
                token = getNextTokenAreaB(false, true);
                if (token == null || token.isEOF()) {
                    logAndThrow("CCCB4203",
                            new Object[]{entry.getInfo()},
                            ErrorManager.Severity.ERROR,
                            null,
                            null);
                }
            }

            if (token.getType() == CocoTokenTypes.NUM_TOKEN) {
                try {
                    count1 = Integer.parseInt(token.getStringValue());
                    count2 = count1;
                } catch (NumberFormatException nfe) {
                    logAndThrow("CCCB4210",
                            new Object[]{token.getStringValue()},
                            ErrorManager.Severity.ERROR, nfe, token);
                }
            }

            /* "OCCURS n to m..." */
            if (expectWord("TO")) {  // OCCURS count1 TO...
                if (skipOptionalSpaces(true)) {
                    token = this.getNextToken();
                }
                token = getNextTokenAreaB(false, true);
                if (token == null || token.isEOF()) {
                    logAndThrow("CCCB4203",
                            new Object[]{entry.getInfo()},
                            ErrorManager.Severity.ERROR,
                            null,
                            null);
                }
                if (token.isEOL()) {
                    token = getNextTokenAreaB(false, true);
                    if (token == null || token.isEOF()) {
                        logAndThrow("CCCB4203", new Object[]{entry.getName()},
                                ErrorManager.Severity.ERROR, null, token);
                    }
                }

                if (token.getType() == CocoTokenTypes.NUM_TOKEN) {
                    try {
                        int count = Integer.parseInt(token.getStringValue());
                        count2 = count;
                    } catch (NumberFormatException nfe) {
                        logAndThrow("CCCB4210",
                                new Object[]{token.getStringValue()},
                                ErrorManager.Severity.ERROR, nfe, token);
                    }
                }
                if (count1 < 0 || count2 <= count1) {
                    logAndThrow("CCCB4118", new Object[]{
                                String.valueOf(count1),
                                String.valueOf(count2)},
                            ErrorManager.Severity.ERROR, null, token);
                }
            } else if (count1 < 1) {
                logAndThrow("CCCB4117", new Object[]{String.valueOf(count1)},
                        ErrorManager.Severity.ERROR, null, token);
            }

            expectWord("TIMES");

            /* "OCCURS" ... DEPENDING ON */
            if (expectWord("DEPENDING")) {

                // When the "DEPENDS ON" phrase is encountered
                // completing a "Foo OCCURS x TIMES" statement,
                // changes the semantic from "Foo occurs x times"
                // to "Foo can occur up to x times".  In other words,
                // minimum recurrence count is no longer equal to the
                // maximum recurrence count; it becomes 0.
                count1 = 0;

                expectWord("ON");
                if (skipOptionalSpaces(true)) {
                    token = this.getNextToken();
                }
                token = getNextTokenAreaB(false, true);
                if (token == null || token.isEOF()) {
                    logAndThrow("CCCB4203", new Object[]{entry.getName()},
                            ErrorManager.Severity.ERROR, null, token);
                }
                if (token.isEOL()) {
                    // next token after EOL
                    token = getNextTokenAreaB(false, true);
                    if (token == null || token.isEOF()) {
                        logAndThrow("CCCB4203", new Object[]{entry.getName()},
                                ErrorManager.Severity.ERROR, null, token);
                    }
                }

                ungetToken(token);

                String entryName = parseCobolWord(mReservedWordCheck);

                if (entryName == null || entryName.trim().length() == 0) {
                    String item1 = entry.getLevel() + " " + entry.getName();
                    // expecting a data name after DEPENDING ON
                    token = getNextTokenAreaB(false, true);
                    logAndThrow("CCCB4224", new Object[]{
                                "data name",
                                "DEPENDING ON",
                                item1},
                                ErrorManager.Severity.ERROR, null, token);
                }

                // the data-name for DEPENDING ON can be qualified
                // e.g., F1 OF L1 OF L2 OF R2 etc

                List qualifiers = null;
                while (expectWord("OF")) {
                    if (skipOptionalSpaces(true)) {
                        token = this.getNextToken();
                    }
                    token = getNextTokenAreaB(false, true);
                    if (token == null || token.isEOF()) {
                        logAndThrow("CCCB4203", new Object[]{entry.getName()},
                                ErrorManager.Severity.ERROR, null, token);
                    }
                    if (token.isEOL()) {
                        // next token after EOL
                        token = getNextTokenAreaB(false, true);
                        if (token == null || token.isEOF()) {
                            logAndThrow("CCCB4203",
                                    new Object[]{entry.getName()},
                                    ErrorManager.Severity.ERROR, null, token);
                        }
                    }
                    ungetToken(token);
                    String qualifierName = parseCobolWord(mReservedWordCheck);
                    if (qualifierName == null || qualifierName.trim().length() == 0) {
                        token = getNextTokenAreaB(false, true);
                        logAndThrow("CCCB4224", new Object[]{
                                    "data name",
                                    "OF",
                                    entry.getInfo()},
                                ErrorManager.Severity.ERROR, null, token);
                    }
                    if (qualifiers == null) {
                        qualifiers = new ArrayList(1);
                    }
                    qualifiers.add(qualifierName);
                }

                entry.setOccursResolveLater(count1, count2, qualifiers, entryName);
                mModel.addOccurs(entry);

            } else if (count1 == count2) {
                entry.setOccurs(count1);
            } else {
                token = getNextTokenAreaB(false, true);
                logAndThrow("CCCB4215", new Object[]{entry.getName()},
                        ErrorManager.Severity.ERROR, null, token);
            }

            /* ignore rest of OCCURS clause (ASCENDING KEY, INDEX BY phrases et al) */
            if (expectWord("ASCENDING") || expectWord("DESCENDING")) {
                expectWord("KEY");
                expectWord("IS");

                skipOptionalSpaces(true);

                token = getNextTokenAreaB(false, true);
                if (token == null || token.isEOF()) {
                    logAndThrow("CCCB4203", new Object[]{entry.getName()},
                            ErrorManager.Severity.ERROR, null, token);
                }

                if (token.isEOL()) {
                    // next token after EOL
                    token = getNextTokenAreaB(false, true);
                    if (token == null || token.isEOF()) {
                        logAndThrow("CCCB4203", new Object[]{entry.getName()},
                                ErrorManager.Severity.ERROR, null, token);
                    }
                }
                ungetToken(token);
                String wd = parseCobolWord(mReservedWordCheck);
                if (wd == null || wd.trim().length() == 0) {
                    token = getNextTokenAreaB(false, true);
                    logAndThrow("CCCB4224", new Object[]{
                                "data name",
                                "ASCENDING or DESCENDINNG [KEY] [IS]",
                                entry.getInfo()},
                            ErrorManager.Severity.ERROR, null, token);
                }
            }

            if (expectWord("INDEXED")) {
                expectWord("BY");

                skipOptionalSpaces(true);

                token = getNextTokenAreaB(false, true);

                if (token == null || token.isEOF()) {
                    logAndThrow("CCCB4203", new Object[]{entry.getName()},
                                ErrorManager.Severity.ERROR, null, token);
                }
                if (token.isEOL()) {
                    // get next token after EOL
                    token = getNextTokenAreaB(false, true);
                    if (token == null || token.isEOF()) {
                        logAndThrow("CCCB4203", new Object[]{entry.getName()},
                                    ErrorManager.Severity.ERROR, null, token);
                    }
                }
                ungetToken(token);

                while (true) {
                    try {
                        // try to parse 1 or more indexed variables
                        // Open-ESB issue 2296
                        String wd = parseCobolWord(mReservedWordCheck);
                        if (wd == null || wd.trim().length() == 0) {
                            token = getNextTokenAreaB(false, true);
                            logAndThrow("CCCB4224", new Object[]{
                                        "data name",
                                        "INDEXED [BY]",
                                        entry.getInfo()},
                                    ErrorManager.Severity.ERROR, null, token);
                        }
                    } catch (CocoParseException e) {
                        break;
                    }
                }
                skipUntilNextClause();
            } else {
                skipClauseSeparator();
            }

            gotClause = true;
        }

        return gotClause;
    }

    /**
     * Consume tokens until a word is encountered that is the start of a
     * recognized clause. USE THIS METHOD ONLY WHEN CURRENT TOKEN POSITION
     * IS IN AREA B, OTHERWISE THE BEHAVIOUR IS UNDEFINED.
     * 
     * @throws CocoParseException if EOD encountered
     * @throws IOException if any I/O error occurs
     */ 
    private void skipUntilNextClause() throws CocoParseException, IOException {
        final List tokenList = new ArrayList();
        final StringBuffer wordBuffer = new StringBuffer();
        String word;
        ListIterator it;
        CocoToken tok;

        try {
            // loop for skipping words
            while(true) {
                tokenList.clear();
                skipOptionalSpaces(true);

                // loop for skipping tokens
                while(true) {
                    tok = getNextTokenAreaB(false, true);
                    if (null == tok) {
                        break;
                    }
            
                    if (tok.getType() == CocoTokenTypes.SEPARATOR_TOKEN) {
                        ungetToken(tok);
                        break;
                    }

                    wordBuffer.append(tok.getStringValue());
                    tokenList.add(0, tok);
                }

                // no word formed
                if (wordBuffer.length() == 0) {
                    break;
                    
                // if word is the start of a clause, then stuff the
                // tokens that formed it, back into the token stream; I'm done
                } else {
                    word = wordBuffer.toString();
                    wordBuffer.delete(0, word.length());

                    if (CocoLanguage.isClauseWord(word)) {
                        it = tokenList.listIterator();
                        while (it.hasNext()) {
                            ungetToken((CocoToken) it.next());
                        }
                        break;
                    }
                }
            }
        } finally {
            tokenList.clear();
            wordBuffer.delete(0, wordBuffer.length());
        }
    }

    /**
     * Process 'Picture'/'Pic' clause from the parser input, and initialize a
     * description entry with its information.
     *
     * @param  entry CocoDescriptionEntry to initialize with the clause information
     * @return true if a 'picture' clause was consumed, false otherwise.
     * @throws CocoParseException if a parsing error occurs
     * @throws java.io.IOException if an I/O error occurs
     */
    private boolean parsePictureClause(CocoDescriptionEntry entry)
            throws CocoParseException, IOException {

        boolean gotClause = false;

        if (expectWord("PIC") || expectWord("PICTURE")) {
            expectWord("IS");
            skipOptionalSpaces(true);
            // make sure EOL is skipped
            CocoToken token = getNextTokenAreaB(false, true);

            if (token == null || token.isEOF()) {
                logAndThrow("CCCB4203", new Object[]{entry.getName()},
                        ErrorManager.Severity.ERROR, null, token);
            }

            if (token.isEOL()) {
                // go to next line and scan for a word as redefined target
                token = this.getNextTokenAreaB(false, true);
                if (token == null || token.isEOF()) {
                    // error
                    if (token == null || token.isEOF()) {
                        logAndThrow("CCCB4203", new Object[]{entry.getName()},
                                ErrorManager.Severity.ERROR, null, token);
                    }
                }
            }

            ungetToken(token);
            parsePictureLiteral(entry);
            skipClauseSeparator();
            gotClause = true;
        }
        return gotClause;
    }

    /**
     * Process a Picture clause's character string from the parser input, and
     * initialize a description entry with its information.
     *
     * @param  entry CocoDescriptionEntry to initialize with the clause information
     * @throws CocoParseException if a parsing error occurs
     * @throws java.io.IOException if an I/O error occurs
     */
    private void parsePictureLiteral(CocoDescriptionEntry entry)
            throws CocoParseException, IOException {

        StringBuffer buffer = new StringBuffer();
        boolean foundPart = false;
        skipOptionalSpaces(true);
        do {
            CocoToken token = getNextTokenAreaB(true, true);
            if (token == null || token.isEOF()) {
                break;
            }
            String value = token.getStringValue();
            CocoTokenTypes type = token.getType();
            foundPart =
                    (type == CocoTokenTypes.ALNUM_TOKEN
                    || type == CocoTokenTypes.NUM_TOKEN
                    || value.equals(CocoLanguage.SLANT)
                    || value.equals(CocoLanguage.COMMA)
                    || value.equals(CocoLanguage.PERIOD)
                    || value.equals(CocoLanguage.PLUS)
                    || value.equals(CocoLanguage.HYPHEN)
                    || value.equals(CocoLanguage.ASTERISK)
                    || value.equals(CocoLanguage.LPARENS)
                    || value.equals(CocoLanguage.RPARENS)
                    || CocoLanguage.isCurrencySymbol(value));

            /*
             * If I find a period or comma, in order to be part of the literal,
             * it must be followed immediately one of the following:
             *    - a separator period
             *    - a separator comma (not just a comma)
             *    - a separator semicolon (not just a semicolon)
             */
            if (value.equals(CocoLanguage.PERIOD) || value.equals(CocoLanguage.COMMA)) {
                CocoToken nextToken = getNextTokenAreaB(true, true);
                if (nextToken == null) {
                    foundPart = false;
                } else {
                    String nextValue = nextToken.getStringValue();
                    if (nextValue.equals(CocoLanguage.SPACE)
                            || nextValue.charAt(0) == '\r'
                            || nextValue.equals("EOF")) {
                        // added check on "EOF" here, see open-esb issue 2273
                        foundPart = false;
                        ungetToken(nextToken);
                    } else if (nextValue.equals(CocoLanguage.COMMA)
                            || nextValue.equals(CocoLanguage.SEMICOLON)
                            || nextValue.equals(CocoLanguage.PERIOD)) {
                        CocoToken anotherToken = getNextTokenAreaB(true, true);
                        ungetToken(nextToken);
                        if (anotherToken != null) {
                            String anotherValue = anotherToken.getStringValue();
                            ungetToken(anotherToken);
                            foundPart = anotherValue.equals(CocoLanguage.SPACE);
                        }
                    } else {
                        ungetToken(nextToken);
                    }
                }
            }

            if (foundPart) {
                buffer.append(value);
            } else {
                ungetToken(token);
            }
        } while (foundPart);

        String picstr = "";
        try {
            picstr = buffer.toString();
            CocoPicture picture = new CocoPicture(picstr);
            entry.setPicture(picture);
        } catch (IllegalArgumentException iae) {
            logAndThrow("CCCB4218", new Object[]{
                        entry.getName(),
                        picstr,
                        iae.getLocalizedMessage()},
                    ErrorManager.Severity.ERROR, iae, null);
        }
    }

    /**
     * Process 'Sign' clause from the parser input, and initialize a description
     * entry with its information.
     *
     * @param  entry CocoDescriptionEntry to initialize with the clause information
     * @return true if a 'sign' clause was consumed, false otherwise.
     * @throws CocoParseException if a parsing error occurs
     * @throws java.io.IOException if an I/O error occurs
     */
    private boolean parseSignClause(CocoDescriptionEntry entry)
            throws CocoParseException, IOException {

        boolean gotClause = true;

        if (expectWord("SIGN")) {
            expectWord("IS");
            if (expectWord("LEADING")) {
                entry.setSign(CocoSign.LeadingSign);
            } else if (expectWord("TRAILING")) {
                entry.setSign(CocoSign.TrailingSign);
            } else {
                logAndThrow("CCCB4219", new Object[]{entry.getName()},
                        ErrorManager.Severity.ERROR, null, null);
            }
            if (expectWord("SEPARATE")) {
                expectWord("CHARACTER");
                entry.setSeparateSign(true);
            }
            skipClauseSeparator();
        } else if (expectWord("LEADING")) {
            entry.setSign(CocoSign.LeadingSign);
            if (expectWord("SEPARATE")) {
                expectWord("CHARACTER");
                entry.setSeparateSign(true);
            }
            skipClauseSeparator();
        } else if (expectWord("TRAILING")) {
            entry.setSign(CocoSign.TrailingSign);
            if (expectWord("SEPARATE")) {
                expectWord("CHARACTER");
                entry.setSeparateSign(true);
            }
            skipClauseSeparator();
        } else {
            gotClause = false;
        }
        return gotClause;
    }

    /**
     * Process 'Synchronized' clause from the parser input, and initialize a
     * description entry with its information.
     *
     * @param  entry CocoDescriptionEntry to initialize with the clause information
     * @return true if a 'synchronized' clause was consumed, false otherwise.
     * @throws CocoParseException if a parsing error occurs
     * @throws java.io.IOException if an I/O error occurs
     */
    private boolean parseSynchronizedClause(CocoDescriptionEntry entry)
            throws CocoParseException, IOException {

        boolean gotClause = false;

        /* SYNCHRONIZED clause not supported (ignored) */
        if (expectWord("SYNCHRONIZED") || expectWord("SYNC")) {
            if (!expectWord("LEFT")) {
                expectWord("RIGHT");
            }
            skipClauseSeparator();
            gotClause = true;
        }
        return gotClause;
    }

    /**
     * Process 'Usage' clause from the parser input, and initialize a description
     * entry with its information.
     *
     * @param  entry CocoDescriptionEntry to initialize with the clause information
     * @return true if a 'usage' clause was consumed, false otherwise.
     * @throws CocoParseException if a parsing error occurs
     * @throws java.io.IOException if an I/O error occurs
     */
    private boolean parseUsageClause(CocoDescriptionEntry entry)
            throws CocoParseException, IOException {

        boolean gotClause = true;

        /*
         * Unrecognized (ignored) USAGE phrases:
         * POINTER
         * PROCEDURE-POINTER
         * Object Reference
         */

        /*
         * do NOT set a default usage
         *
         * there is an important difference between an implicit and explicit
         * usage; an explicit usage for a group item propagates to its subordinates,
         * whereas an implicit usage does not.
         *
         * if I set the default usage here, I force the propagation
         */
        //entry.setUsage(CocoDescriptionEntry.UsageType.DISPLAY);

        if (expectWord("USAGE")) {
            expectWord("IS");
        }

        try {
            if (expectWord("BINARY")) {
                entry.setUsage(CocoDescriptionEntry.UsageType.BINARY);
            } else if (expectWord("COMP")) {
                entry.setUsage(CocoDescriptionEntry.UsageType.BINARY);
            } else if (expectWord("COMPUTATIONAL")) {
                entry.setUsage(CocoDescriptionEntry.UsageType.BINARY);
            } else if (expectWord("DISPLAY")) {
                entry.setUsage(CocoDescriptionEntry.UsageType.DISPLAY);
            } else if (expectWord("PACKED-DECIMAL")) {
                entry.setUsage(CocoDescriptionEntry.UsageType.PACDEC);
            } else if (expectWord("DISPLAY-1")) {
                entry.setUsage(CocoDescriptionEntry.UsageType.DISPLAY1);
            } else if (expectWord("COMP-1")) {
                entry.setUsage(CocoDescriptionEntry.UsageType.COMP1);
            } else if (expectWord("COMPUTATIONAL-1")) {
                entry.setUsage(CocoDescriptionEntry.UsageType.COMP1);
            } else if (expectWord("COMP-2")) {
                entry.setUsage(CocoDescriptionEntry.UsageType.COMP2);
            } else if (expectWord("COMPUTATIONAL-2")) {
                entry.setUsage(CocoDescriptionEntry.UsageType.COMP2);
            } else if (expectWord("COMP-3")) {
                entry.setUsage(CocoDescriptionEntry.UsageType.COMP3);
            } else if (expectWord("COMPUTATIONAL-3")) {
                entry.setUsage(CocoDescriptionEntry.UsageType.COMP3);
            } else if (expectWord("COMP-4")) {
                entry.setUsage(CocoDescriptionEntry.UsageType.COMP4);
            } else if (expectWord("COMPUTATIONAL-4")) {
                entry.setUsage(CocoDescriptionEntry.UsageType.COMP4);
            } else if (expectWord("COMP-5")) {
                entry.setUsage(CocoDescriptionEntry.UsageType.COMP5);
            } else if (expectWord("COMPUTATIONAL-5")) {
                entry.setUsage(CocoDescriptionEntry.UsageType.COMP5);
            } else if (expectWord("INDEX")) {
                entry.setUsage(CocoDescriptionEntry.UsageType.INDEX);
            } else if (expectWord("POINTER")
                    || expectWord("PROCEDURE-POINTER")) {
                entry.setUsage(CocoDescriptionEntry.UsageType.DISPLAY);
            } else if (expectWord("OBJECT")) {
                if (expectWord("REFERENCE")) {
                    if (expectWord("METACLASS")) {
                        expectWord("OF");
                    }
                    formWord(null, true);
                } else {
                    CocoToken token = getNextToken();
                    ungetToken(token);
                    logAndThrow("CCCB4205",
                            new Object[]{token.getStringValue()},
                            ErrorManager.Severity.ERROR, null, token, entry);
                }

            } else {
                gotClause = false;
            }

            if (gotClause) {
                skipClauseSeparator();
            }

            return gotClause;

        } catch (IllegalArgumentException e) {
            throw new CocoParseException(e.getLocalizedMessage(), null, -1, -1, entry);
        }
    }

    /**
     * Process 'Value' clause from the parser input, and initialize a description
     * entry with its information.
     *
     * @param  entry CocoDescriptionEntry to initialize with the clause information
     * @return true if a 'value' clause was consumed, false otherwise.
     * @throws CocoParseException if a parsing error occurs
     * @throws java.io.IOException if an I/O error occurs
     */
    private boolean parseValueClause(CocoDescriptionEntry entry)
            throws CocoParseException, IOException {

        boolean gotClause = true;

        /* VALUE clause not supported (ignored) */
        if (expectWord("VALUE")) {
            expectWord("IS");
            skipValueClause(entry);
            skipClauseSeparator();
        } else if (expectWord("VALUES")) {
            expectWord("ARE");
            skipValueClause(entry);
            skipClauseSeparator();
        } else {
            gotClause = false;
        }
        return gotClause;
    }

    /**
     * Process 'Date Format' clause from the parser input, and initialize a
     * description entry with its information.
     *
     * @param  entry CocoDescriptionEntry to initialize with the clause information
     * @return true if a 'date format' clause was consumed, false otherwise.
     * @throws CocoParseException if a parsing error occurs
     * @throws java.io.IOException if an I/O error occurs
     */
    private boolean parseDateFormatClause(CocoDescriptionEntry entry)
            throws CocoParseException, IOException {

        boolean gotClause = false;

        /* DATE FORMAT clause not supported (ignored) */
        if (expectWord("DATE")) {
            if (expectWord("FORMAT")) {
                expectWord("IS");
                skipOptionalSpaces(true);
                CocoToken token = this.getNextTokenAreaB(false, true);
                if (token == null || token.isEOF()) {
                    logAndThrow("CCCB4203",
                            new Object[]{entry.getInfo()},
                            ErrorManager.Severity.ERROR,
                            null,
                            null);
                }
                if (!token.isEOL()) {
                    ungetToken(token);
                }
                token = getNextTokenAreaB(false, true);
                if (token.getType() != CocoTokenTypes.ALNUM_TOKEN) {
                    logAndThrow("CCCB4205",
                            new Object[]{token.getStringValue()},
                            ErrorManager.Severity.ERROR, null, token, entry);
                }
                skipClauseSeparator();
                gotClause = true;
            } else {
                CocoToken token = getNextTokenAreaB(true, true);
                if (token != null && !token.isEOF()) {
                    ungetToken(token);
                }
                logAndThrow("CCCB4220", new Object[]{
                            entry.getName(),
                            token.getStringValue()},
                        ErrorManager.Severity.ERROR, null, token);
            }
        }
        return gotClause;
    }

    /**
     * Consumes tokens falling in the Sequence Area. If the next tokens are already
     * outside the Sequence Area, this method leaves the tokens unconsumed and
     * exits.
     *
     * @return true if tokens past the sequence area were found
     * @throws java.io.IOException if an I/O error occurs
     */
    private boolean skipSequenceArea() throws IOException {

        CocoToken token = null;
        boolean haveTokens = false;

        do {
            try {
                token = getNextToken();
                if (token == null || token.isEOF()) {
                    break;
                }
                if (!isInSequenceArea(token)) {
                    if (!token.isEOL()) {
                        haveTokens = true;
                    }
                    ungetToken(token);
                    token = null;
                } else {
                    if (token.isEOL()) {
                        // is in SEQ area but encountered EOL, should unget and bail out to caller
                        ungetToken(token);
                        token = null;
                    }
                }
            } catch (CocoParseException cpe) {
                token = null;
            }

        } while (token != null && !token.isEOF());

        return haveTokens;
    }

    /**
     * Consumes tokens falling in the Indicator Area. If the next tokens are already
     * outside the Indicator Area, this method leaves the tokens unconsumed and
     * exits.
     *
     * @return true if tokens were available to try the operation, or else false.
     * @throws java.io.IOException if an I/O error occurs
     */
    private boolean skipIndicatorArea() throws IOException {
        CocoToken token = null;
        boolean haveTokens = false;
        do {
            try {
                token = getNextToken();
            } catch (CocoParseException cpe) {
                token = null;
            }
            haveTokens |= (token != null && !token.isEOF());
            if (token != null && !token.isEOF()) {
                if (!isInIndicatorArea(token)) {
                    ungetToken(token);
                    token = null;
                }
            }
        } while (token != null && !token.isEOF());
        return haveTokens;
    }

    /**
     * Consumes tokens falling in Area A. If the next tokens are already
     * outside Area A, this method leaves the tokens unconsumed and exits.
     *
     * @return true if tokens were available to try the operation, or else false.
     * @throws java.io.IOException if an I/O error occurs
     */
    private boolean skipAreaA() throws IOException {
        CocoToken token = null;
        boolean haveTokens = false;
        do {
            try {
                token = getNextToken();
            } catch (CocoParseException cpe) {
                token = null;
            }
            haveTokens |= (token != null && !token.isEOF());
            if (token != null && !token.isEOF()) {
                if (!isInAreaA(token)) {
                    ungetToken(token);
                    token = null;
                }
            }
        } while (token != null && !token.isEOF());
        return haveTokens;
    }

    /**
     * Consumes tokens falling in Area B. If the next tokens are already
     * outside Area B, this method leaves the tokens unconsumed and exits.
     *
     * @return true if tokens were available to try the operation, or else false.
     * @throws java.io.IOException if an I/O error occurs
     */
    private boolean skipAreaB() throws IOException {
        CocoToken token = null;
        boolean haveTokens = false;
        do {
            try {
                token = getNextToken();
            } catch (CocoParseException cpe) {
                token = null;
            }
            haveTokens |= (token != null && !token.isEOF());
            if (token != null && !token.isEOF()) {
                if (!isInAreaB(token)) {
                    ungetToken(token);
                    token = null;
                }
            }
        } while (token != null && !token.isEOF());
        return haveTokens;
    }

    /**
     * Fetch a token from the input stream and compare its string value to the
     * given input. If they match, consume the token. If they do not match, put
     * it back in the token stream.  Words are expected to be delimited by spaces
     * (1 or more). Word comparisons are case-insensitive.
     *
     * <p>If this method is called in any area except Area B, it will only fetch
     * tokens for that area (and thus fail if that area has no more tokens). If
     * the method is called while in Area B, and there are no more tokens left
     * for the area, it will keep getting tokens from the following line's Area B
     * with the following capability:</p>
     *
     * <ul>
     * <li>It will examine the next indicator area to skip comments.
     * <li>It will examine the next indicator area and Area A to abort if the next
     * line is a non-continuation line
     * <li>It will examine the next indicator area, and Areas A and B, to skip
     * blank lines
     * </ul>
     *
     * @param  word The value in consideration
     * @return true if the next non-space token's value matches the given word;
     *         false otherwise, including if the given word is null
     * @throws CocoParseException if there are no more tokens
     * @throws java.io.IOException if an I/O error occurs
     */
    private boolean expectWord(String word) throws CocoParseException, IOException {
        boolean gotExpected = false;

        if (word != null) {
            String formedWord = formWord(word, false);
            if (formedWord != null) {
                gotExpected = true;
            }
        }
        return gotExpected;
    }

    /**
     * Fetch tokens from input and form a word.  A Cobol word is a sequence
     * of characters consisting of the hyphen, alphabets, and numbers; a
     * hyphen may not be the first or second character in the word.
     *
     * <p>Special case: colon characters occuring in non-contiguous pairs
     * are treated as part of a word.</p>
     *
     * <p>If the first argument is not null, it is compared case-insensitively to
     * the formed word, and if they do not match, the formed word is returned
     * to the input stream, and the method returns null.</p>
     *
     * <p>If the second argument is set to true, the formed word is checked
     * against the reserved word list, and if a match is found, the formed word
     * is returned to the input stream, and the method throws an exception.</p>
     *
     * @param  word The desired word to form; may be null to cause the method to
     *         return the next word in the token input stream
     * @param  checkReserved Set to true to check the formed word against the
     *         reserved word list
     * @return formed cobol word from input stream if not matched to the given
     *         word with case-insensitive comparison, or null if matched. If
     *         checkReserved is true, then if formed word matches any reserved
     *         word, exception is thrown.
     * @throws CocoParseException if there are no more tokens to process, or the
     *         formed word was checked against the reserved word list and a match
     *         for it was found
     * @throws java.io.IOException if an I/O error occurs
     */
    private String formWord(String word, boolean checkReserved)
            throws CocoParseException, IOException {

        ArrayList undoTokens = new ArrayList();
        ArrayList spaces = new ArrayList();
        CobolSourceArea area = getCurrentArea();
        CocoToken previous_token = null, token = null;
        StringBuffer wordBuf = new StringBuffer();
        boolean gotFirst = false;
        boolean gotStartColon = false;
        boolean wasHyphen = false;

        boolean isEOLReached = skipOptionalSpaces(true, spaces);
        if (isEOLReached) {
            token = getNextToken();
        }
        while (true) {
            area = getCurrentArea();
            if (area == null || area instanceof CobolSourceAreaB) {
                token = getNextTokenAreaB(false, true);
                if ((token == null || token.isEOF()) && wordBuf.length() == 0) {
                    logAndThrow("CCCB4216", null,
                            ErrorManager.Severity.ERROR, null, token);
                }
            } else {
                token = getNextToken();
                if (token != null && !token.isEOF()) {
                    if (area instanceof CobolSourceAreaA
                            && (this.isInAreaA(token) || this.isPartiallyInAreaA(token))) {
                        // a data name in A or across the A and B
                        if ((token == null || token.isEOF())
                                && wordBuf.length() == 0) {
                            logAndThrow("CCCB4216", null,
                                    ErrorManager.Severity.ERROR, null, token);
                        }
                    } else if (!isInArea(token, area)) {
                        ungetToken(token);
                        if (wordBuf.length() == 0) {
                            logAndThrow("CCCB4217", null,
                                    ErrorManager.Severity.ERROR, null, token);
                        }
                    }
                }
            }

            if (token == null || token.isEOF()) {
                logAndThrow("CCCB4216", null,
                        ErrorManager.Severity.ERROR, null, token);
            }

            String value = token.getStringValue();
            /* hyphens can't begin words */
            if (value.equals(CocoLanguage.HYPHEN)) {
                wasHyphen = true;
                if (gotFirst) {
                    wordBuf.append(token.getStringValue());
                    undoTokens.add(token);
                } else {
                    ungetToken(token);
                    break;
                }

            } else if (wasHyphen == gotFirst &&
                    (token.getType() == CocoTokenTypes.ALNUM_TOKEN
                        || token.getType() == CocoTokenTypes.NUM_TOKEN)) {
                gotFirst = true;
                wasHyphen = false;
                wordBuf.append(token.getStringValue());
                previous_token = token;
                undoTokens.add(token);

            /* high-level qualifier tokens (e.g., name:hlq:restofname) */
            } else if (value.equals(CocoLanguage.COLON)) {

                if (gotStartColon) {
                    CocoToken previousToken = (CocoToken) (undoTokens.size() == 0
                            ? null : undoTokens.get(undoTokens.size() - 1));

                    if (previousToken != null && value.equals(previousToken.getStringValue())) {
                        ungetToken(token);
                        break;
                    }
                }

                gotStartColon = !gotStartColon;
                wasHyphen = false;
                wordBuf.append(token.getStringValue());
                undoTokens.add(token);

            } else {

                ungetToken(token);

                /* hyphens can't end words */
                if (wasHyphen) {
                    CocoToken lastToken = (CocoToken) undoTokens.get(undoTokens.size() - 1);
                    ungetToken(lastToken);
                    wordBuf.delete(wordBuf.length() - 1, wordBuf.length());
                }

                /* colons must occur in pairs */
                if (gotStartColon) {
                    if (undoTokens.size() > 0) {
                        for (int i = undoTokens.size() - 1; i >= 0; i--) {
                            CocoToken tok = (CocoToken) undoTokens.get(i);
                            ungetToken(tok);
                        }
                    }
                    if (spaces.size() > 0) {
                        for (int i = spaces.size() - 1; i >= 0; i--) {
                            ungetToken((CocoToken) spaces.get(i));
                        }
                    }
                }
                break;
            }
        }

        String name = (wordBuf.length() == 0 ? null : wordBuf.toString());

        if ((word != null) && (name != null)) {
            if (name.compareToIgnoreCase(word) != 0) {
                name = null;
                int size = undoTokens.size();
                for (int i = size - 1; i >= 0; i--) {
                    CocoToken tok = (CocoToken) undoTokens.remove(i);
                    ungetToken(tok);
                }
                for (int i = spaces.size() - 1; i >= 0; i--) {
                    ungetToken((CocoToken) spaces.get(i));
                }
            }
        }

        if (checkReserved && (name != null)) {
            if (CocoLanguage.isReservedWord(name)) {
                int size = undoTokens.size();
                for (int i = size - 1; i >= 0; i--) {
                    CocoToken tok = (CocoToken) undoTokens.remove(i);
                    ungetToken(tok);
                }
                for (int i = spaces.size() - 1; i >= 0; i--) {
                    ungetToken((CocoToken) spaces.get(i));
                }
                logAndThrow2("CCCB4221", new Object[]{name},
                        ErrorManager.Severity.ERROR, null, previous_token);
            }
        }

        undoTokens.clear();
        spaces.clear();
        return name;
    }

    /**
     * Read parser input for the next word, and return it. If the parameter
     * checkReserved is true, the word is checked against the reserved-word list,
     * and if a match is found an exception is thrown.
     *
     * <p>If this method is invoked when the "current" parser read position
     * is in Area B, continuation lines, if available, are consumed intelligently.
     * If the current read position is anywhere else, this method indiscriminately
     * consumes as much tokens as necessary.</p>
     *
     * @param  checkReserved Set to true to check the next parsed word against
     *         the reserved-word list; set to false to not check against the list
     * @return the next word parsed from the input
     * @throws CocoParseException if no word could be formed, or one was formed but
     *         is a reserved word (and checkReserved was set true)
     * @throws java.io.IOException if an I/O error occurs
     */
    private String parseCobolWord(boolean checkReserved)
            throws CocoParseException, IOException {

        String word = formWord(null, checkReserved);
        if (word == null) {
            logAndThrow("CCCB4217", null,
                    ErrorManager.Severity.ERROR, null, null);
        }
        return word;
    }

    /**
     * Consumes one or more Area B tokens to skip a VALUE clause.  It is assumed
     * that the "current" token stream position is in Cobol Area B, and that the
     * VALUE reserved word has been consumed (and thus the presence of a VALUE
     * clause is confirmed). If the next token in the stream is not in this area.
     * the rest of the current line is discarded, and the method will attempt to
     * take the next token from the next Area B, provided that the next Area B is
     * part of the item entry description being processed (i.e., the next Area B is
     * a continuation line).
     *
     * @throws CocoParseException  if there are no tokens available, or a literal
     *                             cannot be skipped with the available lexemes.
     * @throws java.io.IOException if an I/O error occurs
     */
    private void skipValueClause(CocoDescriptionEntry entry)
            throws CocoParseException,
            IOException {

        CocoToken token;
        boolean gotFirstOperand = false;

        skipOptionalSpaces(true);

        while (true) {
            token = getNextTokenAreaB(false, true);
            if (token == null || token.isEOF()) {
                break;
            }
            if (token.isEOL()) {
                // go to next line and scan for a word as redefined target
                token = getNextTokenAreaB(false, true);
                if (token == null || token.isEOF()) {
                    break;
                }
            }
            ungetToken(token);

            String value = token.getStringValue();

            // ignore non-numeric literals
            if (CocoLanguage.isNonnumericLiteralStart(token.getStringValue())) {
                skipNonnumericLiteral(entry);
                gotFirstOperand = true;
            } else if (value.equals(CocoLanguage.PLUS)
                    || value.equals(CocoLanguage.HYPHEN)
                    || CocoLanguage.isNumericLiteral(value)) {
                // ignore numeric literals
                skipNumericLiteral(entry);
                gotFirstOperand = true;
            } else if (skipFigurativeConstant()) {
                // ignore figurative constants
                gotFirstOperand = true;
            } else if (expectWord("THRU") || expectWord("THROUGH")) {
                skipOptionalSpaces(true);
            } else if (gotFirstOperand) {
                break;
            } else {
                token = mLexer.getNextToken();
                mLexer.ungetToken(token);
                logAndThrow("CCCB4205", new Object[]{token.getStringValue()},
                        ErrorManager.Severity.ERROR, null, token, entry);
            }

            skipClauseSeparator();
        }
    }

    /**
     * Consumes from the parser input one value clause operand separator, which is
     * defined as a space, a separator comma, or a separator semicolon.
     *
     * @return True if a value clause operand separator was found, and skipped,
     *         otherwise false.
     *
     * @throws IOException if an I/O error occurs
     */
    private boolean skipValueOperandSeparator()
            throws IOException {
        CocoToken token = null;
        boolean foundSeparator = false;

        if (null != (token = getNextTokenAreaB(false, false))) {

            // A value clause operand separator is one of the following:
            //
            // a space
            // a separator comma (comma followed by a space)
            // a separator semicolon (semicolon followed by a space)
            //
            // I'm adding separator and semi-colon to the mix because user
            // copybook files don't obey the rules.
            if (CocoLanguage.SPACE.equals(token.getStringValue())) {
                foundSeparator = true;
            } else if (CocoLanguage.COMMA.equals(token.getStringValue())
                    || CocoLanguage.SEMICOLON.equals(token.getStringValue())) {
                CocoToken nextToken;
                if (null != (nextToken = getNextTokenAreaB(false, false))) {
                    foundSeparator = true;
                    if (!CocoLanguage.SPACE.equals(nextToken.getStringValue())) {
                        ungetToken(nextToken);
                    }
                }
            }
        }

        if (!foundSeparator && token != null && !token.isEOF()) {
            ungetToken(token);
        }
        return foundSeparator;
    }

    /**
     * Consumes one or more tokens to skip Cobol numeric literals. A Cobol
     * numeric literal conforms to the pattern:
     *
     * <p><code>  [(+|-)]?[0-9]+((\.[0-9]+)|(\.[0-9]+[Ee][+-][0-9]{2}))?</code></p>
     *
     * @throws CocoParseException if there are no tokens available, or a literal
     *         cannot be skipped with the available lexemes.
     * @throws java.io.IOException if an I/O error occurs
     */
    private void skipNumericLiteral(CocoDescriptionEntry entry) throws CocoParseException, IOException {
        CocoToken token;
        boolean findSign = true;
        boolean findDot = true;
        boolean haveNum = false;

        do {
            if (null == (token = getNextToken())) {
                break;
            }

            String value = token.getStringValue();

            if (findSign) {
                /* it must show up first or not at all */
                findSign = false;
                if (value.equals(CocoLanguage.PLUS)
                        || value.equals(CocoLanguage.HYPHEN)) {
                    continue;
                }
            }

            if (token.getType() == CocoTokenTypes.NUM_TOKEN) {
                haveNum = true;
            } else if (value.equals(CocoLanguage.PERIOD)) {

                /* it must occur only once */
                /* it must not be at the end of the literal */

                if (findDot) {
                    findDot = false;
                    try {
                        CocoToken nextToken = getNextToken();
                        ungetToken(nextToken);
                        if (nextToken.getType() != CocoTokenTypes.NUM_TOKEN) {
                            ungetToken(token);
                            token = null;
                        }
                    } catch (Exception e) {
                        ungetToken(token);
                        token = null;
                    }
                } else {
                    ungetToken(token);
                    token = null;
                }
            } else if (value.equalsIgnoreCase("E")) {
                // next token must be a + or -, followed by two digits
                CocoToken nextToken1;
                CocoToken nextToken2;
                CocoToken nextToken3;

                nextToken1 = getNextToken();
                nextToken2 = getNextToken();
                nextToken3 = getNextToken();

                if (nextToken3.getType() != CocoTokenTypes.NUM_TOKEN) {
                    ungetToken(nextToken3);
                    nextToken3 = mLexer.getNextToken();
                    logAndThrow("CCCB4205",
                            new Object[]{nextToken3.getStringValue()},
                            ErrorManager.Severity.ERROR, null, token, entry);
                }

                if (nextToken2.getType() != CocoTokenTypes.NUM_TOKEN) {
                    ungetToken(nextToken3);
                    ungetToken(nextToken2);
                    nextToken2 = mLexer.getNextToken();
                    logAndThrow("CCCB4205",
                            new Object[]{nextToken2.getStringValue()},
                            ErrorManager.Severity.ERROR, null, token, entry);
                }

                if (!nextToken1.getStringValue().equals(CocoLanguage.PLUS)
                        && !nextToken1.getStringValue().equals(CocoLanguage.HYPHEN)) {
                    ungetToken(nextToken3);
                    ungetToken(nextToken2);
                    ungetToken(nextToken1);
                    nextToken1 = mLexer.getNextToken();
                    logAndThrow("CCCB4205",
                            new Object[]{nextToken1.getStringValue()},
                            ErrorManager.Severity.ERROR, null, token, entry);
                }

                break;
            } else {
                ungetToken(token);
                token = null;
            }
        } while (token != null && !token.isEOF());
    }

    /**
     * Consumes one or more tokens to skip Cobol non-numeric literals. A Cobol
     * numeric literal conforms to the pattern:
     *
     * <p><code>  (") ::character-string:: (") </code>, with embedded
     * quotes represented by two consecutive double quotes (""). </p>
     *
     * @throws CocoParseException if there are no tokens available, or a literal
     *         cannot be skipped with the available lexemes.
     * @throws java.io.IOException if an I/O error occurs
     */
    private void skipNonnumericLiteral(CocoDescriptionEntry entry)
            throws CocoParseException, IOException {
        CocoToken token;
        String value;

        {
            if (null == (token = getNextToken())) {
                return;
            }
            value = token.getStringValue();
        }

        String firstQuote = null;
        String lastQuote = null;
        boolean usesDbcsShiftChars = false;
        boolean gotDbcsShiftClose = false;

        // Determine the opening quote type, to match it with the ending quote.
        if (value.equals(CocoLanguage.QUOTATION)
                || value.equals(CocoLanguage.APOSTROPHE)) {
            firstQuote = value;
        } else if (CocoLanguage.isNonnumericLiteralStart(value)) {
            value = String.valueOf(value.charAt(value.length() > 1 ? 1 : 0));
            if (value.equals(CocoLanguage.QUOTATION)
                    || value.equals(CocoLanguage.APOSTROPHE)) {
                firstQuote = value;
            } else {
                ungetToken(token);
                return;
            }
        } else {
            byte[] bytes = token.getBytesValue();
            if (bytes[0] == 0 && CocoLanguage.isSeparator(bytes[1])) {
                // DBCS shift-out character
                usesDbcsShiftChars = true;
            } else {
                // not a non-numeric literal
                ungetToken(token);
                return;
            }
        }

        // Skip everything until the ending quote is encountered.
        while (true) {
            CocoToken secondToken;
            if (null == (secondToken = getNextToken())) {
                break;
            }

            // shift-out (DBCS) character needs matching shift-in character
            // I don't actually bother matching ins vs outs, as I'm discarding
            // the value I find anyway.
            if (usesDbcsShiftChars) {
                byte[] bytes = secondToken.getBytesValue();
                if (bytes[0] == 0 && CocoLanguage.isSeparator(bytes[1])) {
                    gotDbcsShiftClose = true;
                    break;
                }
            } else {
                // Processing non-DBCS character literal
                value = secondToken.getStringValue();

                // value might be one of those frickin' separator-separator
                // characters, e.g., X' or X" because at the lexer level there
                // is no deterministic way to decide whether to get X or X'...
                if (String.valueOf(value.charAt(value.length() - 1)).equals(firstQuote)) {
                    // I found a matching quote
                    // Is it an ending quote, or an embedded quote?
                    CocoToken thirdToken = getNextToken();
                    if (thirdToken == null
                            || !thirdToken.getStringValue().equals(value)) {
                        lastQuote = firstQuote;
                        if (thirdToken != null) {
                            ungetToken(thirdToken);
                        }
                        break;
                    }
                } else if (value.equals(CocoLanguage.QUOTATION)
                        || value.equals(CocoLanguage.APOSTROPHE)) {
                    // If I found a valid quote character that doesn't match the
                    // starting quote character, I found an invalid literal
                    token = secondToken;
                    break;
                }
            }
        }

        if ((usesDbcsShiftChars && !gotDbcsShiftClose) || lastQuote == null) {
            byte[] bytes = token.getBytesValue();
            logAndThrow("CCCB4205",
                    new Object[]{token.getStringValue() + " (" + Integer.toHexString(bytes[0]) + Integer.toHexString(bytes[1]) + ")"},
                    ErrorManager.Severity.ERROR, null, token, entry);
        }
    }

    private boolean skipFigurativeConstant()
            throws IOException {
        List undoTokens = new ArrayList();
        List spaces = new ArrayList();
        CobolSourceArea area = getCurrentArea();
        CocoToken token = null;
        StringBuffer wordBuf = new StringBuffer();
        boolean gotFirst = false;
        boolean gotStartColon = false;
        boolean wasHyphen = false;

        boolean eol = skipOptionalSpaces(true, spaces);

        while (true) {
            if (area == null || area instanceof CobolSourceAreaB) {
                token = getNextTokenAreaB(false, true);
                if (token == null || token.isEOF()) {
                    if (wordBuf.length() == 0) {
                        logAndThrow("CCCB4216", null,
                                ErrorManager.Severity.ERROR, null, token);
                    } else {
                        break;
                    }
                }
            } else {
                token = getNextToken();
                if (token == null || token.isEOF()) {
                    if (wordBuf.length() == 0) {
                        logAndThrow("CCCB4216", null,
                                ErrorManager.Severity.ERROR, null, token);
                    } else {
                        break;
                    }
                }
                if (!isInArea(token, area)) {
                    ungetToken(token);
                    if (wordBuf.length() == 0) {
                        logAndThrow("CCCB4217", null,
                                ErrorManager.Severity.ERROR, null, token);
                    }
                }
            }

            if (token == null || token.isEOF()) {
                break;
            }

            String value = token.getStringValue();

            /* hyphens can't begin words */
            if (value.equals(CocoLanguage.HYPHEN)) {
                wasHyphen = true;
                if (gotFirst) {
                    wordBuf.append(token.getStringValue());
                    undoTokens.add(token);
                } else {
                    ungetToken(token);
                    break;
                }

            } else if (wasHyphen == gotFirst &&
                    (token.getType() == CocoTokenTypes.ALNUM_TOKEN
                        || token.getType() == CocoTokenTypes.NUM_TOKEN)) {
                gotFirst = true;
                wasHyphen = false;
                wordBuf.append(token.getStringValue());
                undoTokens.add(token);

            /* high-level qualifier tokens (e.g., name:hlq:restofname) */
            } else if (value.equals(CocoLanguage.COLON)) {

                if (gotStartColon) {
                    CocoToken previousToken = (CocoToken) (undoTokens.size() == 0
                            ? null : undoTokens.get(undoTokens.size() - 1));

                    if (previousToken != null && value.equals(previousToken.getStringValue())) {
                        ungetToken(token);
                        break;
                    }
                }

                gotStartColon = !gotStartColon;
                wasHyphen = false;
                wordBuf.append(token.getStringValue());
                undoTokens.add(token);

            } else {

                ungetToken(token);

                /* hyphens can't end words */
                if (wasHyphen) {
                    CocoToken lastToken = (CocoToken) undoTokens.get(undoTokens.size() - 1);
                    ungetToken(lastToken);
                    wordBuf.delete(wordBuf.length() - 1, wordBuf.length());
                }

                /* colons must occur in pairs */
                if (gotStartColon) {
                    for (int i = undoTokens.size(); i >= 0; i--) {
                        CocoToken tok = (CocoToken) undoTokens.get(i);
                        ungetToken(tok);
                    }
                    for (int i = spaces.size() - 1; i >= 0; i--) {
                        ungetToken((CocoToken) spaces.get(i));
                    }
                }
                break;
            }
        }

        String name = (wordBuf.length() == 0 ? null : wordBuf.toString());

        boolean gotConstant = (name != null && CocoLanguage.isFigurativeConstant(name));

        if (!gotConstant) {
            name = null;
            int size = undoTokens.size();
            for (int i = size - 1; i >= 0; i--) {
                CocoToken tok = (CocoToken) undoTokens.remove(i);
                ungetToken(tok);
            }
            for (int i = spaces.size() - 1; i >= 0; i--) {
                ungetToken((CocoToken) spaces.get(i));
            }
        }

        undoTokens.clear();
        spaces.clear();
        return gotConstant;
    }

    /**
     * Consume a Cobol clause separator.  Clause separators are consecutive spaces,
     * the separator comma (, or ,b), and the separator semicolon (; or ;b).
     * Additionally, as a special case, carriage return characters as well.  If
     * no clause separator is found, an exception is thrown, unless what is found
     * is separator period, which is the entry terminator, which is OK.
     *
     * @throws CocoParseException if there are no tokens available, or there is no
     *         clause separator at the head of the input stream to skip
     * @throws java.io.IOException if an I/O error occurs
     */
    private void skipClauseSeparator() throws CocoParseException, IOException {

        CocoToken token = null;
        boolean foundSep = false;

        while (true) {
            token = getNextTokenAreaB(false, true);
            if (token == null || token.isEOF()) {
                break;
            } else {
                String value = token.getStringValue();
                if (value.equals(CocoLanguage.COMMA)) {
                    foundSep = true;
                    skipOptionalSpaces(false);
                    break;
                } else if (value.equals(CocoLanguage.SEMICOLON)) {
                    foundSep = true;
                    skipOptionalSpaces(false);
                    break;
                } else if (value.equals(CocoLanguage.SPACE)
                        || value.charAt(0) == '\r') {
                    foundSep = true;
                } else {
                    ungetToken(token);
                    if (!foundSep) { // If I have not been skipping spaces...
                        foundSep = value.equals(CocoLanguage.PERIOD); // fake it
                    }
                    break;
                }
            }
        }

        /*
        if (!foundSep) {
        if (null == token) {
        Message msg = MessageCatalog.getMessage("CCCB4217");
        String text = msg.toString();
        mErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
        generateParseException(text, null);
        } else {
        Message msg = MessageCatalog.getMessage("CCCB4222");
        String text = msg.toString();
        mErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
        generateParseException(text, token);
        }
        }
         */
    }

    /**
     * Consumes zero or more contiguous space tokens, until the next lexeme in the
     * token stream is no longer a space, or until EOD occurs.
     *
     * @param  stayInArea Set to true to skip only within the Cobol source line area
     * @throws java.io.IOException if an I/O error occurs
     */
    private boolean skipOptionalSpaces(boolean stayInArea)
            throws IOException {
        return skipOptionalSpaces(stayInArea, null);
    }

    /**
     * Consumes zero or more contiguous space tokens, until the next lexeme in the
     * token stream is no longer a space, or until EOD occurs.
     *
     * @param  stayInArea Set to true to skip only within the Cobol source line area
     * @throws java.io.IOException if an I/O error occurs
     * @return true if EOL token is reached, or false if otherwise or
     * EOF token is reached.
     */
    private boolean skipOptionalSpaces(boolean stayInArea, List consumed)
            throws IOException {
        CobolSourceArea area = null;
        CocoToken token = null;

        if (stayInArea) {
            area = getCurrentArea();
        }
        do {
            try {
                token = getNextToken();
            } catch (CocoParseException cpe) {
                token = null;
            }
            if (token != null && !token.isEOF()) {
                if (token.isEOL()) {
                    // do not consume EOL here
                    ungetToken(token);
                    return true;
                }
                if (token.getStringValue().equals(CocoLanguage.SPACE)) {
                    if (area != null && area instanceof CobolSourceAreaB && !isInArea(token, area) && isInAreaB(token)) {
                        if (consumed != null) {
                            consumed.add(token);
                        }
                    } else if (area != null && !isInArea(token, area)) {
                        ungetToken(token);
                        token = null;
                    } else {
                        if (consumed != null) {
                            consumed.add(token);
                        }
                    }
                } else {
                    ungetToken(token);
                    token = null;
                }
            }
        } while (token != null && !token.isEOF());
        return false;
    }

    /**
     * Consumes one or more (heterogenous or homogenous) sequences of spaces
     * and periods, until the next lexeme in the token stream is no longer
     * either space or period, or until EOD occurs.
     *
     * @param  stayInArea Set to true to skip only within the Cobol source line area
     * @throws CocoParseException if the next available token is neither space or
     *         period
     * @throws java.io.IOException if an I/O error occurs
     */
    private void skipSpacesOrPeriods(boolean stayInArea)
            throws CocoParseException, IOException {

        CobolSourceArea area = null;
        CocoToken token = null;
        int count = 0;

        if (stayInArea) {
            area = getCurrentArea();
        }
        do {
            token = getNextToken();
            String value = token.getStringValue();
            if (value.equals(CocoLanguage.SPACE)
                    || value.equals(CocoLanguage.PERIOD)) {
                if (area == null || isInArea(token, area)) {
                    count++;
                } else {
                    ungetToken(token);
                    break;
                }
            } else {
                ungetToken(token);
                break;
            }
        } while (token != null && !token.isEOF());

        if (count == 0 && token != null && !token.isEOF()) {
            logAndThrow("CCCB4223", null,
                    ErrorManager.Severity.ERROR, null, token);
        }
    }

    /**
     * Consumes one or more tokens until the next lexeme in the token stream has
     * the coordinate of the next Cobol source line, or until EOD occurs.
     *
     * @throws java.io.IOException if an I/O error occurs
     */
    private void skipLine() throws IOException {
        CocoToken token = null;
        do {
            try {
                token = mLexer.getNextToken();
            } catch (CocoParseException cpe) {
                token = null;
            }
            if (token != null && !token.isEOF()) {
                if (mRow != token.getRow()) {
                    mLexer.ungetToken(token);
                    token = null;
                } else {
                    updatePosition(token);
                }
            }
        } while (token != null && !token.isEOF());
    }

    /**
     * Fetch the next token from the token stream.
     *
     * @return the next token in the stream
     * @throws CocoParseException if EOD occurs
     * @throws java.io.IOException if an I/O error occurs
     */
    private CocoToken getNextToken() throws CocoParseException, IOException {
        CocoToken token = mLexer.getNextToken();

        if (token == null) {
            logAndThrow("CCCB4203", null,
                    ErrorManager.Severity.ERROR, null, null);
        }
        updatePosition(token);
        return token;
    }

    /**
     * Fetch the next token from the token stream.  It is assumed that the
     * "current" token stream position is in Cobol Area B. If the next token
     * in the stream is not in this area. the rest of the current line is
     * discarded, and the method will attempt to take the next token from the
     * next Area B, provided that the next Area B is part of the item entry
     * description being processed (i.e., the next Area B is a continuation line).
     *
     * @param  stayInLine Set to false to have next line's Area B scanned if the
     *         current line's Area B has no more content; set to true to stay
     *         in the Area B of the current line
     * @param  ignoreNextLineSpaces Not used when stayInLine is true. Otherwise,
     *         if it proves necessary to read the next token from the next line,
     *         setting this parameter to true causes space tokens preceeding non-space
     *         content from the next Area B to be discarded.  Specify false causes
     *         the spaces to each be returned as distinct separator tokens.
     * @return the next token in the stream, from Area B, or null if there are
     *         no more tokens available in the area
     * @throws CocoParseException if EOD occurs
     * @throws java.io.IOException if an I/O error occurs
     */
    private CocoToken getNextTokenAreaB(boolean stayInLine,
            boolean ignoreNextLineSpaces)
            throws CocoParseException, IOException {

        CocoToken token = mLexer.getNextToken();

        if (token == null) {
            return null;
        }

        if (token.isEOF()) {
            return token;
        }

        /* got a word in Area B */
        if (isInAreaB(token)) {
            updatePosition(token);

        } else if (stayInLine) {
            token = null;

        /* need to get token from next Area B */
        } else {
            // if the token is EOL - should skip it
            mLexer.ungetToken(token);
            undoUpdatePosition();
            while (true) {

                skipLine();

                skipSequenceArea();

                /* skip comments */
                CocoDescriptionEntry dummyEntry = new CocoDescriptionEntry();
                parseIndicatorArea(dummyEntry);
                if (dummyEntry.isComment()) {
                    continue;
                }

                /* stop if next line is not a continuation */
                parseAreaA(dummyEntry);
                if (!dummyEntry.isContinuation()) {

                    /*
                     * synthesize Area A and Indicator Area tokens, to
                     * pretend we rolled back the original tokens
                     */
                    String levelValue = " ";
                    int level = dummyEntry.getLevel();
                    int col = CobolSourceAreaA.getArea().getStartColumn();
                    CocoTokenTypes type = CocoTokenTypes.SEPARATOR_TOKEN;
                    if (level != -1) {
                        levelValue = Integer.toString(level);
                        type = CocoTokenTypes.NUM_TOKEN;
                    }
                    CocoToken levelToken = new CocoToken(levelValue, type, mRow, col);
                    mLexer.ungetToken(levelToken);
                    undoUpdatePosition();

                    String indicatorValue = dummyEntry.getIndicatorValue();
                    col = CobolSourceIndicatorArea.getArea().getStartColumn();
                    if (indicatorValue.equals("")) {
                        type = CocoTokenTypes.SEPARATOR_TOKEN;
                        indicatorValue = " ";
                    } else if (Character.isDigit(indicatorValue.charAt(0))) {
                        type = CocoTokenTypes.NUM_TOKEN;
                    } else if (Character.isLetter(indicatorValue.charAt(0))) {
                        type = CocoTokenTypes.ALNUM_TOKEN;
                    } else if (CocoLanguage.isSeparator(indicatorValue)) {
                        type = CocoTokenTypes.SEPARATOR_TOKEN;
                    } else if (!CocoLanguage.isInCobolCharSet(indicatorValue.charAt(0))) {
                        type = CocoTokenTypes.NONCOBOL_TOKEN;
                    } else {
                        type = CocoTokenTypes.SPECIALCHAR_TOKEN;
                    }
                    CocoToken indicatorToken = new CocoToken(indicatorValue,
                            type, mRow, col);
                    mLexer.ungetToken(indicatorToken);
                    undoUpdatePosition();
                    token = null;
                    break;
                }

                /* skip blank lines */
                if (ignoreNextLineSpaces) {
                    while (true) {
                        skipOptionalSpaces(true);
                        token = this.getNextToken();
                        if (!token.isEOL()) {
                            ungetToken(token);
                            break;
                        }
                    }
                }

                token = mLexer.getNextToken();
                mLexer.ungetToken(token);
                if (!isInAreaB(token)) {
                    continue;
                }

                /* Back in Area B... grab a token */
                token = mLexer.getNextToken();
                updatePosition(token);
                break;
            }
        }
        return token;
    }

    /**
     * "Pushes" a token at the head of the parser input.
     *
     * @param token the token to put at the head of the input
     * @throws java.lang.IllegalArgumentException if token is null
     */
    private void ungetToken(CocoToken token) throws IllegalArgumentException {
        if (token != null && !token.isEOF()) {
            mLexer.ungetToken(token);
            undoUpdatePosition();
        }
    }

    /**
     * Updates parser's "current" row and column coordinates to those of a
     * token's.
     *
     * @param  token CocoToken object whose coordinates update the parser's
     */
    private void updatePosition(CocoToken token) {
        if (token.isEOF()) {
            return;
        }
        mCols.add(new Integer(mCol));
        mRows.add(new Integer(mRow));
        mCol = token.getColumn() + token.getLength();
        mRow = token.getRow();
    }

    /**
     * Updates parser's "current" row and column coordinates as if the given
     * token was returned to the head of the token input stream.
     */
    private void undoUpdatePosition() {
        if (!mCols.isEmpty() && !mRows.isEmpty()) {
            mCol = ((Integer) mCols.remove(mCols.size() - 1)).intValue();
            mRow = ((Integer) mRows.remove(mRows.size() - 1)).intValue();
        }
    }

    /**
     * Indicate whether a token falls within an area.
     *
     * @param  token The token in consideration
     * @param  area  The area in consideration
     * @return true if the token falls wholly within the given area, else
     *         false
     */
    private boolean isInArea(CocoToken token, CobolSourceArea area) {
        int begPos = token.getColumn();
        int endPos = token.getColumn() + token.getLength() - 1;
        return area.isInArea(begPos, endPos);
    }

    /**
     * Indicate whether a token falls within the Cobol source Sequence Area.
     *
     * @param  token The token in consideration
     * @return true if the token falls wholly within the Sequence Area
     */
    private boolean isInSequenceArea(CocoToken token) {
        CobolSourceArea area = CobolSourceSequenceArea.getArea();
        return isInArea(token, area);
    }

    /**
     * Indicate whether a token falls within the Cobol source Indicator Area.
     *
     * @param  token The token in consideration
     * @return true if the token falls wholly within the Indicator Area
     */
    private boolean isInIndicatorArea(CocoToken token) {
        CobolSourceArea area = CobolSourceIndicatorArea.getArea();
        return isInArea(token, area);
    }

    /**
     * Obtain the current source area in regard
     *
     * @return CobolSourceArea object corresponding to the current area, or
     *         null if the current location indicated by the parser's
     *         internal state does not fall in any of the known areas
     *         (which would be a Bad Thing)
     */
    private CobolSourceArea getCurrentArea() {
        Iterator it = cSourceAreas.iterator();
        CobolSourceArea area = null;
        while (it.hasNext() && (area == null)) {
            area = (CobolSourceArea) it.next();
            area = (area.isInArea(mCol, mCol) ? area : null);
        }
        return area;
    }

    /**
     * Indicate whether a token falls within the Cobol source Sequence Area.
     *
     * @param  token The token in consideration
     * @return true if the token falls wholly within the Sequence Area
     */
    private boolean isInAreaA(CocoToken token) {
        CobolSourceArea area = CobolSourceAreaA.getArea();
        return isInArea(token, area);
    }

    /**
     * Indicate whether a token falls partially within the Cobol source Sequence Area.
     *
     * @param  token The token in consideration
     * @return true if the token falls partially within the Sequence Area
     */
    private boolean isPartiallyInAreaA(CocoToken token) {
        final CobolSourceArea area = CobolSourceAreaA.getArea();
        final int start = area.getStartColumn();
        final int end = area.getEndColumn();
        final int tokStart = token.getColumn();
        final int tokEnd = token.getColumn() + token.getLength() - 1;
        return ((start <= tokEnd && tokEnd <= end)
                || (start <= tokStart && tokStart <= end)
                || (tokStart <= start && end <= tokEnd));
    }

    /**
     * Indicate whether a token falls within the Cobol source Sequence Area.
     *
     * @param  token The token in consideration
     * @return true if the token falls wholly within the Sequence Area
     */
    private boolean isInAreaB(CocoToken token) {
        CobolSourceArea area = CobolSourceAreaB.getArea();
        if (mLexer.is72ColumnLimitEnforced()) {
            return isInArea(token, area);
        } else {
            return isInArea(token, area)
                    || (area.getStartColumn() < token.getColumn());
        }
    }

    /**
     * Indicate whether the token is a comment indicator.  A token is a comment
     * indicator if it falls within the Indicator Area, and is a sole asterisk
     * character.
     *
     * @param  token The token in consideration
     * @return true if the token is a commend indicator
     */
    private boolean isComment(CocoToken token) {
        boolean isIt = false;
        if (isInIndicatorArea(token)) {
            String value = token.getStringValue();
            if (value.equals(CocoLanguage.ASTERISK)
                    || value.equals(CocoLanguage.SLANT)) {
                isIt = true;
            }
        }
        return isIt;
    }

    /**
     * Indicate whether the token is a space.  A token is a space if it consists
     * solely of space characters, regardless of how many.
     *
     * @param  token The token in consideration
     * @return true if the token is a space
     */
    private boolean isSpace(CocoToken token) {
        boolean isIt = true;
        char[] chs = token.getCharsValue();
        for (int i = 0; i < chs.length; i++) {
            isIt &= (chs[i] == CocoLanguage.SPACE.charAt(0));
        }
        return isIt;
    }

    /**
     * Throw a CocoParseException.
     *
     * @param msg    The text to use as the exception message
     * @param token  The subject token of the exception, or null
     */
    protected void generateParseException(String msg, CocoToken token)
            throws CocoParseException {

        if (msg == null) {
            msg = "";
        }
        throw new CocoParseException(msg,
                token,
                (token == null ? mRow : token.getRow()),
                (token == null ? mCol : token.getColumn()),
                mCurrentEntry);
    }

    private static void debugToken(CocoToken token) {
        System.err.println("row: " + token.getRow());
        System.err.println("column: " + token.getColumn());
        System.err.println("value: " + token.getStringValue());
        System.err.println("length: " + token.getLength());
        System.err.println("type: " + token.getType());
        System.err.flush();
    }

    /**
     * helper to log and throw error;
     * @param msgKey
     * @param params
     * @param severity
     * @param token
     */
    private void logAndThrow(String msgKey,
            Object[] params,
            ErrorManager.Severity severity,
            Exception e,
            CocoToken token) {
        Message msg = MessageCatalog.getMessage(msgKey);
        String text = msg.formatText(params);
        mErrorMgr.log(severity, e, text);
        generateParseException(text, token);
    }

    /**
     * helper to log and throw error;
     * @param msgKey
     * @param params
     * @param severity
     * @param token
     * @param entry - needed to check if there is an reserved word exception earlier
     */
    private void logAndThrow(String msgKey,
            Object[] params,
            ErrorManager.Severity severity,
            Exception e,
            CocoToken token, CocoDescriptionEntry entry)
            throws CocoParseException {
        if (this.mReservedWordCheck
                && entry != null
                && entry.getReservedWordAfterLevel() != null) {
            mErrorMgr.log(severity, entry.getReservedWordAfterLevel(),
                    entry.getReservedWordAfterLevel().getMessage());
            throw entry.getReservedWordAfterLevel();
        } else {
            Message msg = MessageCatalog.getMessage(msgKey);
            String text = msg.formatText(params);
            mErrorMgr.log(severity, e, text);
            generateParseException(text, token);
        }
    }

    /**
     * same to logAndThrow but throw a sub class of CococParseException
     * so that the caller can distinguish that it is a reserved word violation
     * @param msgKey
     * @param params
     * @param severity
     * @param e
     * @param token
     */
    private void logAndThrow2(String msgKey,
            Object[] params,
            ErrorManager.Severity severity,
            Exception e,
            CocoToken token) {
        Message msg = MessageCatalog.getMessage(msgKey);
        String text = msg.formatText(params);
        mErrorMgr.log(severity, e, text);
        if (text == null) {
            text = "";
        }
        throw new ReservedWordEncounteredException(text,
                token,
                (token == null ? mRow : token.getRow()),
                (token == null ? mCol : token.getColumn()),
                mCurrentEntry);
    }
}
