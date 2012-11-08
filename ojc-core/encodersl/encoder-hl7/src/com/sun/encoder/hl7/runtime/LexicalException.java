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
 * @(#)LexicalException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.hl7.runtime;

import org.xml.sax.SAXException;

/**
 * This exception is thrown by the lexer which scans the HL7 standard
 * encoding rule encoded data when a lexical problem (checking against
 * the standard encoding rule) is detected in the data.
 * 
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
public class LexicalException extends SAXException {

    private static final long serialVersionUID = 1L;
    
    private final int mLine;
    private final int mCol;
    private final int mLength;
    
    public LexicalException(int line, int col, int length) {
        super();
        mLine = line;
        mCol = col;
        mLength = length;
    }

    public LexicalException(String message, Exception cause,
            int line, int col, int length) {
        super(message, cause);
        mLine = line;
        mCol = col;
        mLength = length;
    }

    public LexicalException(String message, int line, int col, int length) {
        super(message);
        mLine = line;
        mCol = col;
        mLength = length;
    }

    public LexicalException(Exception cause, int line, int col, int length) {
        super(cause);
        mLine = line;
        mCol = col;
        mLength = length;
    }
    
    public int getLine() {
        return mLine;
    }
    
    public int getCol() {
        return mCol;
    }
    
    public int getLength() {
        return mLength;
    }

    @Override
    public String getMessage() {
        return "Line " + mLine + ", Column " + mCol + ": " + super.getMessage();
    }

    @Override
    public String toString() {
        return "Line " + mLine + ", Column " + mCol + ": " + super.toString();
    }
}
