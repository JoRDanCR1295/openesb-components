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
 * @(#)CocoParseException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.coco.model;

import com.sun.encoder.coco.runtime.messages.Message;
import com.sun.encoder.coco.runtime.messages.MessageCatalog;

/**
 * Describes parse errors when reading Cobol Copybook sources.
 *
 * @author  Noel Ang
 *
 * @version $Revision: 1.1 $
 */
public class CocoParseException extends RuntimeException {

private CocoToken mToken;
private String mMsg;
private String mExMsg;
private int    mRow;
private int    mCol;

/**
 * Create a CocoParseException.
 *
 * @param  msg   Message for exception; may be null
 * @param  token Subject of the exception; may be null
 * @param  row   Space coordinate of exception
 * @param  col   Space coordinate of exception
 * @param  entry Subject of the error
 */
public CocoParseException(String msg,
                          CocoToken token,
                          int row,
                          int col,
                          CocoDescriptionEntry entry) {
    if (row < 0) {
        row = -1;
    }
    if (col < 0) {
        col = -1;
    }
    mRow   = row;
    mCol   = col;
    mMsg   = msg;
    mToken = token;

    Message err = MessageCatalog.getMessage("CCCB4200");
    String tokenVal;
    String itemName;
    
    if (token == null || "".equals(token.getStringValue())) {
        tokenVal = "(n/a)";
    }
    else {
        tokenVal = "'" + token.getStringValue() + "'";
    }
    
    if (token == null || "".equals(token.getStringValue())) {
        itemName = "(n/a)";
    }
    else {
        itemName = "'" + entry.getName() + "'";
    }
    
    mExMsg = err.formatText(new Object[] {
        String.valueOf(row),
        String.valueOf(col),
        itemName,
        tokenVal,
        String.valueOf(mMsg)
    });
}

/**
 * Returns the detail message string of this throwable.
 *
 * @return  the detail message string of this <tt>Throwable</tt> instance
 *          (which may be <tt>null</tt>).
 */
public String getMessage() {
    return mExMsg;
}

/**
 * Return the column coordinate of the cause of the exception.
 *
 * @return column coordinate, counting from 1.
 */
public int getColumn() {
    return mCol;
}

/**
 * Return the row coordinate of the cause of the exception.
 *
 * @return row cordinate, counting from 1.
 */
public int getRow() {
    return mRow;
}

/**
 * Return the subject token of the exception.
 *
 * @return the token to which the exception is attributed
 */
public CocoToken getToken() {
    return mToken;
}

}
/* EOF $RCSfile: CocoParseException.java,v $ */
