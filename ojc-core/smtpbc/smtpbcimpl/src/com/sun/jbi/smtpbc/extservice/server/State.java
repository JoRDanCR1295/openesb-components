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
 * @(#)State.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/***************************************************************************
 *
 *          Copyright (c) 2005, SeeBeyond Technology Corporation,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ***************************************************************************/
package com.sun.jbi.smtpbc.extservice.server;

import java.io.Writer;

/**
 *
 *
 * @author       Alexander Fung
 * @version      
 *
 */
public interface State {

    boolean performAction(Writer out) throws Exception;

    State nextState(String content);

    State previousState();

    /**
     * This method parses the SMTP input from the client.  The method returns
     * true if the content is completely acceptable for this state, false
     * otherwise.  The implementor of this method should throw an exception
     * if the content has a valid command, but invalid parameters.
     * For example, if in the MAIL state, the following content should result
     * in a return value of false:
     * <p>
     * RCPT TO:<joe@anywhere.com>
     * <p>
     * This is because the command (RCPT) is not valid for the MAIL state.
     * However, this method should throw an exception for the following input
     * <p>
     * MAIL abcde
     * <p>
     * because "abcde" is an improper parameter list for the MAIL state.
     * <p>
     * The content of the exception should be an acceptable SMTP response as
     * as defined by RFC 2821.
     *
     * @param        content the data to parse
     * @return       true if the content is valid for this state, false otherwise
     * @exception    InvalidParseException if the command is valid but the parameters
     * are invalid
     */
    boolean parse (String content) throws InvalidParseException;
}
