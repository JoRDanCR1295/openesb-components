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
 * @(#)StringFormat.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcsre;

import java.text.Format;
import java.text.ParseException;

/**
 * a class for formatting strings.
 *
 * @author Scott Steadman ssteadman@seebeyond.com
 */
public abstract class StringFormat extends Format {

    /**
     * format a string using specified pattern
     *
     * @param _string the string
     * 
     * @return the formatted string
     */
    public String format(String _string) {
        return super.format(_string);
    }

    /**
     * parse a string using specified pattern
     *
     * @param _string the string
     *
     * @return the parsed string
     *
     * @throws ParseExcepton unable to parse string.
     */
    public String parse(String _string) throws ParseException {
        return (String)parseObject(_string);
    }

}
