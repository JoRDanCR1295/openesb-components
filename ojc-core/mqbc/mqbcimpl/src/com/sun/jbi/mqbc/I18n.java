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
 */

/*
 * @(#)I18n.java
 *
 * Copyright 2007-2010 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc;

import java.util.regex.Pattern;

import com.sun.jbi.common.util.LocalizationSupport;

/**
 * Tool for obtaining localized messages.
 *
 * @author Noel.Ang@sun.com
 */
public class I18n extends LocalizationSupport {

    private static final I18n singleton = new I18n();

    public static final String prefix = "MQBC";

    protected I18n() {
        super(Pattern.compile("([0-9]\\d\\d\\d)(: )(.*)", Pattern.DOTALL), prefix, null);
    }

    /**
     * Yield a localized message. msg must be a string of the form "nnnn:
     * this is a message", where nnnn is a value between 0000 and 9999
     * inclusive.
     *
     * @param msg Message to be localized
     * @param args arguments Text-substitution arguments to apply to the
     * message
     *
     * @return java.lang.String The localized string.
     */
    public static String msg(String msg, Object... args) {
        return singleton.t(msg, args);
    }
}
