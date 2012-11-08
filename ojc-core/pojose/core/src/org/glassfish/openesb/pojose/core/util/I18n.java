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
 * @(#)I18N.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.pojose.core.util;

import java.util.regex.Pattern;
import com.sun.jbi.common.util.LocalizationSupport;

import java.util.regex.Matcher;

/**
 * Internationalization utility for PojoSE.
 * @author Girish Patil
 */
public class I18n extends LocalizationSupport {
    private static final String ID_MSG_DLMTR = ": ";  //NOI18N
    private static final String PREFIX = "";  //NOI18N
    // 1-3 Finest, Finer, Fine
    // 4 - Info
    // 5 - Alert
    // 6 - Warning
    // 7 - Severe
    private static Pattern PATTERN = Pattern.compile("(POJOSE-[4-7]\\d\\d\\d)(: )(.*)", Pattern.DOTALL); //NOI18N
    private static Pattern FINE_PATTERN = Pattern.compile("(POJOSE-[1-3]\\d\\d\\d)(: )(.*)", Pattern.DOTALL); //NOI18N
    // Make sure PATTERN is defined before new I18n() !
    private static final I18n mI18n = new I18n();

    protected I18n() {
        super(PATTERN, PREFIX, LocalizationSupport.DEFAULTBUNDLENAME); //NOI18N
    }

    public static String loc(String message, Object... params) {
        return mI18n.t(message, params);
    }

    public static String lf(String message, Object... params) {
        Matcher matcher = FINE_PATTERN.matcher(message);
        if (!matcher.matches() || matcher.groupCount() <= 1) {

            String[] str = new String[3];
            if (com.sun.jbi.common.util.Util.isEmpty(PREFIX)) {
                str[0] = "I18N"; //NOI18N
            } else {
                str[0] = PREFIX;
            }
            str[1] = format(message, params);
            str[2] = str[0] + ID_MSG_DLMTR + str[1];
            return str[2];
        } else {
            return format(message, params);
        }
    }

    /**
     *
     * @param message
     * @param params
     * @return String[]
     * 0 - MessageID ex: POJOSE-4001
     * 1 - Localized Message w/o MessageID.
     * 2 - Localized Messag with MessageID.
     */
    public static String[] locStr(String message, Object... params) {
        Matcher matcher = PATTERN.matcher(message);
        if (!matcher.matches() || matcher.groupCount() <= 1) {

            String[] str = new String[3];
            if (com.sun.jbi.common.util.Util.isEmpty(PREFIX)) {
                str[0] = "I18N"; //NOI18N
            } else {
                str[0] = PREFIX;
            }
            str[1] = format(message, params);
            str[2] = str[0] + ID_MSG_DLMTR + str[1];
            return str;
        } else {
            String[] str = new String[3];
            str[0] = matcher.group(1);
            str[2] = mI18n.t(message, params);
            str[1] = str[2].substring(matcher.group(1).length() +
                    matcher.group(2).length());
            return str;
        }
    }
}