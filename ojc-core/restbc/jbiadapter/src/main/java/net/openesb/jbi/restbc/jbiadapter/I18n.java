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
package net.openesb.jbi.restbc.jbiadapter;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.sun.jbi.alerter.Alerter;
import com.sun.jbi.alerter.AlerterImpl;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.util.LocalizationSupport;

/**
 * I18n utility
 * @author Edward Chou
 */
public class I18n extends LocalizationSupport {  
    private static final Alerter alerter = new AlerterImpl();
    private static final String ID_MSG_DLMTR = ": ";  //NOI18N
    private static final String PREFIX = "";  //NOI18N
    private static final String SERVER_TYPE = "Glassfish"; //NOI18N
    private static final String COMP_NAME = "sun-rest-binding"; //NOI18N
    private static final String COMP_TYPE = "BindingComponent"; //NOI18N
    private static final String DEPLOY_NAME = null;
    // 1-3 Finest, Finer, Fine
    // 4 - Info
    // 5 - Alert
    // 6 - Warning
    // 7 - Severe
    private static Pattern PATTERN = Pattern.compile("(RESTBC-[4-7]\\d\\d\\d)(: )(.*)", Pattern.DOTALL); //NOI18N
    private static Pattern FINE_PATTERN = Pattern.compile("(RESTBC-[1-3]\\d\\d\\d)(: )(.*)", Pattern.DOTALL); //NOI18N
    // Make sure PATTERN is defined before new I18n() !
    private static final I18n mI18n = new I18n();

    protected I18n() {
        super(PATTERN, PREFIX, LocalizationSupport.DEFAULTBUNDLENAME);
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

    public static void alertInfo(String[] msgParts) {
        alerter.info(msgParts[1], COMP_NAME, DEPLOY_NAME,
                SERVER_TYPE, COMP_TYPE,
                NotificationEvent.OPERATIONAL_STATE_RUNNING,
                NotificationEvent.EVENT_TYPE_ALERT, msgParts[0]);

    }

    public static void alertWarning(String[] msgParts) {
        alerter.warning(msgParts[1], COMP_NAME, DEPLOY_NAME,
                SERVER_TYPE, COMP_TYPE,
                NotificationEvent.OPERATIONAL_STATE_RUNNING,
                NotificationEvent.EVENT_TYPE_ALERT, msgParts[0]);

    }

    public static void alertMinor(String[] msgParts) {
        alerter.minor(msgParts[1], COMP_NAME, DEPLOY_NAME,
                SERVER_TYPE, COMP_TYPE,
                NotificationEvent.OPERATIONAL_STATE_RUNNING,
                NotificationEvent.EVENT_TYPE_ALERT, msgParts[0]);

    }

    public static void alertMajor(String[] msgParts) {
        alerter.major(msgParts[1], COMP_NAME, DEPLOY_NAME,
                SERVER_TYPE, COMP_TYPE,
                NotificationEvent.OPERATIONAL_STATE_RUNNING,
                NotificationEvent.EVENT_TYPE_ALERT, msgParts[0]);

    }

    public static void alertCritical(String[] msgParts) {
        alerter.critical(msgParts[1], COMP_NAME, DEPLOY_NAME,
                SERVER_TYPE, COMP_TYPE,
                NotificationEvent.OPERATIONAL_STATE_RUNNING,
                NotificationEvent.EVENT_TYPE_ALERT, msgParts[0]);

    }

    public static void alertFatal(String[] msgParts) {
        alerter.fatal(msgParts[1], COMP_NAME, DEPLOY_NAME,
                SERVER_TYPE, COMP_TYPE,
                NotificationEvent.OPERATIONAL_STATE_RUNNING,
                NotificationEvent.EVENT_TYPE_ALERT, msgParts[0]);

    }
}

