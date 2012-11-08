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

package com.sun.encoder.hl7.util;

import org.apache.xmlbeans.SchemaGlobalElement;

/**
 *
 * @author sun
 */
public final class Util {

    public static final String FHS = "FHS";
    public static final String FTS = "FTS";
    public static final String BHS = "BHS";
    public static final String BTS = "BTS";
    public static final String MSH = "MSH";
    public static final String QRD = "QRD";
    public static final String QRF = "QRF";
    public static final String BATCH = "BATCH";
    public static final String MESSAGES = "MESSAGES";
    public static final String MESSAGEBATCH = "MESSAGEBATCH";

    static final String DOT = ".";

    /**
     * Returns the group name prefix based on input element. For example, if
     * the localPart of input element is "ADT_A01", then the return prefix
     * would be "ADT_A01.". If the localPart is "ADT_A01.INSURANCE", then
     * the prefix would be "ADT_A01.".
     * 
     * @param element a SchemaGlobalElement instance
     * @return the group name prefix based on input element.
     */
    public static String getGroupNamePrefix(SchemaGlobalElement element) {
        String localPart = element.getName().getLocalPart();
        int lastDot = localPart.lastIndexOf(DOT);
        String groupNamePrefix = null;
        if (lastDot > 0) {
            groupNamePrefix = localPart.substring(0, lastDot + 1);
        } else {
            groupNamePrefix = localPart + DOT;
        }
        return groupNamePrefix;
    }

    /**
     * Checks if the given name is one of the header segment names, such as
     * "MSH", "FHS", or "BHS"
     * @param name to be checked.
     * @return true if the given name is one of the header segment names.
     */
    public static boolean isHeaderSegmentName(String name) {
        if (FHS.equals(name) || BHS.equals(name) || MSH.equals(name)) {
            return true;
        } else {
            return false;
        }
    }
}
