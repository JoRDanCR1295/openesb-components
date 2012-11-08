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

package com.sun.encoder.hl7.runtime.provider;

/**
 *
 * @author sun
 */
public class SemLevel {
    
    // Semantic level. !!These values should NOT be changed because
    // in the program ++ and -- might be used to get new values for level.
    static final int DOCUMENT = 0;
    static final int SEGMENT = 1;
    static final int FIELD = 2;
    static final int COMPONENT = 3;
    static final int SUBCOMPONENT = 4;
    static final int MAX_SEMANTIC_LEVELS = 5;

    static String getSemanticLevelDesc(int level) {
        switch (level) {
            case DOCUMENT: return "DOCUMENT";
            case SEGMENT: return "SEGMENT";
            case FIELD: return "FIELD";
            case COMPONENT: return "COMPONENT";
            case SUBCOMPONENT: return "SUBCOMPONENT";
            default: return "";
        }
    }
}
