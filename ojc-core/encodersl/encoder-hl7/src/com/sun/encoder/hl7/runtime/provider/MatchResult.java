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

import javax.xml.namespace.QName;
import org.apache.xmlbeans.SchemaType;

/**
 *
 * @author sun
 */
class MatchResult {
    public static final int MATCH_ELEMENT = 1;
    public static final int MATCH_WILDCARD = 2;

    public int mResult;
    public QName mElementName;
    public SchemaType mElementType;

    @Override
    public String toString() {
        StringBuffer buff = new StringBuffer("(MatchResult@")
            .append(Integer.toHexString(hashCode()));
        String result = "";
        if (mResult == MATCH_ELEMENT) {
            result = "MATCH_ELEMENT";
        } else if (mResult == MATCH_WILDCARD) {
            result = "MATCH_WILDCARD";
        }
        buff.append(" mResult=").append(result);
        if (mElementName != null) {
            buff.append(" mElementName='").append(mElementName.toString())
                .append("'");
        }
        if (mElementType != null) {
            buff.append(" mElementType=").append(mElementType);
        }
        return buff.toString();
    }
}


