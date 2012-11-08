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

import org.xml.sax.Attributes;

final class EscapeAttributes implements Attributes {

    private String mValue;

    public int getLength() {
        return 1;
    }

    public String getURI(int index) {
        if (index == 0) {
            return "";
        }
        return null;
    }

    public String getLocalName(int index) {
        if (index == 0) {
            return "V";
        }
        return null;
    }

    public String getQName(int index) {
        if (index == 0) {
            return "V";
        }
        return null;
    }

    public String getType(int index) {
        if (index == 0) {
            return "CDATA";
        }
        return null;
    }

    public String getValue(int index) {
        if (index == 0) {
            return mValue;
        }
        return null;
    }

    public int getIndex(String uri, String localName) {
        if (uri == null && "V".equals(localName)) {
            return 0;
        }
        return -1;
    }

    public int getIndex(String qName) {
        if ("V".equals(qName)) {
            return 0;
        }
        return -1;
    }

    public String getType(String uri, String localName) {
        if (uri == null && "V".equals(localName)) {
            return "CDATA";
        }
        return null;
    }

    public String getType(String qName) {
        if ("V".equals(qName)) {
            return "CDATA";
        }
        return null;
    }

    public String getValue(String uri, String localName) {
        if (uri == null && "V".equals(localName)) {
            return mValue;
        }
        return null;
    }

    public String getValue(String qName) {
        if ("V".equals(qName)) {
            return mValue;
        }
        return null;
    }

    public void setValue(int index, String value) {
        if (index == 0) {
            mValue = value;
        }
    }
}
