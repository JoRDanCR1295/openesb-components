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
 * @(#)XPathExpressionSupport.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mm;

import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

/**
 * Support to create <code>XPathExpression</code> from this type of notation:
 * <pre>
 * {xpath express}[,[xmlns={default NS}][,xmlns:ns0={NS 0}][,xmlns:ns1={NS 1}]]
 * </pre>
 * <b>Note</b>, curly braces above are not part of the syntax.
 *
 * @author sunsoabi_edwong
 */
public class XPathExpressionSupport {

    /**
     * Computes a <code>XPathExpression</code> object based on the given
     * XPath notation as described above.
     * @param xpathStr XPath notation.
     * @return <code>XPathExpression</code> object or <code>null</code> if
     * error occurred.
     */
    public static XPathExpression computeXPathExpression(String xpathStr) {
        if ((xpathStr != null) && (xpathStr.trim().length() > 0)) {
            String xpathPart = null;
            String nsContextPart = null;
            int lastParenthesis = xpathStr.lastIndexOf(')');            //NOI18N
            if (lastParenthesis != -1) {
                xpathPart = xpathStr.substring(0, lastParenthesis + 1);
                xpathStr = xpathStr.substring(lastParenthesis + 1);
            }

            int commaSep = xpathStr.indexOf(',');                       //NOI18N
            if (commaSep != -1) {
                xpathPart = (xpathPart != null)
                        ? xpathPart + xpathStr.substring(0, commaSep)
                        : xpathStr.substring(0, commaSep);
                nsContextPart = xpathStr.substring(commaSep + 1);
            } else {
                xpathPart = (xpathPart != null)
                        ? xpathPart + xpathStr : xpathStr;
            }

            Map<String, String> prefixMap = new HashMap<String, String>();
            String defNS = null;
            if (nsContextPart != null) {
                StringTokenizer tokenizer =
                        new StringTokenizer(nsContextPart, ",");        //NOI18N
                while (tokenizer.hasMoreTokens()) {
                    String nsBinding = tokenizer.nextToken().trim();
                    int eqSign = nsBinding.indexOf('=');                //NOI18N
                    if (eqSign != -1) {
                        String prefix = nsBinding.substring(0, eqSign).trim();
                        String ns = nsBinding.substring(eqSign + 1).trim();
                        if (!"xmlns".equals(prefix)) {                  //NOI18N
                            if (prefix.startsWith("xmlns:")) {          //NOI18N
                                prefix = prefix.substring(
                                        "xmlns:".length());             //NOI18N
                            }
                            prefixMap.put(prefix, ns);
                        } else {
                            defNS = ns;
                        }
                    }
                }
            }
            NamespaceContextHelper nsch =
                    new NamespaceContextHelper(prefixMap, defNS);
            XPath xpath = XPathFactory.newInstance().newXPath();
            xpath.setNamespaceContext(nsch);
            try {
                XPathExpression xpathExp = xpath.compile(xpathPart);
                return xpathExp;
            } catch (XPathExpressionException ex) {
                Logger.getLogger(XPathExpressionSupport.class.getName())
                        .log(Level.SEVERE,
                                "Invalid XPath expression!", ex);   //NOI18N
            }
        }
        return null;
    }
}
