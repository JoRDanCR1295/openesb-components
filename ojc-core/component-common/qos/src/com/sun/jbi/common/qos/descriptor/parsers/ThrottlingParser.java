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
 * @(#)DescriptorHandler.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.common.qos.descriptor.parsers;

import javax.jbi.management.DeploymentException;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.sun.jbi.common.descriptor.parsers.AbstractJbiParser;
import com.sun.jbi.common.qos.throttling.Throttling;
import com.sun.jbi.common.qos.throttling.ThrottlingConfig;
import com.sun.jbi.common.util.I18n;
import com.sun.jbi.common.util.Util;

/**
 * Parses JBI endpoints in a service unit descriptor.
 * @author Kevan Simpson
 */
public class ThrottlingParser extends AbstractJbiParser<ThrottlingConfig> {

    public static final String THROTTLING_ELEM = "throttling";
    public static final String ALGORITHM = "algorithm";
    public static final String LEAK_RATE = "leakRate";
    public static final String MAX_CONCURRENCY_LIMIT = "maximumConcurrencyLimit";

    public ThrottlingParser(QosConnectionParser cp) {
        super(cp);
    }

    /** @see com.sun.jbi.common.descriptor.parsers.JbiParser#parse(org.w3c.dom.Element) */
    public ThrottlingConfig parse(Element elem) throws DeploymentException {
        try {
            NodeList list = elem.getElementsByTagNameNS(
                    Throttling.THROTTLING_NS, THROTTLING_ELEM);
            if (list != null && list.getLength() == 1) {
                Element th = (Element) list.item(0);
                String algorithm = th.getAttribute(ALGORITHM);
                if (algorithm.equals("leakyBucket")){
                    int leakRate = Util.parseInt(th.getAttribute(
                            LEAK_RATE), -1);
                    return Throttling.createThrottlingLeakyConfig(leakRate);}
                else {
                    int maxConcurrencyLimit = Util.parseInt(th.getAttribute(
                            MAX_CONCURRENCY_LIMIT), -1);
                    return Throttling.createThrottlingConfig(maxConcurrencyLimit);
                }
            }
        } catch (Exception e) {
            throw error(e, I18n.loc(
                    "QOS-6062: Failed to parse systemic throttling configuration: {0}",
                    e.getMessage()));

        }

        return null;
    }
}
