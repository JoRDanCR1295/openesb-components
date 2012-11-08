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
 * @(#)Throttling.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.common.qos.throttling;

/**
 * Utility for throttling systemic quality.
 * @author Kevan Simpson
 */
public class Throttling {

    public static final String THROTTLING_NS =
            "http://www.sun.com/jbi/qos/throttling";

    public static ThrottlingConfig createThrottlingConfig(int maxConcurrencyLimit) {
        return new ThrottleConfig(maxConcurrencyLimit);
    }

    public static ThrottlingConfig createThrottlingLeakyConfig(int leakRate) {
        return new ThrottleLeakyConfig(leakRate);
    }

    private static class ThrottleConfig implements ThrottlingConfig {

        private int mMaxConcurrencyLimit;

        public ThrottleConfig(int maxConcurrency) {
            mMaxConcurrencyLimit = maxConcurrency;
        }

        /** @see com.sun.jbi.common.qos.throttling.ThrottlingConfig#getMaxConcurrencyLimit() */
        public int getMaxConcurrencyLimit() {
            return mMaxConcurrencyLimit;
        }
    }

    private static class ThrottleLeakyConfig implements ThrottlingLeakyConfig {

        private int rate;

        public ThrottleLeakyConfig(int rate) {
            this.rate = rate;
        }

        public int getLeakRate() {
            return rate;
        }

        public int getMaxConcurrencyLimit() {
            return -1;
        }
    }
}
