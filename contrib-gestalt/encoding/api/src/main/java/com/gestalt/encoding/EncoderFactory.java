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
 * EncoderFactory.java - ver 1.0 - 2006
 *
 * Copyright 2006-2007 Gestalt, LLC. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.gestalt.encoding;

import java.util.HashMap;


/**
 * Factory for obtaining a valid encoder.  This factory uses the singleton
 * pattern for instantiating the implementation of the encoder.
 *
 * @author Phillip Anderson, panderson@gestalt-llc.com
 */
public class EncoderFactory {
    /**
     * String containing the FQN for the FastInfoset Encoder Implementation.
     */
    public static final String FI = "com.gestalt.encoding.FastInfosetEncoderImpl";

    /**
     * String containing the FQN for the GZIP Encoder Implementation.
     */
    public static final String GZIP = "com.gestalt.encoding.GzipEncoderImpl";

    /**
     * String containing the FQN for the Base64 Encoder Implementation.
     */
    public static final String BASE64 = "com.gestalt.encoding.Base64EncoderImpl";

    /**
     * Tracks the single instance of each Encoder Implementation.
     */
    private static HashMap instances = new HashMap(3);

    /**
     * Obtains the current singleton instance of the Encoder.  If the instance
     * has not been created, a call is made to create the singleton instance.
     * @param className The name of the single instance being requested.
     * @return The single instance of the encoder to be used when
     * encoding and decoding information.
     */
    public static Encoder getInstance(String className) {
        Encoder instance = (Encoder) instances.get(className);

        if (null == instance) {
            instance = getImpl(className);
            instances.put(className, instance);
        }

        return instance;
    }

    /**
     * Creates the singleton encoder implementation.
     * @param className The name of the single instance being requested.
     * @return The single instance of the encoder to be used when
     * encoding and decoding information.
     */
    private static Encoder getImpl(String className) {
        try {
            Class cl = Class.forName(className);

            return (Encoder) cl.newInstance();
        } catch (Exception e) {
            e.printStackTrace();
        }

        return null;
    }
}
