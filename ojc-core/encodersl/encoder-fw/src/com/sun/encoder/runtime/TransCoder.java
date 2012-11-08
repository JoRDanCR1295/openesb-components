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
 * @(#)TransCoder.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime;

/**
 * Interface for objects that can translate between byte-arrays of different
 * encodings. This is intended for ante-unmarshal and post-marshal processing
 * in OTDs.
 *
 * @author Michael Libourel
 * @version
 */
public interface TransCoder {

    /**
     * Transforms the input to the output encoding.
     * If "readOnly" is false, the method may replace the contents of "b".
     *
     * @param b  a byte array, or null
     * @param readOnly  if true, will not change or return "b"
     * @return byte array containing the recoded data, or null if b was null
     * @throws CoderException for decoding or encoding problems
     */
    byte[] recode(byte[] b, boolean readOnly)
        throws CoderException;
}
