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
 * @(#)CocoTokenTypes.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.coco.model;

/**
 * Represents classifications of
 * {@link com.sun.encoder.coco.model.CocoToken CocoTokens}.
 *
 * @author Noel Ang, Lixin Tang
 *
 */
public enum CocoTokenTypes {
    /**
     * Alphanumeric token type
     */
    ALNUM_TOKEN,

    /**
     * Numeric token type
     */
    NUM_TOKEN,

    /**
     * Cobol special character token type
     */
    SPECIALCHAR_TOKEN,

    /**
     * Cobol Separator token type
     */
    SEPARATOR_TOKEN,

    /**
     * Non-Cobol character token type
     */
    NONCOBOL_TOKEN,

    /**
     * EOF token type
     */
    EOF_TOKEN
}
