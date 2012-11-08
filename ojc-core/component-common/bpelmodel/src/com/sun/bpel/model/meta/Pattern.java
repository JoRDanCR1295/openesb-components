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
 * @(#)Pattern.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.meta;

import java.io.Serializable;


/**
 * Pattern Interface Pattern is an attribute of the correlation element. Pattern is optional and
 * takes only the enumarated values set as fields.
 *
 * @author Sun Microsystems
 * @version 
 *
 * @since 5.0
 */
public interface Pattern extends Serializable {
    /** IN pattern */
    String IN_STR = "in"; //$NON-NLS-1$

    /** OutIn pattern */
    String OUT_IN_STR = "out-in"; //$NON-NLS-1$

    /** Out pattern */
    String OUT_STR = "out"; //$NON-NLS-1$

    /** In pattern */
    int IN = 0;

    /** Out pattern */
    int OUT = 1;

    /** OutIn pattern */
    int OUT_IN = 2;

    /**
     * int value
     *
     * @return int int value
     */
    public int intValue();
}
