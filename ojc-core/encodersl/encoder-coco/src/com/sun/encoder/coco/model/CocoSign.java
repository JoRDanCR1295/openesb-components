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
 * @(#)CocoSign.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.coco.model;

/**
 * Sign information for Cobol entry descriptions.
 *
 * @author Noel Ang
 *
 * @version $Revision: 1.2 $
 *
 * @see com.sun.encoder.coco.model.CocoDescriptionEntry
 */
public final class CocoSign {

    public static CocoSign LeadingSign;
    public static CocoSign TrailingSign;
    static {
        LeadingSign  = new CocoSign("LeadingSign");
        TrailingSign = new CocoSign("TrailingSign");
    }
    public CocoSign(String type) {
        mType = type;
    }
    private String mType;

    @Override
    public String toString() {
        return mType;
    }
}
/* EOF $RCSfile: CocoSign.java,v $ */
