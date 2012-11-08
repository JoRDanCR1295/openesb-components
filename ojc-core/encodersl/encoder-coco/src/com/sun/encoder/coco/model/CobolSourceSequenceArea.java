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
 * @(#)CocoSourceSequenceArea.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.coco.model;

/**
 * Represents the Sequence Area, columns 1-6 of the Cobol source line.
 *
 * @author  Noel Ang
 * @version $Revision: 1.1 $
 */
public class CobolSourceSequenceArea extends CobolSourceArea {

    private static CobolSourceArea mMe;
    
    protected CobolSourceSequenceArea() {
        super("Sequence Area", 1, 6);
    }
    
    /**
     * Obtain a reference to the CobolSourceSequenceArea.
     * @return CobolSourceArea object
     */
    public static CobolSourceArea getArea() {
        if (mMe == null) {
            mMe = new CobolSourceSequenceArea();
        }
        return mMe;
    }
}
