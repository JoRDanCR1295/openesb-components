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
 * @(#)ApproximateName.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc.util;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author nang
 */
public class ApproximateName {
    
    public ApproximateName(String name) {
        init(name);
    }

    public boolean similar(String name) {
        if (name == null) {
            return false;
        }

        name = name.trim().toUpperCase();
        if ("".equals(name)) {
            return false;
        }

        ApproximateName thatName = new ApproximateName(name);
        return thatName.equals(this);
    }
    
    public String toString() {
        return "~" + mName;
    }

    public boolean equals(Object that) {
        if (that == null) {
            return false;
        }
        if (!(that instanceof ApproximateName)) {
            return false;
        }
        ApproximateName thatName = (ApproximateName) that;
        return thatName.mName.equals(mName);
    }
    
    private final void init(String name) {
        StringBuffer buf = new StringBuffer(name.length());
        for (int i = 0; i < name.length(); ++i) {
            char c = name.charAt(i);
            if (Character.isLetter(c) || Character.isDigit(c)) {
                buf.append(Character.toUpperCase(c));
            }
        }
        if (buf.length() > 0) {
            mName = buf.toString();
        } else {
            throw new IllegalArgumentException("Unsupported (non-alphanumeric) name: " + name);
        }
    }
    
    private String mName;
}
