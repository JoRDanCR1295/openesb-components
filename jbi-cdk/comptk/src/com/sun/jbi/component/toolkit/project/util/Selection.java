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
 * @(#)Selection.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.util;

import java.util.regex.Matcher;

/**
 * 
 * @author Kevan Simpson
 */
public class Selection {
    private String mText;
    private int mStart, mEnd;
    
    public Selection(Matcher m) {
        this(m.group(), m.start(), m.end());
    }
    
    public Selection(String text, int start) {
        this(text, start, start + text.length());
    }

    protected Selection(String text, int start, int end) {
        mText = text;
        mStart = start;
        mEnd = end;
    }

    public int getEnd() {
        return mEnd;
    }
    
    public int getOffset() {
        return mStart;
    }
    
    public int getLength() {
        return mEnd - mStart;
    }
    
    public String getText() {
        return mText;
    }

    /** @see java.lang.Object#toString() */
    @Override
    public String toString() {
        StringBuffer buff = new StringBuffer();
        buff.append("Selection[(").append(mStart).append(",").append(mEnd)
            .append("):\"").append(getText()).append("\"]");
        return buff.toString();
    }
}