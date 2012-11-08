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
 * @(#)NameGenerator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.swift.xsdbuilder;

import java.util.HashSet;
import java.util.Set;

/**
 * A utility that checks if a name has been used before.  Also provides 
 * a way to generate a unique name.
 * 
 * @author Jun Xu
 */
public class NameGenerator {

    Set<String> mNameUsed = new HashSet<String>();
    
    public boolean nameExists(String name) {
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        return mNameUsed.contains(name.replaceAll("\\.", ""));
    }
    
    public String suggestClassName(String name) {
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        String suggestedName = name.replaceAll("\\.", "");
        if (mNameUsed.contains(suggestedName)) {
            suggestedName = name.replace('.', 'x');
        }
        while (mNameUsed.contains(suggestedName)) {
            suggestedName += "x";
        }
        mNameUsed.add(suggestedName);
        return suggestedName;
    }
    
    public void markUsed(String name) {
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        mNameUsed.add(name.replaceAll("\\.", ""));
    }
}
