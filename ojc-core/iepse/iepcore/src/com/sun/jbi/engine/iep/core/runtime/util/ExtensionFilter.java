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
 * @(#)ExtensionFilter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.util;

import java.io.*;

/**
 * ExtensionFilter.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class ExtensionFilter implements FileFilter {
    private String[] mExtensions;
    private boolean mInclude;


    public ExtensionFilter(String[] extensions) {
        this(extensions, true);
    }

    /**
     * @param extensions
     * @todo Document this constructor
     */
    public ExtensionFilter(String[] extensions, boolean include) {
        mExtensions = ArrayUtil.duplicate(extensions);
        mInclude = include;
    }


    /**
     * @param file
     * @todo Document this method
     */
    public boolean accept(File file) {
        if (mExtensions == null || mExtensions.length == 0) {
            return true;
        }

        if (file.isDirectory()) {
            return true;
        }

        String fileName = file.getName();
        int dotInd = fileName.lastIndexOf('.');
        if (dotInd != -1) {
            String ext = fileName.substring(dotInd);
            for (int i = 0; i < mExtensions.length; ++i) {
                if (ext.equalsIgnoreCase(mExtensions[i])) {
                    return mInclude;
                }
            }
        }
        return false;
    }
}
