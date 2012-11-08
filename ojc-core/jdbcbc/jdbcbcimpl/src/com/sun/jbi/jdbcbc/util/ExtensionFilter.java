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

package com.sun.jbi.jdbcbc.util;

import java.io.File;
import java.io.FileFilter;


/**
 *
 * @author Bing Lu
 */
public class ExtensionFilter implements FileFilter {
    private String[] mExtensions;
    private boolean mInclude;

    public ExtensionFilter(final String[] extensions) {
        this(extensions, true);
    }

    /**
     * @param extensions
     * @todo Document this constructor
     */
    private ExtensionFilter(final String[] extensions, final boolean include) {
        mExtensions = ExtensionFilter.duplicate(extensions);
        mInclude = include;
    }

    /**
     * @param file
     * @todo Document this method
     */
    //@Override
    public boolean accept(final File file) {
        if ((mExtensions == null) || (mExtensions.length == 0)) {
            return true;
        }

        if (file.isDirectory()) {
            return true;
        }

        final String fileName = file.getName();
        final int dotInd = fileName.lastIndexOf('.');

        if (dotInd != -1) {
            final String ext = fileName.substring(dotInd);

            for (String element : mExtensions) {
                if (ext.equalsIgnoreCase(element)) {
                    return mInclude;
                }
            }
        }

        return false;
    }

    private static Object[] duplicate(final Object[] a) {
        if (a == null) {
            return a;
        }

        final Object[] c = new Object[a.length];
        System.arraycopy(a, 0, c, 0, a.length);

        return c;
    }

    private static String[] duplicate(final String[] a) {
        if (a == null) {
            return a;
        }

        final String[] c = new String[a.length];
        System.arraycopy(a, 0, c, 0, a.length);

        return c;
    }
}
