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
 * @(#)Preferences.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.common.jsf;

// This class provides functionality for table preferences.
public class Preferences {
    private String preference = null; // Rows preference.
    private int rows = 5; // Rows per page.

    // Default constructor.
    public Preferences() {
    }

    // Table preferences event.
    public void applyPreferences() {
        try {
            int rows = Integer.parseInt(preference);
            if (rows > 0) {
                this.rows = rows;
            }
        } catch (NumberFormatException e) {}
    }

    // Get rows per page.
    public int getRows() {
        return rows;
    }

    // Get preference.
    public String getPreference() {
        return Integer.toString(rows);
    }

    // Set preference.
    public void setPreference(String value) {
        preference = value;
    }
}
