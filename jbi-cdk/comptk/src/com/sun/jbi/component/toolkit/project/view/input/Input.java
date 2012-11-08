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
 * @(#)Input.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.view.input;

import com.sun.jbi.component.toolkit.project.util.XPathElement;
import com.sun.jbi.component.toolkit.project.view.DescriptionList;

/**
 * Defines contract for gui components requesting input from user.
 * Designed for use with {@link DescriptionList}.
 * 
 * @author Kevan Simpson
 */
public interface Input {
    /**
     * Prompts the user for and returns some input.
     * @param txt The text used to identify item about which to prompt user. 
     * @return An item or <code>null</code> if the user cancelled input.
     */
    public Object prompt(String txt);
    
    /**
     * Updates the item.
     * @param item The item to update.
     * @param value The new value.
     * @return <code>true</code> if the item was successfully updated, else <code>false</code>.
     */
    public boolean updateItem(XPathElement item, Object value);
}
