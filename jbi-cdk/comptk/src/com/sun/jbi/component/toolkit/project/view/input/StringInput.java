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
 * @(#)StringInput.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.view.input;

import javax.swing.JOptionPane;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.toolkit.project.util.XPathElement;

/**
 * 
 * @author Kevan Simpson
 */
public class StringInput implements Input {
    private String mItemName, mAttr, mXPath;

    public StringInput(String itemName, String attr) {
        this(itemName, attr, ".");
    }

    public StringInput(String itemName, String attr, String xpath) {
        mItemName = itemName;
        mAttr = attr;
        mXPath = xpath;
    }
    
    /** @see com.sun.jbi.component.toolkit.project.view.input.Input#prompt(java.lang.String) */
    public Object prompt(String txt) {
        return input("Please enter the "+ mAttr +" of the "+ mItemName +":", 
                     txt == null ? "New "+ mItemName : txt);
    }

    /** @see com.sun.jbi.component.toolkit.project.view.input.Input#updateItem(com.sun.jbi.component.toolkit.project.util.XPathElement, java.lang.Object) */
    public boolean updateItem(XPathElement item, Object value) {
        if (item != null && !Util.isEmpty(mXPath)) {
//            System.out.println("updating item: "+ mXPath +" = "+ value);
            return item.setValue(mXPath, String.valueOf(value));
        }
        return false;
    }

    public static String input(String msg, String title) {
        String str = JOptionPane.showInputDialog(null, msg, title);
        return (Util.isEmpty(str)) ? null : str;
    }
}
