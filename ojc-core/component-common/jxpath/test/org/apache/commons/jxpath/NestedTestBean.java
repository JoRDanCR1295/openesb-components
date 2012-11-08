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
 * @(#)NestedTestBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath;

/**
 * A general purpose JavaBean for JUnit tests for the "jxpath" component.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class NestedTestBean {
    private String name = "Name 0";
    private int integer = 1;

    public NestedTestBean() {
    }

    public NestedTestBean(String name) {
        this.name = name;
    }

    public void setName(String name) {
        this.name = name;
    }

    /**
     * A read-only boolean property
     */
    public boolean isBoolean() {
        return false;
    }

    /**
     * A read-only int property
     */
    public int getInt() {
        return integer;
    }

    public void setInt(int value) {
        this.integer = value;
    }

    /**
     * A read-only String property
     */
    public String getName() {
        return name;
    }

    private String[] strings =
        new String[] { "String 1", "String 2", "String 3" };

    public String[] getStrings() {
        return strings;
    }

    public void setStrings(String[] array) {
        strings = array;
    }

    public String toString() {
        return "Nested: " + name;
    }
}
