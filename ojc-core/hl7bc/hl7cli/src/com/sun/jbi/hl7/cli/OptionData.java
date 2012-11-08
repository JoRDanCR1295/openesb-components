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
 * @(#)$Id: OptionData.java
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.hl7.cli;

/**
 *
 * @author ylee
 */
public class OptionData {

    private String name;
    private String shortName;
    private String desc;
    private boolean hasArg = false;

    public OptionData() {
    }

    public OptionData(String name, String shortName, String desc, boolean hasArg) {
        this.name = name;
        this.shortName = shortName;
        this.desc = desc;
        this.hasArg = hasArg;
    }

    public String getDesc() {
        return desc;
    }

    public boolean hasArg() {
        return hasArg;
    }

    public String getName() {
        return name;
    }

    public String getShortName() {
        return shortName;
    }

    public void setDesc(String desc) {
        this.desc = desc;
    }

    public void setHasArg(boolean hasArg) {
        this.hasArg = hasArg;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setShortName(String shortName) {
        this.shortName = shortName;
    }

    public String printUsage() {
       return Constants.TAB+Constants.PREFIX_SHORT+
               shortName+Constants.COMMA+Constants.PREFIX_LONG+name
               +Constants.NEWLINE+Constants.TAB+Constants.TAB+desc;
    }



}
