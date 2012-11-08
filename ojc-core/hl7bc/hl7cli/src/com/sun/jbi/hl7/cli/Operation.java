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
 * @(#)$Id: Operation.java
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7.cli;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author ylee
 */
public class Operation {

    private String name;
    private String description;
    private String shortName;             // short name
    private String methodName;            // actual API method name
    private boolean hasArgs = false;      // method has argument
    private List<String> args = new ArrayList<String>();    // list of arguments
    private String usage;
    private int    numberRequiredArgs;    // number of required args

    
    public Operation() {
    }

    public Operation(String name, String shortName, String description, String methodName, boolean hasArg,
            String usage, int numRequiredArgs) {
        this.name = name;
        this.shortName = shortName;
        this.description = description;
        this.methodName = methodName;
        this.hasArgs = hasArg;
        this.usage = usage;
        this.numberRequiredArgs = numRequiredArgs;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public boolean hasArgs() {
        return hasArgs;
    }

    public void setHasArgs(boolean hasArg) {
        this.hasArgs = hasArg;
    }

    public String getMethodName() {
        return methodName;
    }

    public void setMethodName(String methodName) {
        this.methodName = methodName;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getShortName() {
        return shortName;
    }

    public void setShortName(String shortName) {
        this.shortName = shortName;
    }

    public void addArg(String arg) {
        args.add(arg);
    }

    public String getArg(int index) {
        if ( index<args.size() ) {
            return args.get(index);
        }
        return null;
    }

    public List<String> getArgs() {
        return args;
    }

    public void copy(Operation op) {
        this.name = op.name;
        this.shortName = op.shortName;
        this.description = op.description;
        this.methodName = op.methodName;
        this.hasArgs = op.hasArgs;
        this.usage = op.usage;
    }

    public int getNumberRequiredArgs() {
        return numberRequiredArgs;
    }

    public void setNumberRequiredArgs(int numberRequiredArgs) {
        this.numberRequiredArgs = numberRequiredArgs;
    }

    public boolean validate() {
        if ( hasArgs() ) {
            if ( args.isEmpty() || args.size()<numberRequiredArgs ) {
                return false;
            }
        }
        return true;
    }

    public String toString() {
       //return "Operation:\n" + "\tname="+name + "\n\tshortName=" + shortName + "\n\tdescription="+
       //       description +"\n\tmethodName="+methodName + "\n\targs=" + args;
       StringBuffer sb = new StringBuffer();
       if ( args!=null ) {
           for ( String arg : args ) {
               sb.append(" "+arg);
           }
       }
       return "operation: " + name + sb.toString();
    }

    public String printUsage() {
        return Constants.TAB + usage + Constants.NEWLINE + Constants.TAB + Constants.TAB + description;
    }

}
