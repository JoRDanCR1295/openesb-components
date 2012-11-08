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
 * @(#)$Id: ExpressionInfo.java,v 1.4 2010/02/15 19:24:46 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.xpath;


/**
 * Encapsulates the 
 * @author mei
 *
 */
public class ExpressionInfo {
    private static final String TASK_INPUT_VAR ="TaskInput";
    private static final String TASK_OUTPUT_VAR = "TaskOutput";
    
	//The $variableName
    private String mVariableName;
    //The $variableName.partName
    private String mXpathVariableName;
    //The .partName
    private String mPartName;
    //The xpath part
    private String mXpathQuery;
    
    private String mQuery;
    
    public ExpressionInfo(String expr, int beginIndex, int dotPos, int endIndex) {
    	mQuery = expr;
     	mXpathVariableName = expr.substring(beginIndex, endIndex);
        if (dotPos > 0) {
        	mVariableName = expr.substring(beginIndex, dotPos);
        	mPartName = expr.substring(dotPos + 1, endIndex);
        } else {
        	mVariableName = mXpathVariableName;
        	mPartName = null;
        }
       if (endIndex < expr.length() -1) {
    	   mXpathQuery = expr.substring(endIndex + 1);
       } else {
    	   mXpathQuery = null;
       }
    }

	public String getPartName() {
		return mPartName;
	}

	public String getVariableName() {
	    return mVariableName;
	}

	public String getXpathQuery() {
		return  mXpathQuery;
	}

	public String getXpathVariableName() {
        // part is specified, see if variable name is TaskInput or TaskOutput.
        // If yes, get the variable name and ignore part
        if (mPartName != null && (mVariableName.equals(TASK_INPUT_VAR) || mVariableName.equals(TASK_OUTPUT_VAR))) {
            return mVariableName;
        }
        else {
            return mXpathVariableName;
        }
	}
    
    public String getPath () {
        if (mPartName != null && (mVariableName.equals(TASK_INPUT_VAR) || mVariableName.equals(TASK_OUTPUT_VAR))) {
            return "$" + mVariableName + "/" + mXpathQuery;
        } 
        return mQuery;
    }

	@Override
	public String toString() {
		// TODO Auto-generated method stub
		return mQuery;
	}
    
    
    
}
