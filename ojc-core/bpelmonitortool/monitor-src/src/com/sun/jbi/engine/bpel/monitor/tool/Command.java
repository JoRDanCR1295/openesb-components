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
 * @(#)$Id: Command.java,v 1.2 2007/10/09 15:45:23 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.monitor.tool;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Map;

import com.sun.jbi.engine.bpel.monitor.tool.util.CommandUtil;

/**
 * The class to encapsulate user command
 * @author mei
 *
 */
public abstract class Command {
	
	protected String mName;
	protected String mDescription;
	protected Map<String, String> mParams;
	protected CommandContext mContext;
	
	public static final String OUTPUT_FILE = "OUTPUT_FILE";
	public static final String OUTPUT_FILE_APPEND = "OUTPUT_FILE_APPEND";
	
	public Command (String name, CommandContext context) {
		mName = name;
		mContext = context;
	}	
	
	public void setParams (Map<String, String> params) {
		mParams = params;
	}
	
	public void display () {
		String displayStr = CommandUtil.getCommandDisplay(mName);
		System.out.println(displayStr);
//		CommandUtil.displayPrompt();
	}
	
	public CommandContext getContext () {
		return mContext;
	}
	
	abstract void validate () throws ValidationException;
	
	public  CommandContext execute () throws Exception {
		 System.out.println(mName);
		 return mContext;
	 }

	protected boolean isAppended () {
		String outputFile = mParams.get(Command.OUTPUT_FILE);
		String appendStr = mParams.get(Command.OUTPUT_FILE_APPEND);
		boolean isAppended = false;
		if (outputFile != null && outputFile.length() > 0) {
			isAppended = false?true : appendStr.equals("true");
		}	
		return isAppended;
	}
	
	protected void closeStatement(Statement stm) {
		if (stm != null) {
			try {
				stm.close();
			} catch (SQLException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	protected void closeResultSet(ResultSet resultSet) {
		if (resultSet != null) {
			try {
				resultSet.close();
			} catch (SQLException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	protected void closeConnection(Connection conn) {
		if (conn != null) {
			try {
				conn.close();
			} catch (SQLException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}	
}
