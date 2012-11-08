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
 * @(#)$Id: ViewBPELVariableValueCommand.java,v 1.6 2008/05/07 19:12:09 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.monitor.tool;

import java.io.FileOutputStream;
import java.sql.Connection;
import java.util.ResourceBundle;

import com.sun.jbi.engine.bpel.monitor.tool.util.CommandUtil;
import com.sun.jbi.engine.bpel.monitor.tool.util.I18n;


public class ViewBPELVariableValueCommand extends Command {


	public ViewBPELVariableValueCommand(String name, CommandContext context) {
		super(name, context);
		// TODO Auto-generated constructor stub
	}

	@Override
	public CommandContext execute() throws Exception {
		// TODO Auto-generated method stub
		String instanceId = mParams.get("instid");
		String varid = mParams.get("varid");
		String outputFile = mParams.get(Command.OUTPUT_FILE);
		boolean isAppended = isAppended();
		FileOutputStream fos = null;
		if (outputFile != null && outputFile.length() > 0) {
			 fos = new FileOutputStream(outputFile, isAppended);		
		}
		Connection conn = null;
		try {
//			conn = mContext.getResource().getDataSource().getConnection();
//		    String val = BPELManagementAPIFactory.getAPI().getVariableValue(instanceId, Long.parseLong(varid), conn);
			String val = mContext.getResource().getMBpelService().getVariableValue(instanceId, Long.parseLong(varid), mContext.getResource().getTargetName());
		    StringBuffer output = new StringBuffer();
			output.append("Variable value\n");			
			output.append ("-----------------------------------------\n");		
			output.append(val);
			output.append("\n");
			output.append ("-----------------------------------------\n");	
			if (fos != null) {
				fos.write(output.toString().getBytes());
				fos.close();
			}
			System.out.print(output.toString());
		} catch (Exception e) {
				throw e;
		}  finally {
			if (fos != null) {
				fos.close();
			}
		}
		return mContext;	
	}

	@Override
	void validate() throws ValidationException {
		// TODO Auto-generated method stub
        CommandUtil.checkMonitoringVariableEnabled (mContext.getResource().getMBpelService(), mContext.getResource().getTargetName());        
		String instanceId = mParams.get("instid");
		if (CommandUtil.isEmpty(instanceId)) {
			throw new ValidationException (  I18n
                    .loc("BPCOR-6018: Missing Instance Id"));
		}
		String varid = mParams.get("varid");
		if (CommandUtil.isEmpty(varid)) {
			throw new ValidationException (I18n.loc("BPCOR-6001: Missing variable Id"));
		}	
		try {
			Long.parseLong(varid);
		}catch (NumberFormatException e) {
			String errorStr = I18n.loc("BPCOR-6002: Variable Id: {0} is not a long type", varid);
			throw new ValidationException (errorStr);
		}
	}

}
