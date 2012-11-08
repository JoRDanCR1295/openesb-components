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
 * @(#)$Id: ChangeBPELVariableValueCommand.java,v 1.7 2008/05/07 19:12:06 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.monitor.tool;

import java.io.FileOutputStream;
import java.util.ResourceBundle;

import com.sun.jbi.engine.bpel.monitor.tool.util.CommandUtil;
import com.sun.jbi.engine.bpel.monitor.tool.util.I18n;


public class ChangeBPELVariableValueCommand extends Command {

	private static final String SUCCESS = "BPCOR-5000: Successfully changed variable value";
	private static final String FAIL = "BPCOR-5001: Fail to change variable value. \n Common mistake: please not if xpath and part are specified, the xpath should not include the root element of the part.";
	private static final String NEWVALUE = "BPCOR-5002: Check out the new value: v instid=\"{0}\" varid=\"{1}\"\n ";

	
	public ChangeBPELVariableValueCommand(String name, CommandContext parent) {
		super(name, parent);
		// TODO Auto-generated constructor stub
	}

	@Override
	public CommandContext execute() throws Exception {
		// TODO Auto-generated method stub       
		String instanceId = mParams.get("instid");
		long varid = Long.parseLong(mParams.get("varid"));
		String newval = mParams.get("newval");
		String part = mParams.get("part");
		String xpath = mParams.get("xpath");
		if (CommandUtil.isEmpty(part)) {
			part = null;
		}
		if (CommandUtil.isEmpty(xpath)) {
			xpath = null;
		}
		String outputFile = mParams.get(Command.OUTPUT_FILE);
		boolean isAppended = isAppended();
		FileOutputStream fos = null;
		if (outputFile != null && outputFile.length() > 0) {
			 fos = new FileOutputStream(outputFile, isAppended);		
		}
		String result = null;
		Exception ex = null;
		try {
//			 BPELManagementAPIFactory.getAPI().changeVariableValue(instanceId, varid, part, xpath, newval,  mContext.getResource().getMbeanConn());
			mContext.getResource().getMBpelService().changeVariableValue(instanceId, varid, part, xpath, newval, mContext.getResource().getTargetName());
			result = I18n.loc(SUCCESS);
			 
			 String getNewValue = I18n.loc(NEWVALUE, instanceId, (new Long (varid)).toString());
			 result = result + "\n" +getNewValue;
		} catch (Exception e) {
			result = I18n.loc(FAIL);
			ex = e;			
		}
		if (fos != null) {
			fos.write(result.getBytes());
			fos.close();
		}
		System.out.println(result);		
		if (ex != null) {
			throw ex;
		} 
		return mContext;		
	}

	@Override
	void validate() throws ValidationException {
		// TODO Auto-generated method stub
        CommandUtil.checkMonitoringVariableEnabled (mContext.getResource().getMBpelService(), mContext.getResource().getTargetName());
		String instanceId = mParams.get("instid");
		if (CommandUtil.isEmpty(instanceId)) {
			throw new ValidationException (I18n.loc("BPCOR-6018: Missing Instance Id"));
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
