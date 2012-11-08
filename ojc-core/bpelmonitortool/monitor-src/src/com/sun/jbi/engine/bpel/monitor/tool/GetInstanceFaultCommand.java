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
 * @(#)$Id: GetInstanceFaultCommand.java,v 1.3 2008/04/07 20:32:46 mei_wu Exp $
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

public class GetInstanceFaultCommand extends Command {

	public GetInstanceFaultCommand(String name, CommandContext context) {
		super(name, context);
		// TODO Auto-generated constructor stub
	}

	@Override
	public CommandContext execute() throws Exception {
		// TODO Auto-generated method stub
		String instanceId = mParams.get("instid");
		String outputFile = mParams.get(Command.OUTPUT_FILE);
		boolean isAppended = isAppended();
		FileOutputStream fos = null;
		if (outputFile != null && outputFile.length() > 0) {
			fos = new FileOutputStream(outputFile, isAppended);
		}
		try {
			StringBuffer output = new StringBuffer();
			String result = mContext.getResource().getMBpelService()
					.getBPELInstanceFault(instanceId, mContext.getResource().getTargetName());
			if (result == null) {
				result = "NONE";
			}
			output.append(result);
			if (fos != null) {
				fos.write(output.toString().getBytes());
				fos.close();
			}
			System.out.println(output.toString());
		} catch (Exception e) {
			throw e;
		} finally {
			if (fos != null) {
				fos.close();
			}
		}
		return mContext;
	}

	@Override
	void validate() throws ValidationException {
		// TODO Auto-generated method stub
		String instanceId = mParams.get("instid");
		if (CommandUtil.isEmpty(instanceId)) {
			throw new ValidationException(
                    I18n
                    .loc("BPCOR-6018: Missing Instance Id"));
		}
	}

}
