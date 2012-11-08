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
 * @(#)$Id: ShowParentBPELInstancesStatusCommand.java,v 1.5 2008/05/07 19:12:07 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.monitor.tool;

import java.io.FileOutputStream;
import java.util.List;
import java.util.ResourceBundle;

import com.sun.caps.management.api.bpel.BPELManagementService.BPInstanceInfo;
import com.sun.jbi.engine.bpel.monitor.tool.util.BPInstanceCVSTableBuilder;
import com.sun.jbi.engine.bpel.monitor.tool.util.BPInstanceTableBuilder;
import com.sun.jbi.engine.bpel.monitor.tool.util.BPInstanceTextTableBuilder;
import com.sun.jbi.engine.bpel.monitor.tool.util.CommandUtil;
import com.sun.jbi.engine.bpel.monitor.tool.util.I18n;

public class ShowParentBPELInstancesStatusCommand extends Command {

	public ShowParentBPELInstancesStatusCommand(String name, CommandContext context) {
		super(name, context);
		// TODO Auto-generated constructor stub
	}

	@Override
	public CommandContext execute() throws Exception {
		// TODO Auto-generated method stub
		String actIdStr = mParams.get("actid");
		String instid = mParams.get("instid");
        boolean isCVSFile = false;
		Long actId = null;
		if (!CommandUtil.isEmpty(actIdStr)) {
			actId = Long.parseLong(actIdStr);
		}
		String outputFile = mParams.get(Command.OUTPUT_FILE);
		boolean isAppended = isAppended();
		FileOutputStream fos = null;
		if (outputFile != null && outputFile.length() > 0) {
			fos = new FileOutputStream(outputFile, isAppended);
            if (outputFile.toLowerCase().endsWith(".csv")) {
                isCVSFile = true;
            }            
		}
		try {
			List<BPInstanceInfo> bpifs = mContext.getResource()
					.getMBpelService().getInvokerInstance(instid, actId, mContext.getResource().getTargetName());

			if (bpifs != null) {
                
                BPInstanceTableBuilder builder =  new BPInstanceTextTableBuilder ();
                BPInstanceTableBuilder cvsBuilder = null;
                if (isCVSFile) {
                    cvsBuilder = new BPInstanceCVSTableBuilder ();
                    if (isAppended) {
                        cvsBuilder.setDisplayHeader(false); 
                    }else {
                        cvsBuilder.setDisplayHeader(true);
                    }                    
                }
				for (BPInstanceInfo bpif : bpifs) {
					builder.addRow(bpif);
                    if (cvsBuilder != null) {
                        cvsBuilder.addRow(bpif);
                    }
				}
                
				String text = builder.getTableAsString();
				if (fos != null) {
                    String fileText = null;
                    if (cvsBuilder != null) {
                        fileText = cvsBuilder.getTableAsString();
                    } else {
                        fileText = text;
                    }
                    fos.write(fileText.getBytes());
					fos.flush();
				}
				System.out.println(text);
			}
			if (fos != null) {
				fos.close();
			}

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
        CommandUtil.checkMonitoringVariableEnabled (mContext.getResource().getMBpelService(), mContext.getResource().getTargetName());        
		String instanceId = mParams.get("instid");
		if (CommandUtil.isEmpty(instanceId)) {
			throw new ValidationException(
					I18n
							.loc("BPCOR-6018: Missing Instance Id"));
		}
		String actIdStr = mParams.get("actid");
		if (!CommandUtil.isEmpty(actIdStr)) {
			try {
				Long.parseLong(actIdStr);
			} catch (NumberFormatException e) {
				String errorStr =I18n.loc("BPCOR-6019: Activity Id: {0} is not a long type",
												actIdStr);
				throw new ValidationException(errorStr);
			}
		}
	}
}
