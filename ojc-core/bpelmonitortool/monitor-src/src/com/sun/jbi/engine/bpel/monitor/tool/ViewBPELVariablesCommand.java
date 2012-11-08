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
 * @(#)$Id: ViewBPELVariablesCommand.java,v 1.6 2008/09/26 23:28:42 vinayram Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.monitor.tool;

import java.io.FileOutputStream;
import java.util.List;
import java.util.ResourceBundle;

import com.sun.caps.management.api.bpel.BPELManagementService.VarInfo;
import com.sun.jbi.engine.bpel.monitor.tool.util.BPInstanceCVSTableBuilder;
import com.sun.jbi.engine.bpel.monitor.tool.util.BPVariableCSVTableBuilder;
import com.sun.jbi.engine.bpel.monitor.tool.util.BPVariableTableBuilder;
import com.sun.jbi.engine.bpel.monitor.tool.util.BPVariableTextTableBuilder;
import com.sun.jbi.engine.bpel.monitor.tool.util.CommandUtil;
import com.sun.jbi.engine.bpel.monitor.tool.util.I18n;

public class ViewBPELVariablesCommand extends Command {


	public ViewBPELVariablesCommand(String name, CommandContext context) {
		super(name, context);
		// TODO Auto-generated constructor stub
	}

	/**
	 * @see Command#execute()
	 */
	// TODO Auto-generated method stub
	public CommandContext execute() throws Exception {
		String instanceId = mParams.get("instid");
		String varName = mParams.get("varname");
		String outputFile = mParams.get(Command.OUTPUT_FILE);
		boolean isCVSFile = false;
		boolean isAppended = isAppended();
		FileOutputStream fos = null;
		if (outputFile != null && outputFile.length() > 0) {
			fos = new FileOutputStream(outputFile, isAppended);
			if (outputFile.toLowerCase().endsWith(".csv")) {
                isCVSFile = true;
            }
		}
		
		try {
			List<VarInfo> varInfos = mContext.getResource().getMBpelService().listBPELVaraibles(instanceId, varName, mContext.getResource().getTargetName());
			if (varInfos.size() > 0) {
				BPVariableTableBuilder builder = new BPVariableTextTableBuilder();
				BPVariableTableBuilder csvBuilder = null;
				if (isCVSFile) {
                    csvBuilder = new BPVariableCSVTableBuilder();
                    if (isAppended) {
                        csvBuilder.setDisplayHeader(false); 
                    } else {
                        csvBuilder.setDisplayHeader(true);
                    }                    
                }
				for (VarInfo varinfo : varInfos) {
					builder.addRow(varinfo); 
					if (csvBuilder != null) {
                        csvBuilder.addRow(varinfo);
                    } 
				}
				String text = builder.getTableAsString();
				if (fos != null) {
					String fileText = null;
                    if (csvBuilder != null) {
                        fileText = csvBuilder.getTableAsString();
                    } else {
                        fileText = text;
                    }
                    fos.write(fileText.getBytes());
					fos.flush();
				}
				System.out.print(text);
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
			throw new ValidationException (I18n
					.loc("BPCOR-6018: Missing Instance Id"));
		}
	}
}
