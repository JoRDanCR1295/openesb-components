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
 * @(#)$Id: ShowActivityStatusOnBPELInstanceCommand.java,v 1.6 2008/04/07 20:32:46 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.monitor.tool;

import java.io.FileOutputStream;
import java.io.StringWriter;
import java.io.Writer;
import java.util.List;
import java.util.ResourceBundle;

import com.sun.caps.management.api.bpel.BPELManagementService.ActivityStatus;
import com.sun.jbi.engine.bpel.monitor.tool.util.CommandUtil;
import com.sun.jbi.engine.bpel.monitor.tool.util.I18n;
import com.sun.jbi.engine.bpel.monitor.tool.util.table.Message;
import com.sun.jbi.engine.bpel.monitor.tool.util.table.TableBuilder;
import com.sun.jbi.engine.bpel.monitor.tool.util.table.TextTablePrinter;

public class ShowActivityStatusOnBPELInstanceCommand extends Command {


	public ShowActivityStatusOnBPELInstanceCommand(String name,
			CommandContext context) {
		super(name, context);
		// TODO Auto-generated constructor stub
	}

	// @Override
	public CommandContext execute() throws Exception {
		// TODO Auto-generated method stub
		String outputFile = mParams.get(Command.OUTPUT_FILE);
		String instid = mParams.get("instid");
		boolean isAppended = isAppended();
		FileOutputStream fos = null;
		if (outputFile != null && outputFile.length() > 0) {
			fos = new FileOutputStream(outputFile, isAppended);
		}
		StringBuffer output = new StringBuffer();
		try {
			List<ActivityStatus> actStatusLst = mContext.getResource()
					.getMBpelService().getBPELInstanceActivityStatus(instid, mContext.getResource().getTargetName());
			Writer writer = new StringWriter ();
			TextTablePrinter textPrinter = new TextTablePrinter(writer);
			textPrinter.setColumnSeparator("|");
			textPrinter.setPadding(2);
			textPrinter.setDisplayHeadings(true);
			textPrinter.setHeadingSeparator('=');
			textPrinter.setTotalWidth(250);
			
			textPrinter.setColumnWidth(0, 15);
			textPrinter.setColumnWidth(1, 40);
			textPrinter.setColumnWidth(2, 15);
			textPrinter.setColumnWidth(3, 25);
			textPrinter.setColumnWidth(4, 25);
			textPrinter.setColumnWidth(5, 35);
			
			
			TableBuilder builder = new TableBuilder();
			builder.appendHeading(Message.raw("Activity Id", null));
			builder.appendHeading(Message.raw("Activity xpath", null));
            builder.appendHeading(Message.raw("Iteration", null));
			builder.appendHeading(Message.raw("Status", null));            
			builder.appendHeading(Message.raw("Start Time", null));
			builder.appendHeading(Message.raw("End Time", null));
			builder.appendHeading(Message.raw("Lasted in seconds", null));
			for (ActivityStatus actStatus : actStatusLst) {
				builder.startRow();
				builder.appendCell(actStatus.activityId);
				builder.appendCell(actStatus.activityXpath);
                builder.appendCell(actStatus.iteration);
				builder.appendCell(actStatus.status);
				builder.appendCell(actStatus.startTime);
				String endTime = actStatus.endTime == null ? "XXXXXXXX" : actStatus.endTime.toString();
				builder.appendCell(endTime);
				builder.appendCell(actStatus.lasted);
			}
			builder.print(textPrinter);
			writer.flush();
			String outputStr = writer.toString();			
			

			if (fos != null) {
				fos.write(outputStr.getBytes());
				fos.close();
			}			
			
			System.out.print(outputStr);

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
