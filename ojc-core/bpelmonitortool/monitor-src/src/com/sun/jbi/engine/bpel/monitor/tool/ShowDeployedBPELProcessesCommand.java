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
 * @(#)$Id: ShowDeployedBPELProcessesCommand.java,v 1.8 2008/04/07 20:32:46 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.monitor.tool;

import java.io.FileOutputStream;
import java.util.List;
import java.util.ResourceBundle;

import com.sun.jbi.engine.bpel.monitor.tool.util.CommandUtil;
import com.sun.jbi.engine.bpel.monitor.tool.util.I18n;

public class ShowDeployedBPELProcessesCommand extends Command {

    private static final String NOT_FOUND_BPEL = "BPCOR-6016: Can  not find any bpels for this su,  please make sure :\n 1. {0} is a correct SU name, it should not be an SA name.\n" +
    " 2. MonitorEnabled = true in bpelse properties before the project is deployed.\n";

    public ShowDeployedBPELProcessesCommand(String name, CommandContext context) {
		super(name, context);
		// TODO Auto-generated constructor stub
	}
	
	@Override
	void validate() throws ValidationException {
		// TODO Auto-generated method stub
		String suName = mParams.get("su");
		if (CommandUtil.isEmpty(suName))
	{
			throw new ValidationException (I18n.loc("BPCOR-6017:Missing su name"));
		}
	}
	
	public  CommandContext execute () throws Exception {
		String suName = mParams.get("su");
		String outputFile = mParams.get(Command.OUTPUT_FILE);
		boolean isAppended = isAppended();
		FileOutputStream fos = null;
		if (outputFile != null && outputFile.length() > 0) {
			 fos = new FileOutputStream(outputFile, isAppended);		
		}
//		Connection conn = null;
		List <String> bpelIds = null;
		try {
//			conn = mContext.getResource().getDataSource().getConnection();
		    bpelIds =  mContext.getResource().getMBpelService().getBPELProcessIds(suName, mContext.getResource().getTargetName());
		    StringBuffer output = new StringBuffer();
			output.append("BPEL Process ID\n");			
			output.append ("-----------------------------------------\n");		
			if ( bpelIds == null ||  bpelIds.size() == 0) {
				String msg =I18n.loc( NOT_FOUND_BPEL, suName);
				output.append(msg);
			} else {
				for (String bpeId : bpelIds) {
					output.append(bpeId);
					output.append("\n");
				}
			}
			output.append ("-----------------------------------------\n");	
			if (fos != null) {
				fos.write(output.toString().getBytes());
				fos.close();
			}
			System.out.print(output.toString());
		} catch (Exception e) {
				throw e;
		} finally {
			if (fos != null) {
				fos.close();
			}
		}
		return mContext;	
	}
	
}
	

