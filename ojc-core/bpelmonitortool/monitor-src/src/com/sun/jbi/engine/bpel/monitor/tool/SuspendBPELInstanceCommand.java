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
<<<<<<< SuspendBPELInstanceCommand.java
 * @(#)$Id: SuspendBPELInstanceCommand.java,v 1.10 2008/05/07 19:12:08 mei_wu Exp $
=======
 * @(#)$Id: SuspendBPELInstanceCommand.java,v 1.10 2008/05/07 19:12:08 mei_wu Exp $
>>>>>>> 1.8.4.1
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.monitor.tool;

import java.io.File;
import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;

import com.sun.jbi.engine.bpel.monitor.tool.util.CommandUtil;
import com.sun.jbi.engine.bpel.monitor.tool.util.I18n;

public class SuspendBPELInstanceCommand extends Command {

	private static final String SUCCESS = "BPCOR-5006: Successfully suspended instance:{0}";

	private static final String FAIL = "BPCOR-5007: Fail to suspend instance:{0}";
    
    private static final String TOTAL = "BPCOR-5010: Total {0} instances {1}";

	private static final String NO_INSTANCE = "BPCOR-6020: The instance :{0} might be completed or faulted, please check the instance status";

	private static final String BOTH_PROCESS_AND_INSTANCEIDS_PRESENT = "BPCOR-5005: Instanceid is present, command will use instanceids and ignore processId or csv";
    
    private static final String NOT_VALID_CSV_FILE="BPCOR-6007: The specified csv file : {0} is not valid, it may not exist or not a readable file, please check";
    
    private static final String ERROR_PARSING_CSV_FILE="BPCOR-6008: Error parsing the specified cvs file : {0} ";

	private List<String> mInstances = new ArrayList<String>();
    
    private File  mCSVFile = null;

	public SuspendBPELInstanceCommand(String name, CommandContext context) {
		super(name, context);
		// TODO Auto-generated constructor stub
	}

	@Override
	public CommandContext execute() throws Exception {
		// TODO Auto-generated method stub
		String outputFile = mParams.get(Command.OUTPUT_FILE);
		boolean isAppended = isAppended();
		FileOutputStream fos = null;
		if (outputFile != null && outputFile.length() > 0) {
			fos = new FileOutputStream(outputFile, isAppended);
		}
		StringBuffer result = new StringBuffer();
		Exception ex = null;
		String processId = mParams.get("processId");
		if (mInstances.size() == 0) {
			try {
				List<String> suspended = mContext.getResource()
						.getMBpelService().suspendAllInstance(processId, mContext.getResource().getTargetName());

				if (suspended != null && suspended.size() > 0) {
					for (String instanceid : suspended) {
						result.append(I18n.loc(SUCCESS,  instanceid));
						result.append("\n");
					}
                    result.append(I18n.loc(TOTAL, suspended.size(), "SUSPENDED"));
				}
			} catch (Exception e) {
				result.append(I18n.loc(
                        FAIL,
						processId));
				result.append("\n");
				ex = e;
			}
		} else {

			for (String instanceid : mInstances) {
				try {
					// BPELManagementAPIFactory.getAPI().suspendInstance(instanceid,
					// mContext.getResource().getMbeanConn());
					boolean done = mContext.getResource().getMBpelService()
							.suspendInstance(instanceid, mContext.getResource().getTargetName());
					if (done) {
						result.append(I18n.loc(SUCCESS, instanceid));
						result.append("\n");
					} else {
						result.append(I18n.loc(FAIL, instanceid));
						result.append("\n");
						result.append(I18n.loc(NO_INSTANCE, instanceid));
						result.append("\n");
					}
				} catch (Exception e) {
					result.append(I18n.loc(FAIL, instanceid));
					result.append("\n");
					ex = e;
				}
			}
		}
		if (fos != null) {
			fos.write(result.toString().getBytes());
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
		mInstances.clear();
        mCSVFile = null;
		String instanceId = mParams.get("instid");
		String processId = mParams.get("processId");
        String csvFile = mParams.get("csv");
        
		if (CommandUtil.isEmpty(instanceId) && CommandUtil.isEmpty(processId) && CommandUtil.isEmpty(csvFile)) {
			throw new ValidationException(
					I18n.loc("BPCOR-6009: Require one of the three params: instanceid, processId or csv"));
		}
		if (!CommandUtil.isEmpty(instanceId) && (!CommandUtil.isEmpty(processId) || !CommandUtil.isEmpty(csvFile))) {
			String msg =I18n.loc(                            
                    BOTH_PROCESS_AND_INSTANCEIDS_PRESENT);
			System.out.println(msg);
		}
        if (! CommandUtil.isEmpty(csvFile)) {
            File csvIn = new File (csvFile);
            if (!csvIn.exists() || !csvIn.isFile() || !csvIn.canRead()) {
                String msg =  I18n.loc(NOT_VALID_CSV_FILE, csvFile);                 
                throw new ValidationException(
                        msg);                
            }
            mCSVFile = csvIn;
        }
		if (instanceId != null) {
			String instance[] = instanceId.split("\\|");
			for (int i = 0; i < instance.length; i++) {
				mInstances.add(instance[i].trim());
			}
		} else if (mCSVFile != null) {
            try {
                mInstances.addAll(CommandUtil.parseCSVForInstanceIds (mCSVFile));
            }catch (Exception e) {
                String msg = I18n.loc (
                        ERROR_PARSING_CSV_FILE,
                        csvFile);                                             
                throw new ValidationException(msg, e);                
            }
            
        }
	}

}
