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
<<<<<<< ShowBPELInstancesStatusCommand.java
 * @(#)$Id: ShowBPELInstancesStatusCommand.java,v 1.14 2008/09/26 23:28:41 vinayram Exp $
=======
 * @(#)$Id: ShowBPELInstancesStatusCommand.java,v 1.14 2008/09/26 23:28:41 vinayram Exp $
>>>>>>> 1.11.4.1
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.monitor.tool;

import java.io.FileOutputStream;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;

import com.sun.caps.management.api.bpel.BPELManagementService;
import com.sun.caps.management.api.bpel.BPELManagementService.BPInstanceInfo;
import com.sun.caps.management.api.bpel.BPELManagementService.BPStatus;
import com.sun.jbi.engine.bpel.monitor.tool.util.BPInstanceCVSTableBuilder;
import com.sun.jbi.engine.bpel.monitor.tool.util.BPInstanceTableBuilder;
import com.sun.jbi.engine.bpel.monitor.tool.util.BPInstanceTextTableBuilder;
import com.sun.jbi.engine.bpel.monitor.tool.util.CommandUtil;
import com.sun.jbi.engine.bpel.monitor.tool.util.I18n;
import com.sun.jbi.engine.bpel.monitor.tool.util.table.Message;
import com.sun.jbi.engine.bpel.monitor.tool.util.table.TableBuilder;
import com.sun.jbi.engine.bpel.monitor.tool.util.table.TextTablePrinter;

public class ShowBPELInstancesStatusCommand extends Command {

	private static final String INVALID_STATUS = "BPCOR-6010: The status :{0} is not valid";
    
    private static final String INVALID_PARAM = "BPCOR-6011: The param :{0} is not valid";

	private static final String MAX_RECORDS_IS_INTEGER = "BPCOR-6012: max is Integer type";

	private static final String SORT_COLUMN_INVALID = "BPCOR-6013: sort is invalid";

	private static final String ORDER_INVALID = "BPCOR-6014: order is invalid";

	private static final String INSTANCE_OVERFLOW = "BPCOR-6015: The total count of instances is :{0},  please specify"
			+ " max (e.g max=\"500\"),  sort and order";

	private List<BPStatus> mBpStatuses = new ArrayList<BPStatus>();

	private Integer mMaxRecords = null;

	private BPELManagementService.SortColumn mSortColumn = null;

	private BPELManagementService.SortOrder mOrder = null;

	public ShowBPELInstancesStatusCommand(String name, CommandContext context) {
		super(name, context);
		// TODO Auto-generated constructor stub
	}

	@Override
	public CommandContext execute() throws Exception {
		// TODO Auto-generated method stub
		String processId = mParams.get("processId");
		String instid = mParams.get("instid");
		String searchString = mParams.get("searchString");
        boolean isCVSFile = false;        
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
			List<BPInstanceInfo> bpifs = null;
			if (mBpStatuses.size() == 0) {
				BPELManagementService.BPInstanceQueryResult queryResult;
				if (searchString == null) {
					queryResult = mContext.getResource().getMBpelService().getBPELInstances(processId, null, instid, 
							mMaxRecords, mSortColumn, mOrder, mContext.getResource().getTargetName());
				} else {
					queryResult = mContext.getResource().getMBpelService().searchBPELInstances(processId, null, 
							searchString, mMaxRecords, mSortColumn, mOrder, mContext.getResource().getTargetName());
				}
				if (queryResult.overflow) {
					String msg = I18n.loc(INSTANCE_OVERFLOW, queryResult.total);
					System.out.println(msg);
					if (fos != null) {
						fos.close();
					}
					return mContext;
				}
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
                
				for (BPInstanceInfo bpif : queryResult.bpInstnaceList) {
					builder.addRow(bpif);
                    if (cvsBuilder != null) {
                        cvsBuilder.addRow(bpif);
                    }
				}
				String text = builder.getTableAsString();
				text = text + "\n" + queryResult.returned + " of  total:"
						+ queryResult.total + "\n";
				System.out.println(text);
				if (fos != null) {
                    String fileText = null;
                    if (cvsBuilder != null) {
                        fileText = cvsBuilder.getTableAsString();
                    } else {
                        fileText = text;
                    }
					fos.write(fileText.getBytes());
					fos.flush();
					fos.close();
				}

			} else {
                boolean isBegin = true;
				for (BPStatus bpStatus : mBpStatuses) {
					BPELManagementService.BPInstanceQueryResult queryResult;
					if (searchString == null) {
						queryResult = mContext.getResource().getMBpelService().getBPELInstances(processId, bpStatus, 
								instid, mMaxRecords, mSortColumn, mOrder, mContext.getResource().getTargetName());
					} else {
						queryResult = mContext.getResource().getMBpelService().searchBPELInstances(processId, bpStatus, 
								searchString, mMaxRecords, mSortColumn, mOrder, mContext.getResource().getTargetName());
					}
					if (queryResult.overflow) {
						String msg = I18n.loc(INSTANCE_OVERFLOW, queryResult.total);
						System.out.println(msg);
						if (fos != null) {
							fos.close();
						}
						return mContext;
					}
                    BPInstanceTableBuilder builder =  new BPInstanceTextTableBuilder ();
                    BPInstanceTableBuilder cvsBuilder = null;
                    if (isCVSFile) {
                        cvsBuilder = new BPInstanceCVSTableBuilder ();
                        if (isAppended || !isBegin) {
                            cvsBuilder.setDisplayHeader(false); 
                        }else {
                            cvsBuilder.setDisplayHeader(true);
                        }                    
                    }
					for (BPInstanceInfo bpif : queryResult.bpInstnaceList) {
						builder.addRow(bpif);
                        if (cvsBuilder != null) {
                            cvsBuilder.addRow(bpif);
                        }                        
					}
                    if (isBegin) {
                        isBegin = false;
                    }
					String text = builder.getTableAsString();
                    text = text + "\n" + queryResult.returned + " of  total:"
							+ queryResult.total + " status:" + bpStatus.name()
							+ "\n";
					System.out.println(text);
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
				}
				if (fos != null) {
					fos.close();
				}
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
		mBpStatuses.clear();
		mMaxRecords = null;
		mSortColumn = null;
		mOrder = null;

		String instid = mParams.get("instid");
		String status = mParams.get("status");
		String processId = mParams.get("processId ");
		String searchString = mParams.get("searchString");

		String maxRecordsString = mParams.get("max");
		String sortColumnString = mParams.get("sort");
		String sortOrderString = mParams.get("order");

		if (searchString != null) {
			CommandUtil.checkMonitoringVariableEnabled(mContext.getResource().getMBpelService(), 
					mContext.getResource().getTargetName());
		}
		
		if (!CommandUtil.isEmpty(maxRecordsString)) {
			try {
				mMaxRecords = Integer.parseInt(maxRecordsString);
			} catch (Exception e) {
				String msg = I18n.loc(MAX_RECORDS_IS_INTEGER);
				throw new ValidationException(msg, e);
			}
		}

		if (!CommandUtil.isEmpty(sortColumnString)) {
			if (sortColumnString
					.equalsIgnoreCase(BPELManagementService.SortColumn.STARTTIME
							.toString())) {
				mSortColumn = BPELManagementService.SortColumn.STARTTIME;
			} else if (sortColumnString
					.equalsIgnoreCase(BPELManagementService.SortColumn.ENDTIME
							.toString())) {
				mSortColumn = BPELManagementService.SortColumn.ENDTIME;
			} else if (sortColumnString
					.equalsIgnoreCase(BPELManagementService.SortColumn.UPDATEDTIME
							.toString())) {
				mSortColumn = BPELManagementService.SortColumn.UPDATEDTIME;
			} else {
				String msg = I18n.loc(SORT_COLUMN_INVALID);
				throw new ValidationException(msg);
			}
		}

		if (!CommandUtil.isEmpty(sortOrderString)) {
			if (sortOrderString
					.equalsIgnoreCase(BPELManagementService.SortOrder.ASC
							.name())) {
				mOrder = BPELManagementService.SortOrder.ASC;
			} else if (sortOrderString
					.equalsIgnoreCase(BPELManagementService.SortOrder.DESC
							.name())) {
				mOrder = BPELManagementService.SortOrder.DESC;
			} else {
				String msg = I18n.loc(ORDER_INVALID);
				throw new ValidationException(msg);
			}
		}
		if (status != null) {
			String statuses[] = status.split("\\|");
			for (int i = 0; i < statuses.length; i++) {
                BPStatus bpStatus = null;
                try {
                    bpStatus = BPStatus.valueOf(statuses[i].trim()
						.toUpperCase());
                } catch (Exception e) {
                    // TODO: handle exception
                    throw new ValidationException(I18n.loc(INVALID_STATUS, statuses[i].trim()));
                } 
				if (bpStatus != null) {
					mBpStatuses.add(bpStatus);
				} else {
					throw new ValidationException(I18n.loc(INVALID_STATUS, statuses[i].trim()));
				}
			}
		}
        for (String key : mParams.keySet()) {
            if (! (key.equals("instid") || key.equals("searchString") || key.equals("status") || key.equals("processId") 
            		|| key.equals("max") || key.equals("sort") || key.equals("order") || key.equals("OUTPUT_FILE") 
            		|| key.equals("OUTPUT_FILE_APPEND"))) {
                throw new ValidationException(
                        I18n.loc(INVALID_PARAM,  key));         
            }
            
        }

	}

}
