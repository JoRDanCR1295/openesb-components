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
 * @(#)BPVariablesTextTableBuilder.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.monitor.tool.util;

import java.io.StringWriter;
import java.io.Writer;

import com.sun.caps.management.api.bpel.BPELManagementService.BPInstanceInfo;
import com.sun.caps.management.api.bpel.BPELManagementService.VarInfo;
import com.sun.jbi.engine.bpel.monitor.tool.util.table.Message;
import com.sun.jbi.engine.bpel.monitor.tool.util.table.TableBuilder;
import com.sun.jbi.engine.bpel.monitor.tool.util.table.TextTablePrinter;

/**
 * @author Sun Microsystems
 *
 */
public class BPVariableTextTableBuilder implements BPVariableTableBuilder {
	
	private TextTablePrinter textPrinter;

    private Writer writer;

    private TableBuilder builder = new TableBuilder();
    
    public BPVariableTextTableBuilder() {
    	writer = new StringWriter();
        textPrinter = new TextTablePrinter(writer);
        textPrinter.setColumnSeparator("|");
        textPrinter.setPadding(2);
        textPrinter.setDisplayHeadings(true);
        textPrinter.setHeadingSeparator('=');
        textPrinter.setTotalWidth(120);

        textPrinter.setColumnWidth(0, 45);
        textPrinter.setColumnWidth(1, 15);
        textPrinter.setColumnWidth(2, 65);
        textPrinter.setColumnWidth(3, 25);

        builder.appendHeading(Message.raw("Variable Name", null));
        builder.appendHeading(Message.raw("Variable Id", null));
        builder.appendHeading(Message.raw("Scope XPath", null));
        builder.appendHeading(Message.raw("Notes", null));
    }

	/*
	 * @see com.sun.jbi.engine.bpel.monitor.tool.util.BPVariableTableBuilder#addRow(com.sun.caps.management.api.bpel.BPELManagementService.VarInfo)
	 */
	public void addRow(VarInfo varInfo) {
        builder.startRow();
        builder.appendCell(varInfo.varName);
        builder.appendCell(varInfo.varId);
        builder.appendCell(varInfo.xpath);
        builder.appendCell(varInfo.notes);
	}

	/*
	 * @see com.sun.jbi.engine.bpel.monitor.tool.util.BPVariableTableBuilder#getTableAsString()
	 */
	public String getTableAsString() throws Exception {
        builder.print(textPrinter);
        writer.flush();
        String outputStr = writer.toString();
        return outputStr;
	}

	/*
	 * @see com.sun.jbi.engine.bpel.monitor.tool.util.BPVariableTableBuilder#setDisplayHeader(boolean)
	 */
	public void setDisplayHeader(boolean display) {
	}

}
