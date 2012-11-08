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
 * @(#)$Id: BPInstanceTextTableBuilder.java,v 1.1 2007/12/04 23:32:39 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.monitor.tool.util;

import java.io.StringWriter;
import java.io.Writer;

import com.sun.caps.management.api.bpel.BPELManagementService.BPInstanceInfo;
import com.sun.jbi.engine.bpel.monitor.tool.util.table.Message;
import com.sun.jbi.engine.bpel.monitor.tool.util.table.TableBuilder;
import com.sun.jbi.engine.bpel.monitor.tool.util.table.TextTablePrinter;

public class BPInstanceTextTableBuilder implements BPInstanceTableBuilder {
    
    private TextTablePrinter textPrinter;

    private Writer writer;

    private TableBuilder builder = new TableBuilder();

    public BPInstanceTextTableBuilder() {
        writer = new StringWriter();
        textPrinter = new TextTablePrinter(writer);
        textPrinter.setColumnSeparator("|");
        textPrinter.setPadding(2);
        textPrinter.setDisplayHeadings(true);
        textPrinter.setHeadingSeparator('=');
        textPrinter.setTotalWidth(120);

        textPrinter.setColumnWidth(0, 45);
        textPrinter.setColumnWidth(1, 15);
        textPrinter.setColumnWidth(2, 25);
        textPrinter.setColumnWidth(3, 25);
        textPrinter.setColumnWidth(4, 25);
        textPrinter.setColumnWidth(5, 25);
        textPrinter.setColumnWidth(6, 25);

        builder.appendHeading(Message.raw("Instance Id", null));
        builder.appendHeading(Message.raw("Status", null));
        builder.appendHeading(Message.raw("Start Time", null));
        builder.appendHeading(Message.raw("End Time", null));
        builder.appendHeading(Message.raw("Updated Time", null));
        builder.appendHeading(Message.raw("Lasted in seconds", null));
        builder.appendHeading(Message.raw("Process Name", null));
    }
    
    
    public void addRow(BPInstanceInfo bpif) {
        builder.startRow();
        builder.appendCell(bpif.id);
        builder.appendCell(bpif.status);
        builder.appendCell(bpif.startTime);
        String endTime = bpif.endTime == null ? "XXXXXXXX" : bpif.endTime
                .toString();
        builder.appendCell(endTime);
        builder.appendCell(bpif.lastUpdateTime);
        builder.appendCell(bpif.lasted);
        builder.appendCell(bpif.bpelId);    
    }

    public String getTableAsString() throws Exception{
        builder.print(textPrinter);
        writer.flush();
        String outputStr = writer.toString();
        return outputStr;
    }

    public void setDisplayHeader(boolean display) {
    }

}
