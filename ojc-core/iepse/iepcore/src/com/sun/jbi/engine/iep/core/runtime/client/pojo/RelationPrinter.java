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
 * @(#)RelationPrinter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.client.pojo;

import java.text.SimpleDateFormat;
import java.sql.Connection;
import java.sql.Timestamp;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.Collections;
import java.io.PrintWriter;
import java.io.FileOutputStream;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import com.sun.jbi.engine.iep.core.runtime.operator.OperatorConstants;

/**
 * RelationPrinter.java
 *
 * Created on September 7, 2005, 11:53 PM
 *
 * @author Bing Lu
 */

public class RelationPrinter extends Printer {
    // -3 SeqId, -2 Timestamp, -1 Tag
    protected void printRow(Connection con, String inputTableName, PrintWriter prw, int rowIdx, Object[] row) throws Exception {
        prw.print(rowIdx + ". ");
        for (int k = 0; k < row.length-2; k++) {
            prw.print(row[k].toString().trim() + ",  ");
        }
        prw.print(mSDF.format((Timestamp)row[row.length-2]) + ",  ");
        prw.print(row[row.length - 1].toString().trim());
        prw.print("\n");
        Util.deleteRowBySeqId(con, inputTableName, row[row.length-3]);
    }
    
    public RelationPrinter() {
    }
}
