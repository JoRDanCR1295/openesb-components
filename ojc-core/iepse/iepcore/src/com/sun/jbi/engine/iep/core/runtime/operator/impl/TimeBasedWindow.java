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
 * @(#)TimeBasedWindow.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.operator.impl;

import java.sql.Connection;
import java.sql.Timestamp;
import java.util.Map;
import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;

/**
 * TimeBasedWindow.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class TimeBasedWindow extends AbstractOperator {
    protected double mSize;
    protected String mUnit;
    
    public TimeBasedWindow(Map prop) {
        initialize(prop);
        mSize = PropertyUtil.getdouble(prop, PROP_SIZE, 0);
        mUnit = PropertyUtil.getString(prop, PROP_UNIT, TIME_UNIT_SECOND);
    }
    
    public String getOutputType() {
        return IO_TYPE_RELATION;
    }

    protected void createOperateStmt(Connection con) throws Exception {
        if (con ==  null) {
            return;
        }
        mOperateStmt = mDbSpecial.getTimeBasedWindowDb().createOperateStatements(con, this);
    }
        
    protected final void executeOperateStmt(Timestamp prevT, Timestamp curT) throws Exception {
        mOperateStmt[0].setTimestamp(1, prevT);
        mOperateStmt[0].setTimestamp(2, curT);
        mOperateStmt[0].setTimestamp(3, prevT);
        mOperateStmt[0].setTimestamp(4, curT);
        mOperateStmt[0].executeUpdate();
    }
    
    public double getSize() {
        return mSize;
    }
    
    public String getUnit() {
        return mUnit;
    }
    
}
