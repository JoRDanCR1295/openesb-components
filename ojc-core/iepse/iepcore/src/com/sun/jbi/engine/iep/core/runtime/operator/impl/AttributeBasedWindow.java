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
 * @(#)AttributeBasedWindow.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.operator.impl;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.sql.Connection;
import java.util.Map;
import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;
import java.sql.Timestamp;
import java.util.HashMap;

/**
 * AttributeBasedWindow.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 * @author IEP Team
 */
public class AttributeBasedWindow extends AbstractOperator {
    private static final Messages mMessages = Messages.getMessages(AttributeBasedWindow.class);

    protected String mAttribute;
    protected String mAttributeType;
    protected double mSize; // default unit for date is day, and for timestamp is millisecond
    
    public AttributeBasedWindow(Map prop) {
        initialize(prop);
        // if no attribute is specified, use ems_timestamp
        mAttribute = PropertyUtil.getString(prop, PROP_ATTRIBUTE, COL_TIMESTAMP);
        Schema inputSchema = mInputOperatorList.get(0).getOutputSchema();
        mAttributeType = inputSchema.getColumnMetadata(mAttribute).getColumnType();
        double size = PropertyUtil.getdouble(prop, PROP_SIZE, 0.0);
        // Default unit for timestamp type on IEP editor is second, hence convert to milliseconds
        mSize = mAttributeType.equals(SQL_TYPE_TIMESTAMP)? Math.round(size*1000) : size;
    }
    
    public String getOutputType() {
        return IO_TYPE_RELATION;
    }

    @Override
    protected void createOperateStmt(Connection con) throws Exception {
        if (con ==  null) {
            return;
        }
        mOperateStmt = mDbSpecial.getAttributeBasedWindowDb().createOperateStatements(con, this);
    }

    // template method: used by operate(..)
    @Override
    protected void executeOperateStmt(Timestamp prevT, Timestamp curT) throws Exception {
        mDbSpecial.getAttributeBasedWindowDb().executeOperateStatements(this, prevT, curT);
    }

    public String getAttribute() {
        return mAttribute;
    }
    
    public String getAttributeType() {
        return mAttributeType;
    }
    
    /**
     *  Default unit for date is day, and for timestamp is millisecond
     */
    public double getSize() {
        return mSize;
    }

    @Override
    public Map<String, Object> getAdministrableProperties() {
        HashMap<String,Object> map = (HashMap<String,Object>)super.getAdministrableProperties();
        map.put(PROP_ATTRIBUTE, mAttribute);
        map.put(PROP_SIZE, new Double(mSize));
        return map;
    }

    @Override
    public void setAdministrableProperty(String propName, Object propValue) {
        throw new UnsupportedOperationException("Not supported yet.");
    }
}
