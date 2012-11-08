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
 * @(#)Request.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.operator;

import java.sql.Timestamp;
import org.w3c.dom.Document;

/**
 *
 * @author Bing Lu
 */
public class Request {
    private String mId;
    private Object[] mRow;
    private Timestamp mTimestamp;
    private Invoker mOp;
    
    public Request(String id, Object[] row, Timestamp timestamp, Invoker op) {
        mId = id;
        mRow = row;
        mTimestamp = timestamp;
        mOp = op;
    }
    
    public String getId() {
        return mId;
    }
    
    public Object[] getRow() {
        return mRow;
    }
    
    public Timestamp getTimestamp() {
        return mTimestamp;
    }
    
    public Document getDoc() {
        return mOp.getDoc(this);
    }
    
    public Invoker getInvoker() {
        return mOp;
    }
}
