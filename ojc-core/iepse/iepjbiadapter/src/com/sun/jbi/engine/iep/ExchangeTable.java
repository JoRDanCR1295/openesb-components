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
 * @(#)ExchangeTable.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep;

import java.util.*;

import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import javax.jbi.messaging.InOnly;

/**
 * ExchangeTable.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class ExchangeTable {
    //    private static final Logger mLogger = Messages.getLogger(ExchangeTable.class);

    // meId -> ExchangeRecord
    private HashMap<String, ExchangeRecord> mMeTable = new HashMap<String, ExchangeRecord>();

    // Operator -> Set<meId>
    private HashMap<Operator, ExchangeRecord> mOpTable = new HashMap<Operator, ExchangeRecord>();


    public synchronized void addInOnly(String msgId, ExchangeRecord record) {
        if (record == null) {
            return;
        }
        mMeTable.put(msgId, record);
        Operator op = record.getOperator();
        if (mOpTable.get(op) == null) {
            mOpTable.put(op, record);
        }
    }

    public synchronized ExchangeRecord deleteInOnly(InOnly inOnly) {
        String msgId = (String) inOnly.getProperty(ServiceQuality.MESSAGE_ID);
        ExchangeRecord record = mMeTable.remove(msgId);
        return record;
    }

    public synchronized ExchangeRecord getInOnly(InOnly inOnly) {
        String msgId = (String) inOnly.getProperty(ServiceQuality.MESSAGE_ID);
        ExchangeRecord record = mMeTable.remove(msgId);
        return record;
    }
    
    public synchronized ExchangeRecord getRecord(Operator op) {
        return mOpTable.get(op);
    }

    public synchronized ExchangeRecord deleteRecord(Operator op) {
        return mOpTable.remove(op);
    }

}
