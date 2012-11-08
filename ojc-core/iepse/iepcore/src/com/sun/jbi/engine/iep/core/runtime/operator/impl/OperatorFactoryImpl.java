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
 * @(#)OperatorFactoryImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */


package com.sun.jbi.engine.iep.core.runtime.operator.impl;

import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.util.Map;
import com.sun.jbi.engine.iep.core.runtime.operator.OperatorFactory;
import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.OperatorSpec;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.util.logging.Level;

/**
 * OperatorFactoryImpl.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class OperatorFactoryImpl implements OperatorFactory {
    private static final Messages mMessages = Messages.getMessages(OperatorFactoryImpl.class);

    private DbSpecial mDbSpecial = null;
    
    public OperatorFactoryImpl() {
        mDbSpecial = Util.getDbSpecial();
    }

    public Operator createOperator(OperatorSpec opSpec) {
        String name = opSpec.getOperatorType();
        Map<String, Object> prop = opSpec.getOperatorProperties();
        try {
            prop.put(PROP_DB_SPECIAL, mDbSpecial);

            // input
            if (name.equals(OP_STREAM_INPUT)) {
                return new StreamInput(prop);
            }
            if (name.equals(OP_TABLE_INPUT)) {
                return new TableInput(prop);
            }
            if(name.equals(OP_EXTERNAL_TABLE_POLLING_STREAM)) {
                return new ExternalTablePollingStream(prop);
            }
            if(name.equals(OP_REPLAY_STREAM)) {
                return new ReplayStream(prop);
            }
            
            // output
            if (name.equals(OP_STREAM_OUTPUT)) {
                return new StreamOutput(prop);
            }
            if (name.equals(OP_BATCHED_STREAM_OUTPUT)) {
                return new BatchedStreamOutput(prop);
            }
            if (name.equals(OP_RELATION_OUTPUT)) {
                return new RelationOutput(prop);
            }
            if (name.equals(OP_TABLE_OUTPUT)) {
                return new TableOutput(prop);
            }
            if(name.equals(OP_INVOKE_STREAM)) {
                return new InvokeStream(prop);
            }
            if(name.equals(OP_SAVE_STREAM)) {
                return new SaveStream(prop);
            }
            
            // stream to stream
            if (name.equals(OP_STREAM_PROJECTION_AND_FILTER)) {
                return new StreamProjectionAndFilter(prop);
            }
            if (name.equals(OP_TUPLE_SERIAL_CORRELATION)) {
                return new TupleSerialCorrelation(prop);
            }
            if (name.equals(OP_TUPLE_BASED_AGGREGATOR)) {
                return new TupleBasedAggregator(prop);
            }
            if (name.equals(OP_TIME_BASED_AGGREGATOR)) {
                return new TimeBasedAggregator(prop);
            }
            if (name.equals(OP_INVOKE_SERVICE)) {
                return new InvokeService(prop);
            }
            if (name.equals(OP_MERGE)) {
                return new Merge(prop);
            }

            // stream to relation
            if (name.equals(OP_TUPLE_BASED_WINDOW)) {
                return new TupleBasedWindow(prop);
            }

            if (name.equals(OP_PARTITIONED_WINDOW)) {
                return new PartitionedWindow(prop);
            }

            if (name.equals(OP_ATTRIBUTE_BASED_WINDOW)) {
                return new AttributeBasedWindow(prop);
            }

            if (name.equals(OP_TIME_BASED_WINDOW)) {
                return new TimeBasedWindow(prop);
            }
            
            // relation to relation
            if (name.equals(OP_RELATION_MAP)) {
                return new RelationMap(prop);
            }

            if (name.equals(OP_DISTINCT)) {
                return new Distinct(prop);
            }

            if (name.equals(OP_UNION_ALL)) {
                return new UnionAll(prop);
            }

            if (name.equals(OP_UNION)) {
                return new Union(prop);
            }

            if (name.equals(OP_INTERSECT)) {
                return new Intersect(prop);
            }

            if (name.equals(OP_MINUS)) {
                return new Minus(prop);
            }

            if (name.equals(OP_RELATION_AGGREGATOR)) {
                return new RelationAggregator(prop);
            }

            // relation to stream
            if (name.equals(OP_INSERT_STREAM)) {
                return new InsertStream(prop);
            }

            if (name.equals(OP_DELETE_STREAM)) {
                return new DeleteStream(prop);
            }

            if (name.equals(OP_RELATION_STREAM)) {
                return new RelationStream(prop);
            }
            
            if (name.equals(OP_NOTIFICATION_STREAM)) {
                return new NotificationStream(prop);
            }

            // relation to table
            if (name.equals(OP_TABLE)) {
                return new Table(prop);
            }
            
            // sequence
            if (name.equals(OP_GAP_WINDOW)) {
                return new GapWindow(prop);
            }

            if (name.equals(OP_CONTIGUOUS_ORDER)) {
                return new ContiguousOrder(prop);
            }
            
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "OperatorFactoryImpl.Fail_to_create_operator", name, e);
        }
        return null;
    }
}
