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
 * @(#)OperatorSpec.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.operator;

import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;
import java.util.Map;

/*
 * OperatorSpec.java
 *
 * Created on April 3, 2007, 4:52 PM
 *
 * @author Bing Lu
 */
public class OperatorSpec implements Comparable<OperatorSpec>, OperatorConstants {
    String mOpType;
    Map<String, Object> mOpProp;
    int mTopologicalScore;
    
    /** Creates a new instance of OperatorSpec */
    public OperatorSpec(String opType, Map<String, Object> opProp) {
        mOpType = opType;
        mOpProp = opProp;
        mTopologicalScore = PropertyUtil.getint(opProp, PROP_TOPO_SCORE, 0);
    }
    
    public boolean equals(OperatorSpec o) {
        return mTopologicalScore == o.mTopologicalScore;
    }
    
    public int compareTo(OperatorSpec o) {
        return mTopologicalScore - o.mTopologicalScore;
    }
    
    public String getOperatorType() {
        return mOpType;
    }
    
    public Map<String, Object> getOperatorProperties() {
        return mOpProp;
    }
}
