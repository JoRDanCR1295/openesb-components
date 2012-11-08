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

package com.sun.jbi.engine.iep.core.runtime.change;

/**
 *
 * @author rdwivedi
 */
public class OperatorPropertyChange implements RuntimeChangeObject {

    private String mOprName;
    private String mPropName;
    private Object mPropValue;
    private String mPlanName;
    private short mType = -1;
    
    public OperatorPropertyChange(String planName,String oprName, String propName, Object propValue) {
       mOprName = oprName;
       mPropName = propName;
       mPropValue = propValue;
       mPlanName = planName;
       mType = OPERATOR_PROPERTY_CHANGE;
    }

    public String getMOprName() {
        return mOprName;
    }

    public String getMPropName() {
        return mPropName;
    }

    public Object getMPropValue() {
        return mPropValue;
    }

    public String getPlanName() {
        return mPlanName;
    }
    public short getType() {
        return mType;
    }
    
}
