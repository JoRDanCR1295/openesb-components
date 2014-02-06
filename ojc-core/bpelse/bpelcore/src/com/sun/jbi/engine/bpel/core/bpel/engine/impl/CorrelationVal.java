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
 * @(#)CorrelationVal.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine.impl;

import javax.xml.namespace.QName;

import com.sun.bpel.model.util.Utility;
import com.sun.wsdl4j.ext.bpel.MessagePropertyAlias;

/**
 * The Correlation ID will uniquely identify a BPEL instance. This value is
 * used to route the incoming request to the appropriate instance. The value
 * of this ID will look like
 * {valueOfCorrelationSetUniqueID}{valueOfProp1}{valueOfprop2}....{valueOfLastProp}{uniqueIDofBPEL}
 * As per the BPEL specifcation, the correlation value would be
 * {valueOfProp1}{valueOfProp2}....{valueOfLastProp}. To facilitate, support
 * of correlations that are totally unrelated to each other but end up
 * having the same ID, a prefix of "valueOfCorrelationSetUniqueID" is added.
 * ValueOfCorrelationSetUniqueID is preferred to the
 * valueOfCorrelationSetName because of the possibility of overloaded names
 * in the same BPEL within scopes. When the model is parsed, each
 * CorrelationSet defined in the BPEL is given a uniqueID, and the ID is
 * consistently assigned to the same Set whenever the BPEL is parsed.
 * 
 * Extending the same reasoning further, to differentiate CorrelationIDs
 * across different BPELs within the same engine, the ID is suffixed with
 * the "uniqueIDof BPEL". This plays an important role in the design for
 * recovery. Since the engine uses one persistenceStore, all the
 * correlationIDs across BPEL instances are stored in the same table.
 * 
 * @author Sun Microsystems Jan 20, 2006
 */
public class CorrelationVal {

    private long mCorrSetID = Long.MIN_VALUE;

    private QName[] mPropDataTypes;

    private String[] mPropVals;
    
    private MessagePropertyAlias[] mPropAliases;

    private int mHashCode;

    private String mToString;
    
    private boolean mIsRecoveredVal = false;

    //CorrelationVal(String procId, long corrSetID, String[] propDataTypes, String[] propVals) {
    CorrelationVal(QName procId, long corrSetID, MessagePropertyAlias[] propAliases, QName[] propDataTypes, String[] propVals) {
        mCorrSetID = corrSetID;
        mPropVals = propVals;
        mPropDataTypes = propDataTypes;
        mPropAliases = propAliases;

        StringBuffer strBuf = new StringBuffer();
        strBuf.append(CorrelationManagerImpl.BEGIN_CHAR);
        strBuf.append(corrSetID);
        for (int i = 0; i < mPropVals.length; i++) {
            strBuf.append(CorrelationManagerImpl.APPEND_STRING);
            strBuf.append(mPropDataTypes[i].toString());
            strBuf.append(CorrelationManagerImpl.APPEND_STRING);
            strBuf.append(mPropVals[i]);
        }
        strBuf.append(CorrelationManagerImpl.APPEND_STRING);
        strBuf.append(procId.toString());
        strBuf.append(CorrelationManagerImpl.END_CHAR);

        mToString = strBuf.toString();
        mHashCode = mToString.hashCode();
    }

    /** This constructor is used only in recovery logic. 
     * Unlike in the forward logic, during recovery we store the correlation value 
     * as the string that was persisted to the database and not parsed into 
     * individual objects of propDataTypes and propVals.
     * @param corrSetID
     * @param val
     */
    public CorrelationVal(long corrSetID, String val) {
        mCorrSetID = corrSetID;        
        mIsRecoveredVal = true;
        mToString = val;
        mHashCode = mToString.hashCode();
    }
    
    public long getSetID() {
        return mCorrSetID;
    }

    /**
     * @see java.lang.Object#equals(java.lang.Object)
     */
    public boolean equals(Object obj) {
        if (obj == null || !(obj instanceof CorrelationVal)) {
            return false;
        }
        CorrelationVal ipObj = (CorrelationVal) obj;

        if (mCorrSetID != ipObj.mCorrSetID) {
            return false;
        }
        if (mIsRecoveredVal || ipObj.mIsRecoveredVal) {
            // If either of the CorrelationVal is constructed using the recovery logic, 
            // it will just have the correlationSetID and the string value of the
            // correlation.
            return Utility.areEqual(mToString, ipObj.toString());
        }

        // If either of the CorrelationVals is not recovered, the following logic 
        // will help identify same values. 
        if (mPropVals.length != ipObj.mPropVals.length) {
            return false;
        }
        for (int i = 0; i < mPropVals.length; i++) {
            if (!mPropDataTypes[i].equals(ipObj.mPropDataTypes[i])) {
                return false;
            }
            if (!(mPropVals[i].equals(ipObj.mPropVals[i]))) {
                return false;
            }
        }
        // Note, Don't have to compare the BPELId. Since the correlation
        // map is set at the BPEL level, all the IDs will have the same
        // value anyway. But the BPELId will reflect in the toString()
        return true;
    }

    /**
     * @see java.lang.Object#hashCode()
     */
    public int hashCode() {
        return mHashCode;
    }

    /**
     * @see java.lang.Object#toString()
     */
    public String toString() {
        return mToString;
    }
    
    public boolean isRecovered() {
    	return mIsRecoveredVal;
    }
    
    /**
     * Returns the nubmber of property aliases defined for this correlation value. If
     * the process instance was recovered and we do not know this, returns 0.
     * 
     * @return The number of property aliases or 0 if the information is not available.
     */
    public int getPropertiesLength() {
        if (mPropAliases == null) return 0;
        
        return mPropAliases.length;
    }
    
    public MessagePropertyAlias getPropertyAlias(int index) {
        if (mPropAliases == null) return null;
        
        return mPropAliases[index];
    }
    
    public QName getPropertyName(int index) {
        if (mPropAliases == null) return null;
        
        return mPropAliases[index].getName();
    }
    
    public QName getPropertyType(int index) {
        if (mPropAliases == null) return null;
        
        return mPropDataTypes[index];
    }
    
    public String getPropertyValue(int index) {
        if (mPropAliases == null) return null;
        
        return mPropVals[index];
    }
}
