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
 * @(#)SUArtifacts.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import javax.xml.namespace.QName;

import com.sun.jbi.common.qos.descriptor.QoSAssembly;
import com.sun.jbi.engine.bpel.DeploymentBindings.InComingKey;
import com.sun.jbi.engine.bpel.EngineHelper.ActivationEntry;
import com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel;
import com.sun.jbi.engine.bpel.core.bpel.util.XslCache;

/**
 * Encapsulates artifacts related to a BPEL service unit.
 */
public class SUArtifacts {
    private HashSet mBPSet;
    private String mAssemblyNm, mUnitNm;
    private Map mIncomingEvtMap = new HashMap();
    private List mProviderEPs = new ArrayList();
    private Map<QName, Integer> mMaxInstanceCountByBP = new HashMap<QName, Integer>();
    private XslCache mXslCache = new XslCache();
    private QoSAssembly mQoSAssembly;
    private Map<String, Object> mApplicationVariables = new HashMap<String, Object>();
    
    public SUArtifacts(QoSAssembly sa, String suName){
        mAssemblyNm = sa.getIdentification().getName();
        mUnitNm = suName;
        mQoSAssembly = sa;
    }
    
    public void addIncomingEventModel(InComingKey key, InComingEventModel model){
        mIncomingEvtMap.put(key, model);
    }

    public void addProviderEndpoint(ActivationEntry ep){
        mProviderEPs.add(ep);
    }

    public String getAssemblyName() {
        return mAssemblyNm;
    }
    
    public HashSet getBPs() {
        return mBPSet;
    }

    public int getMaxInstanceCountByBP(QName bpId ){
        return (mMaxInstanceCountByBP.containsKey(bpId)) 
                ? mMaxInstanceCountByBP.get(bpId) : 0;
    }

    public List getProviderEndpoint(){
        return mProviderEPs;
    }

    public QoSAssembly getQoSAssembly() {
    	return mQoSAssembly;
    }
    
    public String getUnitName() {
        return mUnitNm;
    }

    public XslCache getXslCache() {
        return mXslCache;
    }

    public void setBPs(HashSet bps) {
        mBPSet = bps;
    }
    
    public void setMaxInstanceCountByBP(QName bpId, int count){
        mMaxInstanceCountByBP.put(bpId, count);
    }
    
    public Map getInComingEventModel(){
        return mIncomingEvtMap;
    }

    public Map<String, Object> getApplicationVariables() {
        return mApplicationVariables;
    }
}
