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
 * @(#)DeploymentBindings.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel;
import com.sun.jbi.engine.bpel.impl.BPELSEException;
import com.sun.jbi.engine.bpel.util.I18n;

/**
 * DOCUMENT ME!
 * 
 * @author Sun Microsystems
 */
public class DeploymentBindings {
    private static final Logger LOGGER = Logger.getLogger(DeploymentBindings.class.getName());

    private HashMap mIncomingEventModel = new HashMap();
    private Map serviceUnitKeys = new HashMap();

    /**
     * add new insance of InComingEventModel
     * 
     * @param key InComingKey
     * @param model InComingEventModel
     */
    public void addInComingEventModel(String suName, InComingKey key,
            InComingEventModel model) throws BPELSEException {
        InComingEventModel origModel = (InComingEventModel) mIncomingEventModel.put(key, model);
        if (origModel != null) {
            String errorMessage = I18n.loc(
                    "BPJBI-3014: {0} is replaced for: {1}", origModel, key);
            throw new BPELSEException(errorMessage);
        }
        List keyList = (List) serviceUnitKeys.get(suName);
        if (keyList == null) {
            keyList = new ArrayList();
            serviceUnitKeys.put(suName, keyList);
        }
        keyList.add(key);
    }

    public void removeInComingEventModel(String suName){
        List keyList = (List) serviceUnitKeys.get(suName);
        if (keyList != null) {
            for (int i = 0; i < keyList.size(); i++) {
                mIncomingEventModel.remove(keyList.get(i));
            }
        }
    }
    
    /**
     * get InComingEventModel from InComingKey
     * 
     * @param key
     *            IncomingKey
     * @return InComingEventModel incoming event model
     */
    public InComingEventModel getInComingEventModel(InComingKey key) {
        return (InComingEventModel) mIncomingEventModel.get(key);
    }

    /**
     * creates InComingKey from server, endpoint, and operation
     * 
     * @param service Service QName
     * @param endPoint Endpoint local name
     * @param oper Operation
     * @return InComingKey InComingKey instance
     */
    public static InComingKey createInComingBindingsKey(QName service, String endPoint, String oper) {
        return new InComingKey(service, endPoint, oper);
    }

    /**
     * InComingKey class
     */
    public static class InComingKey {
        private QName mService;
        private String mEndPoint;
        private String mOper;

        private int hashVal = Integer.MIN_VALUE;

        /**
         * InComingKey constructor
         * 
         * @param service Service QName
         * @param endPoint Endpoint localname
         * @param oper Operation
         */
        private InComingKey(QName service, String endPoint, String oper) {
            mOper = oper;
            mService = service;
            mEndPoint = endPoint;
            hashVal = (service.toString() + endPoint).hashCode();
        }

        /**
         * IncomingKey hashCode() implementation
         * 
         * @return int hash code
         */
        public int hashCode() {
            return hashVal;
        }

        /**
         * InComingKey equals() implementation
         * 
         * @param obj InComingKey object instance
         * @return boolean true: if objects are equal false: if objects are not equal
         */
        public boolean equals(Object obj) {
            if (!(obj instanceof InComingKey)) {
                return false;
            }

            if (obj == this) {
                return true;
            }
            InComingKey key = (InComingKey) obj;
            return (checkEquals(mOper, key.mOper)
                    && checkEquals(mEndPoint, key.mEndPoint) && checkEquals(
                    mService, key.mService));
        }
        public String toString() {
            return "Service: " + mService + ", EndPoint: " + mEndPoint
                    + ", Operation: " + mOper;
        }
        private static boolean checkEquals(Object o1, Object o2) {
            return (o1 == null) ? (o2 == null) : o1.equals(o2);
        }
    }
}
