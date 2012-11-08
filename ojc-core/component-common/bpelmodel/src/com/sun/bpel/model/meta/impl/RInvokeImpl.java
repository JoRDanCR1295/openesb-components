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
 * @(#)RInvokeImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.meta.impl;

import java.util.Collection;
import java.util.List;

import javax.xml.namespace.QName;

import com.sun.bpel.model.Catch;
import com.sun.bpel.model.CatchAll;
import com.sun.bpel.model.FaultHandlers;
import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.impl.FaultHandlersImpl;
import com.sun.bpel.model.impl.InvokeImpl;
import com.sun.bpel.model.meta.CorrelationDefnWrapper;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RInvoke;
import com.sun.bpel.model.meta.RMessagingElement;
import com.sun.bpel.model.meta.RPersistable;
import com.sun.bpel.model.parser.impl.RBPELDocumentImpl;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.ServiceQuality;


/**
 * Runtime Invoke implementation
 *
 * @author Sun Microsystems
 */
public class RInvokeImpl extends InvokeImpl implements RInvoke, RActivity, RMessagingElement,
    RPersistable {

	private long mUniqueID;
    private RActivity mNextAct;
    private SyntheticFaultHandlers mFaultHandlers = new SyntheticFaultHandlers();
    private CorrelationDefnWrapper mInCorrDefnWrapper = null;
    private CorrelationDefnWrapper mOutCorrDefnWrapper = null;
    private CorrelationDefnWrapper mInOutCorrDefnWrapperForRequest = null;
    private CorrelationDefnWrapper mInOutCorrDefnWrapperForResponse = null;

    private EndpointInfo mEndpointInfo = null;
    private List<ServiceQuality> mQosList = null;
    /**
     * Creates a new RInvokeImpl object.
     *
     * @param bpeldoc runtime BPEL document
     * @param id unique ID
     */
    public RInvokeImpl(RBPELDocumentImpl bpeldoc, long id) {
        super(bpeldoc);
        mUniqueID = id;
    }

    /**
     * @see com.sun.bpel.model.meta.RActivity#getUniqueId()
     */
    public long getUniqueId() {
        return mUniqueID;
    }

    /**
     * @see RActivity#setNextActivity(com.sun.bpel.model.meta.RActivity)
     */
    public void setNextActivity(RActivity act) {
        mNextAct = act;
    }

    /**
     * @see com.sun.bpel.model.meta.RActivity#getNextActivity()
     */
    public RActivity getNextActivity() {
        return mNextAct;
    }

    /**
     * @see com.sun.bpel.model.meta.RMessagingElement#getRPartner()
     */
    public PartnerLink getRPartner() {
        return super.getBPELPartnerLink();
    }

    /**
     * @see com.sun.bpel.model.meta.RMessagingElement#getRPortType()
     */
    public QName getRPortType() {
    	return super.getPortType();
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public CorrelationDefnWrapper getResponseCorrelationDefnWrapper() {
        return mInCorrDefnWrapper;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public CorrelationDefnWrapper getRequestCorrelationDefnWrapper() {
        return mOutCorrDefnWrapper;
    }
    
    /* (non-Javadoc)
     * @see com.sun.bpel.model.meta.RInvoke#getReqRespCorrelationDefnWrapperForRequest()
     */
    public CorrelationDefnWrapper getReqRespCorrelationDefnWrapperForRequest() {
    	return mInOutCorrDefnWrapperForRequest;
    }

    /* (non-Javadoc)
     * @see com.sun.bpel.model.meta.RInvoke#getReqRespCorrelationDefnWrapperForResponse()
     */
    public CorrelationDefnWrapper getReqRespCorrelationDefnWrapperForResponse() {
    	return mInOutCorrDefnWrapperForResponse;
    }

    /**
     * DOCUMENT ME!
     *
     * @param corrDefn DOCUMENT ME!
     */
    public void setResponseCorrelationDefnWrapper(CorrelationDefnWrapper corrDefn) {
        mInCorrDefnWrapper = corrDefn;
    }

    /**
     * DOCUMENT ME!
     *
     * @param corrDefn DOCUMENT ME!
     */
    public void setRequestCorrelationDefnWrapper(CorrelationDefnWrapper corrDefn) {
        mOutCorrDefnWrapper = corrDefn;
    }

    /* (non-Javadoc)
     * @see com.sun.bpel.model.meta.RInvoke#setReqRespCorrelationDefnWrapperForRequest
     * (com.sun.bpel.model.meta.CorrelationDefnWrapper)
     */
    public void setReqRespCorrelationDefnWrapperForRequest(CorrelationDefnWrapper corrDefn) {
    	mInOutCorrDefnWrapperForRequest = corrDefn;
    }

    /* (non-Javadoc)
     * @see com.sun.bpel.model.meta.RInvoke#setReqRespCorrelationDefnWrapperForResponse
     * (com.sun.bpel.model.meta.CorrelationDefnWrapper)
     */
    public void setReqRespCorrelationDefnWrapperForResponse(CorrelationDefnWrapper corrDefn) {
    	mInOutCorrDefnWrapperForResponse = corrDefn;
    }

    /**
     * @see com.sun.bpel.model.meta.Scope#getFaultHandlers()
     */
    public FaultHandlers getFaultHandlers() {
        return mFaultHandlers;
    }

    /**
     * @see com.sun.bpel.model.meta.RInvoke#getServiceQuality(java.lang.Class)
     */
    public <T extends ServiceQuality> T getServiceQuality(Class<T> qosType) {
		if (mQosList != null) {
			// String qosName = (qosType == null) ? "" : qosType.getName();
			for (ServiceQuality qos : mQosList) {
				// if (qosName.equals(qos.getClass().getName())) {
				if (qosType.isInstance(qos)) {
					return (T) qos;
				}
				// }
			}
		}
		return null;
	}

    /**
	 * @see com.sun.bpel.model.meta.RInvoke#getServiceQualityEndpointInfo()
	 */
    public EndpointInfo getServiceQualityEndpointInfo() {
		return mEndpointInfo;
	}

	/**
	 * @see com.sun.bpel.model.meta.RInvoke
	 * 	#setServiceQualityParams(com.sun.jbi.common.descriptor.EndpointInfo, java.util.List)
	 */
	public void setServiceQualityParams
		(EndpointInfo endpointInfo, List<ServiceQuality> qosList) {
		mEndpointInfo = endpointInfo;
		mQosList = qosList;
	}

    private class SyntheticFaultHandlers extends FaultHandlersImpl {
        /**
         * gets catch
         *
         * @param index index
         *
         * @return Catch catch
         */
        public Catch getCatch(int index) {
            return RInvokeImpl.this.getCatch(index);
        }

        /**
         * gets CatchAll
         *
         * @return CatchAll catchall
         */
        public CatchAll getCatchAll() {
            return RInvokeImpl.this.getCatchAll();
        }

        /**
         * getes Catches
         *
         * @return Collection catches
         */
        public synchronized Collection getCatches() {
            return RInvokeImpl.this.getCatches();
        }

        /**
         * get catch size
         *
         * @return int catch size
         */
        public int getCatchSize() {
            return RInvokeImpl.this.getCatchSize();
        }
    }
}
