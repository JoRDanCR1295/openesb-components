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
 * @(#)CorrelationManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine;

import java.util.List;

import com.sun.bpel.model.meta.CorrelationDefnWrapper;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.CorrelationDefnValues;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.CorrelationVal;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;


/**
 * corerlation manager
 *
 * @author Sun Microsystems
 */
public interface CorrelationManager {
    /**
     * DOCUMENT ME!
     *
     * @param corrIDs list of correlation IDs
     * @param bpInstance BPELProcessInstance
     * @throws CorrelationAlreadyExists TODO
     */
    void associateInstance(List<CorrelationVal> corrIDs, BPELProcessInstance bpInstance);

    /**
     * Called when the bpel instance is being recovered.
     * @param corrIDs list of correlation IDs
     * @param bpInstance BPELProcessInstance
     *
     * @throws CorrelationAlreadyExists TODO
     */
    void associateInstanceDuringRecovery(List<CorrelationVal> corrIDs, BPELProcessInstance bpInstance);
    
    /**
     * Assertion of the correlation values associated with the message.
     * cases for assertion are
     * 1. initiate='no'
     * The values for this correlation should have been initialized for this instance,
     * If not StandardException.Fault.CorrelationViolation will be thrown.
     * 
     * 2. initiate='join'
     * The values for this correlation <b>if initialized</b> should be for the <b>same instance</b>.
     * if not StandardException.Fault.CorrelationViola4tion will be thrown.
     * 
     * @param corrDefnWrap, correlation definition wrapper
     * @param wsMesg, the message on which correlations have to be asserted
     * @param bpInstance, process instance
     */
    public void doCorrMessageAssertion(CorrelationDefnWrapper corrDefnWrap,
    		WSMessage wsMesg, BPELProcessInstance bpInstance); 
    
    /**
     * Calculates the correlation values for the given correlation definition wrapper
     * on the given message for the correlation attribute initiate='create' and 'join'
     * encapsultes the return value in the type CorrelationDefnValues.
     * 
     * @param wsMesg, message on which the correlation values have to be calculated
     * @param corrDefnWrapper, correlation definition wrapper
     * @param bpInstance, process instance
     * @return CorrelationDefnValues, the class encapsulates the different correlation 
     * values.
     */
    CorrelationDefnValues calculateCorrValues(WSMessage wsMesg,
			CorrelationDefnWrapper corrDefnWrapper,
			BPELProcessInstance bpInstance);

	/**
	 * Checks for the integrety of the correlation values 
	 * for initiate='join', verifies if there is an instance already associated 
	 * with this value and if its the same as the present instance. If not an
	 * Exception is thrown.
	 * for initiate='yes', verifies if there is an instance associated , if so 
	 * an Exception is thrown.
	 * @param values, the encapsulated correlation values.
	 * @param bpInstance, process instance
	 * @return the verified and updated CorrelationDefnValues.
	 */
    CorrelationDefnValues checkCorrValues(CorrelationDefnValues values,
			BPELProcessInstance bpInstance);

    /**
     * check for the InOut ME correlations pattern attribute 'request-response'.
     * the initiate values can be of 'yes', 'join', 'no'
     * ex: consider the follwoing correlation set defined on a two-way invoke
     * <correlation set="CorrelationSet1" initiate="join" pattern="request-response"/>
     * here the correlation values (based on propertyAlias) will exist for both 
     * the request and response messagees. The value that the correlation parameter
     * evaluates to for both the request and response message has to be the same, if
     * not CorrelationIntegrityCheck exception will be thrown. This exception will 
     * be thrown for the cases with initiate attributes values of 'join' and 'yes'. 
     * The validation matirx is as follows
     * 1. initiate value : 'join', 'yes'
     * compare the values of the correlation set to determine if they are the same 
     * if not throw CorrelationIntegrityCheck exception
     * 
     * @param reqValues
     * @param respValues
     * @param bpInstance
     */
    void checkEquivalenceOfCorrValuesForReqRespPattern(
			CorrelationDefnValues reqValues, CorrelationDefnValues respValues,
			BPELProcessInstance bpInstance);

	/**
	 * Associates the value of the correlations on the instance object.
	 * Used when the message exchange is in the context of a transaction
	 * where the transaction has completed successfully and the DB operations
	 * validate the uniqueness of the correlation values (Db has primary key 
	 * on correlation value). Hence here the values are directly set on the 
	 * instance object.
	 * This api can be used for any context similar to the above one.
	 * 
	 * @param values, correlation values 
	 * @param bpInstance, process instance
	 * @param addToState, boolean flag indicating if values have to be set on
	 * the state object
	 */
	void associateCorrValuesWithInstance(CorrelationDefnValues values,
			BPELProcessInstance bpInstance);
	
	/**
	 * Checks the integrety of the correlation values and associates the values
	 * with the instance. Since multiple threads can potentially be assigning 
	 * correlation IDs to instances, the integrety check of the 
	 * values and its association with the instance has to be done in a atomic 
	 * unit of work.
	 * 
	 * @param values, correlation values
	 * @param bpInstance, process instance
	 */
	void checkAndAssociateCorrValuesWithInstance(CorrelationDefnValues values,
			BPELProcessInstance bpInstance);
}
