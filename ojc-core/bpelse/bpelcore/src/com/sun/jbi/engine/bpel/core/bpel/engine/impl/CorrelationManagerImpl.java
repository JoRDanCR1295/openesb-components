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
 * @(#)CorrelationManagerImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.Pointer;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Node;

import com.sun.bpel.model.meta.CorrelationDefnWrapper;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.bpel.model.meta.RStartElementCorrelationDefnWrapper;
import com.sun.bpel.model.parser.impl.ParseCorrelationHelper.CorrelationDefn;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.CorrelationManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.exception.StandardException;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.BPELProcessInstanceImpl;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;
import com.sun.wsdl4j.ext.bpel.MessagePropertyAlias;

/**
 * @author Sun Microsystems Jan 3, 2006
 */
public class CorrelationManagerImpl implements CorrelationManager {

    private static final Logger LOGGER = Logger.getLogger(CorrelationManagerImpl.class.getName());
    
    static final char BEGIN_CHAR = '{';

    static final char END_CHAR = '}';

    static final String APPEND_STRING = "}{"; //$NON-NLS-1$

    private static final int CREATE_ONLY = 1;

    private static final int CORRELATE_ONLY = 2;

    private static final int JOIN = 3;

    private static final int COMMON_JOIN = 4;

    RBPELProcess mProc;

    //String mProcID;
    QName mProcID;
    
    Engine engine;

    CorrelationManagerImpl(RBPELProcess proc, Engine engine) {
        mProc = proc;
        mProcID = mProc.getBPELId();
        this.engine = engine;
    }

    /** Map<CorrelationVal, BPELProcessInstance> */
    Map mCorrIdToInstance = Collections.synchronizedMap(new HashMap());
    
    /**
     * This will be called for correlations with init="no"
     * 
     * @param elem
     * @return List<CorrelationVal>
     */
    List getCorrelateOnlyIDs(RStartElement elem, WSMessage wsMesg) {
        CorrelationDefnWrapper defnWrap = elem.getCorrelationDefnWrapper();
        return getCorrIds(defnWrap, wsMesg, CORRELATE_ONLY);
    }

    /**
     * @param elem
     * @param wsMesg
     * @return
     */
    List getCommonJoinOnlyIDs(RStartElement elem, WSMessage wsMesg) {
        RStartElementCorrelationDefnWrapper defnWrap = elem
                .getCorrelationDefnWrapper();
        return getCorrIds(defnWrap, wsMesg, COMMON_JOIN);
    }

    /**
     * @see CorrelationManager#associateInstance(List,
     *      com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance)
     */
    public synchronized void associateInstance(List<CorrelationVal> corrIDs,
            BPELProcessInstance bpInstance) {

        if (checkIfInstanceAssociated(corrIDs, bpInstance)) {
        	throw new StandardException(StandardException.Fault.CorrelationViolation);
        }
        bpInstance.checkAndAddCorrelationValues(corrIDs);
        for (int i = 0, size = corrIDs.size(); i < size; i++) {
            mCorrIdToInstance.put(corrIDs.get(i), bpInstance);
        }
        if(bpInstance.getBPELProcessManager().isPersistenceEnabled()){
            engine.getStateManager().persistCorreations(((BPELProcessInstanceImpl)bpInstance).getState());
        }
    }



    /**
     * @param corrIDs
     *            List<CorrelationVal>
     * @return BPELProcessInstance
     * @throws CorrelationConsistencyConstraintFailed
     *             TODO
     */
    BPELProcessInstance getInstance(List corrIDs) {
        if (corrIDs == null || corrIDs.size() == 0) {
            return null;
        }
        BPELProcessInstance bpi = null;
        BPELProcessInstance bpi2 = null;
        synchronized (this) {
            bpi = (BPELProcessInstance) mCorrIdToInstance.get(corrIDs.get(0));
            if (bpi == null) {
                return null;
            }
            for (int i = 1, size = corrIDs.size(); i < size; i++) {
                bpi2 = (BPELProcessInstance) mCorrIdToInstance.get(corrIDs
                        .get(i));
                if (!(bpi.equals(bpi2))) {
                	throw new StandardException(StandardException.Fault.CorrelationViolation);
                }
            }
        }
        return bpi;
    }

    /**
     * checks to see if any of the given correlation Id Vals are associated with
     * an existing instance
     * 
     * @param corrIds
     * @return returns true if there is an instance associated with any of the
     *         passed in correlation values.
     */
    private boolean checkIfInstanceAssociated(List<CorrelationVal> corrIds, 
            BPELProcessInstance bpInstance) {

        for (int i = 0, size = corrIds.size(); i < size; i++) {
            if (mCorrIdToInstance.get(corrIds.get(i)) != null) {
                if (LOGGER.isLoggable(Level.FINE)) {
                    LOGGER.log(Level.FINE, I18n.loc("BPCOR-3026: Instance {0} already " +
                            "associated with the ID {1} ", mCorrIdToInstance.get(corrIds.get(i)), 
                            corrIds.get(i)));
                    LOGGER.log(Level.FINE, I18n.loc("BPCOR-3028: Instance meant to be associated was {0}", 
                            bpInstance.getId()));
                }
                return true;
            }
        }
        return false;
    }

    void cleanUp(BPELProcessInstance bp) {
        cleanUpCorrelations(bp);
    }

    /**
     * Called when the instance is completed or
     * scalability passivated
     * 
     * @param bp
     */
    synchronized void cleanUpCorrelations(BPELProcessInstance bp) {
        Collection corrIDs = bp.getInstanceAssociatedCorrVals();
        for (Iterator itr = corrIDs.iterator(); itr.hasNext();) {
            mCorrIdToInstance.remove(itr.next());
        }
    }

    private static String[] evaluateVals(MessagePropertyAlias[] propAliases,
            WSMessage wsMesg) {
        String[] propertyValues = new String[propAliases.length];
        for (int i = 0; i < propAliases.length; i++) {
            propertyValues[i] = evaluatePropertyAlias(propAliases[i], wsMesg);
        }
        return propertyValues;
    }

    private static String evaluatePropertyAlias(MessagePropertyAlias propAlias, WSMessage wsMesg) {
        String nmProperty = propAlias.getNMProperty();
        // property points to NM property In the wsdl
        // <vprop:propertyAlias propertyName="tns:intProp"
        // nmProperty="com.sun.jms.transport.properties"/>
        if (nmProperty != null) {
            Object propertyValue = null;
            propertyValue = wsMesg.getNMProperty(nmProperty);
            // check for selection failure
            if (propertyValue == null) {
                throw Utility.selectionFailureQueryNMProperty(nmProperty);
            }

            if (propAlias.getQuery() == null) {
                return propertyValue.toString();
            }
            // evaluate query
            String query = propAlias.getQuery().getQueryString();
            String retVal[] = Utility.getKeyForNMProperty(propAlias.getQuery(), query);
            query = retVal[0];
            String keyForNMProperty = retVal[1];
            propertyValue = ((Map) propertyValue).get(keyForNMProperty);
            if (propertyValue instanceof DocumentFragment) {
                propertyValue = ((DocumentFragment) propertyValue).getFirstChild();
            }
            // check for selection failure
            if (propertyValue == null) {
                throw Utility.selectionFailureQueryNMProperty(nmProperty);
            }
            if (Utility.isEmpty(query)) {
                return propertyValue.toString();
            }

            JXPathContext ctx = Utility.createJXPathContextOnObject(propAlias, propertyValue);
            Pointer pointer = ctx.getPointer(query);
            propertyValue = pointer.getValue();
            // check for selection failure
            if (propertyValue == null) {
                throw Utility.selectionFailureQueryNMProperty(nmProperty, query);
            }
            return propertyValue.toString();
        }

        String query = propAlias.getQuery() == null ? null : propAlias.getQuery().getQueryString();
        String part = propAlias.getPartName();
        Node src = wsMesg.getPart(part);
        if (src == null) {
            // Throw uninitialized variable standard fault.
            throw Utility.uninitializedVariableErrorQuery(wsMesg.getMessageType(), part);
        }

        // condtion to handle the empty query string
        if (!Utility.isEmpty(query)) {
            src = (Node) Utility.executeQuery(propAlias, query, src).getNode();
        }

        // If the value is null or is an empty iterator, we throw a selection
        // failure
        if (src == null) {
            throw Utility.selectionFailureQuery(propAlias.getQuery().getQueryString());
        }

        String retVal = src.getTextContent().trim();

        if (Utility.isEmpty(retVal)) {
            throw Utility.selectionFailureQuery(propAlias.getQuery().getQueryString());
        }
        return retVal;
    }

    public static List getCorrelateOnlySetIds(RStartElement elem) {
        CorrelationDefnWrapper defnWrap = elem.getCorrelationDefnWrapper();
        List corrIDs = new ArrayList();
        if (defnWrap == null) {
            return corrIDs;
        }
        List defns = defnWrap.getCorrelateCorrDefns();

        CorrelationDefn defn = null;
        for (int i = 0, size = defns.size(); i < size; i++) {
            defn = (CorrelationDefn) defns.get(i);
            corrIDs.add(new Long(defn.getCorrelationSetID()));
        }

        return corrIDs;
    }

    public static List getCommonJoinOnlySetIds(RStartElement elem) {

        RStartElementCorrelationDefnWrapper defnWrap = elem
                .getCorrelationDefnWrapper();
        List corrIDs = new ArrayList();
        if (defnWrap == null) {
            return corrIDs;
        }
        List defns = defnWrap.getCommonJoinCorrDefs();

        CorrelationDefn defn = null;
        for (int i = 0, size = defns.size(); i < size; i++) {
            defn = (CorrelationDefn) defns.get(i);
            corrIDs.add(new Long(defn.getCorrelationSetID()));
        }
        return corrIDs;
    }

    private List getCorrIds(CorrelationDefnWrapper defnWrap, WSMessage wsMesg,
            int initiateFlag) {
        List corrIDs = new ArrayList();
        if (defnWrap == null) {
            return corrIDs;
        }
        List defns = null;
        if (initiateFlag == CREATE_ONLY) {
            defns = defnWrap.getCreateCorrDefns();
        } else if (initiateFlag == CORRELATE_ONLY) {
            defns = defnWrap.getCorrelateCorrDefns();
        } else if (initiateFlag == JOIN) {
            defns = defnWrap.getJoinCorrDefns();
        } else if (initiateFlag == COMMON_JOIN
                && defnWrap instanceof RStartElementCorrelationDefnWrapper) {
            RStartElementCorrelationDefnWrapper defnWrap2 = (RStartElementCorrelationDefnWrapper) defnWrap;
            defns = defnWrap2.getCommonJoinCorrDefs();
        }

        CorrelationDefn defn = null;
        CorrelationVal corrID = null;
        String[] propVals = null;
        for (int i = 0, size = defns.size(); i < size; i++) {
            defn = (CorrelationDefn) defns.get(i);
            propVals = evaluateVals(defn.getPropertyAlias(), wsMesg);
            corrID = new CorrelationVal(
                    mProcID, 
                    defn.getCorrelationSetID(), 
                    defn.getPropertyAlias(), 
                    defn.getPropertyDataTypes(), 
                    propVals);
            corrIDs.add(corrID);
        }

        return corrIDs;
    }

    /*
	 * (non-Javadoc)
	 * 
	 * @see com.sun.jbi.engine.bpel.core.bpel.engine.CorrelationManager#
	 *      doCorrMessageAssertion(com.sun.bpel.model.meta.CorrelationDefnWrapper,
	 *      com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage,
	 *      com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance)
	 */
	public void doCorrMessageAssertion(CorrelationDefnWrapper corrDefnWrap,
			WSMessage wsMesg, BPELProcessInstance bpInstance) {
		// check for the initiate='no' correlation values exist on the instance
		List corrIDs = getCorrIds(corrDefnWrap, wsMesg, CORRELATE_ONLY);
		checkIfCorrValsForCorrelateOnlyExistOnInstance(corrIDs, bpInstance);

		// check for the initiate='join' correlation vlaues exit on the instance
		corrIDs = getCorrIds(corrDefnWrap, wsMesg, JOIN);
		verifyIfJoinsExist(corrIDs, bpInstance);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.sun.jbi.engine.bpel.core.bpel.engine.CorrelationManager#
	 *      calculateCorrValues(com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage,
	 *      com.sun.bpel.model.meta.CorrelationDefnWrapper,
	 *      com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance)
	 */
	public CorrelationDefnValues calculateCorrValues(WSMessage wsMesg,
			CorrelationDefnWrapper corrDefnWrapper,
			BPELProcessInstance bpInstance) {
		List createIDs = getCorrIds(corrDefnWrapper, wsMesg, CREATE_ONLY);
		List joinIDs = getCorrIds(corrDefnWrapper, wsMesg, JOIN);
		
		CorrelationDefnValues calcValues = new CorrelationDefnValues(createIDs,
				joinIDs);
		return calcValues;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.sun.jbi.engine.bpel.core.bpel.engine.CorrelationManager#
	 *      checkCorrValues(com.sun.jbi.engine.bpel.core.bpel.engine.impl.CorrelationDefnValues,
	 *      com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance)
	 */
	public CorrelationDefnValues checkCorrValues(CorrelationDefnValues values,
			BPELProcessInstance bpInstance) {
		List createIDs = values.getCreateIDs();
		List joinIDs = values.getJoinIDs();
		List corrIDs = new ArrayList();
		synchronized (this) {
			joinIDs = verifyAndCalculateJoins(joinIDs, bpInstance);
			corrIDs.addAll(joinIDs);
			corrIDs.addAll(createIDs);

			if (checkIfInstanceAssociated(corrIDs, bpInstance)) {
				throw new StandardException(
						StandardException.Fault.CorrelationViolation);
			}

			bpInstance.checkCorrelationValues(corrIDs);
		}
		return new CorrelationDefnValues(createIDs, joinIDs);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.sun.jbi.engine.bpel.core.bpel.engine.CorrelationManager#
	 *      checkEquivalenceOfCorrValuesForReqRespPattern(com.sun.jbi.engine.bpel.core.bpel.engine.impl.CorrelationDefnValues,
	 *      com.sun.jbi.engine.bpel.core.bpel.engine.impl.CorrelationDefnValues,
	 *      com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance)
	 */
	public void checkEquivalenceOfCorrValuesForReqRespPattern(
			CorrelationDefnValues reqValues, CorrelationDefnValues respValues,
			BPELProcessInstance bpInstance) {
		List createOnlyIdsForReq = reqValues.getCreateIDs();
		List createOnlyIdsForResp = respValues.getCreateIDs();
		/*
		 * checkCorrValuesForReqRespPattern(createOnlyIdsForReq,
		 * createOnlyIdsForResp);
		 */
		if (!createOnlyIdsForReq.equals(createOnlyIdsForResp)) {
			throw new StandardException(
					StandardException.Fault.CorrelationViolation);
		}
		List joinOnlyIdsForReq = reqValues.getJoinIDs();
		List joinOnlyIdsForResp = respValues.getJoinIDs();
		// checkCorrValuesForReqRespPattern(joinOnlyIdsForReq,
		// joinOnlyIdsForResp);
		if (!joinOnlyIdsForReq.equals(joinOnlyIdsForResp)) {
			throw new StandardException(
					StandardException.Fault.CorrelationViolation);
		}

	}

	/* (non-Javadoc)
	 * @see com.sun.jbi.engine.bpel.core.bpel.engine.CorrelationManager#
	 * checkAndAssociateCorrValuesWithInstance(com.sun.jbi.engine.bpel.core.bpel.engine.impl.CorrelationDefnValues, 
	 * com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance)
	 */
	public void checkAndAssociateCorrValuesWithInstance(CorrelationDefnValues values, BPELProcessInstance bpInstance) {
        List createIDs = values.getCreateIDs();
        List joinIDs = values.getJoinIDs();
        if (createIDs.isEmpty() && joinIDs.isEmpty()) {
            return;
        }
        associateInstance(joinIDs, createIDs, bpInstance);
    }
    
	/* (non-Javadoc)
	 * @see com.sun.jbi.engine.bpel.core.bpel.engine.CorrelationManager#
	 * associateCorrValuesWithInstance(com.sun.jbi.engine.bpel.core.bpel.engine.impl.CorrelationDefnValues, 
	 * com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance)
	 */
	public void associateCorrValuesWithInstance(CorrelationDefnValues values,
			BPELProcessInstance bpInstance) {
		List corrIDs = new ArrayList();
		corrIDs.addAll(values.getCreateIDs());
		corrIDs.addAll(values.getJoinIDs());
        synchronized (this) {
            bpInstance.addCorrelationValues(corrIDs);
            for (int i = 0, size = corrIDs.size(); i < size; i++) {
                mCorrIdToInstance.put(corrIDs.get(i), bpInstance);
            }
        }
	}
    
    
    
    public synchronized void associateInstanceDuringRecovery(List<CorrelationVal> corrIDs, 
            BPELProcessInstance bpInstance) {
        if (checkIfInstanceAssociated(corrIDs, bpInstance)) {
            throw new StandardException(StandardException.Fault.CorrelationViolation);
        }
        bpInstance.addCorrelationValues(corrIDs);
        for (int i = 0, size = corrIDs.size(); i < size; i++) {
            mCorrIdToInstance.put(corrIDs.get(i), bpInstance);
        }
    }
    
    /**
	 * Used to see if the correlation sets that are to correlate into an
	 * instance, have the correlation set value initialized with the instance.
	 * 
	 * @param corrIds
	 * @param bpInstance
	 * @throws CorrelationIntegrityCheck
	 */
    private void checkIfCorrValsForCorrelateOnlyExistOnInstance(List corrIds, BPELProcessInstance bpInstance) {
    	if (corrIds != null && corrIds.size() > 0) {
    		BPELProcessInstance queryInstance = getInstance(corrIds);
    		if(queryInstance != bpInstance) {
    			throw new StandardException(StandardException.Fault.CorrelationViolation);
    		}
    	}
    }
    
    /**
     * This method is used to compare the CorrelationVal object of all the 
     * correlation sets with pattern 'request-response' for two-way invoke
     * of the request and response messages.
     * Refer to CorrelationManager.calculateAndCheckCorrValuesForReqRespPattern()
     * method for details for this requirement.
     * @param vals1 corrIds for the request message with pattern 'request-response'
     * @param vals2 corrIds for the response message with pattern 'request-response'
     * @throws CorrelationIntegrityCheck
     */
    private void checkCorrValuesForReqRespPattern(List vals1, List vals2) {
    	int size1 = vals1.size();
    	int size2 = vals2.size();
    	// since the pattern is 'request-response'
    	// the size of the two list for request and 
    	// response have to be the same.
    	if (size1 != size2) {
    		throw new StandardException(StandardException.Fault.CorrelationViolation);
    	}
    	boolean matchFound;
    	for (Object i : vals1) {
        	matchFound = false;
    		CorrelationVal corVal1 = (CorrelationVal)i;
    		for (Object j : vals2) {
    			CorrelationVal corVal2 = (CorrelationVal)j;
    			if (corVal1.getSetID() == corVal2.getSetID()) {
    				matchFound = true;
    				if (!corVal1.equals(corVal2)) {
    					throw new StandardException(StandardException.Fault.CorrelationViolation);
    				}
    				if (matchFound) {
    					break;
    				}
    			}
    		}
    		if (!matchFound) {
    			throw new StandardException(StandardException.Fault.CorrelationViolation);
    		}
    	}
    }
    
    /**
     * This method associates an instance with the correlation values. Verifies
     * if an instance exists and verifies if multiple join values give an
     * instance that it should be the same instance. if a value is null, just
     * ignore it. The "join" correlation values that are already not associated
     * with an instance will be then associated with the instance.
     * 
     * @return
     */
    private void associateInstance(List joinIDs, List createIDs,
            BPELProcessInstance bpInstance) {

        synchronized (this) {
            List tobeAddedCorrIDs = verifyAndCalculateJoins(joinIDs, bpInstance);

            tobeAddedCorrIDs.addAll(createIDs);
            associateInstance(tobeAddedCorrIDs, bpInstance);
        }
    }

    private synchronized List verifyAndCalculateJoins(List joinIDs,
            BPELProcessInstance bpInstance) {
        if (joinIDs == null || joinIDs.size() == 0) {
            return new ArrayList();
        }
        BPELProcessInstance bpi = null;
        List tobeAddedIDs = new ArrayList();

        for (int i = 0, size = joinIDs.size(); i < size; i++) {
            bpi = (BPELProcessInstance) mCorrIdToInstance.get(joinIDs.get(i));
            if (bpi == null) {
                tobeAddedIDs.add(joinIDs.get(i));
            } else {
                if (!(bpi.equals(bpInstance))) {
                	throw new StandardException(StandardException.Fault.CorrelationViolation);
                }
            }
        }

        return tobeAddedIDs;
    }

/*    private synchronized List calculateJoinsToBeAdded(List joinIDs,
    		BPELProcessInstance bpInstance) {
        List tobeAddedIDs = new ArrayList();
    	if(joinIDs == null || joinIDs.size() == 0) {
            return tobeAddedIDs;
        }
        BPELProcessInstance bpi = null;
        
        for (int i = 0, size = joinIDs.size(); i < size; i++) {
            bpi = (BPELProcessInstance) mCorrIdToInstance.get(joinIDs.get(i));
            if (bpi == null) {
                tobeAddedIDs.add(joinIDs.get(i));
            }
        }
        return tobeAddedIDs;
    }
*/    
    private synchronized void verifyIfJoinsExist(List joinIDs,
			BPELProcessInstance bpInstance) {

		BPELProcessInstance bpi = null;
		for (int i = 0, size = joinIDs.size(); i < size; i++) {
			bpi = (BPELProcessInstance) mCorrIdToInstance.get(joinIDs.get(i));
			if (bpi != null && !(bpi.equals(bpInstance))) {
				throw new StandardException(
						StandardException.Fault.CorrelationViolation);
			}
		}
	}
    
}
