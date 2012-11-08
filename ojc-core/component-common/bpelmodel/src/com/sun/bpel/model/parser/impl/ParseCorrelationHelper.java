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
 * @(#)ParseCorrelationHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.parser.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.wsdl.Input;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Output;
import javax.wsdl.PortType;
import javax.xml.namespace.QName;

import com.sun.bpel.model.Correlation;
import com.sun.bpel.model.Correlations;
import com.sun.bpel.model.Invoke;
import com.sun.bpel.model.meta.CorrelationDefnWrapper;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RCorrelation;
import com.sun.bpel.model.meta.RCorrelationSet;
import com.sun.bpel.model.meta.RInvoke;
import com.sun.bpel.model.meta.RReply;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.bpel.model.meta.RStartElementCorrelationDefnWrapper;
import com.sun.bpel.model.util.Utility;
import com.sun.bpel.model.common.MessageManager;
import com.sun.wsdl4j.ext.bpel.MessageProperty;
import com.sun.wsdl4j.ext.bpel.MessagePropertyAlias;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public class ParseCorrelationHelper {

    /** The logger. */
    private static final Logger LOGGER = Logger.getLogger(ParseCorrelationHelper.class.getName());
    
    /** MessageManager for localized strings. */    
    private static MessageManager MESSAGES = MessageManager.getManager(ParseCorrelationHelper.class);

    /** constructs RStartElementCorrelationDefnWrapper for RStartElements
     * @param sa
     * @param proc
     */
    static void registerCorrelations(RStartElement sa, RBPELProcess proc) {
    	QName normMesgName = sa.getWSDLOperation().getInput().getMessage().getQName();
    	Correlations corrs = sa.getCorrelations();

        if (corrs == null) {
            return;
        }

        int corrsSize = corrs.getCorrelationSize();
        RCorrelation corr = null;
        RCorrelationSet cSet = null;
        String initiateVal = null;
        CorrelationDefn corrDefn = null;
        List corrDefnCreateOnlyList = new ArrayList();
        List corrDefnCorrelateOnlyList = new ArrayList();
        List corrDefnJoinList = new ArrayList();
        RStartElementCorrelationDefnWrapper corrDefnWrapper = null;

        for (int i = 0; i < corrsSize; i++) {
            corr = (RCorrelation) corrs.getCorrelation(i);
            cSet = corr.getCorrelationSet();
            corrDefn = getCorrelationDefn(cSet, normMesgName, proc);

            initiateVal = corr.getInitiate();

            if ((initiateVal == null) || initiateVal.equals(Correlation.INITIATE_NO)) {
                corrDefnCorrelateOnlyList.add(corrDefn);
            } else if (initiateVal.equals(Correlation.INITIATE_YES)) {
                corrDefnCreateOnlyList.add(corrDefn);
            } else {
                corrDefnJoinList.add(corrDefn);
            }

        }
        corrDefnWrapper = new RStartElementCorrelationDefnWrapperImpl(
                corrDefnCreateOnlyList, corrDefnCorrelateOnlyList, corrDefnJoinList
            );

        sa.setCorrelationDefnWrapper(corrDefnWrapper);
    }

    /**
     * DOCUMENT ME!
     *
     * @param mesgQName
     * @param propAliases collection of property aliases associated with one property
     *
     * @return PropertyAlias
     */
    private static MessagePropertyAlias getPropAlias(QName mesgQName,
            Collection<MessagePropertyAlias> propAliases) {
        Iterator<MessagePropertyAlias> itr = propAliases.iterator();
        MessagePropertyAlias propAlias = null;
        QName propMesgQName = null;
        QName normalizedPropMesgQName = null;

        while (itr.hasNext()) {
            propAlias = itr.next();
            propMesgQName = propAlias.getMessageType().getQName();

            if (propMesgQName.equals(mesgQName)) {            
                // for a given property and a given message type, there can only be
                // one property alias.
                return propAlias;
            }
        }

        return null;
    }

    private static CorrelationDefn getCorrelationDefn(
        RCorrelationSet cSet, QName normMesgName, RBPELProcess proc) {
        List associatedPropertyAliases = new ArrayList();
        List propertyAliasDataType = new ArrayList();
        Collection<MessagePropertyAlias> propAliases = null;
        QName[] props = cSet.getProperties();
        MessagePropertyAlias propAlias = null;
        MessageProperty prop = null;

        for (int propsIndex = 0; propsIndex < props.length; propsIndex++) {
            QName propQName = props[propsIndex];
            prop = proc.getBPELProperty(propQName);
            propAliases = proc.getBPELPropertyAlias(propQName);
            propAlias = getPropAlias(normMesgName, propAliases);

            if (propAlias != null) {
                associatedPropertyAliases.add(propAlias);
                propertyAliasDataType.add(prop.getType());
            }
        }

        MessagePropertyAlias[] propAliasArray = (MessagePropertyAlias[]) associatedPropertyAliases.toArray(
                new MessagePropertyAlias[] {}
            );
        QName[] pAliasDataType = (QName[]) propertyAliasDataType.toArray(new QName[] {});

        CorrelationDefn defn = new CorrelationDefn(
                cSet.getUniqueId(), propAliasArray, pAliasDataType
            );

        return defn;
    }

    /**
     * DOCUMENT ME!
     *
     * @param rInvoke DOCUMENT ME!
     * @param proc DOCUMENT ME!
     */
    static void registerCorrelations(RInvoke rInvoke, RBPELProcess proc) {
		Invoke invoke = (Invoke) rInvoke;
		CorrelationDefnWrapper defn = null;
		Correlations corrs = invoke.getCorrelations();
		if (corrs != null) {

			if (invoke.getOutputBPELVariable() == null) {

				defn = getRequestPatternForOneWayInvoke(invoke, proc);
				rInvoke.setRequestCorrelationDefnWrapper(defn);

			} else {

				// correlations for the response need to be set only if the
				// invoke is a two-way invoke.
				defn = getRequestPattern(invoke, proc);
				rInvoke.setRequestCorrelationDefnWrapper(defn);

				defn = getResponsePattern(invoke, proc);
				rInvoke.setResponseCorrelationDefnWrapper(defn);

				// Correlation defn for two-way invoke where pattern is
				// "request-response" defined on the Request message.
				defn = getRequestResponsePattern(invoke, proc, true);
				rInvoke.setReqRespCorrelationDefnWrapperForRequest(defn);

				// Correlation defn for two-way invoke where pattern is
				// "request-response" defined on the Response message.
				defn = getRequestResponsePattern(invoke, proc, false);
				rInvoke.setReqRespCorrelationDefnWrapperForResponse(defn);

			}
		}
	}
    
    /**
     * DOCUMENT ME!
     *
     * @param invoke
     * @param proc
     *
     * @return returns correlations that are defined as pattern response.
     */
    private static CorrelationDefnWrapper getResponsePattern(Invoke invoke, RBPELProcess proc) {
		RCorrelation corr = null;
		RCorrelationSet cSet = null;
		String initiateVal = null;
		CorrelationDefn corrDefn = null;
		List corrDefnCreateOnlyList = new ArrayList();
		List corrDefnCorrelateOnlyList = new ArrayList();
		List corrDefnJoinList = new ArrayList();
		CorrelationDefnWrapper corrDefnWrapper = null;
		Correlations corrs = invoke.getCorrelations();

		//QName normMesgName = invoke.getOutputBPELVariable().getMessageType();
		QName normMesgName = getInvokeResponseMsgQName(invoke);
		int corrsSize = corrs.getCorrelationSize();

		for (int i = 0; i < corrsSize; i++) {
			corr = (RCorrelation) corrs.getCorrelation(i);

			if (corr.getPattern().equals(RCorrelation.PATTERN_REQUEST)
					|| corr.getPattern().equals(RCorrelation.PATTERN_REQ_RESP)) {
				continue;
			}

			cSet = corr.getCorrelationSet();
			corrDefn = getCorrelationDefn(cSet, normMesgName, proc);

			initiateVal = corr.getInitiate();
			
			if ((initiateVal == null)
					|| initiateVal.equals(Correlation.INITIATE_NO)) {
				corrDefnCorrelateOnlyList.add(corrDefn);
			} else if (initiateVal.equals(Correlation.INITIATE_YES)) {
				corrDefnCreateOnlyList.add(corrDefn);
			} else { // else its the pattern 'join'
				corrDefnJoinList.add(corrDefn);
			}
		}
		corrDefnWrapper = new CorrelationDefnWrapperImpl(
				corrDefnCreateOnlyList, corrDefnCorrelateOnlyList,
				corrDefnJoinList);
		return corrDefnWrapper;
		
	}
    
    /**
     * DOCUMENT ME!
     *
     * @param invoke
     * @param proc
     *
     * @return returns correlations that are defined as pattern request 
     */
    private static CorrelationDefnWrapper getRequestPattern(Invoke invoke,
			RBPELProcess proc) {

		RCorrelation corr = null;
		RCorrelationSet cSet = null;
		String initiateVal = null;
		CorrelationDefn corrDefn = null;
		List corrDefnCreateOnlyList = new ArrayList();
		List corrDefnCorrelateOnlyList = new ArrayList();
		List corrDefnJoinList = new ArrayList();
		CorrelationDefnWrapper corrDefnWrapper = null;

		Correlations corrs = invoke.getCorrelations();
		QName normMesgName = getInvokeRequestMsgQName(invoke);
		int corrsSize = corrs.getCorrelationSize();

		for (int i = 0; i < corrsSize; i++) {
			corr = (RCorrelation) corrs.getCorrelation(i);

			if (corr.getPattern().equals(RCorrelation.PATTERN_RESPONSE)
					|| corr.getPattern().equals(RCorrelation.PATTERN_REQ_RESP)) {
				continue;
			}

			cSet = corr.getCorrelationSet();
			corrDefn = getCorrelationDefn(cSet, normMesgName, proc);

			initiateVal = corr.getInitiate();
			if ((initiateVal == null)
					|| initiateVal.equals(Correlation.INITIATE_NO)) {
				corrDefnCorrelateOnlyList.add(corrDefn);
			} else if (initiateVal.equals(Correlation.INITIATE_YES)) {
				corrDefnCreateOnlyList.add(corrDefn);
			} else { //else its the pattern 'join'
				corrDefnJoinList.add(corrDefn);
			}
		}

		corrDefnWrapper = new CorrelationDefnWrapperImpl(
				corrDefnCreateOnlyList, corrDefnCorrelateOnlyList,
				corrDefnJoinList);
		return corrDefnWrapper;

	}
    
    private static QName getInvokeRequestMsgQName(Invoke invoke) {
		QName normMesgName = null;
		PortType pt = invoke.getWSDLPortType();
		// get operation and get input message for this operation.
		Operation op = pt.getOperation(invoke.getOperation(), null, null);
		// Operations [] opsarray = (Operations []) colops.toArray();
		Input oi = op.getInput();
		Message inputMessage = oi.getMessage();
		normMesgName = inputMessage.getQName();
		return normMesgName;
	}
    
    private static QName getInvokeResponseMsgQName(Invoke invoke) {
		QName normMesgName = null;
		PortType pt = invoke.getWSDLPortType();
		// get operation and get input message for this operation.
        Operation op = pt.getOperation(invoke.getOperation(), null, null);
		// Operations [] opsarray = (Operations []) colops.toArray();
        Output oi = op.getOutput();
		Message outputMessage = oi.getMessage();
		normMesgName = outputMessage.getQName();
		return normMesgName;
	}

    private static CorrelationDefnWrapper getRequestResponsePattern(Invoke invoke,
    		RBPELProcess proc, boolean requestFlag) {

    	RCorrelation corr = null;
    	RCorrelationSet cSet = null;
    	String initiateVal = null;
    	CorrelationDefn corrDefn = null;
    	List corrDefnCreateOnlyList = new ArrayList();
    	List corrDefnCorrelateOnlyList = new ArrayList();
    	List corrDefnJoinList = new ArrayList();
    	CorrelationDefnWrapper corrDefnWrapper = null;
    	QName normMesgName = null;
    	
    	Correlations corrs = invoke.getCorrelations();
    	int corrsSize = corrs.getCorrelationSize();

    	if (requestFlag) {
    		normMesgName = getInvokeRequestMsgQName(invoke);
    	} else {
    		//normMesgName = invoke.getOutputBPELVariable().getMessageType();
    		normMesgName = getInvokeResponseMsgQName(invoke);
    	}
    	
		for (int i = 0; i < corrsSize; i++) {
			corr = (RCorrelation) corrs.getCorrelation(i);

			if (corr.getPattern().equals(RCorrelation.PATTERN_RESPONSE)
					|| corr.getPattern().equals(RCorrelation.PATTERN_REQUEST)) {
				continue;
			}
			
			cSet = corr.getCorrelationSet();
			corrDefn = getCorrelationDefn(cSet, normMesgName, proc);

			initiateVal = corr.getInitiate();
			if ((initiateVal == null)
					|| initiateVal.equals(Correlation.INITIATE_NO)) {
				corrDefnCorrelateOnlyList.add(corrDefn);
			} else if (initiateVal.equals(Correlation.INITIATE_YES)) {
				corrDefnCreateOnlyList.add(corrDefn);
			} else { // else its the pattern 'join'
				corrDefnJoinList.add(corrDefn);
			}
		}

		corrDefnWrapper = new CorrelationDefnWrapperImpl(
				corrDefnCreateOnlyList, corrDefnCorrelateOnlyList,
				corrDefnJoinList);
		return corrDefnWrapper;
		
    }
    
    static void registerCorrelations(RBPELProcess proc) {
        if (proc.getActivity() == null) {
            // TODO Fix this.
            // because of 2 times parsing of the BPEL model, we need to check
            // this. This API is called twice while parsing the endElement of
			// the
            // BPELProcess and the results of getStartElements() are cached.\
            return;
        }
        Set StartELemsWhichCorrelate = proc.getStartElements(false);
        Set startElemsWhichCreateInstance = proc.getStartElements(true);
        Utility.setStartType(StartELemsWhichCorrelate, startElemsWhichCreateInstance, 
                proc.getTargetNamespace());

        if (startElemsWhichCreateInstance.size() <= 1) {
            // do nothing.
            return;
        }
        List createOrCorrelateList = new ArrayList();
        RStartElement createElem = null;
        for (Iterator itr = startElemsWhichCreateInstance.iterator(); itr.hasNext(); ) {
            createElem = (RStartElement) itr.next();
            if (createElem.getStartType() == Utility.RECEIVE_TYPE_CREATE_OR_CORRELATE) {
                createOrCorrelateList.add(createElem);
            }
        }
        if (createOrCorrelateList.size() == 0) {
            // For pick based, this condition is possible.
            return;
        }  
        if (createOrCorrelateList.size() == 1) {
            String errMesg = MESSAGES.getString("ParseCorrelationHelper_SINGLE_CREATE_OR_CORRELATE_ACT_ERROR");
            LOGGER.log(Level.WARNING, errMesg);
            throw new RuntimeException(errMesg);
        }
        
        // TODO we may be able to avoid one for loop here, by merging the above and below.
        // get the first set of common Corr Defs.
        RStartElement rElem = (RStartElement) createOrCorrelateList.get(0);
        List firstCommonCorrDefs = rElem.getCorrelationDefnWrapper().getJoinCorrDefns();
        List commonCorrDefs = firstCommonCorrDefs;
        List nextCommonCorrDefs = null;
        for (int i = 1, size = createOrCorrelateList.size(); i < size; i++) {
            rElem = (RStartElement) createOrCorrelateList.get(i);
            nextCommonCorrDefs = rElem.getCorrelationDefnWrapper().getJoinCorrDefns();
            commonCorrDefs = getCommonjoinCorrs(commonCorrDefs, nextCommonCorrDefs);
        }
        if (commonCorrDefs.size() == 0) {
            throw new RuntimeException("There should be atleast one common " +
                    "correlation with initiate =" + "join" + " defined for " +
                            "flow based start activities.");
        }
        
        RStartElement elem;
        RStartElementCorrelationDefnWrapper defnWrapper = null;
        RStartElementCorrelationDefnWrapper oldDefnWrapper = null;
        for (int i = 0, size = createOrCorrelateList.size() ; i < size; i++) {
            elem = (RStartElement) createOrCorrelateList.get(i);
            oldDefnWrapper = elem.getCorrelationDefnWrapper();
            defnWrapper = reconstructCorrDefnWrapper(oldDefnWrapper, commonCorrDefs);
//            List<List<CorrelationDefn>> unCommonCorrDefs = getUnCommonCorrDefs(oldDefnWrapper, commonCorrDefs);
//            defnWrapper = new RStartElementCorrelationDefnWrapperImpl(
//                    oldDefnWrapper.getCreateCorrDefns(), oldDefnWrapper.getCorrelateCorrDefns(),
//                    unCommonCorrDefs.get(0), unCommonCorrDefs.get(1));
            elem.setCorrelationDefnWrapper(defnWrapper);
        }
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param invoke
     * @param proc
     *
     * @return For one way invokes, there is no pattern defined but they will be replied.
     */
    private static CorrelationDefnWrapper getRequestPatternForOneWayInvoke(
        Invoke invoke, RBPELProcess proc
    ) {
        Correlations corrs = invoke.getCorrelations();

        if (corrs == null) {
            return null;
        }

        QName normMesgName = invoke.getInputBPELVariable().getMessageType();        

        CorrelationDefnWrapper corrDefnWrapper = getCorrsforReplyAndInvoke(
                corrs, normMesgName, proc
            );

        return corrDefnWrapper;
    }

    /**
     * DOCUMENT ME!
     *
     * @param rep DOCUMENT ME!
     * @param proc DOCUMENT ME!
     */
    static void registerCorrelations(RReply rep, RBPELProcess proc) {
    	QName normMesgName = rep.getRVariable().getMessageType();
    	
        Correlations corrs = rep.getCorrelations();

        if (corrs == null) {
            return;
        }

        CorrelationDefnWrapper corrDefnWrapper = getCorrsforReplyAndInvoke(
                corrs, normMesgName, proc
            );
        rep.setCorrelationDefnWrapper(corrDefnWrapper);
    }

    private static CorrelationDefnWrapper getCorrsforReplyAndInvoke(
        Correlations corrs, QName normMesgName, RBPELProcess proc
    ) {
        int corrsSize = corrs.getCorrelationSize();
        RCorrelation corr = null;
        RCorrelationSet cSet = null;
        String initiateVal = null;
        CorrelationDefn corrDefn = null;
        List corrDefnCreateOnlyList = new ArrayList();
        List corrDefnCorrelateOnlyList = new ArrayList();
        List corrDefnJoinList = new ArrayList();
        CorrelationDefnWrapper corrDefnWrapper = null;

        for (int i = 0; i < corrsSize; i++) {
            corr = (RCorrelation) corrs.getCorrelation(i);
            cSet = corr.getCorrelationSet();
            corrDefn = getCorrelationDefn(cSet, normMesgName, proc);

            initiateVal = corr.getInitiate();

            if ((initiateVal == null) || initiateVal.equals(Correlation.INITIATE_NO)) {
                corrDefnCorrelateOnlyList.add(corrDefn);
            } else if (initiateVal.equals(Correlation.INITIATE_YES)) {
                corrDefnCreateOnlyList.add(corrDefn);
            } else {
                corrDefnJoinList.add(corrDefn);
            }

        }
        corrDefnWrapper = new CorrelationDefnWrapperImpl(
                corrDefnCreateOnlyList, corrDefnCorrelateOnlyList, corrDefnJoinList
            );

        return corrDefnWrapper;
    }

    private static List getCommonjoinCorrs(final List commonVals, final List newVals) {
        List retVal = new ArrayList();
        CorrelationDefn defn1 = null;
        CorrelationDefn defn2 = null;
        for (int i = 0, size = commonVals.size(); i < size; i++) {
            defn1 = (CorrelationDefn) commonVals.get(i);
            for (int j = 0, newValsSize = newVals.size(); j < newValsSize; j++) {
                defn2 = (CorrelationDefn) newVals.get(j);
                if (defn1.getCorrelationSetID() == defn2.getCorrelationSetID()) {
                    retVal.add(defn1);
                    break;
                }
            }
        }
        return retVal;
    }
    
    private static RStartElementCorrelationDefnWrapper 
    reconstructCorrDefnWrapper(final RStartElementCorrelationDefnWrapper defnWrp, 
            List commonCorrDefs) {
        
        List<CorrelationDefn> oldJoinCorrDefs = defnWrp.getJoinCorrDefns(); 
        List<CorrelationDefn> localCommonCorrDefs = new ArrayList<CorrelationDefn>(); 
        CorrelationDefn defn1 = null;
        CorrelationDefn defn2 = null;
        for (int j = 0, commonCorrDefsSize = commonCorrDefs.size(); j < commonCorrDefsSize; j++) {
            defn1 = (CorrelationDefn) commonCorrDefs.get(j);
            for (Iterator itr = oldJoinCorrDefs.iterator(); itr.hasNext(); ) {
                defn2 = (CorrelationDefn) itr.next();
                if (defn1.getCorrelationSetID() == defn2.getCorrelationSetID()) {
                    itr.remove();
                    localCommonCorrDefs.add(defn2);
                }
            }
        }
        
        return new RStartElementCorrelationDefnWrapperImpl(
                defnWrp.getCreateCorrDefns(), defnWrp.getCorrelateCorrDefns(),
                oldJoinCorrDefs, localCommonCorrDefs);
    }
    
    /**
     * @param defnWrp
     * @param commonCorrDefs
     * @return  List<List<CorrelationDefn>> List{list of uncommon corr defs, list of common corr defs}
     */
    private static List getUnCommonCorrDefs(final 
            RStartElementCorrelationDefnWrapper defnWrp, List commonCorrDefs) {
        List<List<CorrelationDefn>> retVal = new ArrayList<List<CorrelationDefn>>();
        List<CorrelationDefn> oldJoinCorrDefs = defnWrp.getJoinCorrDefns(); 
        List<CorrelationDefn> localCommonCorrDefs = new ArrayList<CorrelationDefn>(); 
        CorrelationDefn defn1 = null;
        CorrelationDefn defn2 = null;
        for (int j = 0, commonCorrDefsSize = commonCorrDefs.size(); j < commonCorrDefsSize; j++) {
            defn1 = (CorrelationDefn) commonCorrDefs.get(j);
            for (Iterator itr = oldJoinCorrDefs.iterator(); itr.hasNext(); ) {
                defn2 = (CorrelationDefn) itr.next();
                if (defn1.getCorrelationSetID() == defn2.getCorrelationSetID()) {
                    itr.remove();
                    localCommonCorrDefs.add(defn2);
                }
            }
        }
        retVal.add(oldJoinCorrDefs);
        retVal.add(localCommonCorrDefs);
        return retVal;
        
//        List unCommonCorrDefs = new ArrayList();
//        for (int i = 0, size = oldJoinCorrDefs.size(); i < size; i++) {
//            defn1 = (CorrelationDefn) oldJoinCorrDefs.get(i);
//            boolean foundCommon = false;
//            for (int j = 0, commonCorrDefsSize = commonCorrDefs.size(); j < commonCorrDefsSize; j++) {
//                defn2 = (CorrelationDefn) commonCorrDefs.get(j);
//                if (defn1.getCorrelationSetID() == defn2.getCorrelationSetID()) {
//                    foundCommon = true;
//                    break;
//                }
//            }
//            if (!foundCommon) {
//                unCommonCorrDefs.add(defn1);
//            }
//        }
//        return unCommonCorrDefs;
    }
    
    /**
     * DOCUMENT ME!
     *
     * @author Sun Microsystems
     * @version 
     */
    public static class CorrelationDefn {
        /**
         * DOCUMENT ME!
         */
        long mCorrSetUniqueID;

        /**
         * DOCUMENT ME!
         */
        MessagePropertyAlias[] mPropAliases;

        /**
         * DOCUMENT ME!
         */
        QName[] mPropDataTypes;

        /**
         * Creates a new CorrelationDefn object.
         *
         * @param corrSetUniqueID DOCUMENT ME!
         * @param propAliases DOCUMENT ME!
         * @param propDataTypes DOCUMENT ME!
         */
        CorrelationDefn(long corrSetUniqueID, MessagePropertyAlias[] propAliases, QName[] propDataTypes) {
            mPropAliases = propAliases;
            mPropDataTypes = propDataTypes;
            mCorrSetUniqueID = corrSetUniqueID;
        }

        /**
         * DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         */
        public long getCorrelationSetID() {
            return mCorrSetUniqueID;
        }

        /**
         * DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         */
        public MessagePropertyAlias[] getPropertyAlias() {
            return mPropAliases;
        }

        /**
         * DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         */
        public QName[] getPropertyDataTypes() {
            return mPropDataTypes;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @author Sun Microsystems
     */
    public static class CorrelationDefnWrapperImpl implements CorrelationDefnWrapper {
        /**
         * DOCUMENT ME!
         */
        List mCreateCorrDefnList = null;

        /**
         * DOCUMENT ME!
         */
        List mCorrelateCorrDefns = null;

        /**
         * DOCUMENT ME!
         */
        List mJoinCorrDefs = null;

        /**
         * Creates a new CorrelationDefnWrapperImpl object.
         *
         * @param createOnly DOCUMENT ME!
         * @param correlateOnly DOCUMENT ME!
         * @param join DOCUMENT ME!
         */
        public CorrelationDefnWrapperImpl(List createOnly, List correlateOnly, List join) {
            mCreateCorrDefnList = createOnly;
            mCorrelateCorrDefns = correlateOnly;
            mJoinCorrDefs = join;
        }

        /**
         * @see com.sun.bpel.model.meta.CorrelationDefnWrapper#getCreateCorrDefns()
         */
        public List getCreateCorrDefns() {
            return mCreateCorrDefnList;
        }

        /**
         * @see com.sun.bpel.model.meta.CorrelationDefnWrapper#getCorrelateCorrDefns()
         */
        public List getCorrelateCorrDefns() {
            return mCorrelateCorrDefns;
        }

        /**
         * @see com.sun.bpel.model.meta.CorrelationDefnWrapper#getJoinCorrDefns()
         */
        public List getJoinCorrDefns() {
            return mJoinCorrDefs;
        }
    }
    
    /**
     * @author Sun Inc
     * Apr 27, 2006
     */
    public static class RStartElementCorrelationDefnWrapperImpl extends 
    CorrelationDefnWrapperImpl implements RStartElementCorrelationDefnWrapper  {
        List mCommonJoin = null;

        /**
         * @param createOnly
         * @param correlateOnly
         * @param uncommonJoin
         * @param commonJoin
         */
        public RStartElementCorrelationDefnWrapperImpl(List createOnly, List correlateOnly, 
                List uncommonJoin, List commonJoin) {
            super(createOnly, correlateOnly, uncommonJoin);
            mCommonJoin = commonJoin;
        }
        
        /**
         * @param createOnly
         * @param correlateOnly
         * @param join
         */
        RStartElementCorrelationDefnWrapperImpl(List createOnly, List correlateOnly, 
                List join) {
            this(createOnly, correlateOnly, join, new ArrayList());
        }
        
        /** @see com.sun.bpel.model.meta.RStartElementCorrelationDefnWrapper#getCommonJoinCorrDefs()
         */
        public List getCommonJoinCorrDefs() {
            return mCommonJoin;
        }
    }
}
