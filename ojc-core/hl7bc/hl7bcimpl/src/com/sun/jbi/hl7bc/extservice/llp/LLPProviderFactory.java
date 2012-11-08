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
 * @(#)LLPProviderFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.llp;

import java.util.logging.Logger;
import java.util.logging.Level;

import com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties;
import com.sun.jbi.hl7bc.extservice.ProtocolInfo;
import com.sun.jbi.hl7bc.I18n;

/**
 * Factory class for creating a LowerLayerProtocolProvider. 
 * This factory bridges a message adapting layer between HL7 Application and Transport Layer
 * 
 * @author S. Nageswara Rao, Raghunadh
 */
public class LLPProviderFactory implements com.sun.jbi.hl7bc.HL7Constants {
    private static final Logger mLog =
        Logger.getLogger(LLPProviderFactory.class.getName());
    
   	// Even though MLLPv2 is there, at present we support MLLPv1 and HLLP
	public enum LLPType { MLLPv1, MLLPv2, HLLP };
	
	/**
     * Create and return the ProtocolProvider given the LLPType
     * 
     * @param llpType The lower layer protocol type
     */
	public static AbstractLLPProtocolProvider createLLPProvider(LLPType llpType) throws Exception {
		  switch (llpType) {
            case MLLPv1: {
                mLog.log (Level.FINE,
                          I18n.msg("I0140: Creating Lower Layer Protocol Provider object of class {0}",
                          MLLPv1ProtocolProvider.class.getName()));
                return new MLLPv1ProtocolProvider ();
            }
            case MLLPv2: {
                mLog.log (Level.FINE,
                          I18n.msg("I0140: Creating Lower Layer Protocol Provider object of class {0}",
                          MLLPv2ProtocolProvider.class.getName()));
                return new MLLPv2ProtocolProvider (); 
                /* mLog.log(Level.SEVERE,
                        "LLPProviderFactory_UNSUPPORTED_LLP_TYPE",
                        new Object[]{llpType});

                String errMsg = mMessages.getString("LLPProviderFactory_UNSUPPORTED_LLP_TYPE",
                        new Object[]{llpType});
                throw new Exception (errMsg); */
            }

            case HLLP: {
                mLog.log (Level.FINE,
                        I18n.msg("I0140: Creating Lower Layer Protocol Provider object of class {0}",
                        HLLPProtocolProvider.class.getName()));
                return new HLLPProtocolProvider ();
            }
            
            
            default: {
                mLog.log(Level.SEVERE,
                        I18n.msg("E0218: The Lower Layer Protocol type {0} is not a valid type",
                        llpType));

                String errMsg = I18n.msg("E0218: The Lower Layer Protocol type {0} is not a valid type",
								llpType);
                throw new Exception (errMsg);
            }

		  }
	}
	
	public static class Util {
	    
		public static LLPType stringToEnumValue(String llpTypeStr) {
		    if(llpTypeStr.equals(MLLPv1)) {
		        return LLPType.MLLPv1;
		    } else if(llpTypeStr.equals(MLLPv2)) {
		        return LLPType.MLLPv2;
		    } else if(llpTypeStr.equals(HLLP)) {
		        return LLPType.HLLP;
		    }
		    return LLPType.MLLPv1;
		}
		
		public static ProtocolInfo populateLLPInfo(HL7ProtocolProperties hl7ProtoProps) {
		    LLPType llpType =  Util.stringToEnumValue(hl7ProtoProps.getLLPType());
		    ProtocolInfo llpInfo = new ProtocolInfo();
	        switch(llpType) {
	        	case MLLPv1: {
	        	    llpInfo.put(LLPConstants.LLP_TYPE, hl7ProtoProps.getLLPType());
	        	    llpInfo.put(LLPConstants.START_BLOCK_CHAR, hl7ProtoProps.getStartBlockChar().toString());
	        	    llpInfo.put(LLPConstants.END_BLOCK_CHAR, hl7ProtoProps.getEndBlockChar().toString());
	        	    llpInfo.put(LLPConstants.END_DATA_CHAR, hl7ProtoProps.getEndDataChar().toString());
	        	    return llpInfo;
	        	}
	        	case MLLPv2: {
	        		 llpInfo.put(LLPConstants.LLP_TYPE, hl7ProtoProps.getLLPType());
		        	 llpInfo.put(LLPConstants.START_BLOCK_CHAR, hl7ProtoProps.getStartBlockChar().toString());
		        	 llpInfo.put(LLPConstants.END_BLOCK_CHAR, hl7ProtoProps.getEndBlockChar().toString());
		        	 llpInfo.put(LLPConstants.END_DATA_CHAR, hl7ProtoProps.getEndDataChar().toString());
		        	 llpInfo.put(HL7ProtocolProperties.MLLPV2_RETRIES_COUNT_ON_NAK,hl7ProtoProps.getMLLPV2RetriesCountOnNak().toString());
		        	 llpInfo.put(HL7ProtocolProperties.MLLPV2_RETRY_INTERVAL,hl7ProtoProps.getMllpv2RetryInterval().toString());
		        	 llpInfo.put(HL7ProtocolProperties.MLLPV2_TIME_TO_WAIT_FOR_ACK_NAK,hl7ProtoProps.getMllpv2TimeToWaitForAckNak().toString());
		        	 return llpInfo;
	        	}
	        	case HLLP: {
	        	    llpInfo.put(LLPConstants.LLP_TYPE, hl7ProtoProps.getLLPType());
	        	    llpInfo.put(LLPConstants.START_BLOCK_CHAR, hl7ProtoProps.getStartBlockChar().toString());
	        	    llpInfo.put(LLPConstants.END_BLOCK_CHAR, hl7ProtoProps.getEndBlockChar().toString());
	        	    llpInfo.put(LLPConstants.END_DATA_CHAR, hl7ProtoProps.getEndDataChar().toString());
					llpInfo.put(LLPConstants.HLLP_CHECKSUM, hl7ProtoProps.getHLLPChkSumEnabled().toString());
					llpInfo.put(LLPConstants.VERSION_ID, hl7ProtoProps.getVersionID());
	        	    return llpInfo;
	        	}
	        	default: 
	        	    //Nothing remains here to handle since LLP type defaulted to MLLPv1
	        	    break;
	        }
	        	return llpInfo;
		    }
	}
}
