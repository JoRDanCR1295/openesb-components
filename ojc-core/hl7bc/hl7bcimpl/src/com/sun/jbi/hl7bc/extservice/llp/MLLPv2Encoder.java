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
 * @(#)MLLPv2Encoder.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */


package com.sun.jbi.hl7bc.extservice.llp;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.mina.common.ByteBuffer;
import org.apache.mina.common.IoSession;
import org.apache.mina.filter.codec.ProtocolCodecException;
import org.apache.mina.filter.codec.ProtocolEncoder;
import org.apache.mina.filter.codec.ProtocolEncoderOutput;
import com.sun.jbi.hl7bc.I18n;

/**
 * This class adapts the hl7 payload as per MLLP version 2.0 specification
 * 
 * @author Nageswara Samudrala, Raghunadh
 *
 */
public class MLLPv2Encoder extends MLLPEncoder {

    private static final Logger mLog =
        Logger.getLogger(MLLPv2Encoder.class.getName());
	    
    public void encode( IoSession session, 
	        			Object message,
	        			ProtocolEncoderOutput out ) throws ProtocolCodecException {
    	
    	byte[] llpMsg = null;
    	String pureMsg = null;
    	byte[] pureMsgBody = null;
    	if(message instanceof java.lang.Byte) {//MLLPv2 ACK/NAK
    		llpMsg = new byte[4];
    	} else if(message instanceof java.lang.String) {//HL7 v3.0 ACK/NAK Message 
    		pureMsg = message.toString();
    		pureMsgBody = pureMsg.getBytes();
    		llpMsg = new byte[pureMsgBody.length + 3];
    		
    	}
    	// SOB
		llpMsg[0] = (byte) getStartBlockChar();
		if(message instanceof java.lang.Byte) {
			//ACK = 0x06.
			// or
			//NAK = 0x15.
			llpMsg[1] = ((Byte)message).byteValue();
		} else if(message instanceof java.lang.String) {
			// Data block
			System.arraycopy(pureMsgBody, 0, llpMsg, 1, pureMsgBody.length);
		} 
		
		// EOD
		llpMsg[llpMsg.length - 2] = (byte)getEndDataChar();
		
		// EOB
		llpMsg[llpMsg.length - 1] = (byte) getEndBlockChar();
		
		ByteBuffer buf = null;
		if(message instanceof java.lang.String) {
			//After Constructing the MLLP message
			pureMsg = new String(llpMsg);
		
			buf = ByteBuffer.allocate( pureMsg.length() );
			for( int i = 0; i < pureMsg.length(); i++ ) {
				buf.put( ( byte ) pureMsg.charAt( i ) );
			}
		} else if(message instanceof java.lang.Byte) {
			buf = ByteBuffer.allocate(4);
			buf.put(llpMsg);
		}
		
		buf.flip();
		out.write( buf );

  
	}//end of encode method
}
