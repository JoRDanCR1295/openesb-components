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
 * @(#)MLLPEncoder.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.llp;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.mina.common.ByteBuffer;
import org.apache.mina.filter.codec.ProtocolEncoder;
import org.apache.mina.filter.codec.ProtocolEncoderOutput;
import org.apache.mina.common.IoSession;
import org.apache.mina.filter.codec.ProtocolCodecException;
import com.sun.jbi.hl7bc.I18n;
/**
 * This class represents the MLLP structure.
 * 
 * <pre>
 *    MLLP (Minimal Lower Layer Protocol):   
 *    
 *      [ SOB ][ HL7 Data ][ EOD ][ EOB]
 *    
 *    HLLP (Hybrid Lower Layer Protocol):   
 *    
 *      [ SOB ][ N | D ][ VER ][ CR ][ HL7 Data ][ LEN ][ BCC ][ EOD ][ EOB ]
 *    
 *    where:
 *    
 *       SOB: start of block (character)
 *       EOD: end of data    (character)
 *       EOB: end of block   (character)
 *       BCC: block checksum (character)
 *       CR : Carriage return(character)
 *       LEN: Length of HL7 Data (starting from SOB to end of HL7 data)
 *       N  : NAK block
 *       D  : Data block
 *       VER: HL7 version :: 21, 22, 23 (i.e., 2.1, 2.2, 2.3, ...)
 *    
 *    Assumptions:
 *    
 *        LEN : is 5 digit decimal (left zero filled) 
 *             -- largest value is 99999 bytes
 *    
 *        BCC : is 3 digit decimal (left zero filled)
 *              calculation algorithm is XOR.
 *             (999 means ignore BCC verification/calculation)
 *    
 *    NOTE:
 *    
 *        The values of the Start of Block [SOB],  
 *        End of Data [EOD] and End of Block [EOB], 
 *        must be UNIQUE.
 *    
 * </pre>
 * 
 * The original specification is as follow:
 * 
 * <pre>
 *    For HLLP (Hybrid Lower Layer Protocol):
 *    
 *    &lt;SB&gt;tvv&lt;CR&gt;ddddcccccxxx&lt;EB&gt;&lt;CR&gt;
 *    
 *    Blocks consist of the following fields. Note that these are LLP 
 *    fields and are not the same as HL7 message fields.
 *    
 *    &lt;SB&gt; = Start Block character (1 byte)
 *         Configurable on a site specific basis. Unless there is a conflict, the
 *         value should be ASCII &lt;VT&gt;, i.e., &lt;0x0B&gt;. This should
 *         not be confused with the ASCII characters SOH or STX.
 *    t = Block Type (1 byte)
 *         'D' = data block
 *         'N' = NAK block
 *    vv = Protocol ID (2 bytes)
 *         The characters '2' '3' for this version
 *    &lt;CR&gt; = Carriage Return (1 byte)
 *         The ASCII carriage return character, i.e., &lt;0x0D&gt;.
 *    dddd = Data (variable number of bytes)
 *         In a data block, this is the data content of the block. The data can
 *         contain any displayable ASCII characters and the carriage return
 *         character, &lt;CR&gt;. Carriage returns that are not part of the HL7
 *         message may be inserted as described in &quot;Carriage Return Stuffing.&quot;
 *         In a NAK block, this field contains a 1-byte reason code as follows:
 *             'C' - character count wrong in previous data block received
 *             'X' - checksum wrong in previous data block received
 *             'B' - data too long for input buffer in previous block received
 *             'G' - Error not covered elsewhere.
 *    ccccc = Block Size (5 bytes)
 *         Character count of all characters so far in the data block up to and
 *         including the last data character. For this version of the protocol this
 *         is 5 + the size of the dddd field. Note: HL7 message ends with a
 *         &lt;CR&gt; character. This character is considered as part of the data.
 *    xxx = Checksum (3 bytes)
 *         Exclusive-OR checksum of all characters in the block up to and
 *         including the last data character. The checksum is expressed as a
 *         decimal number in three ASCII digits.
 *         If the value of this field is 999, the checksum should not be
 *         computed. Processing will proceed as if it were correct. This feature
 *         is used for applications where the messages will be translated from
 *         one character set to another during transmission.
 *    &lt;EB&gt; = End Block character (1 byte)
 *         Configurable on a site specific basis. Unless there is a conflict, the
 *         value should be ASCII &lt;FS&gt;, i.e., &lt;0x1C&gt;. This should not be
 *         confused with the ASCII characters ETX or EOT.
 *    &lt;CR&gt; = Carriage Return (1 byte)
 *         The ASCII carriage return character, i.e., &lt;0x0D&gt;.
 *    
 *    
 *    For MLLP (Minimal Lower Layer Protocol):
 *    
 *    &lt;SB&gt;dddd&lt;EB&gt;&lt;CR&gt;
 *    
 *    &lt;SB&gt; = Start Block character (1 byte)
 *         ASCII &lt;VT&gt;, i.e., &lt;0x0B&gt;. This should not be confused with the ASCII characters
 *         SOH or STX.
 *    dddd = Data (variable number of bytes)
 *         This is the HL7 data content of the block. The data can contain any displayable
 *         ASCII characters and the carriage return character, &lt;CR&gt;.
 *    &lt;EB&gt; = End Block character (1 byte)
 *         ASCII &lt;FS&gt;, i.e., &lt;0x1C&gt;. This should not be confused with the ASCII characters
 *         ETX or EOT.
 *    &lt;CR&gt; = Carriage Return (1 byte)
 *         The ASCII carriage return character, i.e., &lt;0x0D&gt;.
 * </pre>
 * 
 * 
 */

/**
 * @author S. Nageswara Rao, TVA Raghunadh
 * This class adapts the hl7 payload as per MLLP version 1.0 specification
 *
 */
public class MLLPEncoder implements ProtocolEncoder {

    private static final Logger mLog =
        Logger.getLogger(MLLPEncoder.class.getName());
	private String logMsg;
    
    private char mSBChar, mEBChar, mEDChar;
    
	public void encode( IoSession session, 
	        			Object message,
	        			ProtocolEncoderOutput out ) throws ProtocolCodecException {

		String pureMsg = message.toString();
		byte[] pureMsgBody = pureMsg.getBytes();
		byte[] llpMsg = new byte[pureMsgBody.length + 3];
		
		// SOB
		llpMsg[0] = (byte) this.mSBChar;
		
		// Data block
		System.arraycopy(pureMsgBody, 0, llpMsg, 1, pureMsgBody.length);
		
		// EOD
		llpMsg[llpMsg.length - 2] = (byte)this.mEDChar;
		
		// EOB
		llpMsg[llpMsg.length - 1] = (byte) this.mEBChar;
		
		//After Constructing the MLLP message
		pureMsg = new String(llpMsg);
		
		ByteBuffer buf = ByteBuffer.allocate( pureMsg.length() );
		for( int i = 0; i < pureMsg.length(); i++ ) {
		    buf.put( ( byte ) pureMsg.charAt( i ) );
		}
		
		buf.flip();
		out.write( buf );

	    
	}//end of encode method
	
	public void setStartBlockChar(final char sbChar) throws ProtocolCodecException {
	    this.mSBChar = sbChar;
		if ((int) sbChar < 1 || 
            (int) sbChar > 127) {
          
             mLog.log(Level.SEVERE,
						I18n.msg("E0233: MLLPEncoder.setStartBlockChar():Invalid parameter value : {0} (in decimal ascii). The allowed range is 1 to 127.",
                        "" + (int)sbChar));
			 logMsg = I18n.msg("E0233: MLLPEncoder.setStartBlockChar():Invalid parameter value : {0} (in decimal ascii). The allowed range is 1 to 127.",
                        "" + (int)sbChar);
            throw new ProtocolCodecException(logMsg);
        }

        if(mLog.isLoggable(Level.FINE)){
            mLog.log(Level.FINE,
						I18n.msg("I0141: MLLPEncoder.setStartBlockChar():Value is set as : {0} (in decimal ascii).", "" + (int)sbChar));
        }
       
	}
	
	public void setEndBlockChar(final char ebChar) throws ProtocolCodecException {
	    this.mEBChar = ebChar;
		if ((int) ebChar < 1 || 
            (int) ebChar > 127) {
            mLog.log(Level.SEVERE,
						I18n.msg("E0234: MLLPEncoder.setEndBlockChar():Invalid parameter value : {0} (in decimal ascii). The allowed range is 1 to 127.",
                        "" + (int)ebChar));
            logMsg = I18n.msg("E0234: MLLPEncoder.setEndBlockChar():Invalid parameter value : {0} (in decimal ascii). The allowed range is 1 to 127.",
                        "" + (int)ebChar);
            throw new ProtocolCodecException(logMsg);
        }

        if(mLog.isLoggable(Level.FINE)){
            mLog.log(Level.FINE,
						I18n.msg("I0142: MLLPEncoder.setEndBlockChar():Value is set as : {0} (in decimal ascii).",
                        "" + (int)ebChar));
        }
   }
	
	public void setEndDataChar(final char edChar) throws ProtocolCodecException {
		this.mEDChar = edChar;
		if ((int) edChar < 1 || 
		    (int) edChar > 127) {
			 mLog.log(Level.SEVERE,
						I18n.msg("E0235: MLLPEncoder.setEndDataChar():Invalid parameter value : {0} (in decimal ascii). The allowed range is 1 to 127.",
						"" + (int)edChar));
			 logMsg = I18n.msg("E0235: MLLPEncoder.setEndDataChar():Invalid parameter value : {0} (in decimal ascii). The allowed range is 1 to 127.",
		              "" + (int)edChar);
			 throw new ProtocolCodecException(logMsg);
		}

        if(mLog.isLoggable(Level.FINE)){
			mLog.log(Level.FINE,
						I18n.msg("I0143: MLLPEncoder.setEndDataChar():Value is set as : {0} (in decimal ascii).",
						"" + (int)edChar));
        }
        
	}

	public char getStartBlockChar() {
	   return this.mSBChar;
	}

	public char getEndBlockChar()  {
	    return this.mEBChar;
		
   }

   	public char getEndDataChar() {
		return this.mEDChar;
		
	}
    
    public void dispose( IoSession session ) throws Exception {
    }
    
    
}//end of class
