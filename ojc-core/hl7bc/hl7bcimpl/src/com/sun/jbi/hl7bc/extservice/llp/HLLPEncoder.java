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
 * @(#)HLLPEncoder.java 
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
import org.apache.mina.filter.codec.ProtocolEncoder;
import org.apache.mina.filter.codec.ProtocolEncoderOutput;
import org.apache.mina.filter.codec.ProtocolCodecException;
import com.sun.jbi.hl7bc.I18n;

/**
 * This class represents the HLLP structure.
 * 
 * <pre>
 * MLLP (Minimal Lower Layer Protocol):   
 * 
 *   [ SOB ][ HL7 Data ][ EOD ][ EOB]
 * 
 * HLLP (Hybrid Lower Layer Protocol):   
 * 
 *   [ SOB ][ N | D ][ VER ][ CR ][ HL7 Data ][ LEN ][ BCC ][ EOD ][ EOB ]
 * 
 * where:
 * 
 *    SOB: start of block (character)
 *    EOD: end of data    (character)
 *    EOB: end of block   (character)
 *    BCC: block checksum (character)
 *    CR : Carriage return(character)
 *    LEN: Length of HL7 Data (starting from SOB to end of HL7 data)
 *    N  : NAK block
 *    D  : Data block
 *    VER: HL7 version :: 21, 22, 23 (i.e., 2.1, 2.2, 2.3, ...)
 * 
 * Assumptions:
 * 
 *     LEN : is 5 digit decimal (left zero filled) 
 *          -- largest value is 99999 bytes
 * 
 *     BCC : is 3 digit decimal (left zero filled)
 *           calculation algorithm is XOR.
 *          (999 means ignore BCC verification/calculation)
 * 
 * NOTE:
 * 
 *     The values of the Start of Block [SOB],  
 *     End of Data [EOD] and End of Block [EOB], 
 *     must be UNIQUE.
 * 
 * </pre> 
 * 
 * 
 * The original specification is as follow:
 * 
 * <pre>
 * For HLLP (Hybrid Lower Layer Protocol):
 * 
 * <SB>tvv<CR>ddddcccccxxx<EB><CR>
 * 
 * Blocks consist of the following fields. Note that these are LLP 
 * fields and are not the same as HL7 message fields.
 * 
 * <SB> = Start Block character (1 byte)
 *      Configurable on a site specific basis. Unless there is a conflict, the
 *      value should be ASCII <VT>, i.e., <0x0B>. This should
 *      not be confused with the ASCII characters SOH or STX.
 * t = Block Type (1 byte)
 *      'D' = data block
 *      'N' = NAK block
 * vv = Protocol ID (2 bytes)
 *      The characters '2' '3' for this version
 * <CR> = Carriage Return (1 byte)
 *      The ASCII carriage return character, i.e., <0x0D>.
 * dddd = Data (variable number of bytes)
 *      In a data block, this is the data content of the block. The data can
 *      contain any displayable ASCII characters and the carriage return
 *      character, <CR>. Carriage returns that are not part of the HL7
 *      message may be inserted as described in "Carriage Return Stuffing."
 *      In a NAK block, this field contains a 1-byte reason code as follows:
 *          'C' - character count wrong in previous data block received
 *          'X' - checksum wrong in previous data block received
 *          'B' - data too long for input buffer in previous block received
 *          'G' - Error not covered elsewhere.
 * ccccc = Block Size (5 bytes)
 *      Character count of all characters so far in the data block up to and
 *      including the last data character. For this version of the protocol this
 *      is 5 + the size of the dddd field. Note: HL7 message ends with a
 *      <CR> character. This character is considered as part of the data.
 * xxx = Checksum (3 bytes)
 *      Exclusive-OR checksum of all characters in the block up to and
 *      including the last data character. The checksum is expressed as a
 *      decimal number in three ASCII digits.
 *      If the value of this field is 999, the checksum should not be
 *      computed. Processing will proceed as if it were correct. This feature
 *      is used for applications where the messages will be translated from
 *      one character set to another during transmission.
 * <EB> = End Block character (1 byte)
 *      Configurable on a site specific basis. Unless there is a conflict, the
 *      value should be ASCII <FS>, i.e., <0x1C>. This should not be
 *      confused with the ASCII characters ETX or EOT.
 * <CR> = Carriage Return (1 byte)
 *      The ASCII carriage return character, i.e., <0x0D>.
 * 
 * 
 * For MLLP (Minimal Lower Layer Protocol):
 * 
 * <SB>dddd<EB><CR>
 * 
 * <SB> = Start Block character (1 byte)
 *      ASCII <VT>, i.e., <0x0B>. This should not be confused with the ASCII characters
 *      SOH or STX.
 * dddd = Data (variable number of bytes)
 *      This is the HL7 data content of the block. The data can contain any displayable
 *      ASCII characters and the carriage return character, <CR>.
 * <EB> = End Block character (1 byte)
 *      ASCII <FS>, i.e., <0x1C>. This should not be confused with the ASCII characters
 *      ETX or EOT.
 * <CR> = Carriage Return (1 byte)
 *      The ASCII carriage return character, i.e., <0x0D>.
 * </pre>
 * 
 * 
 *
 */

/**
 * @author S. Nageswara Rao, Raghunadh
 * This class adapts the hl7 payload as per HLLP specification
 *
 */
public class HLLPEncoder implements ProtocolEncoder {

    private static final Logger mLog =
        Logger.getLogger(HLLPEncoder.class.getName());
    private String logMsg, mVersionID;
    private char mSBChar, mEBChar, mEDChar, mBlockType;

	private boolean mHLLPCheckSumEnabled = false;

	public static final char HLLP_DATA_BLOCK = 'D';
    public static final char HLLP_NAK_BLOCK = 'N';
    public static final String HLLP_REASON_CODE_CHARACTER_COUNT_WRONG = "C - character count wrong in previous data block received";
    public static final String HLLP_REASON_CODE_CHECKSUM_WRONG = "X - checksum wrong in previous data block received";
    public static final String HLLP_REASON_CODE_DATA_TOO_LONG = "B - data too long for input buffer in previous block received";
    public static final String HLLP_REASON_CODE_OTHER_ERRORS = "G - Error not covered elsewhere";
    
    
	public void encode( IoSession session, 
	        			Object message,
	        			ProtocolEncoderOutput out ) throws ProtocolCodecException {

			 // Block Type validation
        if (HLLP_NAK_BLOCK != mBlockType && HLLP_DATA_BLOCK != mBlockType) {
          	mLog.log(Level.SEVERE,
						I18n.msg("E0245: HLLPEncoder.encoder(): Invalid Block Type [{0}],]. The Block Type (the second character) should be D or N.",
                        ""+mBlockType));
			 logMsg = I18n.msg("E0245: HLLPEncoder.encoder(): Invalid Block Type [{0}],]. The Block Type (the second character) should be D or N.",
                        ""+mBlockType);
            throw new ProtocolCodecException(logMsg);
        }
		 // Protocol ID (Version ID) validation
        if (null == this.mVersionID || 2 > mVersionID.length()) {
           	mLog.log(Level.SEVERE,
						I18n.msg("E0246: HLLPEncoder.encoder(): Invalid Protocol ID (Version ID) [{0}].",
                        ""+mVersionID));
			 logMsg = I18n.msg("E0246: HLLPEncoder.encoder(): Invalid Protocol ID (Version ID) [{0}].",
                        ""+mVersionID);
            throw new ProtocolCodecException(logMsg);
        }
		  // note: the passed-in versionID may be like 21, 22, 23 or 2.1, 2.2, 2.3, etc.
        char oneChar = mVersionID.charAt(0);
        char anotherChar = mVersionID.charAt(1);
        if ('.' == anotherChar && 2 < mVersionID.length()) {
            anotherChar = mVersionID.charAt(2);
        }

        if (('2' != oneChar && '3' != oneChar)
                || ('0' != anotherChar && '1' != anotherChar && '2' != anotherChar && '3' != anotherChar
                        && '4' != anotherChar && '5' != anotherChar)) {
          	mLog.log(Level.SEVERE,
						I18n.msg("E0247: HLLPEncoder.encoder(): Invalid Protocol ID (Version ID) [ {0} ] It should represent a valid HL7 version ID like 23 or 2.3, etc..",
                        ""+mVersionID));
			 logMsg = I18n.msg("E0247: HLLPEncoder.encoder(): Invalid Protocol ID (Version ID) [ {0} ] It should represent a valid HL7 version ID like 23 or 2.3, etc..",
                        ""+mVersionID);
            throw new ProtocolCodecException(logMsg);
        }

		
		
		String pureMsg = message.toString();
		byte[] pureMsgBody = pureMsg.getBytes();
		byte[] llpMsg = new byte[pureMsgBody.length + 15];

        // SOB
        llpMsg[0] = (byte) this.mSBChar;

        // Block Type
        llpMsg[1] = (byte)this.mBlockType;

        // Protocol ID
        llpMsg[2] = (byte) oneChar;
        llpMsg[3] = (byte) anotherChar;

        // CR
        llpMsg[4] = '\r';

        // Data Block
        System.arraycopy(pureMsgBody, 0, llpMsg, 5, pureMsgBody.length);

        // Block Size
        int size = 5 + pureMsgBody.length;
        String blockSize = String.valueOf(size);
        blockSize = "00000" + blockSize;
        blockSize = blockSize.substring(blockSize.length() - 5);

        System.arraycopy(blockSize.getBytes(), 0, llpMsg, size, 5);

        // Checksum
		try{
        int xorValue = 999; // initial to no-checksum
        if (this.mHLLPCheckSumEnabled) {
            xorValue = this.checksum(llpMsg, 0, llpMsg.length - 5);
        }

        String checksum = String.valueOf(xorValue);
        checksum = "000" + checksum;
        checksum = checksum.substring(checksum.length() - 3);
		

        System.arraycopy(checksum.getBytes(), 0, llpMsg, 10 + pureMsgBody.length, 3);
		} catch(Exception e){
		}

        // EOD
		
        llpMsg[llpMsg.length - 2] = (byte) this.mEDChar;

        // EOB
        llpMsg[llpMsg.length - 1] = (byte) this.mEBChar;

       //After Constructing the HLLP messae
		pureMsg = new String(llpMsg);

        ByteBuffer buf = ByteBuffer.allocate( pureMsg.length() );
        for( int i = 0; i < pureMsg.length(); i++ )
        {
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
						I18n.msg("E0248: HLLPEncoder.setStartBlockChar(): Invalid parameter value : {0} (in decimal ascii). The allowed range is 1 to 127.",
                        "" + (int)sbChar));
			 logMsg = I18n.msg("E0248: HLLPEncoder.setStartBlockChar(): Invalid parameter value : {0} (in decimal ascii). The allowed range is 1 to 127.",
                        "" + (int)sbChar);
            throw new ProtocolCodecException(logMsg);
        }

        if(mLog.isLoggable(Level.FINE)){
            mLog.log(Level.FINE,
						I18n.msg("I0147: HLLPEncoder.setStartBlockChar(): Value is set as : {0} (in decimal ascii).","" + (int)sbChar));
        }
       
	}
	
	public void setEndBlockChar(final char ebChar) throws ProtocolCodecException {
	    this.mEBChar = ebChar;
		if ((int) ebChar < 1 || 
            (int) ebChar > 127) {
            mLog.log(Level.SEVERE,
						I18n.msg("E0249: HLLPEncoder.setEndBlockChar(): Invalid parameter value : {0} (in decimal ascii). The allowed range is 1 to 127.",
                        "" + (int)ebChar));
            logMsg = I18n.msg("E0249: HLLPEncoder.setEndBlockChar(): Invalid parameter value : {0} (in decimal ascii). The allowed range is 1 to 127.",
                        "" + (int)ebChar);
            throw new ProtocolCodecException(logMsg);
        }

        if(mLog.isLoggable(Level.FINE)){
            mLog.log(Level.FINE,
						I18n.msg("I0148: HLLPEncoder.setEndBlockChar(): Value is set as : {0} (in decimal ascii).",
                        "" +(int) ebChar));
        }
   }
	
	public void setEndDataChar(final char edChar) throws ProtocolCodecException {
		this.mEDChar = edChar;
		if ((int) edChar < 1 || 
		    (int) edChar > 127) {
			 mLog.log(Level.SEVERE,
						I18n.msg("E0250: HLLPEncoder.setEndDataChar(): Invalid parameter value : {0} (in decimal ascii). The allowed range is 1 to 127.",
							"" +(int) edChar));
			 logMsg = I18n.msg("E0250: HLLPEncoder.setEndDataChar(): Invalid parameter value : {0} (in decimal ascii). The allowed range is 1 to 127.",
						"" + (int)edChar);
			 throw new ProtocolCodecException(logMsg);
		}

        if(mLog.isLoggable(Level.FINE)){
			mLog.log(Level.FINE,
						I18n.msg("I0149: HLLPEncoder.setEndDataChar(): Value is set as : {0} (in decimal ascii).",
							"" + (int)edChar));
        }
        
	}

	public void setVersionID(String version){
		this.mVersionID = version; 
	
	}
	public void setBlockType(char blockType){
		this.mBlockType = blockType; 
	
	}

	public void setHLLPChecksumEnabled(boolean hllpCheckSumEnabled){
		this.mHLLPCheckSumEnabled = hllpCheckSumEnabled;
	}

	public char getStartBlockChar() {
	   return this.mSBChar;
	}

	public char getEndBlockChar() {
	    return this.mEBChar;
		
   }

   	public char getEndDataChar() {
		return this.mEDChar;
		
	}

	public String getVersionID() {
		return this.mVersionID;
	}

	public char getBlockType() {
		return this.mBlockType;
	}

	public boolean getHLLPChecksumEnabled() {
		return this.mHLLPCheckSumEnabled;
	}
	    /**
     * Method checksum.
     * 
     * @param llpMessage
     * @param offset
     * @param len
     * @return int
     * @throws Exception
     */
    private int checksum(byte[] bytes, int offset, int len) throws Exception {
        if (null == bytes || bytes.length < offset + len) {
           
            
			
			 mLog.log(Level.SEVERE,
						I18n.msg("E0251: HLLPEncoder.checksum(): Invalid request. The data is [{0}]  the offset is < {1} > ,>, the length is < {2}>.",
							bytes.toString(),""+offset,""+len));
			 logMsg = I18n.msg("E0251: HLLPEncoder.checksum(): Invalid request. The data is [{0}]  the offset is < {1} > ,>, the length is < {2}>.",
                        bytes.toString(),""+offset,""+len);
            throw new Exception(logMsg);
        }

        int xorValue = 0;
        for (int i = offset; i < len; i++) {
            xorValue ^= bytes[i];
        }

        return xorValue;
    }
    
    public void dispose( IoSession session ) throws Exception {
    }
}//end of classw