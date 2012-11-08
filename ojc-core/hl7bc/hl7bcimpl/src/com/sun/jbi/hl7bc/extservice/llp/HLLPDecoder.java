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
 * @(#)HLLPDecoder.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.llp;

import java.io.ByteArrayOutputStream;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.mina.common.ByteBuffer;
import org.apache.mina.common.IoSession;
import org.apache.mina.filter.codec.ProtocolDecoder;
import org.apache.mina.filter.codec.ProtocolDecoderOutput;
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
public class HLLPDecoder implements ProtocolDecoder {

    private static final Logger mLog = Logger.getLogger(HLLPDecoder.class.getName());

    private String logMsg, mVersionID;

    private char mSBChar, mEBChar, mEDChar, mBlockType;

    private boolean mHLLPCheckSumEnabled = false;

    public static final char HLLP_DATA_BLOCK = 'D';

    public static final char HLLP_NAK_BLOCK = 'N';

    public static final String HLLP_REASON_CODE_CHARACTER_COUNT_WRONG = "C - character count wrong in previous data block received";

    public static final String HLLP_REASON_CODE_CHECKSUM_WRONG = "X - checksum wrong in previous data block received";

    public static final String HLLP_REASON_CODE_DATA_TOO_LONG = "B - data too long for input buffer in previous block received";

    public static final String HLLP_REASON_CODE_OTHER_ERRORS = "G - Error not covered elsewhere";

    public void decode(IoSession session, ByteBuffer in, ProtocolDecoderOutput out)
            throws ProtocolCodecException {

        // Gets the message payload in bytes
        java.nio.ByteBuffer nioBuf = in.buf();
        int offset = nioBuf.position();
        int length = nioBuf.limit() - offset;
        if (!nioBuf.hasArray()) {
            ByteBuffer heapBuf = ByteBuffer.allocate(length, false);
            heapBuf.put(in);
            heapBuf.flip();
            nioBuf = heapBuf.buf();
            offset = 0;
        }
        ByteArrayOutputStream baos = (ByteArrayOutputStream) session.getAttribute("currentInput");
        byte[] bytes = nioBuf.array();
        for (int ii = offset; ii < length; ii++) {
            baos.write(bytes[ii]);
        }
	//retrieve the total bytes received till the last message 
        //that value been maintained as an attribute in the session
        long sessionReadBytes = (Long) session.getAttribute("readBytes");
        long totalReadBytes = session.getReadBytes() - sessionReadBytes;

        //Read all the message bytes that were received on the session 
        //and then only propogate the hl7 message to the listener. 
	//Mina framework may fire multiple events for a single message
	if (baos.size() < totalReadBytes)
		return;
        byte[] llpMessage = baos.toByteArray();
		//Resets the count field of this byte array output stream to zero, 
        //so that all currently accumulated output in the output stream is discarded.
        baos.reset();
        session.setAttribute("currentInput", baos);
		session.setAttribute("readBytes", session.getReadBytes());
        if (null == llpMessage || 15 > llpMessage.length) {
            mLog.log(Level.SEVERE, I18n.msg("E0252: HLLPDecoder.decode(): Invalid HLLP data [ {0} ]. Its length shouldn't be less than 15.", llpMessage.toString()));
            logMsg = I18n.msg("E0252: HLLPDecoder.decode(): Invalid HLLP data [ {0} ]. Its length shouldn't be less than 15.", llpMessage.toString());

            throw new ProtocolCodecException(logMsg);
        }

        // SOB validation
        if (this.mSBChar != (char) llpMessage[0]) {

            mLog.log(Level.SEVERE, I18n.msg("E0253: HLLPDecoder.decode(): Invalid HLLP data,.The configuration value of Start Block Character is [ {0}}] ,It does not match with the actual data. [{1}}]", (int)mSBChar,(int)(char) llpMessage[0]));
            logMsg = I18n.msg("E0253: HLLPDecoder.decode(): Invalid HLLP data,.The configuration value of Start Block Character is [ {0}}] ,It does not match with the actual data. [{1}}]", (int)mSBChar,(int)(char) llpMessage[0]);

            throw new ProtocolCodecException(logMsg);
        }

        // EOB validation
        if (this.mEBChar != (char) llpMessage[llpMessage.length - 1]) {

            mLog.log(Level.SEVERE, I18n.msg("E0254: HLLPDecoder.decode(): Invalid HLLP data,.The configuration value of End Block Character is [ {0}}] ,It does not match with the actual data. [{1}}]", (int)mEBChar,(int)(char)llpMessage[llpMessage.length - 1] ));
            logMsg = I18n.msg("E0254: HLLPDecoder.decode(): Invalid HLLP data,.The configuration value of End Block Character is [ {0}}] ,It does not match with the actual data. [{1}}]", (int)mEBChar,(int)(char)llpMessage[llpMessage.length - 1]);

            throw new ProtocolCodecException(logMsg);
        }

        // EOD validation
        if (this.mEDChar != (char) llpMessage[llpMessage.length - 2]) {

            mLog.log(Level.SEVERE, I18n.msg("E0255: HLLPDecoder.decode(): Invalid HLLP data,.The configuration value of End Data Character is [ {0}}] ,It does not match with the actual data. [{1}}]",(int)mEDChar, (int)(char)llpMessage[llpMessage.length - 2]));
            logMsg = I18n.msg("E0255: HLLPDecoder.decode(): Invalid HLLP data,.The configuration value of End Data Character is [ {0}}] ,It does not match with the actual data. [{1}}]",(int)mEDChar, (int)(char)llpMessage[llpMessage.length - 2]);

            throw new ProtocolCodecException(logMsg);
        }

        // Block Type validation
        char oneChar = (char) llpMessage[1];
        if (HLLP_NAK_BLOCK != oneChar && HLLP_DATA_BLOCK != oneChar) {

            mLog.log(Level.SEVERE, I18n.msg("E0256: HLLPDecoder.decode(): Invalid HLLP data {0}, the Block Type (second character) should be {1} or {2}",
                    new String(llpMessage), "" + HLLP_DATA_BLOCK, "" + HLLP_NAK_BLOCK ));
            logMsg = I18n.msg("E0256: HLLPDecoder.decode(): Invalid HLLP data {0}, the Block Type (second character) should be {1} or {2}",
                    new String(llpMessage), "" + HLLP_DATA_BLOCK, "" + HLLP_NAK_BLOCK );

            throw new ProtocolCodecException(logMsg);
        }

        // Protocol ID (Version ID) validation
        oneChar = (char) llpMessage[2];
        char anotherChar = (char) llpMessage[3];
        if (('2' != oneChar && '3' != oneChar)
                || ('0' != anotherChar && '1' != anotherChar && '2' != anotherChar && '3' != anotherChar && '4' != anotherChar)) {

            mLog.log(Level.SEVERE, I18n.msg("E0257: The Protocol ID (the third and fourth character) doesn't have valid value, please check documentation for details :{0}", new String(
                    llpMessage) ));
            logMsg = I18n.msg("E0257: The Protocol ID (the third and fourth character) doesn't have valid value, please check documentation for details :{0}",
						 new String(llpMessage) );

            throw new ProtocolCodecException(logMsg);
        }

        // Carriage Return validation
        if ('\r' != (char) llpMessage[4]) {

            mLog.log(Level.SEVERE, I18n.msg("E0258: HLLPDecoder.decode(): Invalid HLLP data {0} The fifth character should be a Carriage Return (CR).",
						 new String(llpMessage)));
            logMsg = I18n.msg("E0258: HLLPDecoder.decode(): Invalid HLLP data {0} The fifth character should be a Carriage Return (CR).",
						new String(llpMessage) );

            throw new ProtocolCodecException(logMsg);
        }

        // now the hl7 message shapes out, then the more specific exception
        // may be thrown to indicate the specific cases

        // data length validation -- Block Size can hold 99999 at most
        if (llpMessage.length - 10 > 99999) {

            mLog.log(Level.SEVERE, I18n.msg("E0259: HLLPDecoder.decode(): Invalid HLLP data {0}, The data is too long. Its length is < {1} > >. HLLP message Block Size field is a 5-digits number which maximal value is 99999.",
                    new String(llpMessage), "" + llpMessage.length ));
            logMsg = I18n.msg("E0259: HLLPDecoder.decode(): Invalid HLLP data {0}, The data is too long. Its length is < {1} > >. HLLP message Block Size field is a 5-digits number which maximal value is 99999.", 
                    new String(llpMessage), "" + llpMessage.length );

            throw new ProtocolCodecException(logMsg);
        }

        // Block Size validation
        String blockSize = new String(llpMessage, llpMessage.length - 10, 5);
        int size = 0;
        // Block Size validation -- numeric validation
        try {
            size = Integer.parseInt(blockSize);
        } catch (Exception e) {

            mLog.log(Level.SEVERE, I18n.msg("E0260: HLLPDecoder.decode(): Invalid HLLP data {0},. The Block Size {1}, it should only contain numeric characters.",
                    new String(llpMessage), "" + blockSize ));
            logMsg = I18n.msg("E0260: HLLPDecoder.decode(): Invalid HLLP data {0},. The Block Size {1}, it should only contain numeric characters.",
                    new String(llpMessage), "" + blockSize );
        }

        // Block Size validation -- value validation
        if (size != llpMessage.length - 10) {

            mLog.log(Level.SEVERE, I18n.msg("E0261: HLLPDecoder.decode(): Invalid HLLP data {0},The Block Size value is {1}, but the actual Character count of all characters so far in the data block is {2}", 
                    new String(llpMessage), "" + size, "" + (llpMessage.length - 10) ));
            logMsg = I18n.msg("E0261: HLLPDecoder.decode(): Invalid HLLP data {0},The Block Size value is {1}, but the actual Character count of all characters so far in the data block is {2}",
                    new String(llpMessage), "" + size, "" + (llpMessage.length - 10) );
        }
        // Checksum validation
        try {
            String checksum = new String(llpMessage, llpMessage.length - 5, 3);
            int value = 0;
            // Checksum validation -- numeric validation
            try {
                value = Integer.parseInt(checksum);
            } catch (Exception e) {

                mLog.log(Level.SEVERE, I18n.msg("E0262: HLLPDecoder.decode(): Invalid HLLP data {0},The Checksum value is {1}, should only contain numeric characters.",
                        new String(llpMessage), "" + checksum ));
                logMsg = I18n.msg("E0262: HLLPDecoder.decode(): Invalid HLLP data {0},The Checksum value is {1}, should only contain numeric characters.",
                        new String(llpMessage), "" + checksum );
            }

            // Checksum validation -- value validation
            if (value != 999) {
                int xorValue = this.checksum(llpMessage, 0, llpMessage.length - 5);
                if (value != xorValue) {

                    mLog.log(Level.SEVERE, I18n.msg("E0263: HLLPDecoder.decode(): Invalid HLLP data {0}, The Checksum value is {1}, but the expected value is {2}",
                            new String(llpMessage), "" + value, "" + xorValue));
                    logMsg = I18n.msg("E0263: HLLPDecoder.decode(): Invalid HLLP data {0}, The Checksum value is {1}, but the expected value is {2}",
								new String(llpMessage), "" + value, "" + xorValue );
                }
            }
        } catch (Exception e) {
        }
        //////////////////////////////////////
        // extract the pure HL7 message body
        //////////////////////////////////////
        byte[] hl7MessageBody =  new byte[llpMessage.length - 15];
       
        System.arraycopy(llpMessage, 5, hl7MessageBody, 0, hl7MessageBody.length);
        byte[] hl7MsgBody = null;
        if (this.mEBChar == (char) hl7MessageBody[hl7MessageBody.length - 1]
                && this.mEBChar == (char) hl7MessageBody[hl7MessageBody.length - 2]) {
            hl7MsgBody = new byte[hl7MessageBody.length - 1];

        } else {
            hl7MsgBody = new byte[hl7MessageBody.length];
        }
        System.arraycopy(hl7MessageBody, 0, hl7MsgBody, 0, hl7MsgBody.length);
        
        String hl7PayLoad = new String(hl7MsgBody);
       

        out.write(hl7PayLoad);
    }// end of decode method

    public void setStartBlockChar(final char sbChar) throws ProtocolCodecException {
        this.mSBChar = sbChar;
        if ((int) sbChar < 1 || (int) sbChar > 127) {

            mLog.log(Level.SEVERE, I18n.msg("E0264: HLLPDecoder.setStartBlockChar(): Invalid parameter value < {0} > (in decimal ascii). The allowed range is 1 to 127.", "" +(int) sbChar ));
            logMsg = I18n.msg("E0264: HLLPDecoder.setStartBlockChar(): Invalid parameter value < {0} > (in decimal ascii). The allowed range is 1 to 127.", "" +(int) sbChar );
            throw new ProtocolCodecException(logMsg);
        }

        if(mLog.isLoggable(Level.FINE)){
            mLog.log(Level.FINE, I18n.msg("I0150: HLLPDecoder.setStartBlockChar(): Value is set as <{0}>> (in decimal ascii).", "" + (int)sbChar ));
        }

    }

    public void setEndBlockChar(final char ebChar) throws ProtocolCodecException {
        this.mEBChar = ebChar;
        if ((int) ebChar < 1 || (int) ebChar > 127) {
            mLog.log(Level.SEVERE, I18n.msg("E0265: HLLPDecoder.setEndBlockChar(): Invalid parameter value < {0} > (in decimal ascii). The allowed range is 1 to 127.", "" + (int)ebChar ));
            logMsg = I18n.msg("E0265: HLLPDecoder.setEndBlockChar(): Invalid parameter value < {0} > (in decimal ascii). The allowed range is 1 to 127.",  "" + (int)ebChar );
            throw new ProtocolCodecException(logMsg);
        }

        if(mLog.isLoggable(Level.FINE)){
            mLog.log(Level.FINE, I18n.msg("I0152: HLLPDecoder.setEndBlockChar(): Value is set as <{0}>> (in decimal ascii).",  "" + (int)ebChar ));
        }
    }

    public void setEndDataChar(final char edChar) throws ProtocolCodecException {
        this.mEDChar = edChar;
        if ((int) edChar < 1 || (int) edChar > 127) {
            mLog.log(Level.SEVERE, I18n.msg("E0266: HLLPDecoder.setEndDataChar(): Invalid parameter value < {0} > (in decimal ascii). The allowed range is 1 to 127.", "" + (int)edChar));
            logMsg = I18n.msg("E0266: HLLPDecoder.setEndDataChar(): Invalid parameter value < {0} > (in decimal ascii). The allowed range is 1 to 127.",  "" + (int)edChar );
            throw new ProtocolCodecException(logMsg);
        }

        if(mLog.isLoggable(Level.FINE)){
            mLog.log(Level.FINE, I18n.msg("I0153: HLLPDecoder.setEndDataChar(): Value is set as <{0}>> (in decimal ascii).", "" +(int) edChar ));
        }

    }

    public void setVersionID(String version) {
        this.mVersionID = version;

    }

    public void setBlockType(char blockType) {
        this.mBlockType = blockType;

    }

    public void setHLLPChecksumEnabled(boolean hllpCheckSumEnabled) {
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

            mLog.log(Level.SEVERE, I18n.msg("E0267: HLLPDecoder.checksum(): Invalid request. The data is [{0}]  the offset is < {1} > ,>, the length is < {2}>.", bytes.toString(), "" + offset,
                    "" + len ));
            logMsg = I18n.msg("E0267: HLLPDecoder.checksum(): Invalid request. The data is [{0}]  the offset is < {1} > ,>, the length is < {2}>.", bytes.toString(), "" + offset,
                    "" + len );
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
    
    public void finishDecode( IoSession session, ProtocolDecoderOutput out ) throws Exception {
    }   

}//end of class
