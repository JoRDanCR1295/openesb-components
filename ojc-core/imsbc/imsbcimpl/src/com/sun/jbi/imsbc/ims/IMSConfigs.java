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

package com.sun.jbi.imsbc.ims;


import java.util.logging.Logger;
import java.util.logging.Level;

import com.sun.jbi.imsbc.extensions.IMSMessage;
import com.sun.jbi.imsbc.IMSException;

import com.sun.jbi.internationalization.Messages;

public class IMSConfigs {

    // IMS information
    public String TranCode;
    public String TranCodeSrc;
    public String datastoreID;
    public String LtermName;

    // RACF security information
    public String RacfUserID;
    public String RacfGroupName;
    public String Password;

    // IMS Connect information
    public String clientID;
    public String exitID;
    public byte syncLevel;
    public byte commitMode;
    public byte flowCtl;
    public byte rcvDelay;
    public byte soctType;
    public byte mfsMod;
    public char bandrs;
    public int prefixLength;
    public String irmHeaderEncoding;
    public String sendDataEncoding;
    public String replyDataEncoding;

    private String propVar = null;
    static final String EIGHT_SPACES = "        ";
    
    private static final Messages mMessages = Messages.getMessages(IMSConfigs.class);
    private static final Logger mLogger = Messages.getLogger(IMSConfigs.class);


    public IMSConfigs(IMSMessage imsMessage) throws IMSException {

        /*
         * IRM_RACF_USERID (String)
         */
        propVar = imsMessage.getIrmRacfUserId();
        try {
            if (propVar != null) {
                RacfUserID = (propVar.concat(EIGHT_SPACES)).substring(0, 8);
            }
            else {
                RacfUserID = EIGHT_SPACES;
            }
        }
        catch (Exception ex) {
        	String errMsg = mMessages.getString("IMSBC-E00817.Racf_UserId_Failed", new Object[] { ex.toString()});
        	mLogger.log(Level.INFO, errMsg);
        	throw new IMSException(errMsg);    
        }

        /*
         * IRM_RACF_GRNAME (String)
         */
        propVar = imsMessage.getIrmRacfGrpName();
        try {
            if (propVar != null) {
                RacfGroupName = (propVar.concat(EIGHT_SPACES)).substring(0, 8);
            }
            else {
                RacfGroupName = EIGHT_SPACES;
            }
        }
        catch (Exception ex) {
        	String errMsg = mMessages.getString("IMSBC-E00818.Racf_Grp_Name_Failed", new Object[] { ex.toString()});
        	mLogger.log(Level.INFO, errMsg);
        	throw new IMSException(errMsg); 
        }

        /*
         * IRM_RACF_PW (String)
         */
        propVar = imsMessage.getIrmRacfPwd();
        try {
            if (propVar != null) {
                Password = (propVar.concat(EIGHT_SPACES)).substring(0, 8);
            }
            else {
                Password = EIGHT_SPACES;
            }
        }
        catch (Exception ex) {
        	String errMsg = mMessages.getString("IMSBC-E00819.Racf_Pwd_Failed", new Object[] { ex.toString()});
        	mLogger.log(Level.INFO, errMsg);
        	throw new IMSException(errMsg); 
        }

        /*
         * IRM_LEN (String -> int)
         */
        propVar = String.valueOf(imsMessage.getIrmLen());
        try {
            if (propVar != null) {
                prefixLength = Integer.parseInt(propVar);
            }
        }
        catch (Exception ex) {
        	String errMsg = mMessages.getString("IMSBC-E00820.Irm_Len_Failed", new Object[] { ex.toString()});
        	mLogger.log(Level.INFO, errMsg);
        	throw new IMSException(errMsg); 
        }

        /*
         * IRM_ID (String)
         */
        propVar = imsMessage.getIrmId();
        try {
            if (propVar != null) {
                exitID = (propVar.concat(EIGHT_SPACES)).substring(0, 8);
            }
            else {
                exitID = EIGHT_SPACES;
            }
        }
        catch (Exception ex) {
        	String errMsg = mMessages.getString("IMSBC-E00821.Irm_Id_Failed", new Object[] { ex.toString()});
        	mLogger.log(Level.INFO, errMsg);
        	throw new IMSException(errMsg); 
        }

        /*
         * IRM_F5 (flow control, String -> byte)
         */
        propVar = imsMessage.getIrmFlow();
        try {
            if (propVar != null) {
                if ("Client_Translation".equals(propVar)) {
                    flowCtl = (byte) 0x40;
                }
                else if ("No_Auto_Flow".equals(propVar)) {
                    flowCtl = (byte) 0x00;
                }
                else if ("Single_Message".equals(propVar)) {
                    flowCtl = (byte) 0x01;
                }
                else if ("Auto_Flow_Out".equals(propVar)) {
                    flowCtl = (byte) 0x02;
                }
                else if ("No_Auto_Flow_Out".equals(propVar)) {
                    flowCtl = (byte) 0x04;
                }
                else {
                    flowCtl = (byte) 0x00;
                }
            }
        }
        catch (Exception ex) {
        	String errMsg = mMessages.getString("IMSBC-E00822.Irm_F5_Failed", new Object[] { ex.toString()});
        	mLogger.log(Level.INFO, errMsg);
        	throw new IMSException(errMsg); 
        }

        /*
         * IRM_TIMER (String -> byte)
         */
        propVar = imsMessage.getIrmTimer();
        rcvDelay = (byte) 0x00;      
        if (propVar != null) {
                if (".25 SEC".equals(propVar)) {
                    rcvDelay = (byte) 0x00;
                }
                else if ("No_Wait".equals(propVar)) {
                    rcvDelay = (byte) 0xE9;
                }
                else if ("Block".equals(propVar)) {
                    rcvDelay = (byte) 0xFF;
                }
                else {
                	String errMsg = mMessages.getString("IMSBC-E00823.Irm_Timer_Failed");
					if (mLogger.isLoggable(Level.SEVERE))
                		mLogger.log(Level.SEVERE, errMsg);
                	throw new IMSException(errMsg);
                }
            }
        else {
        	String errMsg = mMessages.getString("IMSBC-E00823.Irm_Timer_Failed");
			if (mLogger.isLoggable(Level.SEVERE))
        		mLogger.log(Level.SEVERE, errMsg);
        	throw new IMSException(errMsg);
        }
     

        /*
         * IRM_SOCT (String -> byte)
         */
        propVar = imsMessage.getIrmSocket();
        try {
            if (propVar != null) {
                if ("Transaction".equals(propVar)) {
                    soctType = (byte) 0x00;
                }
                else if ("Persistent".equals(propVar)) {
                    soctType = (byte) 0x10;
                }
                else if ("Non_Persistent".equals(propVar)) {
                    soctType = (byte) 0x40;
                }
                else {
                    soctType = (byte) 0x00;
                }
            }
        }
        catch (Exception ex) {
        	String errMsg = mMessages.getString("IMSBC-E00824.Irm_Socket_Failed", new Object[] { ex.toString()});
        	mLogger.log(Level.SEVERE, errMsg);
        	throw new IMSException(errMsg);
        }

        /*
         * IRM_CLIENTID (String)
         */
        propVar = imsMessage.getIrmClientId();
        try {
            if (propVar != null) {
                clientID = (propVar.concat(EIGHT_SPACES)).substring(0, 8);
            }
            else {
                clientID = EIGHT_SPACES;
            }
        }
        catch (Exception ex) {
        	String errMsg = mMessages.getString("IMSBC-E00825.Irm_Client_Id_Failed", new Object[] { ex.toString()});
        	mLogger.log(Level.SEVERE, errMsg);
        	throw new IMSException(errMsg);
        }

        /*
         * IRM_F1 MFS mod name flag (String -> byte)
         */
        propVar = imsMessage.getIrmMod();
        try {
            if (propVar != null) {
                if ("MFS".equals(propVar)) {
                    mfsMod = (byte) 0x80;
                }
                else if ("NO_MFS".equals(propVar)) {
                    mfsMod = 0x00;
                }
                else {
                    mfsMod = 0x00;
                }
            }
        }
        catch (Exception ex) {
        	String errMsg = mMessages.getString("IMSBC-E00826.Irm_F1_Failed", new Object[] { ex.toString()});
        	mLogger.log(Level.SEVERE, errMsg);
        	throw new IMSException(errMsg);
        }

        /*
         * IRM_F2 Commit Mode (String -> byte)
         */
        propVar = imsMessage.getIrmCommitMode();
        try {
            if (propVar != null) {
                if ("COMMIT_MODE_0".equals(propVar)) {
                    commitMode = 0x40;
                }
                else if ("COMMIT_MODE_1".equals(propVar)) {
                    commitMode = 0x20;
                }
                else {
                    commitMode = 0x20;
                }
            }
        }
        catch (Exception ex) {
        	String errMsg = mMessages.getString("IMSBC-E00827.Irm_F2_Failed", new Object[] { ex.toString()});
        	mLogger.log(Level.SEVERE, errMsg);
        	throw new IMSException(errMsg);
        }

        /*
         * IRM_F3 Sync Level (String -> byte)
         */
        propVar = imsMessage.getIrmSyncLevel();
        try {
            if (propVar != null) {
                if ("SYNC_LEVEL_NONE".equals(propVar)) {
                    syncLevel = 0x00;
                }
                else if ("SYNC_LEVEL_CONFIRM".equals(propVar)) {
                    syncLevel = 0x01;
                }
                else {
                    syncLevel = 0x00;
                }
            }
        }
        catch (Exception ex) {
        	String errMsg = mMessages.getString("IMSBC-E00828.Irm_F3_Failed", new Object[] { ex.toString()});
        	mLogger.log(Level.SEVERE, errMsg);
        	throw new IMSException(errMsg);
        }

        /*
         * IRM_F4 (send type, String -> byte)
         */
        propVar = imsMessage.getIrmAck();
        try {
            if (propVar != null) {
                if ("NO_ACK".equals(propVar)) {
                    bandrs = ' ';
                }
                else if ("ACK".equals(propVar)) {
                    bandrs = 'A';
                }
                else if ("NACK".equals(propVar)) {
                    bandrs = 'N';
                }
                else if ("DEALLOCATE".equals(propVar)) {
                    bandrs = 'D';
                }
                else if ("RESUME".equals(propVar)) {
                    bandrs = 'R';
                }
                else if ("SENDONLY".equals(propVar)) {
                    bandrs = 'S';
                }
                else {
                    this.bandrs = ' ';
                }
            }
        }
        catch (Exception ex) {
        	String errMsg = mMessages.getString("IMSBC-E00829.Irm_F4_Failed", new Object[] { ex.toString()});
        	mLogger.log(Level.SEVERE, errMsg);
        	throw new IMSException(errMsg);
        }

        /*
         * IRM_TRNCOD (String)
         */
        propVar = imsMessage.getIrmTranCode();
        try {
            if (propVar != null) {
                this.TranCode = (propVar.concat(EIGHT_SPACES)).substring(0, 8);
            }
            else {
                this.TranCode = (EIGHT_SPACES);
            }
        }
        catch (Exception ex) {
        	String errMsg = mMessages.getString("IMSBC-E00830.Trans_Code_Failed", new Object[] { ex.toString()});
        	mLogger.log(Level.SEVERE, errMsg);
        	throw new IMSException(errMsg);
        }

        /*
         * IRM_TRNCOD_SRC (String)
         */
        propVar = imsMessage.getIrmTranCodeSrc();
        try {
            if (propVar != null) {
                TranCodeSrc = propVar;
            }
        }
        catch (Exception ex) {
        	String errMsg = mMessages.getString("IMSBC-E00831.Trans_Code_Src_Failed", new Object[] { ex.toString()});
        	mLogger.log(Level.SEVERE, errMsg);
        	throw new IMSException(errMsg);
        }

        /*
         * IRM_DESTID (String)
         */
        propVar = imsMessage.getIrmDestId();
        try {
            if (propVar != null) {
                datastoreID = (propVar.concat(EIGHT_SPACES)).substring(0, 8);
            }
            else {
                datastoreID = EIGHT_SPACES;
            }
        }
        catch (Exception ex) {
        	String errMsg = mMessages.getString("IMSBC-E00832.Data_StoreId_Failed", new Object[] { ex.toString()});
        	mLogger.log(Level.SEVERE, errMsg);
        	throw new IMSException(errMsg);
        }

        /*
         * IRM_LTERM (String)
         */
        propVar = imsMessage.getIrmLterm();
        try {
            if (propVar != null) {
                LtermName = (propVar.concat(EIGHT_SPACES)).substring(0, 8);
            }
            else {
                LtermName = EIGHT_SPACES;
            }
        }
        catch (Exception ex) {
        	String errMsg = mMessages.getString("IMSBC-E00833.Lterm_Failed", new Object[] { ex.toString()});
        	mLogger.log(Level.SEVERE, errMsg);
        	throw new IMSException(errMsg);
        }

        /*
         * IRM_HEADER_ENCODING (String)
         */
        propVar = imsMessage.getIrmHeaderEncod();
        try {
            if (propVar != null) {
                irmHeaderEncoding = propVar;
            }
            else {
                irmHeaderEncoding = "ISO-8859-1";
            }
        }
        catch (Exception ex) {
        	String errMsg = mMessages.getString("IMSBC-E00834.Irm_Header_Encoding_Failed", new Object[] { ex.toString()});
        	mLogger.log(Level.SEVERE, errMsg);
        	throw new IMSException(errMsg);
        }

        /*
         * SEND_DATA_ENCODING (String)
         */
        propVar = imsMessage.getSendDataEncod();
        try {
            if (propVar != null) {
                sendDataEncoding = propVar;
            }
            else {
                sendDataEncoding = "NO TRANSLATION";
            }
        }
        catch (Exception ex) {
        	String errMsg = mMessages.getString("IMSBC-E00835.Send_Data_Encoding_Failed", new Object[] { ex.toString()});
        	mLogger.log(Level.SEVERE, errMsg);
        	throw new IMSException(errMsg);
        }

        /*
         * REPLY_DATA_ENCODING (String)
         */
        propVar = imsMessage.getReplyDataEncod();
        try {
            if (propVar != null) {
                replyDataEncoding = propVar;
            }
            else {
                replyDataEncoding = "ISO-8859-1";
            }
        }
        catch (Exception ex) {
        	String errMsg = mMessages.getString("IMSBC-E00836.Reply_Data_Encoding_Failed", new Object[] { ex.toString()});
        	mLogger.log(Level.SEVERE, errMsg);
        	throw new IMSException(errMsg);
        }

    }

    /**
     * Pads or truncates the String to the specified length.
     */
    public String stringPad(String string, char padChar, int padLength) {

        // construct a stringbuffer for padding efficiency
        StringBuffer stringBuffer = new StringBuffer(string);

        // pad the stringbuffer if string.length() is less than padLength
        for (int i = 0; i < (padLength - string.length()); i++) {
            stringBuffer.append(padChar);
        }

        // if truncation was necessary, substring will take care of that
        return stringBuffer.substring(0, padLength);
    }

}
