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


import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.net.Socket;
import java.util.Vector;

import java.util.logging.Logger;
import java.util.logging.Level;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.alerter.NotificationEvent;

import com.sun.jbi.imsbc.IMSException;
import com.sun.jbi.imsbc.util.AlertsUtil;
import com.sun.jbi.imsbc.IMSBindingComponent;

public class IMSClient {

	
	static final Logger mLogger = Messages.getLogger(IMSClient.class);
	
	private static final Messages mMessages = Messages.getMessages(IMSClient.class);
	
	private Socket socket = null;

    // connection information
    private String hostName;
    private int portNumber;

    // IMS information
    // public String TranCode;
    protected String datastoreID;
    protected String LtermName;

    // RACF security information
    protected String RacfUserID;
    protected String RacfGroupName;
    protected String Password;

    // IMS Connect information
    private String clientID;
    private String exitID;
    private byte syncLevel;
    private byte commitMode;
    private byte flowCtl;
    private byte rcvDelay;
    private byte sockType;
    private byte mfsMod;
    private char bandrs;
    private int prefixLength;

    private String tExitID;
    private String tRacfUserID;
    private String tRacfGroupName;
    private String tPassword;
    private String tTrancode;
    private String tDatastoreID;
    private String tClientID;
    private String tLtermName;
    private String tSegment;
    private String[] tSegments;
    protected Vector byteVector;
    protected String returnCodeTxt;
    protected String reasonTxt;

    private String encoding = "ISO-8859-1";
    private String irmHeaderEncoding;
    private String sendDataEncoding;
    private String replyDataEncoding;

    private String ascii_csmoky = "*CSMOKY*";
    private String ascii_reqsts = "*REQSTS*";
    private String ascii_reqmod = "*REQMOD*";
    private String ascii_sample = "*SAMPLE*";

    private boolean connected = false;
    private boolean isSampl1 = false;
    
    public IMSClient(String datastoreID, String LtermName,
            String exitID, String clientID,
            String RacfUserID,
            String RacfGroupName,
            String Password,
            byte syncLevel,
            byte commitMode,
            int prefixLength,
            byte flowCtl,
            byte rcvDelay,
            byte sockType,
            byte mfsMod,
            String irmHeaderEncoding,
            String sendDataEncoding,
            String replyDataEncoding) {
   
	        /* set the corresponding transaction data,
            making all strings 8 characters long */
		   this.exitID = exitID;
		   this.datastoreID = datastoreID;
		   this.LtermName = LtermName;
		   this.clientID = clientID;
		   this.RacfUserID = RacfUserID;
		   this.RacfGroupName = RacfGroupName;
		   this.Password = Password;
		   this.syncLevel = syncLevel;
		   this.commitMode = commitMode;
		   this.flowCtl = flowCtl;
		   this.rcvDelay = rcvDelay;
		   this.sockType = sockType;
		   this.mfsMod = mfsMod;
		   this.prefixLength = prefixLength;
		   this.bandrs = bandrs;
		   this.irmHeaderEncoding = irmHeaderEncoding;
		   this.sendDataEncoding = sendDataEncoding;
		   this.replyDataEncoding = replyDataEncoding;
		
		   tExitID = "";
		   tRacfUserID = "";
		   tRacfGroupName = "";
		   tPassword = "";
		   tTrancode = "";
		   tDatastoreID = "";
		   tClientID = "";
		   tLtermName = "";
		   tSegment = "";			   
    }
    /**
     * Connects to the host.
     */
    public void open(String server, int serverPort) throws IMSException {
        try {
            // open a socket for the transaction
            socket = new Socket(server, serverPort);
            socket.setSoLinger(true, 10);
            connected = true;
        }
        catch (Exception e) {
        	String errMsg = mMessages.getString("IMSBC-E00808.Ims_Connect_Unavailable", new Object[] { server, serverPort});
       		mLogger.log(Level.INFO, errMsg);
			AlertsUtil.getAlerter().critical(errMsg, 
									IMSBindingComponent.SHORT_DISPLAY_NAME, 
									null, 
									AlertsUtil.getServerType(),
									AlertsUtil.COMPONENT_TYPE_BINDING,
									NotificationEvent.OPERATIONAL_STATE_RUNNING, 
									NotificationEvent.EVENT_TYPE_ALERT,
									"IMSBC-E00808");

        	throw new IMSException(errMsg);
        }
    }

    /**
     * Disconnects from the host.
     */
    public void close() throws IMSException {
        // verify socket open before attempting to disconnect
        if (socket != null) {
            try {
                socket.close();
                socket = null;
                connected = false;
            }
            catch (Exception e) {
            	String errMsg = mMessages.getString("IMSBC-E00809.Ims_Socket_Close_Failed", new Object[] { e.toString()});
            	mLogger.log(Level.INFO, errMsg);
				AlertsUtil.getAlerter().critical(errMsg, 
										IMSBindingComponent.SHORT_DISPLAY_NAME, 
										null, 
										AlertsUtil.getServerType(),
										AlertsUtil.COMPONENT_TYPE_BINDING,
										NotificationEvent.OPERATIONAL_STATE_RUNNING, 
										NotificationEvent.EVENT_TYPE_ALERT,
										"IMSBC-E00809");
            	throw new IMSException(errMsg);
            }
        }
    }
    /** Need to figure out what to do here **/

    public boolean isOpen() {
        return connected;
    }
    /**
     * Sends prefix and segment data.
     */
    public Vector send(String trancode,
                       String segment,
                       char bandrs,
                       boolean isAck) throws IMSException {
        int totalLength;

        // +4 for first LL, ZZ and final LL, ZZ
        totalLength = 4 + prefixLength + 4;

        try {
            DataOutputStream out = new DataOutputStream(socket.getOutputStream());

            StringBuffer strBuff = new StringBuffer(100);
            strBuff.append("Inside IMSClient send" + "\n");
            strBuff.append("irmHeaderEncoding: " + irmHeaderEncoding + "\n");
            strBuff.append("sendDataEncoding: " + sendDataEncoding + "\n");
            strBuff.append("replyDataEncoding: " + replyDataEncoding);
            mLogger.log(Level.INFO, strBuff.toString());
            if (! (irmHeaderEncoding.equals("ISO-8859-1"))) {
                tExitID = new String(exitID.getBytes(irmHeaderEncoding),
                                     encoding);
                tRacfUserID = new String(RacfUserID.getBytes(irmHeaderEncoding),
                                         encoding);
                tRacfGroupName = new String(RacfGroupName.getBytes(
                    irmHeaderEncoding), encoding);
                tPassword = new String(Password.getBytes(irmHeaderEncoding),
                                       encoding);
                tTrancode = new String(trancode.getBytes(irmHeaderEncoding),
                                       encoding);
                tDatastoreID = new String(datastoreID.getBytes(
                    irmHeaderEncoding), encoding);
                tClientID = new String(clientID.getBytes(irmHeaderEncoding),
                                       encoding);
                tLtermName = new String(LtermName.getBytes(irmHeaderEncoding),
                                        encoding);
            }
            else {
                tExitID = exitID;
                tRacfUserID = RacfUserID;
                tRacfGroupName = RacfGroupName;
                tPassword = Password;
                tTrancode = trancode;
                tDatastoreID = datastoreID;
                tClientID = clientID;
                tLtermName = LtermName;
            }

            if (sendDataEncoding.equals("NO TRANSLATION")) {
                tSegment = segment;
            }
            else {
                tSegment = new String(segment.getBytes(sendDataEncoding),
                                      encoding);
            }

            // add in segment length, if segment is defined
            if ( (tSegment != null) && (tSegment.length() > 0)) {
                totalLength += tSegment.length() + 12; // +12 for LL, ZZ, trancode
            }
            out.writeInt(totalLength); // send total message length
            if (tExitID != null && tExitID.equals("*SAMPL1*")) {
                isSampl1 = true;
            } else {
                isSampl1 = false;
            }
            out.writeShort(prefixLength); // send LL
            out.writeShort( (short) 0); // send ZZ
            out.writeBytes(tExitID); // send identifier
            out.writeInt(0); // send RESV
            out.write(flowCtl); // Flow control
            out.write(rcvDelay); // Receive delay
            out.write(sockType); // Socket Connection Type
            out.writeByte(0); // send RSV02
            out.writeBytes(tClientID); // send client id
            out.write(mfsMod); // send FLG 1 - mfs mod name returned?
            out.write(commitMode); // send FLG 2 - commit mode
            out.write(syncLevel); // send FLG 3 - synclevel
            out.writeByte(bandrs); // send FLG 4 - ack/nack/decallocate
            out.writeBytes(tTrancode); // send transaction code
            out.writeBytes(tDatastoreID); // send datastore id
            out.writeBytes(tLtermName); // send lterm name
            out.writeBytes(tRacfUserID); // send RACF id
            out.writeBytes(tRacfGroupName); // send RACF group
            out.writeBytes(tPassword); // send Password

            /* the 'if' is not supposed to be executed when you
                         ACK, NACK, DECALLOCATE, as data is irrelevant */
            if ( (tSegment != null) && (tSegment.length() > 0)) {
                // + 12 for LL and ZZ and trancode
                short recordLength = (short) (tSegment.length() + 12);
                out.writeShort(recordLength); // send LL
                out.writeShort( (short) 0); // send ZZ
                out.writeBytes(tTrancode); // send transaction code
                out.writeBytes(tSegment); // send segment
            }

            // send final LL ZZ to signal no more data to IMS Connect
            out.writeShort( (short) 4); // send LL
            out.writeShort( (short) 0); // send ZZ
            out.flush();
        }
        catch (Exception e) {
        	String errMsg = mMessages.getString("IMSBC-E00810.Client_Send_Failed", new Object[] { e.toString()});
        	mLogger.log(Level.INFO, errMsg);
			AlertsUtil.getAlerter().critical(errMsg, 
									IMSBindingComponent.SHORT_DISPLAY_NAME, 
									null, 
									AlertsUtil.getServerType(),
									AlertsUtil.COMPONENT_TYPE_BINDING,
									NotificationEvent.OPERATIONAL_STATE_RUNNING, 
									NotificationEvent.EVENT_TYPE_ALERT,
									"IMSBC-E00810");

        	connected = false;
        	throw new IMSException(errMsg);
        }

        // get transaction return
        Vector segments = this.receive(isAck);
        // return segment vector
        return segments;

    }
    
    /**
     * Receives output segments.
     */
    public Vector receive(boolean isAck) throws IMSException {

        // initialize segment vector
        Vector segments = new Vector();
        byteVector = new Vector();
        String identifier = null;
        String segmentData = null;
        returnCodeTxt = null;
        reasonTxt = null;

        try {
            DataInputStream in = new DataInputStream(socket.getInputStream());

            int totalLength = 0;
            if (isSampl1) {
                // for SAMPL1 the format is LLLL LLZZDATA1 LLZZDATA2 ... CSM
                totalLength = in.readInt();
            // read total length
                totalLength = (int) in.readShort();
            // read ZZ
            in.readShort();
            } else {
                // read total length
                totalLength = (int) in.readShort();
                // read ZZ
                in.readShort();
            }
            byte[] segmentBytes = new byte[totalLength - 4];
            in.readFully(segmentBytes);

            // check first 8 bytes for identifier (exitID)
            if (totalLength < 12) {
                identifier = new String(segmentBytes, 0, (totalLength - 4),
                                        irmHeaderEncoding);
            }
            else {
                identifier = new String(segmentBytes, 0, 8, irmHeaderEncoding);
            }
            String ebcdic_reqmod = new String(ascii_reqmod.getBytes("cp500"),
                                              encoding);
            String ebcdic_reqsts = new String(ascii_reqsts.getBytes("cp500"),
                                              encoding);
            String ebcdic_csmoky = new String(ascii_csmoky.getBytes("cp500"),
                                              encoding);

            // check first segment for possible errors / alerts
            if (identifier.equals("*REQMOD*") ||
                identifier.equals(ebcdic_reqmod)) {
                // read mod name
                String modName = new String(segmentBytes, 8, 8,
                                            irmHeaderEncoding);
                // add mod name to segment vector
                segments.add(identifier + modName);
            }
            else if (identifier.equals("*REQSTS*") ||
                     identifier.equals(ebcdic_reqsts)) {

                // read return code and reason code
                int returnCode = NET2INT(segmentBytes, 8);
                int reasonCode = NET2INT(segmentBytes, 12);
                returnCodeTxt = getReturn(returnCode);
                reasonTxt = getReason(reasonCode);

                // print them
                StringBuffer str = new StringBuffer(100);
                str.append("*REQSTS* from IMS Connect - Return Code: " +
                           returnCode + " Reason Code: " + reasonCode + "\n");
                str.append("For sample IBM user exits, Return Code " +
                           returnCode + " = " + returnCodeTxt);
                str.append("For standard IBM user exits, Reason Code " +
                           reasonCode + " = " + reasonTxt);

                // if acknowledgement, some return codes are valid so log as info,
                // otherwise log as error
                if (isAck) {
                    switch (returnCode) {
                        case  4: // Exit request error message sent to client before socket termination.
                        case 32: // IRM_TIMER value has expired. and the socket is disconnected by IMS Connect.
                        case 36: // A default IRM_TIMER value has expired. The socket is disconnected by IMS Connect.
                        case 40: // IRM_TIMER value has expired.  The socket remains connected.
                        case 44: // Cancel Timer has completed successfully.
                            break;
                        default: // All others are "errors"
                            str.insert(9, "ERROR ");
                    }
                } else {
                }

                // add them to the segment vector
                segments.add(identifier + returnCode);
                segments.add(identifier + reasonCode);
                // return since there should be no more data
                return segments;
            }
            else {
                // this is a data segment
                segmentData = new String(segmentBytes, replyDataEncoding);
				if (mLogger.isLoggable(Level.INFO)) 
					mLogger.log(Level.INFO, "Segment Data : " + segmentData);
                segments.add(segmentData);
                byteVector.add(segmentBytes);
            }

            // continue trying to read in data till we come across *CSMOKY*

            identifier = "";

            while (! (identifier.equals("*CSMOKY*") ||
                      identifier.equals(ebcdic_csmoky))) {
                // read next segment
                // read LL, ZZ

                int recordLength = 0;
                recordLength = (int)in.readShort();
                in.readShort();
                segmentBytes = new byte[recordLength - 4];
                in.readFully(segmentBytes);

                // check first 8 bytes for identifier (exitID)
                if (totalLength < 12) {
                    identifier = new String(segmentBytes, 0, (totalLength - 4),
                                            irmHeaderEncoding);
                }
                else {
                    identifier = new String(segmentBytes, 0, 8,
                                            irmHeaderEncoding);
                }
                if (identifier.equals("*CSMOKY*") ||
                    identifier.equals(ebcdic_csmoky)) {
                    segments.add(identifier);
                }
                else {
                    segmentData = new String(segmentBytes, replyDataEncoding);
					if (mLogger.isLoggable(Level.INFO)) 
						mLogger.log(Level.INFO, "Segment Data : " + segmentData);
                    segments.add(segmentData);
                    byteVector.add(segmentBytes);
                }
            }

        }
        catch (Exception e) {
        	String errMsg = mMessages.getString("IMSBC-E00811.Client_Receive_Failed", new Object[] { e.toString()});
        	mLogger.log(Level.INFO, errMsg);
			AlertsUtil.getAlerter().critical(errMsg, 
									IMSBindingComponent.SHORT_DISPLAY_NAME, 
									null, 
									AlertsUtil.getServerType(),
									AlertsUtil.COMPONENT_TYPE_BINDING,
									NotificationEvent.OPERATIONAL_STATE_RUNNING, 
									NotificationEvent.EVENT_TYPE_ALERT,
									"IMSBC-E00811");
        	connected = false;
        	throw new IMSException(errMsg);
        }

        // return segment vector
        return segments;
    }
       
    /**
     * Returns text string for Return Code.
     * These are the return codes for the default sample IBM user exits
     */
    private String getReturn(int code) {

        switch (code) {
            case 4:
                return
                    "Exit request error message sent to client before socket termination";
            case 8:
                return "Error detected by IMS Connect";
            case 9:
                return "Contents of buffer invalid";
            case 12:
                return "Error returned by IMS OTMA";
            case 16:
                return
                    "Error returned by IMS OTMA, with sense code returned in the Reason Code field";
            case 24:
                return "SCI error detected";
            case 28:
                return "OM error detected";
            case 32:
                return "IRM_TIMER value has expired.  The Reason Code contains the value of the IRM_TIMER and the socket is disconnected by IMS Connect";
            case 36:
                return "A default value has expired.  The Reason Code contains the value of the IRM_TIMER";
            case 40:
                return "IRM_TIMER value has expired. The reason code value is the value of the IRM_TIMER. The socket remains connected.";
            case 44:
                return "Cancel Timer has completed successfully";
            default:
                return "UNKNOWN";
        }
    }

    /**
     * Returns text string for Reason Code.
     * These are the reason codes for the "Standard" IBM user exits
     */
    private String getReason(int code) {

        switch (code) {
            case 4:
                return "Input data exceeds buffer size";
            case 5:
                return "Negative length value";
            case 6:
                return "IRM length invalid";
            case 7:
                return "Total message length invalid";
            case 8:
                return "OTMA NAK with no sense code or RC";
            case 9:
                return "Contents of buffer invalid";
            case 10:
                return "Output data exceeds buffer size";
            case 11:
                return "Invalid unicode definition";
            case 12:
                return "Invalid message, no data";
            case 16:
                return "Do not know who client is";
            case 20:
                return "OTMA segment length error";
            case 24:
                return "FIC missing";
            case 28:
                return "LIC missing";
            case 32:
                return "Sequence number error";
            case 34:
            		return "Unable to locate context token";
            case 36:
                return "Protocol error";
            case 40:
                return "Security violation";
            case 44:
                return "Message incomplete";
            case 48:
                return "Incorrect message length";
            case 51:
                return "Security failure - no OTMA security header";
            case 52:
                return
                    "Security failure - no security data in OTMA security header";
            case 53:
                return "Security failure - no password in OTMA security header";
            case 54:
                return "Security failure - no user ID in OTMA security header";
            case 55:
                return "Security failure - no password in OTMA user data and no user ID in OTMA security header";
            case 56:
                return
                    "Duplicate Client ID is used; the Client ID currently in use";
            case 57:
                return "Invalid token is being used - internal error";
            case 58:
                return "Invalid client status - internal error";
            case 59:
            	  return "Cancel Timer completed successfully";
            case 70:
                return "Component not found";
            case 71:
                return "Function not found";
            case 72:
                return "Datastore not found";
            case 73:
                return "IMS Connect in shutdown";
            case 74:
                return "Datastore/IMSplex in stop or close process";
            case 75:
                return "Datastore communication error";
            case 76:
                return "Datastore/IMSplex was stopped by command";
            case 77:
                return
                    "Datastore/IMSplex communication error to pending client";
            case 78:
                return
                    "Security failure.  RACF call failed, IMS Connect call failed";
            case 79:
                return "IMS Connect protocol error";
            case 80:
                return "The IMSplex connection is not active";
            case 93:
                return
                    "Invalid commit mode of 1 specified on the RESUME TPIPE request";
            case 94:
                return "Request";
            case 95:
                return "Conversation";
            case 96:
                return "Request anc conversation";
            case 97:
                return "Deallocate confirmed";
            case 98:
                return "Deallocate abort";
            case 99:
                return "Default reason code";
            default:
                return "UNKNOWN";
        }
    }

    /** Convert a byte[4] in network format (Big Endian) into an integer;
     ** @param bytes - a byte[4] representing a Big Endian integer;
     ** @return -1 - invalid length header;
     ** otherwise, the length envelope of the message;
     **/
    private int NET2INT(byte[] bytes, int offset) {
        int ret = (bytes[offset + 0] & 0x000000ff) << 24;
        ret |= ( (bytes[offset + 1] & 0x000000ff) << 16);
        ret |= ( (bytes[offset + 2] & 0x000000ff) << 8);
        ret |= (bytes[offset + 3] & 0x000000ff);
        return ret;
    }

    /**
     * Pads or truncates the String to the specified length.
     */
    private String stringPad(String string, char padChar, int padLength) {

        // construct a stringbuffer for padding efficiency
        StringBuffer stringBuffer = new StringBuffer(string);

        // pad the stringbuffer if string.length() is less than padLength
        for (int i = 0; i < (padLength - string.length()); i++) {
            stringBuffer.append(padChar);
        }

        // if truncation was necessary, substring will take care of that
        return stringBuffer.substring(0, padLength);
    }
    
    public void setClientID (String clientID) {
        this.clientID = clientID;
    }    

    
}
