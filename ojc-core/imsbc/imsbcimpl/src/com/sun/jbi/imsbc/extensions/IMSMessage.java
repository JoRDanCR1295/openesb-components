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

package com.sun.jbi.imsbc.extensions;

import java.io.Serializable;
import javax.wsdl.BindingOperation;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 * Represents ims:message extensibility element
 * 
 * @author Sun Microsystems
 */
public class IMSMessage implements ExtensibilityElement, Serializable {

    private static final long serialVersionUID = 1L;

    public static final String USE_TYPE_LITERAL = "literal";

    public static final String USE_TYPE_ENCODED = "encoded";

    private QName fieldElementType = IMSConstants.QNAME_OPERATION;

    private BindingOperation bindingOp;

    private Boolean fieldRequired = null;

    private String mep = null;
    
    private int irmLen = 80;
    
    private String irmId = "*SAMPL1*";
    
    private String irmTimer = ".25 SEC";
    
    private String irmSocket = "Persistent";
    
    private String irmClientId = null;
    
    private String irmMod = "NO_MFS";
    
    private String irmCommitMode = "COMMIT_MODE_1";
    
    private String irmSyncLevel = "SYNC_LEVEL_NONE";
    
    private String irmAck = "NO_ACK";
    
    private String irmFlow = "NO_AUTO_FLOW";
    
    private String irmTranCode = null;
    
    private String irmTranCodeSrc = "CFG";
    
    private String irmDestId = null ;
    
    private String irmLterm = null ;
    
    private String irmRacfGrpName = null;
    
    private String irmRacfUserId = null;
    
    private String irmRacfPwd = null;
    
    private String irmHeaderEncod = "ISO-8859-1";
    
    private String sendDataEncod = "NO TRANSLATION";
    
    private String replyDataEncod = "ISO-8859-1";
    
    private String useType = USE_TYPE_LITERAL; // default

    private String encodingStyle;

    private String messagePart;

    /**
     * Get the elementType attribute value
     * 
     * @return ElementType of ims:message wsdl extension element
     */
    public QName getElementType() {
        return fieldElementType;
    }

    /**
     * Set the elementType attribute
     * 
     * @param ElementType of ims:message wsdl extension element
     */
    public void setElementType(QName elementType) {
        this.fieldElementType = elementType;
    }


    /**
     * Get the BindingOperation attribute value
     * 
     * @return BindingOperation of the ims:extension wsdl element
     */
    public BindingOperation getBindingOp() {
        return bindingOp;
    }

    /**
     * Set the BindingOperation attribute value
     * 
     * @param BindingOperation for the ims:message wsdl extension element
     */
    public void setBindingOp(BindingOperation bindingOp) {
        this.bindingOp = bindingOp;
    }

    /**
     * Get the FileElementType property
     * 
     * @return QName
     */
    public QName getFieldElementType() {
        return fieldElementType;
    }

    /**
     * Set the FileElementType property
     * 
     * @param QName
     */
    public void setFieldElementType(QName fieldElementType) {
        this.fieldElementType = fieldElementType;
    }

    /**
     * Get the required property
     * 
     * @return Boolean
     */
    public Boolean getRequired() {
        return fieldRequired;
    }

    /**
     * Set the required property
     * 
     * @param Boolean
     */
    public void setRequired(Boolean fieldRequired) {
        this.fieldRequired = fieldRequired;
    }

    /**
     * Get the MessageExchangePattern value
     * 
     * @return message exchange pattern
     */
    public String getMep() {
        return mep;
    }

    /**
     * Set the MessageExchangePattern value
     * 
     * @param message extension pattern
     */
    public void setMep(String mep) {
        this.mep = mep;
    }

	public String getIrmAck() {
		return irmAck;
	}

	public void setIrmAck(String irmAck) {
		this.irmAck = irmAck;
	}

	public String getIrmClientId() {
		return irmClientId;
	}

	public void setIrmClientId(String irmClientId) {
		this.irmClientId = irmClientId;
	}

	public String getIrmCommitMode() {
		return irmCommitMode;
	}

	public void setIrmCommitMode(String irmCommitMode) {
		this.irmCommitMode = irmCommitMode;
	}

	public String getIrmDestId() {
		return irmDestId;
	}

	public void setIrmDestId(String irmDestId) {
		this.irmDestId = irmDestId;
	}

	public String getIrmFlow() {
		return irmFlow;
	}

	public void setIrmFlow(String irmFlow) {
		this.irmFlow = irmFlow;
	}

	public String getIrmHeaderEncod() {
		return irmHeaderEncod;
	}

	public void setIrmHeaderEncod(String irmHeaderEncod) {
		this.irmHeaderEncod = irmHeaderEncod;
	}

	public String getIrmId() {
		return irmId;
	}

	public void setIrmId(String irmId) {
		this.irmId = irmId;
	}

	public int getIrmLen() {
		return irmLen;
	}

	public void setIrmLen(int irmLen) {
		this.irmLen = irmLen;
	}

	public String getIrmLterm() {
		return irmLterm;
	}

	public void setIrmLterm(String irmLterm) {
		this.irmLterm = irmLterm;
	}

	public String getIrmMod() {
		return irmMod;
	}

	public void setIrmMod(String irmMod) {
		this.irmMod = irmMod;
	}

	public String getIrmRacfGrpName() {
		return irmRacfGrpName;
	}

	public void setIrmRacfGrpName(String irmRacfGrpName) {
		this.irmRacfGrpName = irmRacfGrpName;
	}

	public String getIrmRacfPwd() {
		return irmRacfPwd;
	}

	public void setIrmRacfPwd(String irmRacfPwd) {
		this.irmRacfPwd = irmRacfPwd;
	}

	public String getIrmRacfUserId() {
		return irmRacfUserId;
	}

	public void setIrmRacfUserId(String irmRacfUserId) {
		this.irmRacfUserId = irmRacfUserId;
	}

	public String getIrmSocket() {
		return irmSocket;
	}

	public void setIrmSocket(String irmSocket) {
		this.irmSocket = irmSocket;
	}

	public String getIrmSyncLevel() {
		return irmSyncLevel;
	}

	public void setIrmSyncLevel(String irmSyncLevel) {
		this.irmSyncLevel = irmSyncLevel;
	}

	public String getIrmTimer() {
		return irmTimer;
	}

	public void setIrmTimer(String irmTimer) {
		this.irmTimer = irmTimer;
	}

	public String getIrmTranCode() {
		return irmTranCode;
	}

	public void setIrmTranCode(String irmTranCode) {
		this.irmTranCode = irmTranCode;
	}

	public String getIrmTranCodeSrc() {
		return irmTranCodeSrc;
	}

	public void setIrmTranCodeSrc(String irmTranCodeSrc) {
		this.irmTranCodeSrc = irmTranCodeSrc;
	}

	public String getReplyDataEncod() {
		return replyDataEncod;
	}

	public void setReplyDataEncod(String replyDataEncod) {
		this.replyDataEncod = replyDataEncod;
	}

	public String getSendDataEncod() {
		return sendDataEncod;
	}

	public void setSendDataEncod(String sendDataEncod) {
		this.sendDataEncod = sendDataEncod;
	}
	
    public String getMessagePart() {
        return messagePart;
    }

    public void setMessagePart(String messagePart) {
        this.messagePart = messagePart;
    }
    
    public void setUseType(String useType) {
        this.useType = useType;
    }

    public String getUseType() {
        return this.useType;
    }

    public void setEncodingStyle(String encodingStyle) {
        this.encodingStyle = encodingStyle;
    }

    public String getEncodingStyle() {
        return this.encodingStyle;
    }    

	public String toString() {
		StringBuffer strBuf = new StringBuffer(super.toString());
		strBuf.append("\nIMSMessage " + fieldElementType + ":");
		strBuf.append("\nRequired=" + fieldRequired);
		strBuf.append("\nIRMLEN=" + irmLen);
		strBuf.append("\nIRMID=" + irmId);
		strBuf.append("\nTIMER=" + irmTimer);
		strBuf.append("\nSOCKET=" + irmSocket);
		strBuf.append("\nCLIENTID=" + irmClientId);
		strBuf.append("\nMOD=" + irmMod);
		strBuf.append("\nCOMMITMODE=" + irmCommitMode);
		strBuf.append("\nSYNCLEVEL=" + irmSyncLevel);
		strBuf.append("\nACK=" + irmAck);
		strBuf.append("\nFLOW=" + irmFlow);
		strBuf.append("\nTRANCODE=" + irmTranCode);
		strBuf.append("\nTRANCODESRC=" + irmTranCodeSrc);
		strBuf.append("\nDESTID=" + irmDestId);
		strBuf.append("\nIRMID=" + irmLterm);	
		strBuf.append("\nRAFGRPNAME=" + irmRacfGrpName);
		strBuf.append("\nRACFUSERID=" + irmRacfUserId);	
		strBuf.append("\nRACFPWD=" + irmRacfPwd);
		strBuf.append("\nHEADERENCODING=" + irmHeaderEncod);	
		strBuf.append("\nSENDDATAENCODING=" + sendDataEncod);
		strBuf.append("\nREPLYDATAENCODING=" + replyDataEncod);	
		strBuf.append("\nENCODINGSTYLE=" + encodingStyle);	
		strBuf.append("\nUSE=" + useType);	
		strBuf.append("\nMESSAGEPART=" + messagePart);	
		return strBuf.toString();
    }


}
