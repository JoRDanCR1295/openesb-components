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
 * @(#)HL7v25ACKBuilder.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.ack;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.Marshaller;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import com.sun.jbi.hl7bc.extservice.ack.hl7v25.*;
import com.sun.jbi.hl7bc.HL7Constants;

import org.w3c.dom.Document;
import org.w3c.dom.Node;




/**
 * This class builds and return HL7 v2.5 compliant ACK
 * 
 * @author T.V.A Raghunadh
 */

public class HL7v25ACKBuilder implements ACKBuilder, HL7Constants {
    
    private DocumentBuilder db = null;

    private JAXBContext jaxbContext = null;

    private Unmarshaller unmarshaller = null;

    private Marshaller marshaller = null;

    private Node mMSHSeg;
   
    private String mAckCode;

    private String mErrMsg;

    private String mErrCode;
    
    private int mExpectdSeqNo = INVALID_SEQNO;
    
    private boolean mEnabledSFT;
    
    private String mSoftwareVendorOrganization;
    
    private String mSoftwareCertifiedVersionOrReleaseNumber;
    
    private String mSoftwareProductName;
    
    private String mSoftwareBinaryID;
    
    private String mSoftwareProductInformation;
    
    private String mSoftwareInstallDate;

    private MSHCONTENT mReqMSHContent;
    
   
   /* static {
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        try {
            db = dbf.newDocumentBuilder();
            // acclerate the JAXBContext Initialization
            System.setProperty("com.sun.xml.bind.v2.runtime.JAXBContextImpl.fastBoot", "true");
            // create a JAXBContext capable of handling class ACKCONTENT.class
            jaxbContext = JAXBContext.newInstance(ACKCONTENT.class);
            // create an Unmarshaller
            unmarshaller = jaxbContext.createUnmarshaller();
            // create a Marshaller
            marshaller = jaxbContext.createMarshaller();
        } catch (Exception exc) {
            exc.printStackTrace();
        }
    }*/

    public HL7v25ACKBuilder() {
        DocumentBuilderFactory dbf = null;
        try {
			dbf = DocumentBuilderFactory.newInstance();
			dbf.setNamespaceAware(true);
            db = dbf.newDocumentBuilder();
            // acclerate the JAXBContext Initialization
            System.setProperty("com.sun.xml.bind.v2.runtime.JAXBContextImpl.fastBoot", "true");
            // create a JAXBContext capable of handling class ACKCONTENT.class
            jaxbContext = JAXBContext.newInstance(ACKCONTENT.class);
            // create an Unmarshaller
            unmarshaller = jaxbContext.createUnmarshaller();
            // create a Marshaller
            marshaller = jaxbContext.createMarshaller();
        } catch (Exception exc) {
            exc.printStackTrace();
        }

    }

    public void setMSHSegment(Node node) {
        this.mMSHSeg = node;
    }

    public void setAcknowledgmentCode(String ackCode) {
        this.mAckCode = ackCode;
    }

    public void setErrorMessage(String errMsg) {
        this.mErrMsg = errMsg;
    }

    public void setErrorCode(String errCode) {
        this.mErrCode = errCode;
    }
    public void setExpectdSeqNo(int expSeqNo) {
       this.mExpectdSeqNo =  expSeqNo;
    }
    /**
     * Set the Validate SFT segment enabled
     */
    public void setSFTEnabled(boolean val) {
        this.mEnabledSFT = val;
    }

    /**
     * Set the Software Vendor Organization
     */
    public void setSoftwareVendorOrganization(String val) {
        this.mSoftwareVendorOrganization = val;
    }

    /**
     * Set the Software Certified Version Or ReleaseNumber
     */
    public void setSoftwareCertifiedVersionOrReleaseNumber(String val) {
        this.mSoftwareCertifiedVersionOrReleaseNumber = val;
    }

    /**
     * Set the Software Product Name
     */
    public void setSoftwareProductName(String val) {
        this.mSoftwareProductName = val;
    }

    /**
     * Set the Software Binary ID
     */
    public void setSoftwareBinaryID(String val) {
        this.mSoftwareBinaryID = val;
    }

    /**
     * Set the Software Product Information
     */
    public void setSoftwareProductInformation(String val) {
        this.mSoftwareProductInformation = val;
    }

   /**
     * Set the Software Install Date
     */
    public void setSoftwareInstallDate(String val) {
        this.mSoftwareInstallDate = val;
    }

    /**
     * This method builds MSH, MSA and ERR segments and finally returns the ACK
     * 
     * @return Document Dom Node contains an ACK
     * @throws Exception In case any exception occurs
     */
    public Document buildACK() throws Exception {
        // unmarshal MSH instance document into a tree of Java content
        // objects composed of classes from the com.sun.jbi.hl7bc.extservice.ack.hl7v25 package.
        JAXBElement<?> mshElement = (JAXBElement<?>) unmarshaller.unmarshal(mMSHSeg);
        mReqMSHContent = ((MSHCONTENT) mshElement.getValue());
        // unmarshal SFT instance document into a tree of Java content if SFT enabled
        SFTCONTENT sftContent = null;
        if(mEnabledSFT){
        	sftContent = buildSFT();
        }
        MSHCONTENT mshContent = buildMSH();
        MSACONTENT msaContent = buildMSA();
        ERRCONTENT errContent = buildERR();
        // create ACKCONTENT instance
        ACKCONTENT ackContent = new ACKCONTENT();
        ackContent.setMSH(mshContent);
        if(sftContent != null){
        	ackContent.getSFT().add(sftContent);
        }
        ackContent.setMSA(msaContent);
        if (mAckCode.equals(ACKBuilder.APP_ERROR) || mAckCode.equals(ACKBuilder.APP_REJECT)
                || mAckCode.equals(ACKBuilder.COMMIT_REJECT) || mAckCode.equals(ACKBuilder.COMMIT_ERROR)) {
           ackContent.getERR().add(errContent);
        }
        QName ackQName = new QName(mshElement.getName().getNamespaceURI(), ACK);
        JAXBElement ackElement = new JAXBElement(ackQName, ACKCONTENT.class, ackContent);
        Document doc = db.newDocument();
        marshaller.marshal(ackElement, doc);
        return doc;
    }

    /**
     * This method builds and return MSH segment
     * 
     * @return MSHCONTENT MSH segment of the Acknowledgment
     * @throws Exception In case any exception occurs
     */
    private MSHCONTENT buildMSH() throws Exception {
        MSHCONTENT ackMSHContent = new MSHCONTENT();
        // Field seperator
        ackMSHContent.setMSH1(mReqMSHContent.getMSH1());
        // Encoding characters
        ackMSHContent.setMSH2(mReqMSHContent.getMSH2());
        if (mReqMSHContent.getMSH3() != null) {
            // Receiving Application
            MSH5CONTENT msh5 = new MSH5CONTENT();
            msh5.setHD1(mReqMSHContent.getMSH3().getHD1());
            msh5.setHD2(mReqMSHContent.getMSH3().getHD2());
            msh5.setHD3(mReqMSHContent.getMSH3().getHD3());
            ackMSHContent.setMSH5(msh5);
        }
        if (mReqMSHContent.getMSH4() != null) {
            // Receiving Facility
            MSH6CONTENT msh6 = new MSH6CONTENT();
            msh6.setHD1(mReqMSHContent.getMSH4().getHD1());
            msh6.setHD2(mReqMSHContent.getMSH4().getHD2());
            msh6.setHD3(mReqMSHContent.getMSH4().getHD3());
            ackMSHContent.setMSH6(msh6);
        }
        if (mReqMSHContent.getMSH5() != null) {
            // Sending Application
            MSH3CONTENT msh3 = new MSH3CONTENT();
            msh3.setHD1(mReqMSHContent.getMSH5().getHD1());
            msh3.setHD2(mReqMSHContent.getMSH5().getHD2());
            msh3.setHD3(mReqMSHContent.getMSH5().getHD3());
            ackMSHContent.setMSH3(msh3);
        }
        if (mReqMSHContent.getMSH6() != null) {
            // Sending Facility
            MSH4CONTENT msh4 = new MSH4CONTENT();
            msh4.setHD1(mReqMSHContent.getMSH6().getHD1());
            msh4.setHD2(mReqMSHContent.getMSH6().getHD2());
            msh4.setHD3(mReqMSHContent.getMSH6().getHD3());
            ackMSHContent.setMSH4(msh4);
        }
        if (mReqMSHContent.getMSH7() != null) {
            // Date/time of Message
            MSH7CONTENT msh7 = new MSH7CONTENT();
            msh7.setTS1(mReqMSHContent.getMSH7().getTS1());
            msh7.setTS2(mReqMSHContent.getMSH7().getTS2());
            ackMSHContent.setMSH7(msh7);
        }
        // Message Type
        MSH9CONTENT msh9 = new MSH9CONTENT();
        MSG1CONTENT msg1 = new MSG1CONTENT();
        msg1.setValue(ACK);
        msh9.setMSG1(msg1);
        ackMSHContent.setMSH9(msh9);
        // Message control ID
        ackMSHContent.setMSH10(mReqMSHContent.getMSH10());
        // Processing ID
        MSH11CONTENT msh11 = new MSH11CONTENT();
        msh11.setPT1(mReqMSHContent.getMSH11().getPT1());
        msh11.setPT2(mReqMSHContent.getMSH11().getPT2());
        ackMSHContent.setMSH11(msh11);
        // Version ID
        MSH12CONTENT msh12 = new MSH12CONTENT();
        msh12.setVID1(mReqMSHContent.getMSH12().getVID1());
        msh12.setVID2(mReqMSHContent.getMSH12().getVID2());
        msh12.setVID3(mReqMSHContent.getMSH12().getVID3());
        ackMSHContent.setMSH12(msh12);
         // Sequence number 
         if (mReqMSHContent.getMSH13() != null) { 
            ackMSHContent.setMSH13(mReqMSHContent.getMSH13()); 
         }
        return ackMSHContent;
    }

    /**
     * This method builds and return MSA segment
     * 
     * @return MSACONTENT MSA segment of the Acknowledgment
     * @throws Exception In case any exception occurs
     */
    private MSACONTENT buildMSA() throws Exception {
        MSACONTENT msaContent = new MSACONTENT();
        // Acknowledgment Code
        MSA1CONTENT msa1 = new MSA1CONTENT();
        msa1.setValue(mAckCode);
        // Message Control ID
        MSA2CONTENT msa2 = new MSA2CONTENT();
        msa2.setValue(mReqMSHContent.getMSH10().getValue());
        msaContent.setMSA1(msa1);
        msaContent.setMSA2(msa2);
        if(mExpectdSeqNo != INVALID_SEQNO) {
            MSA4CONTENT msa4 = new MSA4CONTENT();
            msa4.setValue(""+mExpectdSeqNo);
            msaContent.setMSA4(msa4);
        }
        return msaContent;
    }

    /**
     * This method builds and return ERR segment
     * 
     * @return ERRCONTENT ERR segment of the Acknowledgment
     * @throws Exception In case any exception occurs
     */
    private ERRCONTENT buildERR() throws Exception {
        ERRCONTENT errContent = new ERRCONTENT();
        ERR1CONTENT err1Content = new ERR1CONTENT();
        ELD1CONTENT eld1 = new ELD1CONTENT();
        eld1.setValue(MSH);
        err1Content.setELD1(eld1);
        ELD4CONTENT eld4 = new ELD4CONTENT();
        CE1CONTENT ce1 = new CE1CONTENT();
        if(mErrCode != null && !mErrCode.equals("")) {
            ce1.setValue(mErrCode);
            eld4.setCE1(ce1);
        }
        CE2CONTENT ce2 = new CE2CONTENT();
        if(mErrMsg != null &&  !mErrMsg.equals("")) {
            ce2.setValue(mErrMsg);
            eld4.setCE2(ce2);
        }
        err1Content.setELD4(eld4);
        errContent.getERR1().add(err1Content);
        return errContent;
    }
    
    /**
     * This method builds and return SFT segment
     * 
     * @return SFTCONTENT SFT segment of the Acknowledgment
     * @throws Exception In case any exception occurs
     */
    private SFTCONTENT buildSFT() throws Exception {
        SFTCONTENT ackSFTContent = new SFTCONTENT();
        // Software Vendor Organization
        SFT1CONTENT sft1 = new SFT1CONTENT();
        XON1CONTENT xon1 = new XON1CONTENT();
        xon1.setValue(mSoftwareVendorOrganization);
        sft1.setXON1(xon1);
        ackSFTContent.setSFT1(sft1);

        //Software Certified Version Or Release Number
        SFT2CONTENT sft2 = new SFT2CONTENT();
        sft2.setValue(mSoftwareCertifiedVersionOrReleaseNumber);
        ackSFTContent.setSFT2(sft2);
        
        //Software Product Name
        SFT3CONTENT sft3 = new SFT3CONTENT();
        sft3.setValue(mSoftwareProductName);
        ackSFTContent.setSFT3(sft3);
        
        //Software Binary ID
        SFT4CONTENT sft4 = new SFT4CONTENT();
        sft4.setValue(mSoftwareBinaryID);
        ackSFTContent.setSFT4(sft4);

        //Software Product Information
        SFT5CONTENT sft5 = new SFT5CONTENT();
        EscapeType escType = new EscapeType();
        escType.setV(mSoftwareProductInformation);
        //TX tx = new TX();
        //tx.getContent().add(escType);
        sft5.getContent().add(mSoftwareProductInformation);
        ackSFTContent.setSFT5(sft5);
       
        //Software Installed Date
        SFT6CONTENT sft6 = new SFT6CONTENT();
        TS1CONTENT ts1 = new TS1CONTENT();
        ts1.setValue(mSoftwareInstallDate);
        sft6.setTS1(ts1);
        ackSFTContent.setSFT6(sft6);
        
        return ackSFTContent;
    }

}
