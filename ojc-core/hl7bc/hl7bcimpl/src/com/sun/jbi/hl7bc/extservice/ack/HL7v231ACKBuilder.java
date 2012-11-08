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
 * @(#)HL7v231ACKBuilder.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.ack;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.Marshaller;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import com.sun.jbi.hl7bc.extservice.ack.hl7v231.*;
import com.sun.jbi.hl7bc.HL7Constants;

import org.w3c.dom.Document;
import org.w3c.dom.Node;

/**
 * This class builds and return HL7 v2.3.1 compliant ACK
 * 
 * @author S. Nageswara Rao, Raghunadh Teegavarapu
 */

public class HL7v231ACKBuilder implements ACKBuilder, HL7Constants {

    // private static HL7v231ACKBuilder singleton = null;
    private  DocumentBuilder db = null;

    private  JAXBContext jaxbContext = null;

    private Unmarshaller unmarshaller = null;

    private  Marshaller marshaller = null;

    private Node mMSHSeg;

    private String mAckCode;

    private String mErrMsg;

    private String mErrCode;
    private int mExpectdSeqNo = INVALID_SEQNO;

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
	public HL7v231ACKBuilder() {
		
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

    /*
     * public static HL7v231ACKBuilder getInstance() { if (singleton == null) { singleton = new
     * HL7v231ACKBuilder(); } return singleton; }
     */

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
     * This method builds MSH, MSA and ERR segments and finally returns the ACK
     * 
     * @return Document Dom Node contains an ACK
     * @throws Exception In case any exception occurs
     */
    public Document buildACK() throws Exception {
        // unmarshal MSH instance document into a tree of Java content
        // objects composed of classes from the com.sun.jbi.hl7bc.extservice.ack.hl7v231 package.
        JAXBElement<?> mshElement = (JAXBElement<?>) unmarshaller.unmarshal(mMSHSeg);
        mReqMSHContent = ((MSHCONTENT) mshElement.getValue());
        MSHCONTENT mshContent = buildMSH();
        MSACONTENT msaContent = buildMSA();
        ERRCONTENT errContent = buildERR();
        // create ACKCONTENT instance
        ACKCONTENT ackContent = new ACKCONTENT();
        ackContent.setMSH(mshContent);
        ackContent.setMSA(msaContent);
        if (mAckCode.equals(ACKBuilder.APP_ERROR) || mAckCode.equals(ACKBuilder.APP_REJECT)
                || mAckCode.equals(ACKBuilder.COMMIT_REJECT) || mAckCode.equals(ACKBuilder.COMMIT_ERROR)) {
            ackContent.setERR(errContent);
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
        /*
         * if (mReqMSHContent.getMSH8() != null) { // Security
         * ackMSHContent.setMSH8(mReqMSHContent.getMSH8()); }
         */
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
        /*
         if (mReqMSHContent.getMSH14() !=
         * null) { // Continuation Pointer ackMSHContent.setMSH14(mReqMSHContent.getMSH14()); } if
         * (mReqMSHContent.getMSH15() != null) { // Accept Acknowledgment Type
         * ackMSHContent.setMSH15(mReqMSHContent.getMSH15()); } if (mReqMSHContent.getMSH16() !=
         * null) { // Application Acknowledgment Type
         * ackMSHContent.setMSH16(mReqMSHContent.getMSH16()); } if (mReqMSHContent.getMSH17() !=
         * null) { // Country Code ackMSHContent.setMSH17(mReqMSHContent.getMSH17()); } if
         * (mReqMSHContent.getMSH18() != null) { // Character Set
         * ackMSHContent.getMSH18().addAll(mReqMSHContent.getMSH18()); } if
         * (mReqMSHContent.getMSH19() != null) { // Prinicipal Language Of Message MSH19CONTENT
         * msh19 = new MSH19CONTENT(); msh19.setCE1(mReqMSHContent.getMSH19().getCE1());
         * msh19.setCE2(mReqMSHContent.getMSH19().getCE2());
         * msh19.setCE3(mReqMSHContent.getMSH19().getCE3());
         * msh19.setCE4(mReqMSHContent.getMSH19().getCE4());
         * msh19.setCE5(mReqMSHContent.getMSH19().getCE5());
         * msh19.setCE6(mReqMSHContent.getMSH19().getCE6()); ackMSHContent.setMSH19(msh19); } if
         * (mReqMSHContent.getMSH20() != null) { // Alternate Character Set Handling Scheme
         * ackMSHContent.setMSH20(mReqMSHContent.getMSH20()); }
         */
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

}
