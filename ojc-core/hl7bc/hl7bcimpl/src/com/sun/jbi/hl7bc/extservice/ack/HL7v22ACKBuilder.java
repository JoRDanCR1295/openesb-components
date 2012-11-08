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
 * @(#)HL7v22ACKBuilder.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.hl7bc.extservice.ack;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Node;

import com.sun.jbi.hl7bc.HL7Constants;
import com.sun.jbi.hl7bc.extservice.ack.hl7v22.*;


/**
 * This class builds and return HL7 v2.2 compliant ACK
 * 
 * @author S. Nageswara Rao, Raghunadh Teegavarapu
 */
public class HL7v22ACKBuilder implements ACKBuilder, HL7Constants {
    
    private DocumentBuilder db = null;

    private JAXBContext jaxbContext = null;

    private Unmarshaller unmarshaller = null;

    private Marshaller marshaller = null;

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
    
    public HL7v22ACKBuilder() {
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
     * This method builds MSH, MSA and ERR segments and finally returns the ACK
     * 
     * @return Document Dom Node contains an ACK
     * @throws Exception In case any exception occurs
     */
    public Document buildACK() throws Exception {
        // unmarshal MSH instance document into a tree of Java content
        // objects composed of classes from the com.sun.jbi.hl7bc.extservice.ack.hl7v22 package.
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
            msh5.setValue(mReqMSHContent.getMSH3().getValue());
            ackMSHContent.setMSH5(msh5);
        }
        if (mReqMSHContent.getMSH4() != null) {
            // Receiving Facility
            MSH6CONTENT msh6 = new MSH6CONTENT();
            msh6.setValue(mReqMSHContent.getMSH4().getValue());
            ackMSHContent.setMSH6(msh6);
        }
        if (mReqMSHContent.getMSH5() != null) {
            // Sending Application
            MSH3CONTENT msh3 = new MSH3CONTENT();
            msh3.setValue(mReqMSHContent.getMSH5().getValue());
            ackMSHContent.setMSH3(msh3);
        }
        if (mReqMSHContent.getMSH6() != null) {
            // Sending Facility
            MSH4CONTENT msh4 = new MSH4CONTENT();
            msh4.setValue(mReqMSHContent.getMSH6().getValue());
            ackMSHContent.setMSH4(msh4);
        }
        if (mReqMSHContent.getMSH7() != null) {
            //Date/time of Message
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
        CMMSG1CONTENT cmMsg1 = new CMMSG1CONTENT();
        cmMsg1.setValue(ACK);
        msh9.setCMMSG1(cmMsg1);
        ackMSHContent.setMSH9(msh9);
        // Message control ID
        ackMSHContent.setMSH10(mReqMSHContent.getMSH10());
        // Processing ID
        MSH11CONTENT msh11 = new MSH11CONTENT();
        msh11.setValue(mReqMSHContent.getMSH11().getValue());
        ackMSHContent.setMSH11(msh11);
        // Version ID
        MSH12CONTENT msh12 = new MSH12CONTENT();
        msh12.setValue(mReqMSHContent.getMSH12().getValue());
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
        //Expected Sequence Number
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
        //Segment-ID
        CMELD1CONTENT cmELD1Content = new CMELD1CONTENT();
        cmELD1Content.setValue(MSH);
        err1Content.setCMELD1(cmELD1Content);
        //Code Identifying Error
        CMELD4CONTENT cmELD4Content = new CMELD4CONTENT();
        //Identifier
        CE1CONTENT ce1 = new CE1CONTENT();
        if(mErrCode != null && !mErrCode.equals("")) {
            ce1.setValue(mErrCode);
            cmELD4Content.setCE1(ce1);
        }
        //text
        CE2CONTENT ce2 = new CE2CONTENT();
        if(mErrMsg != null &&  !mErrMsg.equals("")) {
            ce2.setValue(mErrMsg);
            cmELD4Content.setCE2(ce2);
        }
        err1Content.setCMELD4(cmELD4Content);
        errContent.getERR1().add(err1Content);
        
        return errContent;
    }

}
