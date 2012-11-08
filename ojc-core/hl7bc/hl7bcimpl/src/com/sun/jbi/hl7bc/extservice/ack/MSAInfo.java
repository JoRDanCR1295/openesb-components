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
 * @(#)MSAInfo.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.ack;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Unmarshaller;

import org.w3c.dom.Node;

import com.sun.jbi.hl7bc.HL7Constants;
import com.sun.jbi.hl7bc.extservice.ack.hl7v25.*;

/**
 * Wrapper around MSA segment
 * 
 * @author S. Nageswara Rao
 */

public class MSAInfo implements HL7Constants {

   // private static MSAInfo singleton = null;

    private static JAXBContext jaxbContext = null;

    private Unmarshaller unmarshaller = null;

    private Node mMSASeg;

    private MSACONTENT mMSAContent;

   /* static {
        try {
            // acclerate the JAXBContext Initialization
            System.setProperty("com.sun.xml.bind.v2.runtime.JAXBContextImpl.fastBoot", "true");
            // create a JAXBContext capable of handling class MSACONTENT.class
            jaxbContext = JAXBContext.newInstance(MSACONTENT.class);
            // create an Unmarshaller
            unmarshaller = jaxbContext.createUnmarshaller();
        } catch (Exception exc) {
            exc.printStackTrace();
        }
    }*/
    public MSAInfo() {
        try {
            // acclerate the JAXBContext Initialization
            System.setProperty("com.sun.xml.bind.v2.runtime.JAXBContextImpl.fastBoot", "false");
            // create a JAXBContext capable of handling class MSACONTENT.class
            if (jaxbContext == null) {
                jaxbContext = JAXBContext.newInstance(MSACONTENT.class);
            }
            // create an Unmarshaller
            unmarshaller = jaxbContext.createUnmarshaller();
        } catch (Exception exc) {
            exc.printStackTrace();
        }
    }

   /* public static MSAInfo getInstance() {
        if (singleton == null) {
            singleton = new MSAInfo();
        }
        return singleton;
    }*/

    public void setMSASegment(Node node) {
        this.mMSASeg = node;
    }

    /**
     * Unmarshal the Dom Message to Java Bean
     * 
     * @throws Exception
     */
    public void unmarshal() throws Exception {
        // unmarshal MSA instance document into a tree of Java content
        // objects composed of classes from the com.sun.jbi.hl7bc.extservice.ack.hl7v231 package.
        JAXBElement<?> msaElement = (JAXBElement<?>) unmarshaller.unmarshal(mMSASeg);
        mMSAContent = ((MSACONTENT) msaElement.getValue());
    }

    /**
     * Returns acknowledgment code. It should be one of the values AA, AR or AE in Original
     * Acknowledgment mode. CA, CR or CE in Enhanced Acknowledgment mode
     * 
     * @return
     */
    public String getAcknowledgmentCode() {
        return mMSAContent.getMSA1().getValue();
    }
    
    /**
     * Returns the Message Contorl ID
     * @return String MSA-2-MessageControlId
     */
    public String getMsgControlID() {
        return mMSAContent.getMSA2().getValue();
    }
    
    /**
     * Returns the Sequence Number
     * @return String MSA-4-SequenceNumber
     */
    public int getSequenceNumner() {
        int expSeqNo = INVALID_SEQNO;
        String msaSeqno = mMSAContent.getMSA4().getValue();
        if (msaSeqno == null || msaSeqno.equals("")) {
            return expSeqNo;
        }
        try {
            expSeqNo = Integer.parseInt(msaSeqno);
        } catch (Exception e) {
            return expSeqNo;
        }
        return expSeqNo;
    }

}
